
rm(list = ls())
options(scipen=999)

# Load packages
library(here)
library(haven)
library(dplyr)
library(sae)
library(tidyr)

# Load data
CSEW_all <- read_dta(here("data/Police_Rating_Trust.dta"))

# Select only variables of interest
# Change variables to numeric format
# Filter out NAs
# Create domain variable
CSEW_all <- CSEW_all %>%
  dplyr::select(wave, gor, sex, age_40, notwhite, urban, imd5, highereduc,
         police_rating_01, police_rating, police_rating_01_v2, weight) %>%
  mutate(across(c(wave, gor, sex, age_40, notwhite, urban, imd5, highereduc,
                  police_rating_01, police_rating, police_rating_01_v2, weight),
                ~ as.numeric(.))) %>%
  filter(if_all(c(wave, gor, sex, age_40, notwhite, urban, imd5, weight),
                ~ !is.na(.))) %>%
  mutate(
    domain = paste(wave, gor, sex, age_40, notwhite, urban, imd5, sep = "_")
  )

# Create numeric domain code
CSEW_all <- CSEW_all %>%
  mutate(domain_num = as.integer(factor(domain)))

# Build domsize variable
domsize <- CSEW_all %>%
  group_by(domain_num) %>%
  summarise(Nd = sum(weight), .groups = "drop") %>%
  as.data.frame()

# Horvitz-Thompson direct estimation binary variable
direct_01 <- sae::direct(
  y       = CSEW_all$police_rating_01,
  dom     = CSEW_all$domain_num,
  sweight = CSEW_all$weight,
  domsize = domsize
)

# Horvitz-Thompson direct estimation ordinal variable
direct_num <- sae::direct(
  y       = CSEW_all$police_rating,
  dom     = CSEW_all$domain_num,
  sweight = CSEW_all$weight,
  domsize = domsize
)

# Merge both datasets
direct <- direct_01 %>%
  rename("Direct_01" = Direct,
         "SD_01" = SD,
         "CV_01" = CV) %>%
  dplyr::select(-SampSize) %>%
  left_join(direct_num, by = "Domain") %>%
  rename("Direct_num" = Direct,
         "SD_num" = SD,
         "CV_num" = CV)

# Left join domain name
domain_name <- CSEW_all %>%
  dplyr::select(domain, domain_num, wave, gor, sex, age_40, notwhite, urban, imd5) %>%
  unique()
direct <- direct %>%
  left_join(domain_name, by = c("Domain" = "domain_num"))

# Set a floor for SD where it is 0 or NA (5th percentile of positive SD)
sd_floors <- direct %>%
  group_by(SampSize) %>%
  summarise(
    floor_SD_01  = quantile(SD_01[SD_01 > 0], 0.05, na.rm = TRUE),
    floor_SD_num = quantile(SD_num[SD_num > 0], 0.05, na.rm = TRUE),
    .groups = "drop"
  )

# Replace SD == 0 values
direct <- direct %>%
  left_join(sd_floors, by = "SampSize") %>%
  mutate(
    SD_01_adj  = ifelse(SD_01 <= 0 | is.na(SD_01),  floor_SD_01,  SD_01),
    SD_num_adj = ifelse(SD_num <= 0 | is.na(SD_num), floor_SD_num, SD_num)
  ) %>%
  dplyr::select(-floor_SD_01, -floor_SD_num)

# Calcualte inverse variance weighting
direct <- direct %>%
  mutate(
    var_01_adj  = SD_01_adj^2,
    var_num_adj = SD_num_adj^2,
    w_01_inv    = 1 / var_01_adj,
    w_num_inv   = 1 / var_num_adj
  )

# Reorder dataset
direct <- direct %>%
  dplyr::select(Domain, domain, wave, gor, sex, age_40, notwhite, urban, imd5,
                SampSize,
                Direct_01, SD_01_adj, var_01_adj, w_01_inv,
                Direct_num, SD_num_adj, var_num_adj, w_num_inv) %>%
  rename("domain_numer" = Domain)

# Create full list of domains (present and not)
waves     <- 2003:2023       # 21 years
gors      <- 1:10            # 10 regions
sexes     <- 0:1             # 2 categories
age_40s   <- 0:1             # 2 categories
notwhites <- 0:1             # 2 categories
urbans    <- 0:1             # 2 categories
imd5s     <- 1:5             # 5 IMD groups

all_domains <- expand_grid(
  wave      = waves,
  gor       = gors,
  sex       = sexes,
  age_40    = age_40s,
  notwhite  = notwhites,
  urban     = urbans,
  imd5      = imd5s
)

all_domains <- all_domains %>%
  mutate(domain = paste(wave, gor, sex, age_40, notwhite, urban, imd5, sep = "_")) %>%
  filter(wave != "2020" & wave != "2021") # COVID

# Left join
direct <- direct %>%
  dplyr::select(-domain_numer, -wave, -gor, -sex, -age_40, -notwhite, -urban, -imd5)
all_domains <- all_domains %>%
  left_join(direct, by = "domain") %>%
  mutate(SampSize = ifelse(is.na(SampSize), 0, SampSize),
         domain= substr(domain, 6, nchar(domain))
         ) %>%
  arrange(domain, wave)

# Check missing data for domains
vars <- c("gor", "sex", "age_40", "notwhite", "urban", "imd5")

all_domains_NA <- lapply(vars, function(v) {
  all_domains %>%
    group_by(wave, .data[[v]]) %>%
    summarise(
      n_total = n(),
      n_NA    = sum(is.na(Direct_01)),
      prop_NA = n_NA / n_total,
      .groups = "drop"
    )
})

names(all_domains_NA) <- vars

all_domains_NA <- bind_rows(
  list(
    gor      = all_domains_NA$gor,
    sex      = all_domains_NA$sex,
    age_40   = all_domains_NA$age_40,
    notwhite = all_domains_NA$notwhite,
    urban    = all_domains_NA$urban,
    imd5     = all_domains_NA$imd5
  ),
  .id = "variable"
)

# Download estimates
write.csv(all_domains, here('data/direct_estimates.csv'))
