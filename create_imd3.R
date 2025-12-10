# create imd3 variable

library(haven)
library(dplyr)

trust <- read_dta("data/Police_Rating_Trust.dta")
# View(head(trust))

trust <- trust %>% mutate(imd3 = case_when(imd > 7 ~ "High deprivation",
                        imd <8 & imd > 3 ~ "Medium deprivation",
                        imd < 4 ~ "Low deprivation"))



