library(haven)
library(ggplot2)
library(srvyr)
library(dplyr)

trust <- read_dta("data/Police_Rating_Trust.dta")
# View(head(trust))

# first, replicate the CoP chart:
freq_trust <- trust %>%
  filter(wave > 2002) %>%
  group_by(wave) %>%
  summarise(count = n(),
            proportion_trust = mean(police_rating_01))

  ggplot() +
  geom_point(data = freq_trust, aes(x = wave , y = proportion_trust)) +
  geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust)) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust)) +
  geom_segment(data = data.frame(x = 2019, xend = 2022,
                                 y = freq_trust$proportion_trust[freq_trust$wave == 2019],
                                 yend = freq_trust$proportion_trust[freq_trust$wave == 2022]),
               aes(x = x, xend = xend, y = y, yend = yend),
               linetype = "dashed") +
  scale_x_continuous(breaks = 2003:2023) +
    ylim(c(0,1)) +
  theme_bw()


# Now break it down. Let's start gender

  freq_trust <- trust %>%
    filter(wave > 2002) %>%
    group_by(wave, sex) %>%
    summarise(count = n(),
              proportion_trust = mean(police_rating_01))

  # with weights
  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    group_by(wave, sex) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(sex))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(sex))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(sex))) +
    geom_segment(data = data.frame(x = 2019, xend = 2022,
                                   y = freq_trust %>% filter(sex == 1 & wave == 2019) %>% pull(proportion_trust),
                                   yend = freq_trust %>% filter(sex == 1 & wave == 2022) %>% pull(proportion_trust)),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = "dashed", colour = "#1f78b4") +
    geom_segment(data = data.frame(x = 2019, xend = 2022,
                                   y = freq_trust %>% filter(sex == 0 & wave == 2019) %>% pull(proportion_trust),
                                   yend = freq_trust %>% filter(sex == 0 & wave == 2022) %>% pull(proportion_trust)),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = "dashed", colour = "#a6cee3") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    xlab




  # For 30 year split

  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    group_by(wave, age_30) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(age_30))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(age_30))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(age_30))) +
    geom_segment(data = data.frame(x = 2019, xend = 2022,
                                   y = freq_trust %>% filter(age_30 == 1 & wave == 2019) %>% pull(proportion_trust),
                                   yend = freq_trust %>% filter(age_30 == 1 & wave == 2022) %>% pull(proportion_trust)),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = "dashed", colour = "#1f78b4") +
    geom_segment(data = data.frame(x = 2019, xend = 2022,
                                   y = freq_trust %>% filter(age_30 == 0 & wave == 2019) %>% pull(proportion_trust),
                                   yend = freq_trust %>% filter(age_30 == 0 & wave == 2022) %>% pull(proportion_trust)),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = "dashed", colour = "#a6cee3") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



  # For 40 year split

  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    group_by(wave, age_40) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(age_40))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(age_40))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(age_40))) +
    geom_segment(data = data.frame(x = 2019, xend = 2022,
                                   y = freq_trust %>% filter(age_40 == 1 & wave == 2019) %>% pull(proportion_trust),
                                   yend = freq_trust %>% filter(age_40 == 1 & wave == 2022) %>% pull(proportion_trust)),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = "dashed", colour = "#1f78b4") +
    geom_segment(data = data.frame(x = 2019, xend = 2022,
                                   y = freq_trust %>% filter(age_40 == 0 & wave == 2019) %>% pull(proportion_trust),
                                   yend = freq_trust %>% filter(age_40 == 0 & wave == 2022) %>% pull(proportion_trust)),
                 aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = "dashed", colour = "#a6cee3") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



# urban/rural


  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    group_by(wave, urban, gor) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(urban))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(urban))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(urban))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)), aes(x = wave , y = proportion_trust, colour = as_factor(urban)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~as_factor(gor))


  # ethnicity

  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    group_by(wave, notwhite) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)),
              aes(x = wave , y = proportion_trust, colour = as_factor(notwhite)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    group_by(wave, notwhite, gor) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)),
              aes(x = wave , y = proportion_trust, colour = as_factor(notwhite)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~as_factor(gor))




  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    group_by(wave, notwhite, imd5) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)),
              aes(x = wave , y = proportion_trust, colour = as_factor(notwhite)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~as_factor(imd5))

  # IMD

  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    filter(gor != 10) %>% # remove Wales because they don't have IMD
    group_by(wave, imd) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(imd))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(imd))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(imd))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)),
              aes(x = wave , y = proportion_trust, colour = as_factor(imd)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    filter(gor != 10) %>% # remove Wales because they don't have IMD
    group_by(wave, imd5, gor) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(imd5))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(imd5))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(imd5))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)),
              aes(x = wave , y = proportion_trust, colour = as_factor(imd5)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~as_factor(gor))

# group together

  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    filter(gor != 10) %>% # remove Wales because they don't have IMD
    mutate(imd3 = case_when(imd > 7 ~ "High deprivation",
                            imd <8 & imd > 3 ~ "Medium deprivation",
                            imd < 4 ~ "Low deprivation")) %>%
    group_by(wave, imd3, gor) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(imd3))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(imd3))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(imd3))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)),
              aes(x = wave , y = proportion_trust, colour = as_factor(imd3)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~as_factor(gor))

# white women v non white women

  freq_trust <- trust %>%
    as_survey(weights = c(weight_c)) %>%
    filter(wave > 2002) %>%
    filter(gor != 10) %>% # remove Wales because they don't have IMD
    filter(sex == 0) %>%
    group_by(wave, notwhite, gor) %>%
    summarise(proportion_trust = survey_mean(police_rating_01))

  ggplot() +
    geom_point(data = freq_trust, aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave < 2020), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave >2021), aes(x = wave , y = proportion_trust, colour = as_factor(notwhite))) +
    geom_line(data = freq_trust %>% filter(wave %in% c(2019,2020,2021,2022)),
              aes(x = wave , y = proportion_trust, colour = as_factor(notwhite)),
              linetype = "dashed") +
    scale_x_continuous(breaks = 2003:2023) +
    scale_color_brewer(type = "qual", palette = "Paired") +
    ylim(c(0,1)) +
    theme_bw() +
    ggtitle("Police in the local area do a 'good' or 'excellent' job") +
    ylab("Per cent (agree)") +
    theme(legend.title=element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~as_factor(gor))



  table(trust$wave, trust$police_trust_01)

