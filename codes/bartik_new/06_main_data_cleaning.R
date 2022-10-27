library(pacman)

p_load(tidyverse, janitor, conflicted, labelled)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data.RData")
load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/shift_shares.RData")
#load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.RData")

upazila_data %>%
  mutate(density = pop/area91) -> upazila_data

remove_labels(upazila_data) -> upazila_data

data <- upazila_data %>%
  group_by(ipum1991) %>%
  mutate(across(c(starts_with("m_"), f_yrschool1564, age_1564_pop, electrification, urban_share, density), 
                ~dplyr::lag(.), .names = "lag_{.col}")) %>%
  mutate(across(c(starts_with("f_"), fertility_1520, fertility_2130, fertility_3040, married_1520, married_2130, age_1564_share, 
                         electrification, urban_share, density), 
                ~(. -dplyr::lag(.)), .names = "dif_{.col}")) %>%
  select(ipum1991, year, starts_with("lag"), starts_with("dif")) %>% ungroup()

data <- left_join(ungroup(data), autor_shares)

export_vals <- arrange(trade, year)

export_vals %>% select(year, knit, woven) %>%
  mutate(ch_knit = knit - dplyr::lag(knit),ch_woven = woven - dplyr::lag(woven)) -> export_vals

data <- left_join(data, export_vals %>% select(year, ch_knit, ch_woven))

data %>%
  mutate(knit_exposure = dplyr::lag(knit_share)*ch_knit,
         woven_exposure = dplyr::lag(woven_share)*ch_woven,
         export_exposure_total = knit_exposure + woven_exposure,
         export_exposure_pc = export_exposure_total/(lag_age_1564_pop),
         export_exposure_pcpk = export_exposure_total/(1000*(lag_age_1564_pop))) %>%
  filter(year > 1995) -> data

save(data, file="C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/final_data.RData")



