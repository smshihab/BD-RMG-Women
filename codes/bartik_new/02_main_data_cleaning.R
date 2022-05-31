library(pacman)

p_load(tidyverse, sf, janitor, sp)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


### eexports
trade <- read_csv("comtrade.csv") %>%
  clean_names() %>%
  group_by(year, commodity_code, trade_flow) %>%
  summarise(value = sum(trade_value_us)) %>%
  filter(year %in% c(1991, 2001, 2011)) %>%
  ungroup()

exports <- trade %>%
  filter(commodity_code == 61 & trade_flow == "Export" |
           commodity_code == 62 & trade_flow == "Export")

import <- trade %>%
  filter(trade_flow == "Import") %>%
  group_by(commodity_code, year) %>%
  summarise(value = sum(value))%>%
  ungroup()

import_knit <- import %>% filter(commodity_code == 60)
import_wov <- import %>% filter(commodity_code > 100) %>%
  group_by(year) %>% summarise(value = sum(value))

exports <- data.frame(
  year = c(1991, 2001, 2011),
  knit_import = import_knit$value,
  wov_import = import_wov$value,
  knit_ex = c(152276422, 1281533792, 9936304901),
  wov_ex = c(687361669, 2757655123, 9225733521))

exports <- exports %>%
  mutate(knit = (knit_ex - knit_import),
         woven = (wov_ex - wov_import))

rm(list = setdiff(ls(), "exports"))

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factory_panel.RData")

factory_panel <- factory_panel %>%
  mutate(knit = as.integer(knit), date_est = as.integer(date_est), exist = existence)

# Using factory panel from before
factory_panel <- factory_panel %>%
  mutate(woven = 1-knit)

# Estimating shocks to areas

temp <- factory_panel %>% filter(year >=1985 & year <=2011)

# Estimating total knit and woven capacity in BD

bd_capacity <- temp %>%
  data.frame() %>%
  group_by(year) %>%
  summarise(
    knit_bd = sum(machines*knit*exist, na.rm = T),
    wov_bd = sum(machines*woven*exist, na.rm = T)) %>%
  ungroup()

# Estimating a region's share of knit and woven

  
temp_data <- temp %>%
  group_by(ipum1991, year) %>%
  summarize(knit_machines = sum(exist*machines*knit, na.rm = T), 
            wov_machines = sum(exist*machines*woven, na.rm = T)) %>% ungroup()

temp_data <- left_join(temp_data, bd_capacity, by = "year")

temp_data <- temp_data %>% mutate(knit_coef = knit_machines/knit_bd,
                                  wov_coef = wov_machines/wov_bd)

temp_data <- temp_data %>%
  group_by(ipum1991) %>%
  mutate(
    knit_coefL1 = dplyr::lag(knit_coef, 1),
    knit_coefL2 = dplyr::lag(knit_coef, 2),
    knit_coefL3 = dplyr::lag(knit_coef, 3),
    knit_coefL4 = dplyr::lag(knit_coef, 4),
    knit_coefL5 = dplyr::lag(knit_coef, 5),
    wov_coefL1 = dplyr::lag(wov_coef, 1),
    wov_coefL2 = dplyr::lag(wov_coef, 2),
    wov_coefL3 = dplyr::lag(wov_coef, 3),
    wov_coefL4 = dplyr::lag(wov_coef, 4),
    wov_coefL5 = dplyr::lag(wov_coef, 5)) %>% ungroup()


load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data.RData")

temp_data <- left_join(temp_data, exports %>% select(year, knit, woven), by = "year")


upazila_data_combined <- left_join(upazila_data, temp_data, by = c("ipum1991","year"))

upazila_data_combined <- upazila_data_combined %>%
  arrange(ipum1991, year)

save(upazila_data_combined, file="C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data_combined.RData")

###########



