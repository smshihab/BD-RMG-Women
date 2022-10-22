library(pacman)

p_load(tidyverse, conflicted, janitor)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.Rdata") 
#, upaz2001 =as.integer(upaz2001),  upaz1991 = as.integer(upaz1991))

factory_upazilas %>%
  mutate(ipum1991 = as.integer(ipum1991), 
         upaz2011 =as.integer(upaz2011)) -> factory_upazilas

# Laksham and comilla sadar of 1991 is united later on

factory_upazilas$ipum1991[factory_upazilas$ipum1991 == 20019072] <- 20019067

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factories.Rdata")

matched_data_01 <- matched_data_01 %>%
  filter(!is.na(fac_type)) %>%
  left_join(factory_upazilas %>% select(upaz2011, ipum1991))

matched_data_09 <- matched_data_09 %>% 
  filter(!is.na(fac_type)) %>%
  left_join(factory_upazilas %>% select(upaz2011, ipum1991))


## Autor et al way of share

total_knit91 <- sum(matched_data_01$machine*matched_data_01$fac_type*matched_data_01$exist91)
total_knit01 <- sum(matched_data_01$machine*matched_data_01$fac_type*matched_data_01$exist01)
total_wove91 <- sum(matched_data_01$machine*(-matched_data_01$fac_type+1)*matched_data_01$exist91)
total_wove01 <- sum(matched_data_01$machine*(-matched_data_01$fac_type+1)*matched_data_01$exist01)

matched_data_01 %>%
  group_by(ipum1991) %>%
  summarise(knit91_share = (sum(machine*fac_type*exist91))/total_knit91,
            wove91_share = (sum(machine*(-fac_type+1)*exist91))/total_wove91,
            knit01_share = (sum(machine*fac_type*exist01))/total_knit01,
            wove01_share = (sum(machine*(-fac_type+1)*exist01))/total_wove01) %>%
  ungroup()-> autor_shares01


#total_knit06 <- sum(matched_data_09$machine*matched_data_09$fac_type*matched_data_09$exist06)
#total_wove06 <- sum(matched_data_09$machine*(-matched_data_09$fac_type+1)*matched_data_09$exist06)

#matched_data_09 %>%
 # group_by(ipum1991) %>%
  #summarise(knit06_share = (sum(machine*fac_type*exist06))/total_knit91,
            wove06_share = (sum(machine*(-fac_type+1)*exist06))/total_wove06) %>%
  #ungroup() -> autor_shares06

autor_shares <- data.frame(ipum1991 = factory_upazilas$ipum1991 %>% unique())

autor_shares <- autor_shares %>%
  left_join(autor_shares01, by ="ipum1991") %>%
  mutate_all(~replace(., is.na(.), 0))

autor_shares %>% select(contains("knit"), ipum1991) %>%
  pivot_longer(!ipum1991, names_to = "year", values_to = "knit_share") %>%
  mutate(year = case_when(year == "knit91_share" ~ 1991,
                          year == "knit01_share" ~ 2001))-> autor_shares_knit

autor_shares %>% select(contains("wov"), ipum1991) %>%
  pivot_longer(!ipum1991, names_to = "year", values_to = "woven_share") %>%
  mutate(year = case_when(year == "wove91_share" ~ 1991,
                          year == "wove01_share" ~ 2001)) -> autor_shares_woven

autor_shares <- left_join(autor_shares_knit, autor_shares_woven, by = c("ipum1991", "year"))
  
rm(list = setdiff(ls(), c("autor_shares")))

### exports
trade <- read_csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/comtrade.csv") %>%
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

export_vals <- exports %>%
  mutate(knit = (knit_ex - knit_import),
         woven = (wov_ex - wov_import))

rm(list = setdiff(ls(), c("export_vals", "autor_shares")))

save(list = c("autor_shares", "export_vals"), file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/shift_shares.RData")


