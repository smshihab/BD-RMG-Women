library(pacman)

p_load(tidyverse, sf, janitor, sp)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


### eexports
trade <- read_csv("./data/comtrade.csv") %>%
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

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data.RData")
load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/shares.RData")
load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.RData")

autor_shares[is.na(autor_shares)] <- 0
bartik_shares[is.na(bartik_shares)] <- 0

autor_shares <- read.csv("hi_autor.csv")
bartik_shares <- read.csv("hi_bartik.csv")

data <- upazila_data %>%
arrange(ipum1991, year) %>%
  group_by(ipum1991) %>%
  mutate(
    
    #lagged controls
    
    urban_lag = dplyr::lag(urban_share),
#    density_lag = dplyr::lag(density),
    elec_lag = dplyr::lag(electrification),
    age_1564_lag = dplyr::lag(age_1564),
    m_educ_lag = dplyr::lag(m_educ),
    f_educ_lag = dplyr::lag(f_educ),
    f39_educ_lag = dplyr::lag(f39_educ),
    
    
    # lag outcomes
    
    m_ind_share_lag = dplyr::lag(m_ind_share),
    m_ind_share2_lag = dplyr::lag(m_ind_share2),
    m_lfp_lag = dplyr::lag(m_lfp),
    f_ind_share2_lag = dplyr::lag(f_ind_share2),
    f_ind_share_lag = dplyr::lag(f_ind_share) ,
    school15_m_lag = dplyr::lag(school15_m),
    school15_f_lag = dplyr::lag(school15_f),
    f_lfp_lag = dplyr::lag(f_lfp),
    f39_ind_share2_lag = dplyr::lag(f39_ind_share2),
    f39_lfp_lag = dplyr::lag(f39_lfp),
    f39_ind_share_lag = dplyr::lag(f39_ind_share)) %>%
  mutate(
    
    # first diff
    
    m_ind_share2_diff = m_ind_share2 - m_ind_share2_lag,
    m_ind_share_diff = m_ind_share - m_ind_share_lag,
    m_lfp_diff = m_lfp - m_lfp_lag,
    f_ind_share2_diff = f_ind_share2 - f_ind_share2_lag,
    f_ind_share_diff = f_ind_share2 - f_ind_share_lag,
    school15_m_diff = school15_m - school15_f_lag,
    school15_f_diff = school15_f - school15_f_lag,
    f_lfp_diff = f_lfp - f_lfp_lag,
    f39_ind_share2_diff = f39_ind_share2 - f39_ind_share2_lag,
    f39_ind_share_diff = f39_ind_share - f39_ind_share_lag,
    f39_lfp_diff = f39_lfp - f39_lfp_lag,
    fertility_39_diff = fertility_39 - dplyr::lag(fertility_39),
    fertility_25_diff = fertility_25 - dplyr::lag(fertility_25)) %>%
  ungroup()

autor_shares <- left_join(autor_shares, exports)

autor_shares <- left_join(autor_shares, factory_upazilas %>%
                            mutate(ipum1991 = as.numeric(ipum1991),
                                   upaz1991 = as.integer(upaz1991)) %>%
                            select(ipum1991, upaz1991))

data_autor <- left_join(data, autor_shares, by = c("year", "ipum1991"))


data_autor <- data_autor %>%
  mutate(autor_knit = knit_share*(knit - dplyr::lag(knit)),
         autor_wove = wove_share*(woven - dplyr::lag(woven)),
         autor_instrume = (autor_knit + autor_wove)/(1000000*age_1564))

####

p_load(broom)

lm(f_lfp_diff ~ autor_instrume + m_lfp_lag + urban_lag, data_autor) %>%
  tidy()



bartik_shares <- left_join(bartik_shares, exports)

bartik_shares <- left_join(bartik_shares, factory_upazilas %>%
                            mutate(ipum1991 = as.numeric(ipum1991),
                                   upaz1991 = as.integer(upaz1991)) %>%
                            select(ipum1991, upaz1991))

data_bartik <- left_join(data, bartik_shares, by = c("year", "ipum1991"))


data_bartik <- data_bartik %>%
  mutate(bartik_knit = knit_share*(knit - dplyr::lag(knit)),
         bartik_wove = wove_share*(woven - dplyr::lag(woven)),
         bartik_instrume = (bartik_knit + bartik_wove)/(1000000*age_1564))


lm(bartik_instrume ~ m_lfp, data_bartik) %>%
  glance()





upazila_data_combined <- left_join(upazila_data, temp_data, by = c("ipum1991","year"))

upazila_data_combined <- upazila_data_combined %>%
  arrange(ipum1991, year)

save(upazila_data_combined, file="C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data_combined.RData")

###########



