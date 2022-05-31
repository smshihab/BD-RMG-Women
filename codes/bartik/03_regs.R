library(pacman)
p_load(tidyverse)

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/upazila_data_combined.RData")

data <- upazila_data_combined %>%
  arrange(ipum1991, year) %>%
  group_by(ipum1991) %>%
  mutate(
    
    #lagged controls
    
    urban_lag = dplyr::lag(urban_share),
    density_lag = dplyr::lag(density),
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
    f39_ind_share_lag = dplyr::lag(f39_ind_share),
    
    
    knit_exposure1 = (knit_coefL1 * (knit - dplyr::lag(knit))),
    knit_exposure2 = (knit_coefL2 * (knit - dplyr::lag(knit))),
    knit_exposure3 = (knit_coefL3 * (knit - dplyr::lag(knit))),
    knit_exposure4 = (knit_coefL4 * (knit - dplyr::lag(knit))),
    knit_exposure5 = (knit_coefL5 * (knit - dplyr::lag(knit))),
    
    wov_exposure1 = (wov_coefL1 * (woven - dplyr::lag(woven))),
    wov_exposure2 = (wov_coefL2 * (woven - dplyr::lag(woven))),
    wov_exposure3 = (wov_coefL3 * (woven - dplyr::lag(woven))),
    wov_exposure4 = (wov_coefL4 * (woven - dplyr::lag(woven))),
    wov_exposure5 = (wov_coefL5 * (woven - dplyr::lag(woven))),
    
    export_exposure1 = (knit_exposure1 + wov_exposure1)/1000000,
    export_exposure2 = (knit_exposure2 + wov_exposure2)/1000000,
    export_exposure3 = (knit_exposure3 + wov_exposure3)/1000000,
    export_exposure4 = (knit_exposure4 + wov_exposure4)/1000000,
    export_exposure5 = (knit_exposure5 + wov_exposure5)/1000000,
    
    export_exposure1pc = export_exposure1/((population + dplyr::lag(population))/2),
    export_exposure2pc = export_exposure2/(population + dplyr::lag(population)/2),
    export_exposure3pc = export_exposure3/(population + dplyr::lag(population)/2),
    export_exposure4pc = export_exposure4/(population + dplyr::lag(population)/2),
    export_exposure5pc = export_exposure5/(population + dplyr::lag(population)/2),
    ) %>%
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
    fertility_39_diff = fertility_39 - dplyr::lag(fertility_39)) %>%
  ungroup()

save(data, file = "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/final_data.RData")