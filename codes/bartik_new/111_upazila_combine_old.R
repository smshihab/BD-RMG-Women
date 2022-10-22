


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