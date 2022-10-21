data %>%
  filter(sex == 1  & age >14 & age <65) %>%
  group_by(year) %>%
  summarise(flfp = sum(labforce == 1) / n(),
            flfp_ind = sum(ind %in% c(2))/n())
  

# samples to select from IPUMS data

ninetyone_sample <- factory_upazilas$ipum1991
twentyone_sample <- factory_upazilas$ipum2001
twenty11_sample <-  factory_upazilas$ipum2011

### Upazila data in 1991

### Gets you general values

# Load IPUMs

# Selecting sample

fac_up_sample <- data %>%
  filter(geo3_bd1991 %in% ninetyone_sample | geo3_bd2001 %in% twentyone_sample  | geo3_bd2011 %in% twenty11_sample)

### Gets you general values

fac_up_sample91 <- fac_up_sample %>% filter(year == 1991) 

upazila91_gen <- fac_up_sample91 %>%
  group_by(geo3_bd1991) %>%
  summarise(pop = n() / 0.1,
            urban_share = sum(urban == 2)/n(),
            electrification = sum(electric == 1)/n(),
            #toilet_share = 1 - sum(toilet == 10)/n(),
            age_15 = sum(age < 15)/n(),
            age_1564 = sum(age >= 15 & age <64)/n(),
            age_65 = 1 - age_15 - age_1564, 
            school15_m = sum(school == 1 & age <15 & sex == 1) / sum(age <15 & sex == 1),
            school15_f = sum(school == 1 & age <15 & sex == 2) / sum(age <15 & sex == 2),
            school15_m = sum(school == 1 & age <15 & sex == 1) / sum(age <15 & sex == 1),) %>%
  ungroup() 


upazila91_male <- fac_up_sample91 %>%
  filter(age >=15 & age < 64 & sex == 1) %>%
  group_by(geo3_bd1991) %>%
  summarise(m_1564_pop = n()/0.1,
            m_ind_share = sum(ind %in% c(05, 06, 07)) / n(),
            m_ind_share2 = sum(ind == 05)/ n(),# based on less granular data
            m_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            m_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            m_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            m_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            m_lfp = sum(labforce %in% c(2))/ n(),
            m_educ = mean(yrschool[yrschool < 50])) %>% 
  ungroup()


upazila91_female <- fac_up_sample91 %>%
  filter(age >=15 & age < 64 & sex == 2) %>%
  group_by(geo3_bd1991) %>%
  summarise(f_1564_pop = n()/0.1,
            f_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f_ind_share2 = sum(ind == 05)/ n(),# based on granular data
            f_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f_lfp = sum(labforce %in% c(2))/ n(),
            f_educ = mean(yrschool[yrschool < 50])) %>%
  ungroup()

upazila91_fu39 <- fac_up_sample91 %>%
  filter(age >=15 & age <= 39 & sex == 2) %>%
  group_by(geo3_bd1991) %>%
  summarise(f_1539_pop = n()/0.1,
            f39_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f39_ind_share2 = sum(ind == 05)/ n(),# based on granular data
            f39_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f39_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f39_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f39_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f39_lfp = sum(labforce %in% c(2))/ n(),
            f39_educ = mean(yrschool[yrschool < 50]),
            fertility_39 = mean(nchild)) %>%
  ungroup()


upazila91_fu25 <- fac_up_sample91 %>%
  filter(age >=15 & age <= 25 & sex == 2) %>%
  group_by(geo3_bd1991) %>%
  summarise(f_1525_pop = n()/0.1,
            f25_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f25_ind_share2 = sum(ind == 05)/ n(),# based on granular data
            f25_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f25_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f25_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f25_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f25_lfp = sum(labforce %in% c(2))/ n(),
            f25_educ = mean(yrschool[yrschool < 50]),
            fertility_25 = mean(nchild)) %>%
  ungroup()


upazila91 <- left_join(upazila91_gen, upazila91_male, by = "geo3_bd1991")
upazila91 <- left_join(upazila91, upazila91_female, by = "geo3_bd1991")
upazila91 <- left_join(upazila91, upazila91_fu39, by = "geo3_bd1991")
upazila91 <- left_join(upazila91, upazila91_fu25, by = "geo3_bd1991")

rm(upazila91_gen, upazila91_male, upazila91_female, upazila91_fu39, fac_up_sample91, upazila91_fu25)

upazila91 <- upazila91 %>% 
  mutate(ipum1991 = remove_labels(geo3_bd1991)) %>%
  select(-geo3_bd1991)

# upazila91 <- left_join(upazila91, factory_upazilas %>% select(ipum1991, geo3_bd1991))  

#####

fac_up_sample01 <- fac_up_sample %>% filter(year == 2001)

factory_upazilas <- factory_upazilas %>%
  rename(geo3_bd2001 = ipum2001, geo3_bd2011 = ipum2011)

fac_up_sample01 <- left_join(fac_up_sample01, factory_upazilas %>% select(ipum1991, geo3_bd2001))



### Gets you general values

upazila01_gen <- fac_up_sample01 %>%
  group_by(ipum1991) %>%
  summarise(pop = n()/0.1,
            urban_share = sum(urban == 2)/n(),
            electrification = sum(electric == 1)/n(),
            #toilet_share = 1 - sum(toilet == 10)/n(),
            age_15 = sum(age < 15)/n(),
            age_1564 = sum(age >= 15 & age <64)/n(),
            age_65 = 1 - age_15 - age_1564, 
            school15_m = sum(school == 1 & age <15 & sex == 1) / sum(age <15 & sex == 1),
            school15_f = sum(school == 1 & age <15 & sex == 2) / sum(age <15 & sex == 2)) %>%
  ungroup() 


upazila01_male <- fac_up_sample01 %>%
  filter(age >=15 & age < 64 & sex == 1) %>%
  group_by(ipum1991) %>%
  summarise(m_1564_pop = n()/0.1,
            m_ind_share = sum(ind %in% c(05, 06, 07)) / n(),
            m_ind_share2 = sum(ind == 2)/ n(),# based on granular data
            m_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            m_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            m_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            m_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            m_lfp = sum(labforce %in% c(2))/ n(),
            m_educ = mean(yrschool[yrschool < 50])) %>% 
  ungroup()


upazila01_female <- fac_up_sample01 %>%
  filter(age >=15 & age < 64 & sex == 2) %>%
  group_by(ipum1991) %>%
  summarise(f_1564_pop = n()/0.1,
            f_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f_ind_share2 = sum(ind == 2)/ n(),# based on granular data
            f_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f_lfp = sum(labforce %in% c(2))/ n(),
            f_educ = mean(yrschool[yrschool < 50])) %>%
  ungroup()

upazila01_fu39 <- fac_up_sample01 %>%
  filter(age >=15 & age <= 39 & sex == 2) %>%
  group_by(ipum1991) %>%
  summarise(f_1539_pop = n()/0.1,
            f39_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f39_ind_share2 = sum(ind == 2)/ n(),# based on granular data
            f39_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f39_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f39_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f39_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f39_lfp = sum(labforce %in% c(2))/ n(),
            f39_educ = mean(yrschool[yrschool < 50]),
            fertility_39 = mean(nchild)) %>%
  ungroup()


upazila01_fu25 <- fac_up_sample01 %>%
  filter(age >=15 & age <= 25 & sex == 2) %>%
  group_by(ipum1991) %>%
  summarise(f_1525_pop = n()/0.1,
            f25_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f25_ind_share2 = sum(ind == 05)/ n(),# based on granular data
            f25_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f25_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f25_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f25_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f25_lfp = sum(labforce %in% c(2))/ n(),
            f25_educ = mean(yrschool[yrschool < 50]),
            fertility_25 = mean(nchild)) %>%
  ungroup()


upazila01 <- left_join(upazila01_gen, upazila01_male, by = "ipum1991")
upazila01 <- left_join(upazila01, upazila01_female, by = "ipum1991")
upazila01 <- left_join(upazila01, upazila01_fu39, by = "ipum1991")
upazila01 <- left_join(upazila01, upazila01_fu25, by = "ipum1991")


rm(upazila01_gen, upazila01_male, upazila01_female, upazila01_fu39, fac_up_sample01, upazila01_fu25)


#####

fac_up_sample11 <- fac_up_sample %>% filter(year == 2011)

fac_up_sample11 <- left_join(fac_up_sample11, factory_upazilas %>% 
                               select(ipum1991, geo3_bd2011))



### Gets you general values

upazila11_gen <- fac_up_sample11 %>%
  group_by(ipum1991) %>%
  summarise(pop = n()/0.05,
            urban_share = sum(urban == 2)/n(),
            electrification = sum(electric == 1)/n(),
            #toilet_share = 1 - sum(toilet == 10)/n(),
            age_15 = sum(age < 15)/n(),
            age_1564 = sum(age >= 15 & age <64)/n(),
            age_65 = 1 - age_15 - age_1564, 
            school15_m = sum(school == 1 & age <15 & sex == 1) / sum(age <15 & sex == 1),
            school15_f = sum(school == 1 & age <15 & sex == 2) / sum(age <15 & sex == 2)) %>%
  ungroup() 


upazila11_male <- fac_up_sample11 %>%
  filter(age >=15 & age < 64 & sex == 1) %>%
  group_by(ipum1991) %>%
  summarise(m_1564_pop = n()/0.05,
            m_ind_share = sum(ind %in% c(05, 06, 07)) / n(),
            m_ind_share2 = sum(ind == 2)/ n(),# based on granular data
            m_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            m_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            m_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            m_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            m_lfp = sum(labforce %in% c(2))/ n(),
            m_educ = mean(yrschool[yrschool < 50])) %>% 
  ungroup()


upazila11_female <- fac_up_sample11 %>%
  filter(age >=15 & age < 64 & sex == 2) %>%
  group_by(ipum1991) %>%
  summarise(f_1564_pop = n()/0.05,
            f_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f_ind_share2 = sum(ind == 2)/ n(),# based on granular data
            f_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f_lfp = sum(labforce %in% c(2))/ n(),
            f_educ = mean(yrschool[yrschool < 50])) %>%
  ungroup()

upazila11_fu39 <- fac_up_sample11 %>%
  filter(age >=15 & age <= 39 & sex == 2) %>%
  group_by(ipum1991) %>%
  summarise(f_1539_pop = n()/0.05,
            f39_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f39_ind_share2 = sum(ind == 2)/ n(),# based on granular data
            f39_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f39_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f39_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f39_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f39_lfp = sum(labforce %in% c(2))/ n(),
            f39_educ = mean(yrschool[yrschool < 50]),
            fertility_39 = mean(nchild)) %>%
  ungroup()

upazila11_fu25 <- fac_up_sample11 %>%
  filter(age >=15 & age <= 25 & sex == 2) %>%
  group_by(ipum1991) %>%
  summarise(f_1525_pop = n()/0.05,
            f25_ind_share = sum(ind %in% c(05, 06, 07))/ n(),
            f25_ind_share2 = sum(ind == 2)/ n(),# based on granular data
            f25_cons_share = sum(ind %in% c(06, 07, 08))/ n(),# based on granular data
            f25_trad_share = sum(ind %in% c(03, 04))/ n(),# based on granular data, agri an household
            f25_service_share = sum(ind %in% c(08, 09, 11))/ n(),# based on granular data, agri an household
            f25_business_share = sum(ind %in% c(10))/ n(),# based on granular data, agri an household
            f25_lfp = sum(labforce %in% c(2))/ n(),
            f25_educ = mean(yrschool[yrschool < 50]),
            fertility_25 = mean(nchild)) %>%
  ungroup()


upazila11 <- left_join(upazila11_gen, upazila11_male, by = "ipum1991")
upazila11 <- left_join(upazila11, upazila11_female, by = "ipum1991")
upazila11 <- left_join(upazila11, upazila11_fu39, by = "ipum1991")
upazila11 <- left_join(upazila11, upazila11_fu25, by = "ipum1991")


rm(upazila11_gen, upazila11_male, upazila11_female, upazila11_fu39, fac_up_sample11, upazila11_fu25)


data %>% filter

upazila91$year <- 1991
upazila01$year <- 2001
upazila11$year <- 2011

upazila_data <- bind_rows(upazila91, upazila01, upazila11)

save(upazila_data, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data.RData")