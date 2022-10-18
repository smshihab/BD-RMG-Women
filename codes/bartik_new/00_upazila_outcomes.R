library(pacman)
p_load(tidyverse, conflicted, janitor, sf, sp, labelled)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Loading Upazila data and the outcome data in census

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.RData")

factory_upazilas <- factory_upazilas %>%
  mutate(across(-upazila, ~as.integer(.)))

# Data is the IPUMS dataset from Bangladesh that covers the variables for parents and spouse in same HH.
load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data.Rdata")

data <- data %>% clean_names() %>%
  select(year, urban, geo3_bd1991, geo3_bd2001, geo3_bd2011, ownership, electric, 
         famsize, nchild, nchlt5, age, sex, marst, religion, school, lit, yrschool, empstat, labforce, ind,
       starts_with(c("age","lit", "yrs", "empstat_", "lab", "ind")))


# Collecting the variables available
as.data.frame(names(data)) -> variables_in_data

#data cleaning
## removing labels 
data <- data %>% remove_labels()

# resolving issues with industry comparability

## 2011 has only three categories, going back to the text of questionnaire
## 1991, 2001 industry and agriculture can be matched to 2011 but not much else.
## 1991, 2001 must be conditioned on employed
# 1 is agriculture, 2 is industry, 3 should better be interpreted as I do not know.

data %>%
  mutate(ind = case_when(year %in% c(1991, 2001) & empstat == 1 & ind == 4 ~ 1,
                         year %in% c(1991, 2001) & empstat == 1 & ind == 5 ~ 2,
                         year == 2011 ~ ind,
                         TRUE ~ 3)) -> data

## same must be done for all of industry variables
# for spouses
data %>%
  mutate(ind_sp = case_when(year %in% c(1991, 2001) & empstat_sp == 1 & ind_sp == 4 ~ 1,
                            year %in% c(1991, 2001) & empstat_sp == 1 & ind_sp == 5 ~ 2,
                            year == 2011 ~ ind_sp,
                            TRUE ~ 3)) -> data
# for pops
data %>%
  mutate(ind_pop = case_when(year %in% c(1991, 2001) & empstat_pop == 1 & ind_pop == 4 ~ 1,
                             year %in% c(1991, 2001) & empstat_pop == 1 & ind_pop == 5 ~ 2,
                             year == 2011 ~ ind_pop,
                             TRUE ~ 3)) -> data
# for mom
data %>%
  mutate(ind_mom = case_when(year %in% c(1991, 2001) & empstat_mom == 1 & ind_mom == 4 ~ 1,
                             year %in% c(1991, 2001) & empstat_mom == 1 & ind_mom == 5 ~ 2,
                             year == 2011 ~ ind_mom,
                             TRUE ~ 3)) -> data

data %>%
  mutate(across(contains("labforce"), ~ifelse(.x == 2, 1, 0))) %>%
  mutate(across(contains("ind"), ~ifelse(.x == 2, 1, 0), .names = "lfp_{.col}")) %>%
  mutate(across(contains("lit"), ~ifelse(.x == 9, NA, .x))) %>%
  mutate(across(contains("yrschool"), ~ifelse(.x %in% c(98,99), NA, .x))) %>%
  mutate(across(contains("empstat"), ~ifelse(.x %in% c(3,9), 0, .x))) %>%
  #mutate(across(contains("age"), ~ifelse(.x == 999, NA, .x))) %>%
  mutate(across(c(electric, ownership), ~ifelse(.x %in% c(0,9), NA, .x))) %>%
  mutate(sex = ifelse(sex %in% c(0,9), NA, sex),
         nchlt5 = ifelse(nchlt5 == 98, NA, nchlt5),
         school = ifelse(school == 1, 1, 0)) -> data

# Re-coding to dummy variables for easier interpretation

data %>%
  mutate(urban = urban -1,
         ownership = 2 - ownership,
         electric = 2 - electric,
         muslim = ifelse(religion == 5, 1, 0),
         lit = lit -1,
         across(contains("lit"), ~(.x = .x-1))) -> data
         
## how are FLFP changing over time
data %>%
  filter(sex == 2  & age >14 & age <65) %>%
  group_by(year) %>%
  summarise(flfp = sum(labforce == 1) / n(),
            flfp_ind = sum(ind %in% c(2))/n())

# creating a grouping variable
data <- data %>% 
  mutate(groups = case_when(year == 1991 ~ paste0(year, geo3_bd1991),
                            year == 2001 ~ paste0(year, geo3_bd2001),
                            year == 2011 ~ paste0(year, geo3_bd2011)))

## What determines FLFP

## First, creating some year-upazila variables

## male LFP and industrial LFP as a measure of local economic conditions

data %>% filter(sex == 1 & age >=15 & age <=64) %>%
  group_by(groups) %>%
  summarise(m_lfp = sum(labforce == 1) / n(),
            m_ind = sum(ind %in% c(2))/n()) %>%
  right_join(data, by = "groups") -> data

## male education as a measure of location educational conditions

data %>% filter(sex == 1 & age >=5 & age <=20) %>%
  group_by(groups) %>%
  summarise(m_schooling = sum(school == 1) / n()) %>%
  right_join(data, by = "groups") -> data

## Cut-offs chosen by age-specific marriage rates 
## since we observe children ONLY in HH. 
  
data %>% filter(sex == 2 & age >=30 & age <=40) %>%
  group_by(groups) %>%
  summarise(fertility30_40 = mean(nchild)) %>%
  right_join(data, by = "groups") -> data

### Running individual regressions
# without location variables

# unmarried women, no location specific variables

## Overall FLFP
lm(100*labforce ~ factor(year) + ownership + electric + factor(marst) +
     age + I(age^2) + muslim + yrschool + labforce_mom,
   data = data %>% filter(sex == 2 & age >=15 & age <=64)) -> reg1
  
### time trend, being poorer, non-Muslim, less education and LFP of women is important.
### most important is labforce_mom, and being poor. Low R squared. R squared of less than 2 percent 
### with only mom's FLFP

## Industrial FLFP, ownership dropped
lm(100*ind ~ factor(year) +  electric + factor(marst) + 
     age + I(age^2) + muslim + yrschool,
   data = data %>% filter(sex == 2 & age >=15 & age <=64)) -> reg2


lm(100*ind ~ factor(year) +  electric + factor(marst) + m_ind +
     age + I(age^2) + muslim + yrschool,
   data = data %>% filter(sex == 2 & age >=15 & age <=64)) -> reg3


lm(100*labforce ~ factor(year) + ownership + electric + factor(marst) + m_lfp +
     age + I(age^2) + muslim + yrschool + labforce_mom,
   data = data %>% filter(sex == 2 & age >=15 & age <=64)) -> reg4


p_load(stargazer) 

lm(nchild ~ fertility30_40 + factor(year) + electric + factor(marst) + 
     yrschool + yrschool_sp + muslim + labforce, I(groups),  
   data = data %>% filter(sex == 2 & age >=30 & age <=40)) %>% summary()
  
  
  stargazer(reg3, reg4, type = "text")

##






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


upazila91$year <- 1991
upazila01$year <- 2001
upazila11$year <- 2011

upazila_data <- bind_rows(upazila91, upazila01, upazila11)

save(upazila_data, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data.RData")