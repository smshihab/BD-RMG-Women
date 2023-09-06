# First we find impact 
library(pacman)

p_load(tidyverse, readxl, janitor, labelled, stringr, fuzzyjoin)


load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data_cleaned_indiv.Rdata")

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.RData")

factory_upazilas %>%
  mutate(across(-upazila, ~as.integer(.))) -> factory_upazilas

ninetyone_sample <- factory_upazilas$ipum1991
twentyone_sample <- factory_upazilas$ipum2001
twenty11_sample <-  factory_upazilas$ipum2011
  
  
matched_data_01 %>%
  filter(exist91 == 1) %>%
  select(upaz2011) %>%
  left_join(factory_upazilas %>%
              select(upaz2011, ipum1991)) %>%
  select(ipum1991) %>%
  unique() ->hi

hi$ipum1991 %>% unique() -> hi

data %>%
  filter(geo3_bd1991 %in% hi) %>%
  summarise(n = n())

# gives us 1105450/0.1 in fac area in 1991

data %>%
  group_by(year) %>%
  summarise(n = n())

# gives us 10580904/0.1 in BD in 1991, 12442115/0.1 in 2001 and
# 7205720/0.1 in 2011

# gives 2509976/0.1 (2001) and 1570749/0.05 (2011) in fac areas


###############

data1 %>%
  filter(geo3_bd1991 %in% ninetyone_sample |
          geo3_bd2001 %in% twentyone_sample |
           geo3_bd2011 %in% twenty11_sample) %>%
  filter(sex == 2 & age >=14 & age <=19) %>%
  group_by(year) %>%
  summarise(school = mean(school),
            literacy = mean(lit),
            yrschool = mean(yrschool))

data %>%
  filter(geo3_bd1991 %in% ninetyone_sample |
           geo3_bd2001 %in% twentyone_sample |
           geo3_bd2011 %in% twenty11_sample) %>%
  group_by(year) %>%
  summarise(share1564 = sum(age %in% c(15:64)) / n())



data %>%
  group_by(year) %>%
  summarise(hi = mean(export_exposure_pcpk))










