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