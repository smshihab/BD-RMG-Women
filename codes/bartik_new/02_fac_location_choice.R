library(pacman)

p_load(tidyverse, conflicted, janitor, sf, sp)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

## What drives FLFP

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data.Rdata")

vars <- names(data) %>% as.data.frame()

data %>% filter(sex == 2 & age > 14) -> data

data %>%
  filter(labforce > 2) %>% mutate(labforce = labforce - 1) %>%
  mutate(across(urban, ) = na_if(urban, 9)) %>%
  mutate(0, 9 for ownership, )
age 999
marst 9
lit 0 9
yrschool > 18
ind
lit_mom 9
lit_sp 9
labforce_sp > 2

data$labforce_sp %>% unique() 
  

## Are factories locating in areas with more favorable labor supply?
# Or with better infrastructure.


upaz81 <- read.csv("./data/upaz81.csv") %>%
  mutate(religious_schooling = primary81 / madrasa81,
         road_condition = kutcha81 / metalledroad81,
         roof_condition = roof_bamboo81/hh81,
         family_size = pop81 / hh81,
         mfg_importance = occ_mfg81 / (occ_hh81 + occ_culti81),
         urban = urbanpop81 / pop81, 
         density = pop81 / nonRiverareaSQKM)

reg_loc81 <- 
  
lm(factory05 ~  urban + density + mfg_importance + literacy81,  upaz81) %>%
  summary()
   
+ mfg_importance + roof_condition + religious_schooling + literacy81+
  urban + mfg_importance + road_condition,

lm(factory05 ~  literacy81 + density + urban + road_condition, upaz81 ) %>% summary()

+
                   religious_schooling + density + family_size + roof_condition,  upaz81)

summary(reg_loc81)


data01 <- read.csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/04_final_data_01.csv")

data09 <- read.csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/04_final_data_09.csv")

data01 

## Kolmogorov-smirnoff test

p_load(stats)

data01 %>% filter(exist91 == 1 & fac_type == 0) %>% select(machine) -> wov01

data09 %>% filter(fac_type == 0) %>% select(machine) -> wov01

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/00_upazilas.RData")

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data_full.Rdata")
  


data_full[1:1000,] %>%  clean_names() -> data



rm(list = setdiff(ls(), c("data", "data_full")))

matched_data_01 %>%
  filter(exist0)

 