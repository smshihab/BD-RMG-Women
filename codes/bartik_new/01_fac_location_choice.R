library(pacman)

p_load(tidyverse, conflicted, janitor, sf, sp, stats, ggplot2, data.table)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")





## Find Factory numbers per upazila and date of establishment


upaz81_data <- read_csv("C:/Users/smshi/OneDrive/Documents/large_datasets/BBS reports/Census reports/Census 1981/upaz81.csv")

upaz81_data %>%
  mutate(mfg_share = occ_mfg81 / (occ_hh81 + occ_culti81 + occ_mfg81),
         road_metal_share = metalledroad81 / kutcha81,
         madrasa_prev = madrasa81 / primary81,
         density81 = pop81 / nonRiverareaSQKM,
         family_size = pop81 / hh81,
         urban_rate = urbanpop81 / pop81,
         wealth_index = (roof_bamboo81 + agrolandowner81)/(2*pop81)) -> upaz81_data

p_load(broom)   

lm(factory ~ literacy81 + urban_rate, upaz81_data) %>% summary()









## Kolmogorov-smirnoff test fails with discrete variables, so plotting empirical CDFs

data01 <- read.csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/04_final_data_01.csv") %>%
  as.data.table()

data01 <- data01[machine >= quantile(machine, 0.005) & machine <= quantile(machine, 0.995)]


data09 <- read.csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/04_final_data_09.csv") %>%
  as.data.table()  

data09 <- data09[machine >= quantile(machine, 0.005, na.rm = T) & machine <= quantile(machine, 0.995, na.rm = T)]

data01 <- as.data.frame(data01)
data09 <- as.data.frame(data09)

# For woven

data01 %>% filter(exist91 == 1 & fac_type == 0) %>% 
  select(machine) %>% as.vector() -> wov91

data01 %>% filter(exist91 == 0 & fac_type == 0) %>% 
  select(machine) %>% as.vector() -> wov01

data09 %>% filter(exist05 == 1 & fac_type == 0) %>% 
  select(machine) %>% as.vector() -> wov05


wov <- list(wov91$machine, wov01$machine, wov05$machine)

wov <- map_dfr(wov, ~as_tibble(t(.)))

wov <- as_tibble(t(wov))

ggplot(wov, aes(V2)) +
  stat_ecdf(geom = "step") +
  stat_ecdf(aes(V1), geom = "step", color = "red") +
  stat_ecdf(aes(V3), geom = "step", color = "blue")


# For knit

data01 %>% filter(exist91 == 1 & fac_type == 1) %>% 
  select(machine) %>% as.vector() -> knit91

data01 %>% filter(exist91 == 0 & fac_type == 1) %>% 
  select(machine) %>% as.vector() -> knit01

data09 %>% filter(exist05 == 1 & fac_type == 1, ) %>% 
  select(machine) %>% as.vector() -> knit05


knit <- list(knit91$machine, knit01$machine, knit05$machine)

knit <- map_dfr(knit, ~as_tibble(t(.)))

knit <- as_tibble(t(knit))

ggplot(knit, aes(V2)) +
  stat_ecdf(geom = "step") +
  stat_ecdf(aes(V1), geom = "step", color = "red") +
  stat_ecdf(aes(V3), geom = "step", color = "blue")



# For mixed

data01 %>% filter(exist91 == 1 & fac_type == 0.5) %>% 
  select(machine) %>% as.vector() -> mixed91

data01 %>% filter(exist91 == 0 & fac_type == 0.5) %>% 
  select(machine) %>% as.vector() -> mixed01

data09 %>% filter(exist05 == 1 & fac_type == 0.5, ) %>% 
  select(machine) %>% as.vector() -> mixed05


mixed <- list(mixed91$machine, mixed01$machine, mixed05$machine)

mixed <- map_dfr(mixed, ~as_tibble(t(.)))

mixed <- as_tibble(t(mixed))

ggplot(mixed, aes(V2)) +
  stat_ecdf(geom = "step") +
  stat_ecdf(aes(V1), geom = "step", color = "red") +
  stat_ecdf(aes(V3), geom = "step", color = "blue")


rm(list = setdiff(ls(), c("data01", "data09")))



<<<<<<< HEAD

=======
## What drives FLFP

#load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data.Rdata")

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
>>>>>>> be010827697b7ac1da4e3ecec3050901b4afb4d6
  

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






apply(wov, 2 , table) -> test

chisq.test(wov01$machine, wov09$machine)

x3 <- map_dfr(x2, ~as_data_frame(t(.)))

#ks.test(wov91$machine, wov01$machine, simulate.p.value = T)

wov01$machine %>%
  table() -> wov01




data01 %>% filter(exist91 == 1 & fac_type == 1) %>% select(machine) -> knit91

data01 %>% filter(exist91 == 0 & fac_type == 1) %>% select(machine) -> knit01


data01 %>% filter(exist91 == 1 & fac_type == 1) %>% select(machine) -> wov91

data01 %>% filter(exist91 == 0 & fac_type == 1) %>% select(machine) -> wov01

data09 %>% filter(fac_type == 0) %>% select(machine) -> wov01






load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/00_upazilas.RData")

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data_full.Rdata")
  


data_full[1:1000,] %>%  clean_names() -> data



rm(list = setdiff(ls(), c("data", "data_full")))

matched_data_01 %>%
  filter(exist0)

 