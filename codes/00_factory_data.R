
#### Loading packages and data ####

library(pacman)

# Non-geo packages

library(conflicted)
p_load(readxl, openxlsx, devtools, plm, tidyverse, knitr, memoise, Rcpp, RhpcBLASctl, gridExtra, stargazer, broom, magick, cowplot, gganimate, ggplot2, ggrepel)

# geo packages
p_load(sf, sp, raster, rgdal, ggmap, rgeos) 


conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

### MiB does not have data on BEPZ garments, but BGMEA does. Here we combine them and fill in missing values. ###

factory_bgmea <- read_csv("C:/Users/smshi/Dropbox/Research/Data/BD RMG factory data/facdata_bgmea_scrape_final.csv") %>%
  mutate_if(is.character, tolower)

factory_mib <- read_excel("C:/Users/smshi/Dropbox/Research/Data/BD RMG factory data/rmg_mib_April22.xlsx") %>%
  mutate_if(is.character, tolower)

### Extract epz factories from BGMEA data

factory_epz_bgmea <- factory_bgmea %>%
  filter(str_detect(fac_ad, "epz|export|processing"))

# We note 8 missing observations in machines, we assign them mean values conditional on type of factory in the EPZ data

mean_machines <- factory_epz_bgmea %>%
  filter(machine > 0) %>%
  group_by(knit) %>%
  summarise(machine = round(mean(machine)))

factory_epz_bgmea <- factory_epz_bgmea %>%
  mutate(machine = ifelse(machine > 0, machine, case_when(knit == 0 ~ as.numeric(mean_machines[1,2]),
                                                          knit == 0.5 ~ as.numeric(mean_machines[2,2]),
                                                          knit == 1 ~ as.numeric(mean_machines[3,2]))))
rm(mean_machines)


# Extract BGMEA factories from mib data

factory_mib_bgmea <- factory_mib %>% 
  filter(str_detect(memberships, "bgmea"))

# Match non-epz factories from mib dataset to the BGMEA dataset

non_epz_rmg_bgmea <- inner_join(factory_mib_bgmea %>% select(name, longitude, latitude, established_in, workers_total,	workers_female),
                                factory_bgmea %>% select(name, knit, machine), by = "name") %>%
  mutate(fshare = workers_female / workers_total, date_est = established_in)

# Build a model for number of workers

non_epz_rmg_bgmea %>%
  select(-name) %>%
  cor(use = "complete.obs")

# By observing the correlation table, we can see that all factors have some correlation whereas number of machines has a large correlation

# Model for total number of workers

model_workers_total <- lm(workers_total ~ longitude + latitude + date_est + knit + machine, non_epz_rmg_bgmea)

# Predict total number of workers based on last model and add to factory_epz_bgmea data

factory_epz_bgmea$workers_total <- predict(model_workers_total, factory_epz_bgmea %>% select(longitude, latitude, date_est, knit, machine)) %>% round()

# Model share of women in workforce

model_fshare <- lm(fshare ~ longitude + latitude + date_est + knit + machine, non_epz_rmg_bgmea)

# Predict share of women in workforce based on last model and add to factory_epz_bgmea data, then use to predict female workforce

factory_epz_bgmea$fshare <- predict(model_fshare, factory_epz_bgmea %>% select(longitude, latitude, date_est, knit, machine))

factory_epz_bgmea <- factory_epz_bgmea %>% 
  mutate(workers_female = round(fshare * workers_total)) %>% 
  select(-fshare)

rm(model_fshare, model_workers_total, factory_bgmea, factory_mib_bgmea, non_epz_rmg_bgmea)

## Append this data to rmg_mib




factory_mib <- factory_mib %>%
  select(name, established_in, longitude, latitude, premises_type, types, products, workers_total, workers_female, address_display, firestation_distance, medical_distance, memberships) %>%
  mutate(workers_male = workers_total - workers_female)

names(factory_mib) <- c("name", "date_est", "long", "lat", "premesis", "type", "products", "workers_total", "workers_female", "fac_ad", "firestation_distance", "medical_distance", "memberships", "workers_male")


factory_epz_bgmea <- factory_epz_bgmea %>%
  mutate(premesis = "epz", long = longitude, lat = latitude, memberships = "bgmea", medical_distance = NA, firestation_distance = NA, workers_male = workers_total - workers_female, products = NA ) %>%
  select(name, date_est, long, lat, premesis, fac_ad, workers_total, workers_female, memberships, medical_distance, firestation_distance, workers_male, type, products)


factory <- rbind(factory_mib, factory_epz_bgmea) %>%
  select(-c(premesis, firestation_distance, medical_distance, memberships))

## changing types to knit dummy


# The ordering of the mutates are very important

factory <- factory%>%
  mutate(knitfac = type) %>%
  mutate(knitfac = ifelse((str_detect(knitfac, "knit") & str_detect(knitfac, "woven")), 0.5, knitfac)) %>%
  mutate(knitfac = ifelse(str_detect(knitfac, "knit|sweater"), 1,knitfac)) %>%
  mutate(knitfac = ifelse(str_detect(knitfac, "woven"), 0,knitfac)) %>%
  # only one NA entry in input data. I checked BGMEA to know they are knit producers
  mutate(knitfac = ifelse(is.na(knitfac), 1, knitfac)) %>%
  mutate(knitfac = ifelse((str_detect(knitfac, 'accessories|dyeing|print|mixed')), 0.5, knitfac)) %>%
  mutate(knitfac = ifelse((str_detect(knitfac, 'accessories|dyeing|print')), 0.5, knitfac)) %>%
  mutate(knitfac = as.numeric(knitfac)) %>%
  mutate(fac_id = as.numeric(row.names(factory)))


rm(list = setdiff(ls(), "factory"))


#### Panelizing Factory data ####

# number of years
t <- max(factory$date_est) - min(factory$date_est) + 1

# number of fac
n <- length(factory$fac_id)

#forming panel to hold data on fac_id, time, machine, knitfac, wovenfac, Latitude, longitude, existence

factory_panel <- matrix(nrow = t * n, ncol = 10)

# creating a unique fac_id-year part of data

temp <- expand(factory, fac_id, year = c(min(factory$date_est):max(factory$date_est))) %>% as.matrix()

factory_panel[,c(1,2)] <- temp[,c(1,2)]


factory <- factory %>% select(fac_id, date_est, long, lat, knitfac, workers_male, workers_female, workers_total)


# Extract all data from factory in a panelized form. To be multiplied with existence dummy for final data

for(columns in c(2,3,4,5,6,7,8)){
  
  temp <- matrix(nrow = t*n, ncol = 1)
  
  for(i in 1:n){
    # first find the value
    value <- as.numeric(factory[i,columns])
    for(j in 1:t){
      k = (i-1)*t + j
      temp[k,1] <- value
    }
    factory_panel[,columns+1] <- temp
  }
}


# existence data

for(i in 1: (n*t)){
  
  fac_id <- as.numeric(factory_panel[i,1])
  year <- as.numeric(factory_panel[i,2])
  date_est <- as.numeric(factory_panel[i,3])
  if (year < date_est){factory_panel[i,10] <- 0} else {factory_panel[i,10] <- 1}
}


factory_panel <- data.frame(factory_panel)

names(factory_panel) <- c("fac_id", "year", "date_est", "long", "lat", "knit", "mworkers", "fworkers", "workers", "exist")

rm(list = ls.str(mode = 'numeric'))

save(factory, file = "factory.RData")
save(factory_panel, file = "factory_panel.RData")