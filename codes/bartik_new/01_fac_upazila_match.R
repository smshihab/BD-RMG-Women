library(pacman)

p_load(tidyverse, readxl, knitr, sf, sp, janitor, units)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

factory_upazilas11  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2011/geo3_bd2011.shp"

factory_upazilas11 <- st_read(factory_upazilas11, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas11 <- st_as_sf(factory_upazilas11) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area11 = st_area(geometry) %>% drop_units()) %>%
  mutate(upaz2011 = as.numeric(upaz2011))

# loading factory data
factory_01 <- read_csv("./data/04_final_data_01.csv") %>%
  mutate_if(is.character,tolower)


factory_09 <- read_csv("./data/04_final_data_09.csv") %>%
  mutate_if(is.character,tolower)

# factory upazilas

rbind(factory_01 %>% select(upazila, upaz2011),  
      factory_09 %>% select(upazila, upaz2011))%>% unique() -> factory_upazilas


## Matching with 2011

factory_upazilas <- left_join(factory_upazilas, 
                              factory_upazilas11 %>% select(-admin_name)) %>%
  st_as_sf()


factory_upazilas01  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2001/geo3_bd2001.shp"

factory_upazilas01 <- st_read(factory_upazilas01, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas01 <- st_as_sf(factory_upazilas01) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  rename(upazila01 = admin_name) %>%
  mutate(area01 = st_area(geometry) %>% drop_units())


factory_upazilas <- st_intersection(factory_upazilas, factory_upazilas01) %>%
  mutate(overlap_size = drop_units(st_area(geometry))) %>%
  filter(overlap_size > 100) %>%
  mutate(overlap_ratio = overlap_size/area11) %>% filter(overlap_ratio > 0.25)


factory_upazilas <- factory_upazilas %>% as.data.frame() %>%
  select(-c(parent.1, overlap_size, geometry)) %>%
  rename(overlap_ratio_1101 = overlap_ratio)

factory_upazilas <- left_join(factory_upazilas,
                              factory_upazilas01 %>%
                                select(upaz2001)) %>% st_as_sf()
  

# 1991 dataset

factory_upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

factory_upazilas91 <- st_read(factory_upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas91 <- st_as_sf(factory_upazilas91) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())


factory_upazilas <- st_intersection(factory_upazilas, factory_upazilas91) %>%
  mutate(overlap_size = drop_units(st_area(geometry))) %>%
  filter(overlap_size > 100) %>%
  mutate(overlap_ratio = overlap_size/area01) %>% filter(overlap_ratio >0.25) %>%
  as.data.frame() %>%
  select(-c(geometry, overlap_size, parent.1, upazila, area11, area01, upazila01,
            area91, overlap_ratio, overlap_ratio_1101)) %>%
  rename(upazila = admin_name)

save(factory_upazilas, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/00_upazilas.RData")