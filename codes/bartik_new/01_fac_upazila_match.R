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
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area11 = st_area(geometry) %>% drop_units()) %>%
  mutate(upaz2011 = as.numeric(upaz2011))

# loading factory data
factory <- read_csv("./data/04_final_data.csv") %>%
  mutate_if(is.character,tolower)

# factory upazilas

factory_upazilas <- factory %>% 
  select(upazila, upaz2011) %>% unique() %>% rename(admin_name = upazila)

## Matching with 2011

factory_upazilas <- left_join(factory_upazilas, factory_upazilas11) %>%
  st_as_sf()


factory_upazilas01  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2001/geo3_bd2001.shp"

factory_upazilas01 <- st_read(factory_upazilas01, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas01 <- st_as_sf(factory_upazilas01) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area01 = st_area(geometry) %>% drop_units())


factory_upazilas <- st_intersection(factory_upazilas, factory_upazilas01) %>%
  mutate(overlap_size = drop_units(st_area(geometry))) %>%
  filter(overlap_size > 100) %>%
  mutate(overlap_ratio = overlap_size/area11) %>% filter(overlap_ratio > 0.2)


factory_upazilas <- factory_upazilas %>% as.data.frame() %>%
  select(-c(geometry, upaz2011, upaz2001, overlap_size, overlap_ratio)) %>%
  rename(admin_name = admin_name, admin_name01 = admin_name.1, parent01 = parent.1)

factory_upazilas <- left_join(factory_upazilas, factory_upazilas01 %>% select(ipum2001), by ="ipum2001") %>% st_as_sf()


# 1991 dataset

factory_upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

factory_upazilas91 <- st_read(factory_upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas91 <- st_as_sf(factory_upazilas91) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())


factory_upazilas <- st_intersection(factory_upazilas, factory_upazilas91) %>%
  mutate(overlap_size = drop_units(st_area(geometry))) %>%
  filter(overlap_size > 100) %>%
  mutate(overlap_ratio = overlap_size/area01) %>% filter(overlap_ratio >0.2) %>%
  as.data.frame() %>%
  select(-c(geometry, upaz1991, overlap_ratio, overlap_size))

factory_upazilas %>% rename(upazila = admin_name, admin_name91 = admin_name.1, parent11 = parent, parent91 = parent.1) -> factory_upazilas


save(factory_upazilas, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/00_upazilas.RData")