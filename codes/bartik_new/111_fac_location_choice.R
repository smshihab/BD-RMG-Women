library(pacman)

p_load(tidyverse, conflicted, janitor, sf, sp, stats, ggplot2, data.table)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")



### Creating a let of factory upazila matching over time.

upazilas11  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2011/geo3_bd2011.shp"

upazilas11 <- st_read(upazilas11, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
upazilas11 <- st_as_sf(upazilas11) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), contains("IP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area11 = st_area(geometry) %>% drop_units())

upazilas01  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2001/geo3_bd2001.shp"

upazilas01 <- st_read(upazilas01, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
upazilas01 <- st_as_sf(upazilas01) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), contains("IP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  rename(upazila01 = admin_name) %>%
  mutate(area01 = st_area(geometry) %>% drop_units())

upazilas <- st_intersection(upazilas11, upazilas01) %>%
  mutate(overlap_size = drop_units(st_area(geometry))) %>%
  filter(overlap_size > 100) %>%
  mutate(overlap_ratio = overlap_size/area11) %>% filter(overlap_ratio > 0.25)

upazilas <- upazilas %>% as.data.frame() %>%
  select(-c(parent.1, overlap_size, geometry)) %>%
  rename(overlap_ratio_1101 = overlap_ratio)

upazilas <- left_join(upazilas,
                              upazilas01 %>%
                                select(upaz2001)) %>% st_as_sf()


# 1991 dataset

upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

upazilas91 <- st_read(upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
upazilas91 <- st_as_sf(upazilas91) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), contains("IP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())


upazilas <- st_intersection(upazilas, upazilas91) %>%
  mutate(overlap_size = drop_units(st_area(geometry))) %>%
  filter(overlap_size > 100) %>%
  mutate(overlap_ratio = overlap_size/area01) %>% filter(overlap_ratio >0.25) %>%
  as.data.frame() %>%
  select(ipum1991, ipum2001, ipum2011)

save(upazilas, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazilas_all.RData")

