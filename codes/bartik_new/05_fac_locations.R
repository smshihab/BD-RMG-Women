library(pacman)

p_load(tidyverse, conflicted, janitor, sf)

#conflict_prefer_all("dplyr")
#conflict_prefer("filter", "dplyr")
#conflict_prefer("lag", "dplyr")
#conflict_prefer("mutate", "dplyr")


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

rm(list = setdiff(ls(), "upazilas"))

#load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazilas_all.RData")

# matching

upazilas %>% mutate(across(everything(), ~as.integer(.))) -> upazilas

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.Rdata")

# change all to integers

factory_upazilas %>%
  mutate(across(-upazila, ~as.integer(.x))) -> factory_upazilas

factory_upazilas <- factory_upazilas %>%
  mutate(across(-upazila, ~as.integer(.)))

ninetyone_sample <- factory_upazilas$ipum1991
twentyone_sample <- factory_upazilas$ipum2001
twenty11_sample <-  factory_upazilas$ipum2011

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data_cleaned_indiv.Rdata")

data %>%
  mutate(has_fac = case_when(
    geo3_bd1991 %in% ninetyone_sample |
    geo3_bd2001 %in% twentyone_sample |
    geo3_bd2011 %in% twenty11_sample ~ 1, TRUE ~0
  )) -> data

data %>%
  filter(year == 1991 & has_fac == 1) %>%
  select(geo3_bd1991) %>% unique()
  
data %>%
  filter(year == 1991) -> data1991

data %>%
  filter(year == 2001) -> data2001

data %>%
  filter(year == 2011) -> data2011

rm("data")

data1991 %>%
  mutate(ipum1991 = as.integer(geo3_bd1991)) -> data1991

data2001 %>%
  mutate(ipum2001 = as.integer(geo3_bd2001)) %>%
  left_join(upazilas %>% select(ipum2001, ipum1991), by = "ipum2001") %>%
  select(-ipum2001) %>% rbind(data1991) -> data

rm("data1991", "data2001")

data2011 %>%
  mutate(ipum2011 = as.integer(geo3_bd2011)) %>%
  left_join(upazilas %>% select(ipum2011, ipum1991), by = "ipum2011") %>%
  select(-ipum2011) %>% rbind(data) -> data

rm("data2011")

lm(factory_location ~ electrification + urban + density1564, data91) %>% summary()
%>%
  -> data91

data2001 %>%
  group_by(year, ipum1991) %>%
  summarise(urban_share = mean(urban, na.rm=T),
            electrification = mean(electric, na.rm=T),
            age_1564_share = sum(age >= 15 & age <=64)/n(),
            yrschool = mean(yrschool, na.rm=T)) %>% 
ungroup() -> data_fac_loc