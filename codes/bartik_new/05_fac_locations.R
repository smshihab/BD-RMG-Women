library(pacman)

p_load(tidyverse, conflicted, janitor, sf, units)

conflict_prefer_all("dplyr")
#conflict_prefer("filter", "dplyr")
#conflict_prefer("lag", "dplyr")
#conflict_prefer("mutate", "dplyr")


### Creating upazila matchers by areas of intersection

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
  mutate(overlap_ratio = overlap_size/area11) %>% filter(overlap_ratio > 0.2)

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
  mutate(overlap_ratio = overlap_size/area01) %>% filter(overlap_ratio >0.2) %>%
  as.data.frame() %>%
  select(ipum1991, ipum2001, ipum2011)

upazilas <- left_join(upazilas, upazilas91 %>% 
                        as.data.frame() %>% select(ipum1991))


rm(list = setdiff(ls(), "upazilas"))

save(upazilas, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazilas_matcher_all.RData")

# matching

upazilas %>% mutate(across(everything(), ~as.integer(.))) -> upazilas


# Loading ipums data

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data_cleaned_indiv.Rdata")

data %>%
  filter(sex == 2 & age >= 15 & age <= 64) %>%
  group_by(year) %>%
  summarise(flfp = mean(labforce),
            ind = sum(ind == 2) / n())

# creating a common 1991 variable to group by
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

# obtaining upazila estimates

data %>%
  filter(!is.na(ipum1991)) %>%
  group_by(year, ipum1991) %>%
  summarise(sample_pop = n(),
            m_ind = mean(m_ind, na.rm = T),
            m_schooling = mean(m_schooling, na.rm=T),
            urban_share = mean(urban, na.rm=T),
            electrification = mean(electric, na.rm=T),
            age_1564_share = sum(age >= 15 & age <=64)/n(),
            yrschool = mean(yrschool, na.rm=T)) %>% 
  ungroup() -> data_fac_loc

# Obtaining areas

# 1991 dataset

upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

upazilas91 <- st_read(upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
upazilas91 <- st_as_sf(upazilas91) %>% st_transform(3106) %>%
  select(contains("IP")) %>%
  clean_names() %>%
  mutate_if(is.character,as.integer) %>%
  mutate(area91 = st_area(geometry) %>% drop_units(),
         area91 = area91 / 1000^2) %>%
  as.data.frame() %>% select(-geometry)

data_fac_loc <- left_join(data_fac_loc, upazilas91)

data_fac_loc %>%
  mutate(pop = ifelse(year == 2011, sample_pop/0.05, sample_pop/0.1),
         pop_1564 = age_1564_share*pop,
         density = pop/area91,
         density_1564 = pop_1564/area91) %>%
  select(-sample_pop) -> data_fac_loc

# Factory data and list of upazilas with facotry

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.Rdata")

# change all to integers

factory_upazilas %>%
  mutate(across(-upazila, ~as.integer(.x))) -> factory_upazilas

factory_upazilas <- factory_upazilas %>%
  mutate(across(-upazila, ~as.integer(.)))

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factories.Rdata")

rm("matched_data_09", "data", "upazilas91")

matched_data_01 %>%
  filter(exist91 == 1) %>%
  select(upaz2011) -> has_fac91

factory_upazilas %>%
  filter(upaz2011 %in% has_fac91$upaz2011) %>% 
  select(ipum1991) -> has_fac91

matched_data_01 %>%
  filter(exist01 == 1) %>%
  select(upaz2011) -> has_fac01

factory_upazilas %>%
  filter(upaz2011 %in% has_fac01$upaz2011) %>% 
  select(ipum1991) -> has_fac01

has_fac91 <- has_fac91$ipum1991
has_fac01 <- has_fac01$ipum1991

data_fac_loc %>%
  mutate(has_fac91 = case_when(ipum1991 %in% has_fac91 ~ 1, TRUE ~ 0),
         has_fac01 = case_when(ipum1991 %in% has_fac01 ~ 1, TRUE ~ 0)) -> data_fac_loc

rm("has_fac91", "has_fac01")

data_fac_loc %>%
  mutate(has_fac_by01 = pmax(has_fac91, has_fac01)) -> data_fac_loc

factory_upazilas$ipum1991 %>% unique() -> fac2006

data_fac_loc %>%
  mutate(has_fac_by06 = ifelse(ipum1991 %in% fac2006, 1, 0)) -> data_fac_loc

data_fac_loc %>%
  filter(year < 2011 & !is.na(ipum1991)) %>%
  select(year, has_fac_by06, has_fac_by01, has_fac91, has_fac01, electrification, urban_share, density, density_1564, yrschool) %>%
  mutate(across(!c(year, has_fac_by06, has_fac_by01, has_fac91, has_fac01), ~(. - mean(.))/sd(.))) -> 
  data_fac_loc_standardized

data_fac_loc %>%
  group_by(ipum1991) %>%
  mutate(across(c(m_ind, m_schooling, pop, pop_1564, electrification, urban_share), 
                ~(. - dplyr::lag(.)), .names = "diff_{.col}")) %>% ungroup() -> data_share_reg

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/shift_shares.RData")
  
left_join(data_share_reg, autor_shares, by =c("ipum1991", "year")) -> data_share_reg

data_share_reg %>%
  filter(has_fac_by01 == 1) -> data_share_reg

save(list = c("data_share_reg", "data_fac_loc_standardized"),
     file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_location_result1.RData")