library(pacman)

p_load(tidyverse, sf, janitor, sp)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


### eexports
trade <- read_csv("comtrade.csv") %>%
  clean_names() %>%
  group_by(year, commodity_code, trade_flow) %>%
  summarise(value = sum(trade_value_us)) %>%
  filter(year %in% c(1991, 2001, 2011)) %>%
  ungroup()

exports <- trade %>%
  filter(commodity_code == 61 & trade_flow == "Export" |
           commodity_code == 62 & trade_flow == "Export")

import <- trade %>%
  filter(trade_flow == "Import") %>%
  group_by(commodity_code, year) %>%
  summarise(value = sum(value))%>%
  ungroup()

import_knit <- import %>% filter(commodity_code == 60)
import_wov <- import %>% filter(commodity_code > 100) %>%
  group_by(year) %>% summarise(value = sum(value))

exports <- data.frame(
  year = c(1991, 2001, 2011),
  knit_import = import_knit$value,
  wov_import = import_wov$value,
  knit_ex = c(152276422, 1281533792, 9936304901),
  wov_ex = c(687361669, 2757655123, 9225733521))

exports <- exports %>%
  mutate(knit = (knit_ex - knit_import),
         woven = (wov_ex - wov_import))

rm(list = setdiff(ls(), "exports"))

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/factory.RData")

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/factory_panel.RData")


# Geo-coding garments data
# Set coordinates
coordinates(factory) <- c("long", "lat")

# Set projection string
proj4string(factory) <- CRS("+init=epsg:4326") 

# Convert to Sf and transform to Mercator Gulshan
factory <- st_as_sf(factory) %>% st_transform(3106)


factory_upazilas  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/factories_ipums/factories_ipums.shp"

factory_upazilas <- st_read(factory_upazilas, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas <- st_as_sf(factory_upazilas) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names()

factory_upazilas[11,3] <- "020015061"                                                
factory_upazilas[11,4] <- "020015061"

factory_upazilas[11,6] <- "201561"
factory_upazilas[11,7] <- "201561"

factory_upazilas[25,3] <- "030026072"
factory_upazilas[25,4] <- "030026072"

factory_upazilas[25,6] <- "302672"
factory_upazilas[25,7] <- "302672"


factory_upazilas[27,3] <- "030033032"
factory_upazilas[27,4] <- "030033032"

factory_upazilas[27,6] <- "303332"
factory_upazilas[27,7] <- "303332"


factory_upazilas[factory_upazilas$admin_name == "Uttara", 3]  <- "030026095"
factory_upazilas[factory_upazilas$admin_name == "Uttara", 4]  <- "030026095"

factory_upazilas[factory_upazilas$admin_name == "Uttara", 6]  <- "302695"
factory_upazilas[factory_upazilas$admin_name == "Uttara", 7]  <- "302695"


factory_upazilas <- factory_upazilas %>% select(ipum1991) %>% unique()


factory <- factory %>%
  mutate(
    upazila = as.integer(st_intersects(geometry, factory_upazilas)), 
    ipum1991 = if_else(is.na(upazila), '', factory_upazilas$ipum1991[upazila]))


factory[3969, 9] <- "020015020"


factory_panel <- left_join(factory_panel, 
                           factory %>% as.data.frame() %>% select(fac_id, ipum1991),
                           by = "fac_id")




# Using factory panel from before
factory_panel <- factory_panel %>%
  mutate(woven = 1-knit)

# Estimating shocks to areas

temp <- factory_panel %>% filter(year >=1985 & year <=2011)

# Estimating total knit and woven capacity in BD

bd_capacity <- temp %>%
  data.frame() %>%
  group_by(year) %>%
  summarise(
    knit_bd = sum(workers*knit*exist, na.rm = T),
    wov_bd = sum(workers*woven*exist, na.rm = T)) %>%
  ungroup()

# Estimating a region's share of knit and woven

  
temp_data <- temp %>%
  group_by(ipum1991, year) %>%
  summarize(knit_workers = sum(exist*workers*knit, na.rm = T), 
            wov_workers = sum(exist*workers*woven, na.rm = T)) %>% ungroup()

temp_data <- left_join(temp_data, bd_capacity, by = "year")

temp_data <- temp_data %>% mutate(knit_coef = knit_workers/knit_bd,
                                  wov_coef = wov_workers/wov_bd)

temp_data <- temp_data %>%
  group_by(ipum1991) %>%
  mutate(
    knit_coefL1 = dplyr::lag(knit_coef, 1),
    knit_coefL2 = dplyr::lag(knit_coef, 2),
    knit_coefL3 = dplyr::lag(knit_coef, 3),
    knit_coefL4 = dplyr::lag(knit_coef, 4),
    knit_coefL5 = dplyr::lag(knit_coef, 5),
    wov_coefL1 = dplyr::lag(wov_coef, 1),
    wov_coefL2 = dplyr::lag(wov_coef, 2),
    wov_coefL3 = dplyr::lag(wov_coef, 3),
    wov_coefL4 = dplyr::lag(wov_coef, 4),
    wov_coefL5 = dplyr::lag(wov_coef, 5)) %>% ungroup()


load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/population.RData")

temp_data <- left_join(temp_data, exports %>% select(year, knit, woven), by = "year")

temp_data <- left_join(temp_data, population, by = c("ipum1991","year")) %>%
  filter(year %in% c(1991, 2001, 2011))

temp_data <- temp_data %>% mutate(ipum1991 = as.integer(ipum1991))


load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/upazila_data.RData")

upazila_data_combined <- left_join(upazila_data, temp_data, by = c("ipum1991","year"))

upazila_data_combined <- upazila_data_combined %>%
  arrange(ipum1991, year)

save(upazila_data_combined, file="C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/upazila_data_combined.RData")

###########



