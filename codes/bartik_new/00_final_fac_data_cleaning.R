library(pacman)

p_load(tidyverse, readxl, knitr, sf, sp, janitor, labelled, stringr, fuzzyjoin)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

## Loading data

# working with the matched data

matched_data_01 <- read.csv("~/large_datasets/BD Garments/03_all_matched_data_01.csv") %>%
  mutate_if(is.character,tolower) %>%
  select(-c(name, fac_ad, product_type))


matched_data_09 <- read.csv("~/large_datasets/BD Garments/03_all_matched_data_09.csv") %>%
  mutate_if(is.character,tolower) %>%
  select(-c(name, fac_ad, product_type))


# Upazilas in 2011

upazilas11  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2011/geo3_bd2011.shp"

upazilas11 <- st_read(upazilas11, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
upazilas11 <- st_as_sf(upazilas11) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower)

upazilas11 <- tibble::rowid_to_column(upazilas11, "id")


# split matched datasets into ones with and without missing upaz

missing_upz01 <- matched_data_01 %>% filter(is.na(upazila))

missing_upz09 <- matched_data_09 %>% filter(is.na(upazila))


# geo-coding missing upzaila data for 01 and combining them to original

coordinates(missing_upz01) <- c("Longitude", "Latitude")

# Set projection string
proj4string(missing_upz01) <- CRS("+init=epsg:4326") 

# Convert to Sf and transform to Mercator Gulshan
missing_upz01 <- st_as_sf(missing_upz01) %>% st_transform(3106)


missing_upz01 %>%
  mutate(id = as.integer(st_intersects(geometry, upazilas11)),
         upazila = upazilas11$admin_name[id]) %>%
  as.data.frame() %>%
  select(-c(geometry, id)) -> missing_upz01


matched_data_01 %>%
  filter(!is.na(upazila)) %>%
  select(-c(Longitude, Latitude)) %>%
  rbind(missing_upz01) -> matched_data_01

rm(missing_upz01)


# geo-coding missing upzaila data for 09 and combining them to original

coordinates(missing_upz09) <- c("Longitude", "Latitude")

# Set projection string
proj4string(missing_upz09) <- CRS("+init=epsg:4326") 

# Convert to Sf and transform to Mercator Gulshan
missing_upz09 <- st_as_sf(missing_upz09) %>% st_transform(3106)


missing_upz09 %>%
  mutate(id = as.integer(st_intersects(geometry, upazilas11)),
         upazila = upazilas11$admin_name[id]) %>%
  as.data.frame() %>%
  select(-c(geometry, id)) -> missing_upz09


matched_data_09 %>%
  filter(!is.na(upazila)) %>%
  select(-c(Longitude, Latitude)) %>%
  rbind(missing_upz09) -> matched_data_09

rm(missing_upz09)

## Three unmatched facs are removed

matched_data_09 %>%
  filter(!is.na(upazila)) -> matched_data_09

matched_data_01 %>%
  filter(!is.na(upazila)) -> matched_data_01


# Next, fix upazila to have congruence with the 2011 names

## Finding issues

matched_data_01 %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz01

matched_data_09 %>%
  filter(exist05 == 1) %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz91

rbind(upaz01, upaz91) %>% unique() -> fac_upaz

rm("upaz01", "upaz91")

# we find the following issues and solve them

## We change, all (i) Uttara and Biman Bandar to uttara, bimanbandar, (ii) pallbi to pallabi

matched_data_01$upazila[matched_data_01$upazila %in% c("uttara", "biman bandar")] <- "uttara, bimanbandar"

matched_data_01$upazila[matched_data_01$upazila == "pallbi"] <- "pallabi"

## (iii) Shabagh to shahbag, (iv) tejgaon ind. area to tejgaon industrial area, and

## (v) kadamtali to kadam tali

matched_data_01$upazila[matched_data_01$upazila == "shahbagh"] <- "shahbag"

matched_data_01$upazila[matched_data_01$upazila == "tejgaon ind. area"] <- "tejgaon industrial area"
matched_data_01$upazila[matched_data_01$upazila == "tejgoan ind. area"] <- "tejgaon industrial area"

matched_data_01$upazila[matched_data_01$upazila == "kadamtali"] <- "kadam tali"



# we find the following issues and solve them

## We change, all (i) Uttara and Biman Bandar to uttara, bimanbandar, (ii) pallbi to pallabi

matched_data_09$upazila[matched_data_09$upazila %in% c("uttara", "biman bandar")] <- "uttara, bimanbandar"

matched_data_09$upazila[matched_data_09$upazila == "pallbi"] <- "pallabi"

## (iii) Shabagh to shahbag, (iv) tejgaon ind. area to tejgaon industrial area, and

## (v) kadamtali to kadam tali

matched_data_09$upazila[matched_data_09$upazila == "shahbagh"] <- "shahbag"

matched_data_09$upazila[matched_data_09$upazila == "tejgaon ind. area"] <- "tejgaon industrial area"
matched_data_09$upazila[matched_data_09$upazila == "tejgoan ind. area"] <- "tejgaon industrial area"

matched_data_09$upazila[matched_data_09$upazila == "kadamtali"] <- "kadam tali"
matched_data_09$upazila[matched_data_09$upazila == "shabujbagh"] <- "sabujbagh"

matched_data_01 %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz01

matched_data_09 %>%
  filter(exist05 == 1) %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz91

rbind(upaz01, upaz91) %>% unique() -> fac_upaz

rm("upaz01", "upaz91")

## Removing upazilas from upazilas11 (all upazila file) that generates duplicates
# There are multiple Kaliganj, remove all but the one in Gazipur
## 406552 is lohagara not in chit, 405094 is mirpur not in Dhaka,
## 405566 is mohammadpur not in dhaka, 405595 is not in gazipur

fac_upaz %>%
  filter(!upaz2011 %in% c("555239", "552769", "408747", "404433", "406552", "405094", "405566", "405595")) -> fac_upaz

left_join(matched_data_01, fac_upaz) %>%
  mutate(across(c(upaz2011, parent), as.integer)) -> matched_data_01


left_join(matched_data_09, fac_upaz) %>%
  mutate(across(c(upaz2011, parent), as.integer)) -> matched_data_09

### Writing 91,01 data, then the 05 data, then the factory list

write.csv(matched_data_01, "~/large_datasets/BD Garments/04_final_data_01.csv", row.names = F)

write.csv(matched_data_01, "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/04_final_data_01.csv", row.names = F)


write.csv(matched_data_09, "~/large_datasets/BD Garments/04_final_data_09.csv", row.names = F)

write.csv(matched_data_09, "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/04_final_data_09.csv", row.names = F)