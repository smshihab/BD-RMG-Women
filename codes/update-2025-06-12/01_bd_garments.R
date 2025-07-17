library(pacman)

p_load(tidyverse, stringr, readxl, janitor, labelled, stringr, fuzzyjoin, sf, sp)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


######### Loading data #########
######### Loading data #########
# Loading the 2000-01 and 09-10 file
bgmea_01 <- read_excel("~/large_datasets/BD Garments/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_01") %>%
  select(-obs_year) %>%
  mutate(across(where(is.character), tolower)) %>%
  filter(!duplicated(bgmea_num))

bgmea_09 <- read_excel("~/large_datasets/BD Garments/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_09") %>%
  select(-obs_year) %>%
  mutate(across(where(is.character), tolower)) %>%
  filter(!duplicated(bgmea_num))

# Loading the 2013 file

bgmea_13 <- read_excel("~/large_datasets/BD Garments/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_2015_final") %>%
  mutate(across(where(is.character), tolower)) %>%
  select(name, date_est, fac_type, fac_ad, Latitude, Longitude, upazila)


# Loading the latest BGMEA file

bgmea_21 <- read.csv("C:/Users/smshi/Dropbox/Research/Data/BD RMG factory data/BGMEA Scrape/facdata.csv") %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(bgmea_num = as.numeric(bgmea),
         fac_type = as.numeric(case_when(type %in% c("sweater", "knit") ~ "1",
                                         type  == "woven" ~ "0",
                                         type  == "mixed" ~ "0.5",
                                         TRUE ~ "NA"))) %>%
  select(name, bgmea_num, fac_type, Latitude, Longitude, fac_ad, date_est)


######### Creating an Upazila and greater region cross-walk #########
######### Creating an Upazila and greater region cross-walk #########

cross_walk_geo  <- "C:/Users/smshi/Dropbox/Research/smshi_ra/SpatialCleaning/output/crosswalk_bdgeo3_91_11.shp"

cross_walk_geo <- st_read(cross_walk_geo, quiet = TRUE)

cross_walk_geo %>% filter(!is.na(ipum1991)) -> cross_walk_geo

cross_walk_geo %>% 
  group_by(merged_id) %>%
  summarise(geometry = st_union(geometry)) -> cross_walk_geo

# Convert to Sf and transform to Mercator Gulshan
cross_walk_geo <- st_as_sf(cross_walk_geo) %>% st_transform(3106)

cross_walk_geo %>% summarise(geometry = st_union(geometry)) %>% mutate(id = "bd") -> bd


# Note that only bgmea 13 has named upazilas 

# setting up GIS
bgmea_13 -> temp_loc_file

coordinates(temp_loc_file) <- c("Longitude", "Latitude")

# Set projection string
proj4string(temp_loc_file) <- CRS("+init=epsg:4326") 

# Convert to Sf and transform to Mercator Gulshan
temp_loc_file <- st_as_sf(temp_loc_file) %>% st_transform(3106)

# are they in bd?
st_intersection(temp_loc_file, bd) -> in_bd

dplyr::anti_join(bgmea_13, in_bd) -> wrong_geo_bgmea13

# Obtain geometry of those that are in bd
st_intersection(temp_loc_file, cross_walk_geo) %>% st_drop_geometry() -> temp_loc_file

inner_join(bgmea_13, temp_loc_file) -> bgmea_13

## now deal with the unmatach ones, create a crosswalk with distric, upazila and merged_id

wrong_geo_bgmea13 %>%
  mutate(fac_district =  str_extract(fac_ad, '\\b[^,]+$')) -> wrong_geo_bgmea13

wrong_geo_bgmea13 %>% select(fac_district, upazila) %>% unique() -> dis_upaz_pairs

#manually adding meregd id
dis_upaz_pairs %>%
  mutate(merged_id = c(269, 271, 264, 229, 180, 94, 94, 117, 94, 442, 79, 61, 60, 427)) -> dis_upaz_pairs
  
left_join(wrong_geo_bgmea13, dis_upaz_pairs) -> wrong_geo_bgmea13

wrong_geo_bgmea13 %>% select(-fac_district) %>%
  rbind.data.frame(bgmea_13) -> bgmea_13

#### Now with 2021 file

# setting up GIS
bgmea_21 -> temp_loc_file

coordinates(temp_loc_file) <- c("Longitude", "Latitude")

# Set projection string
proj4string(temp_loc_file) <- CRS("+init=epsg:4326") 

# Convert to Sf and transform to Mercator Gulshan
temp_loc_file <- st_as_sf(temp_loc_file) %>% st_transform(3106)

# are they in bd?
st_intersection(temp_loc_file, bd) -> in_bd

dplyr::anti_join(bgmea_21, in_bd) -> wrong_geo_bgmea21

# Obtain geometry of those that are in bd
st_intersection(temp_loc_file, cross_walk_geo) %>% st_drop_geometry() -> temp_loc_file

inner_join(bgmea_13, temp_loc_file) -> bgmea_13

## now deal with the unmatach ones, create a crosswalk with distric, upazila and merged_id

wrong_geo_bgmea13 %>%
  mutate(fac_district =  str_extract(fac_ad, '\\b[^,]+$')) -> wrong_geo_bgmea13

wrong_geo_bgmea13 %>% select(fac_district, upazila) %>% unique() -> dis_upaz_pairs






# Cleaning the names in 2000-01, 2009-10 and 2013 file for matching

# getting rid of "['-,.:?]", then changing pvt to private, limited to ltd, & to, making fashion a separate word in all cases, and and changing a accent to a

bgmea_01$name <- str_replace_all(bgmea_01$name, "['-,.:?\\(\\)]", "")
  
bgmea_01$name <- str_replace_all(bgmea_01$name, "(?<![:space:])pvt(?![:space:])|(?<![:space:])pvt|(?![:space:])pvt", " pvt ")
  
bgmea_01$name <- str_replace_all(bgmea_01$name, "(?<![:space:])limited(?![:space:])|(?<![:space:])limited|(?![:space:])limited", " ltd ")

## Changing fashions to fashion

bgmea_01$name <- str_replace_all(bgmea_01$name, "fashions", " fashion ")

bgmea_01$name <- str_replace_all(bgmea_01$name, "(?<![:space:])fashion(?![:space:])|(?<![:space:])fashion|(?![:space:])fashion", " fashions ")

bgmea_01$name <- str_replace_all(bgmea_01$name, "&", " and ")

#bgmea_01$name <- str_replace_all(bgmea_01$name, "?", "a")

bgmea_01$name <- str_replace_all(bgmea_01$name, "\\s+", " ")


### 2009-10 files

bgmea_09$name <- str_replace_all(bgmea_09$name, "['-,.:?\\(\\)]", "")

bgmea_09$name <- str_replace_all(bgmea_09$name, "(?<![:space:])pvt(?![:space:])|(?<![:space:])pvt|(?![:space:])pvt", " pvt ")

bgmea_09$name <- str_replace_all(bgmea_09$name, "(?<![:space:])limited(?![:space:])|(?<![:space:])limited|(?![:space:])limited", " ltd ")

## Changing fashions to fashion

bgmea_09$name <- str_replace_all(bgmea_09$name, "fashions", " fashion ")
bgmea_09$name <- str_replace_all(bgmea_09$name, "(?<![:space:])fashion(?![:space:])|(?<![:space:])fashion|(?![:space:])fashion", " fashions ")

bgmea_09$name <- str_replace_all(bgmea_09$name, "&", " and ")

#bgmea_09$name <- str_replace_all(bgmea_09$name, "?", "a")

bgmea_09$name <- str_replace_all(bgmea_09$name, "\\s+", " ")


### Cleaning 2013 files
  
bgmea_13$name <- str_replace_all(bgmea_13$name, "['-,.:?\\(\\)]", "")

bgmea_13$name <- str_replace_all(bgmea_13$name, "(?<![:space:])pvt(?![:space:])|(?<![:space:])pvt|(?![:space:])pvt", " pvt ")

bgmea_13$name <- str_replace_all(bgmea_13$name, "(?<![:space:])limited(?![:space:])|(?<![:space:])limited|(?![:space:])limited", " ltd ")

bgmea_13$name <- str_replace_all(bgmea_13$name, "fashions", " fashion ")
bgmea_13$name <- str_replace_all(bgmea_13$name, "(?<![:space:])fashion(?![:space:])|(?<![:space:])fashion|(?![:space:])fashion", " fashions ")

bgmea_13$name <- str_replace_all(bgmea_13$name, "&", " and ")

#bgmea_13$name <- str_replace_all(bgmea_13$name, "?", "a")

bgmea_13$name <- str_replace_all(bgmea_13$name, "\\s+", " ")

# obtaining BGMEA 2013 data for matching\

bgmea_13 %>%
  filter(!duplicated(name)) %>%
  filter(date_est < 2002) -> bgmea_13_for_matching

rm("bgmea_13")

# matching BGMEA 2000-01 dataset with BGMEA 2013 by name

match_01 <- inner_join(bgmea_01, bgmea_13_for_matching, by = "name")

# Extracting the non-matches in 2000-01 dataset

not_match_01 <- anti_join(bgmea_01, match_01, by = "bgmea_num")

# Using the 2021 BGMEA dataset to match using the BGMEA number

inner_join(not_match_01, bgmea_21 %>% select(-name), by = "bgmea_num") %>%
  mutate(upazila = NA) %>%
  rbind(match_01) -> match_01

# Extracting the non-matches in 2000-01 dataset
not_match_01 <- anti_join(bgmea_01, match_01, by = "bgmea_num")

# Extracting the non-matches in BGMEA 2013 dataset
updated_bgmea_13_for_matching <- anti_join(bgmea_13_for_matching, match_01, by = "name")

# Separating BGMEA 2000-01 non-matches into factories that are in BGMEA in 2009-10
not_match_01_try_bgmea <- inner_join(not_match_01, bgmea_09 %>% select(bgmea_num), by = "bgmea_num")

# Separating BGMEA 2000-01 non-matches into factories that are NOT in BGMEA in 2009-10
not_match_01_try_others <- anti_join(not_match_01, not_match_01_try_bgmea, by = "bgmea_num")


## The following code was used to use soundex to find matches

#stringdist_join(not_match_01_try_bgmea, updated_bgmea_13_for_matching,
                #by = "name",
                #method = "soundex") -> soundex_tryBGMEA13

## soundex_tryBGMEA13 was exported and manually sorted for genuine matches
#write.csv(stringdist_join, "~/large_datasets/BD Garments/01_bgmea_factory_soundexMATCH13.csv")

### Start running codes from here

soundex_tryBGMEA13 <- read.csv("~/large_datasets/BD Garments/01_bgmea_factory_soundexMATCH13_completed.csv") %>%
  filter(X == 1) %>%
  select(-c(name.y, X)) %>%
  rename(name = name.x)

match_01 <- rbind(match_01, soundex_tryBGMEA13)

# separating out the soundex matches from non-matches

not_match_01_try_bgmea <- anti_join(not_match_01_try_bgmea, soundex_tryBGMEA13, by = "bgmea_num")

# All of non-mactches in 2000-01 file
not_match_01 <- rbind(not_match_01_try_bgmea, not_match_01_try_others)

### Tried soundex matches with BGMEA members in NYU file - but too many unmatches.

##  Export the unmatched BGMEA 2000-01 to manually work on them.

## write.csv(not_match_01, "~/large_datasets/BD Garments/02_not_match_01.csv", row.names = FALSE)

#rm(list = setdiff(ls(), c("match_01", "not_match_01")))

manually_matched_BGMEA_01 <- read.csv("~/large_datasets/BD Garments/02_manually_matched_BGMEA_01.csv")

manually_matched_BGMEA_01 %>%
  filter(upazila != "") %>%
  mutate(Latitude = NA, Longitude = NA, fac_ad = NA, date_est = NA) %>%
  select(-remarks) -> manually_matched_BGMEA_01

# binding data

all_matched_data_01 <- rbind(manually_matched_BGMEA_01, match_01)

all_matched_data_01 %>%
  mutate(machine_pred = case_when(is.na(machine) ~ 1, TRUE ~ 0)) -> all_matched_data_01

# Assigning missing machines to be their mean values

all_matched_data_01$machine[is.na(all_matched_data_01$machine)] <- mean(all_matched_data_01$machine, na.rm=T)

# Separating the all_matched data to those with and without date of establishment

all_matched_data_01 %>% filter(!is.na(date_est) & 
                                 date_est < 2002) -> has_date

all_matched_data_01 %>% filter(is.na(date_est)) -> has_no_date

### OLS for prediction based on those that has date

reg_date <- lm(date_est ~ bgmea_num, has_date)


has_date$predicted_year <- round(reg_date$fitted.values, digits = 0)

# obtaining existence estimation error rate.

has_date %>%
  mutate(exist91 = case_when(date_est <1992 ~ 1,
                             TRUE ~ 0),
         exist01 = case_when(date_est <2002 ~ 1,
                             TRUE ~ 0),
         exist91_pred = case_when(predicted_year <1992 ~ 1,
                                  TRUE ~ 0),
         exist01_pred = case_when(predicted_year <2002 ~ 1,
                                  TRUE ~ 0)) -> has_date

#(sum(abs(has_date$exist91 - has_date$exist91_pred)) + sum(abs(has_date$exist01 - has_date$exist01_pred))) / (2*2159)

# providing the predicted values to those that do not have date of establishment

has_no_date$date_est <- round(predict(reg_date, has_no_date), digits = 0)

# existence value creator

has_no_date %>%
  mutate(exist91 = case_when(date_est <1992 ~ 1,
                             TRUE ~ 0),
         exist01 = case_when(date_est <2002 ~ 1,
                             TRUE ~ 0)) %>%
  mutate(predicted_date = 1) -> has_no_date

has_date %>% select(-c(predicted_year, exist91_pred, exist01_pred)) %>%
  mutate(predicted_date = 0) %>%
  rbind(has_no_date) -> all_matched_data_01


# cleaning environment

keep = c("all_matched_data_01", "bgmea_01", "bgmea_09", "bgmea_21", "updated_bgmea_13_for_matching")

rm(list = setdiff(ls(), keep))


### Recall that I can use information about upazila,date and fac types
### from directory of 2000-01 (worked on extensively) to 2009-10 if
### They are matches, so only issue is with non-matches, aka-new facs.

# Joining info from data from 2001 to data from 2009 

matches_09 <- inner_join(bgmea_09 %>% rename(machine09 = machine), 
                         all_matched_data_01 %>% 
                            select(-c(name, capacity, product_type, exist91, exist01)), 
                            by = "bgmea_num")

# Assigning 2009 machines to be their mean values

matches_09 %>% mutate(machine_pred = case_when(is.na(machine) ~ 1,
                                                 TRUE ~ 0)) -> matches_09

matches_09$machine09[is.na(matches_09$machine09)] <- mean(matches_09$machine09, na.rm=T)

# machine in 2006 is the average between 2001 and 2009
# if found in 2001 and 2009, it surely existed in 2006

matches_09 %>%
  mutate(machine = (machine + machine09)/2,
         exist06 = 1) %>%
  select(-machine09) -> matches_09

# finding firms that started between 2000-01 to 2009-10
non_matches_09 <- anti_join(bgmea_09, bgmea_01, by = "bgmea_num")

# matching them with 2021 BGMEA data

match_09_21 <- inner_join(non_matches_09, bgmea_21 %>% select(-name), by = "bgmea_num")

match_09_21 %>%
  mutate(upazila = NA, predicted_date = 0,
         machine_pred = case_when(is.na(machine) ~ 1, TRUE ~ 0),
         exist06 = case_when(date_est <= 2006 ~ 1, TRUE ~ 0))  -> match_09_21

# mean machine for missing machine
match_09_21$machine[is.na(match_09_21$machine)] <- mean(match_09_21$machine, na.rm=T)

# Combining the BGMEA 2009-10 matches so far
all_matched_data_09 <- rbind(matches_09, match_09_21)

# Separating the BGMEA 2009-10 matches so far
non_matches_09 <- anti_join(non_matches_09,all_matched_data_09, by = "bgmea_num")

# Estimating the date of establishment and will work only with those that were likely to exist in 2006

reg_date <- lm(date_est ~ bgmea_num, all_matched_data_09)

non_matches_09$date_est <- round(predict(reg_date, non_matches_09), digits = 0)

# only keeping factories that provides 2006 estimates

non_matches_09 %>%
  filter(date_est <= 2006) -> non_matches_09

## Export these data for manual matching
# write.csv(non_matches_09, "~/large_datasets/BD Garments/02_not_match_09.csv", row.names = FALSE)

manually_matched_BGMEA_09 <- read.csv("~/large_datasets/BD Garments/02_manually_matched_BGMEA_09.csv")

manually_matched_BGMEA_09 %>%
  filter(upazila != "") %>%
  mutate(exist06 = 1, Latitude = NA, Longitude = NA, fac_ad = NA,
         predicted_date = 1,
         machine_pred = case_when(is.na(machine) ~ 1, 
                                    TRUE ~0)) -> manually_matched_BGMEA_09
# Bind the relevant 2009-10 data

all_matched_data_09 <- rbind(all_matched_data_09, manually_matched_BGMEA_09)

rm(list = setdiff(ls(), c("all_matched_data_01", "all_matched_data_09")))

### Placing the data of factories established in 2000 and 2001 in dataset for 2001

all_matched_data_01 %>%
  filter(date_est %in% c(2000, 2001)) -> matching01

all_matched_data_09 %>%
  filter(date_est %in% c(2000, 2001)) -> matching09

anti_join(matching09, matching01, by = "bgmea_num") -> matching09

all_matched_data_01 <- rbind(matching09 %>% select(-c(exist06)) %>%
                               mutate(exist91 = 0, exist01 = 1),
                             all_matched_data_01)



# Loading the 2013 file

bgmea_11 <- read_excel("~/large_datasets/BD Garments/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_2015_final") %>%
  mutate(name = tolower(name)) %>%
  filter(date_est <2011)


# Loading the latest BGMEA file

bgmea_21 <- read.csv("C:/Users/smshi/Dropbox/Research/Data/BD RMG factory data/BGMEA Scrape/facdata.csv") %>%
  mutate_if(is.character, ~tolower(.)) %>%
  mutate(bgmea_num = as.numeric(bgmea),
         fac_type = as.numeric(case_when(type %in% c("sweater", "knit") ~ "1",
                                         type  == "woven" ~ "0",
                                         type  == "mixed" ~ "0.5",
                                         TRUE ~ "NA")))


save(list = c("all_matched_data_01", "all_matched_data_09", "bgmea_11", "bgmea_21"), file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/factories_2025-01-26.Rdata")


#all_matched_data_01 %>% select(-predicted_date) -> all_matched_data_01

#all_matched_data_09 %>% select(-c(predicted_date, machine09_pred)) -> all_matched_data_09

########## manually correct the exported files fac_types ##########
########## manually correct the exported files fac_types ##########
########## manually correct the exported files fac_types ##########

# Exporting files

write.csv(all_matched_data_01, "~/large_datasets/BD Garments/03_all_matched_data_01.csv", row.names = F) 
write.csv(all_matched_data_09, "~/large_datasets/BD Garments/03_all_matched_data_09.csv", row.names = F)

########## manually correct the exported files fac_types ##########
########## manually correct the exported files fac_types ##########
########## manually correct the exported files fac_types ##########