library(pacman)

p_load(tidyverse, readxl, janitor, labelled, stringr, fuzzyjoin)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Loading the 2000-01 and 09-10 file
bgmea_01 <- read_excel("~/large_datasets/BD Garments/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_01") %>%
  select(-c(...7, obs_year)) %>%
  mutate(name = tolower(name)) %>%
  filter(!duplicated(bgmea_num))

bgmea_09 <- read_excel("~/large_datasets/BD Garments/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_09") %>%
  select(-obs_year) %>%
  mutate(name = tolower(name)) %>%
  filter(!duplicated(bgmea_num))

# Loading the 2013 file

bgmea_13 <- read_excel("~/large_datasets/BD Garments/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_2015_final") %>%
  mutate(name = tolower(name)) %>%
  select(name, date_est, fac_type, fac_ad, Latitude, Longitude, upazila)


# Loading the NYU

bgmea_NYU <- read.csv("~/large_datasets/BD Garments/bangladesh_factory_list_NYU.csv") %>%
  mutate_if(is.character, ~tolower(.)) %>%
  filter(str_detect(member_of, "bgmea") | str_detect(member_of_stated, "bgmea") | str_detect(source, "bgmea"))
  
# Loading the latest BGMEA file

bgmea_21 <- read.csv("C:/Users/smshi/Dropbox/Research/Data/BD RMG factory data/BGMEA Scrape/facdata.csv") %>%
  mutate_if(is.character, ~tolower(.)) %>%
  mutate(bgmea_num = as.numeric(bgmea),
         fac_type = as.numeric(case_when(type %in% c("sweater", "knit") ~ "1",
                                         type  == "woven" ~ "0",
                                         type  == "mixed" ~ "0.5",
                                         TRUE ~ "NA"))) %>%
  select(name, bgmea_num, fac_type, Latitude, Longitude, fac_ad, date_est)


# Cleaning the names in 2000-01, 2009-10 and 2013 file for matching

# getting rid of "['-,.:·]", then changing pvt to private, limited to ltd, & to, making fashion a separate word in all cases, and and changing a accent to a

bgmea_01$name <- str_replace_all(bgmea_01$name, "['-,.:·\\(\\)]", "")
  
bgmea_01$name <- str_replace_all(bgmea_01$name, "(?<![:space:])pvt(?![:space:])|(?<![:space:])pvt|(?![:space:])pvt", " pvt ")
  
bgmea_01$name <- str_replace_all(bgmea_01$name, "(?<![:space:])limited(?![:space:])|(?<![:space:])limited|(?![:space:])limited", " ltd ")

## Changing fashions to fashion

bgmea_01$name <- str_replace_all(bgmea_01$name, "fashions", " fashion ")
bgmea_01$name <- str_replace_all(bgmea_01$name, "(?<![:space:])fashion(?![:space:])|(?<![:space:])fashion|(?![:space:])fashion", " fashions ")

bgmea_01$name <- str_replace_all(bgmea_01$name, "&", " and ")

bgmea_01$name <- str_replace_all(bgmea_01$name, "á", "a")

bgmea_01$name <- str_replace_all(bgmea_01$name, "\\s+", " ")


### 2009-10 files

bgmea_09$name <- str_replace_all(bgmea_09$name, "['-,.:·\\(\\)]", "")

bgmea_09$name <- str_replace_all(bgmea_09$name, "(?<![:space:])pvt(?![:space:])|(?<![:space:])pvt|(?![:space:])pvt", " pvt ")

bgmea_09$name <- str_replace_all(bgmea_09$name, "(?<![:space:])limited(?![:space:])|(?<![:space:])limited|(?![:space:])limited", " ltd ")

## Changing fashions to fashion

bgmea_09$name <- str_replace_all(bgmea_09$name, "fashions", " fashion ")
bgmea_09$name <- str_replace_all(bgmea_09$name, "(?<![:space:])fashion(?![:space:])|(?<![:space:])fashion|(?![:space:])fashion", " fashions ")

bgmea_09$name <- str_replace_all(bgmea_09$name, "&", " and ")

bgmea_09$name <- str_replace_all(bgmea_09$name, "á", "a")

bgmea_09$name <- str_replace_all(bgmea_09$name, "\\s+", " ")


### Cleaning 2013 files
  
bgmea_13$name <- str_replace_all(bgmea_13$name, "['-,.:·\\(\\)]", "")

bgmea_13$name <- str_replace_all(bgmea_13$name, "(?<![:space:])pvt(?![:space:])|(?<![:space:])pvt|(?![:space:])pvt", " pvt ")

bgmea_13$name <- str_replace_all(bgmea_13$name, "(?<![:space:])limited(?![:space:])|(?<![:space:])limited|(?![:space:])limited", " ltd ")

bgmea_13$name <- str_replace_all(bgmea_13$name, "fashions", " fashion ")
bgmea_13$name <- str_replace_all(bgmea_13$name, "(?<![:space:])fashion(?![:space:])|(?<![:space:])fashion|(?![:space:])fashion", " fashions ")

bgmea_13$name <- str_replace_all(bgmea_13$name, "&", " and ")

bgmea_13$name <- str_replace_all(bgmea_13$name, "á", "a")

bgmea_13$name <- str_replace_all(bgmea_13$name, "\\s+", " ")

bgmea_13 %>%
  filter(!duplicated(name)) %>%
  filter(date_est < 2002) -> bgmea_13_for_matching

rm("bgmea_13")


match_01 <- inner_join(bgmea_01, bgmea_13_for_matching, by = "name")

not_match_01 <- anti_join(bgmea_01, match_01, by = "bgmea_num")

# Using the 2021 bgmea dataset to match using the bgmea number

inner_join(not_match_01, bgmea_21 %>% select(-name), by = "bgmea_num") %>%
  mutate(upazila = NA) %>%
  rbind(match_01) -> match_01


not_match_01 <- anti_join(bgmea_01, match_01, by = "bgmea_num")

updated_bgmea_13_for_matching <- anti_join(bgmea_13_for_matching, match_01, by = "name")

not_match_01_try_bgmea <- inner_join(not_match_01, bgmea_09 %>% select(bgmea_num), by = "bgmea_num")

not_match_01_try_others <- anti_join(not_match_01, not_match_01_try_bgmea, by = "bgmea_num")

# rm("bgmea_09", "not_match_01")



## The following code was used to use soundex to find matches


#stringdist_join(not_match_01_try_bgmea, updated_bgmea_13_for_matching,
                #by = "name",
                #method = "soundex") -> soundex_tryBGMEA13

#write.csv(stringdist_join, "~/large_datasets/BD Garments/01_bgmea_factory_soundexMATCH13.csv")

## soundex_tryBGMEA13 was exported and manually sorted for genuine matches


# Start running codes from here

soundex_tryBGMEA13 <- read.csv("~/large_datasets/BD Garments/01_bgmea_factory_soundexMATCH13_completed.csv") %>%
  filter(X == 1) %>%
  select(-c(name.y, X)) %>%
  rename(name = name.x)

match_01 <- rbind(match_01, soundex_tryBGMEA13)

not_match_01_try_bgmea <- anti_join(not_match_01_try_bgmea, soundex_tryBGMEA13, by = "bgmea_num")

#rm("soundex_tryBGMEA13", "updated_bgmea_13_for_matching", "bgmea_13_for_matching")

### Now try 21 with BGMEA number and does not work. 
## Since both datasets has the same unique BGMEA number, checking with soundex is not needed.  

### Now try BGMEA members in NYU file - but too many unmatches

not_match_01 <- rbind(not_match_01_try_bgmea, not_match_01_try_others)

##  Export the unmatched stuff to manually work on them.

## write.csv(not_match_01, "~/large_datasets/BD Garments/02_not_match_01.csv", row.names = FALSE)

#rm(list = setdiff(ls(), c("match_01", "not_match_01")))

manually_matched_BGMEA_01 <- read.csv("~/large_datasets/BD Garments/02_manually_matched_BGMEA_01.csv")

manually_matched_BGMEA_01 %>%
  filter(upazila != "") %>%
  mutate(Latitude = NA, Longitude = NA, fac_ad = NA, date_est = NA) %>%
  select(-remarks) -> manually_matched_BGMEA_01

# Export matched data for use in awesome table

all_matched_data_01 <- rbind(manually_matched_BGMEA_01, match_01)

# Assigning machines to be their mean values

all_matched_data_01$machine[is.na(all_matched_data_01$machine)] <- mean(all_matched_data_01$machine, na.rm=T)

# Only working with all_matched data

keep = c("all_matched_data_01", "bgmea_01", "bgmea_09", "bgmea_21", "updated_bgmea_13_for_matching")


# remove unncessary data

rm(list = setdiff(ls(), keep))


#### Working with factory data


### Recall that I can use information about upazila,date and fac types
### from directory of 2000-01 (worked on extensively) to 2009-10 if
### They are matches, so only issue is with non-matches, aka-new facs.

# Joining info from data from 2001 to data from 2009 

matches_09 <- inner_join(bgmea_09 %>% rename(machine09 = machine), 
                         all_matched_data_01 %>% 
                            select(-c(name, capacity, product_type)), 
                            by = "bgmea_num")

# Assigning 2009 machines to be their mean values

matches_09$machine09[is.na(matches_09$machine09)] <- mean(matches_09$machine09, na.rm=T)

# machine in 2005 is the average between 2005 and 2009
# if found in 2001 and 2009, it surely existed in 2005

matches_09 %>%
  mutate(machine = (machine + machine09)/2,
         exist05 = 1) %>%
  select(-machine09) -> matches_09

# finding firms that started between 2000-01 to 2009-10

non_matches_09 <- anti_join(bgmea_09, bgmea_01, by = "bgmea_num")

# matching them with 2001 bGMEA DATA


match_09_21 <- inner_join(non_matches_09, bgmea_21 %>% select(-name), by = "bgmea_num")

match_09_21 %>%
  mutate(upazila = NA,
         exist05 = case_when(date_est <2006 ~ 1,
                      TRUE ~ 0)) -> match_09_21

all_matched_data_09 <- rbind(match_09_21, matches_09)

rm(list = setdiff(ls(), c("all_matched_data_01", "all_matched_data_09")))


### Placing the 2000 and 2001 established data in 01 file


all_matched_data_01 %>%
  filter(date_est %in% c(2000, 2001)) -> matching01

all_matched_data_09 %>%
  filter(date_est %in% c(2000, 2001)) -> matching09

anti_join(matching09, matching01, by = "bgmea_num") -> matching09


all_matched_data_01 <- rbind(all_matched_data_01,
                             matching09 %>% select(-exist05))
  

write.csv(all_matched_data_01, "~/large_datasets/BD Garments/03_all_matched_data_01.csv", row.names = F) 

write.csv(all_matched_data_09, "~/large_datasets/BD Garments/03_all_matched_data_09.csv", row.names = F)