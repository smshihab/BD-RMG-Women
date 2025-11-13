library(pacman)
p_load(tidyverse, conflicted, janitor, sf, sp, labelled, units, readr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# load data sets: CROSSWALK data set
crosswalk <- read_csv("/Users/kamanga/Documents/Hamilton/Research/with Shihab/BD-RGM/codes/bartik_new/crosswalk.csv")

# split it into three by year and rename variables in preparation for merging
crosswalk_91 <- crosswalk %>%
  select(ipum1991, merged_id, source) %>%
  filter(source == "upazillas91") %>%
  rename(geo3_bd1991 = ipum1991)  %>%
  mutate( # convert from string to numeric
    geo3_bd1991 = as.numeric(geo3_bd1991)
    )

crosswalk_01 <- crosswalk %>%
  select(ipum2001, merged_id, source) %>%
  filter(source == "upazillas01") %>%
  rename(geo3_bd2001 = ipum2001)  %>%
  mutate( # convert from string to numeric
    geo3_bd2001 = as.numeric(geo3_bd2001)
    )

crosswalk_11 <- crosswalk %>%
  select(ipum2011, merged_id, source) %>%
  filter(source == "upazillas11") %>%
  rename(geo3_bd2011 = ipum2011) %>%
  mutate( # convert from string to numeric
    geo3_bd2011 = as.numeric(geo3_bd2011)
    )

# load data sets: IPUMS dataset
load("/Users/kamanga/Documents/Hamilton/Research/with Shihab/BD-RGM/codes/bartik_new/data.RData")

data <- data %>% clean_names() %>%
  select(year, urban, geo3_bd1991, geo3_bd2001, geo3_bd2011, ownership, electric, 
         famsize, nchild, nchlt5, age, sex, marst, religion, school, lit, yrschool, empstat, labforce, ind,
         starts_with(c("age","lit", "yrs", "empstat_", "lab", "ind"))) %>%
  mutate( # convert from string to numeric
    geo3_bd1991 = as.numeric(geo3_bd1991),
    geo3_bd2001 = as.numeric(geo3_bd2001),
    geo3_bd2011 = as.numeric(geo3_bd2011)
  )

# Creating grouping variables (I am not sure what these grouping variables are for)
data <- data %>%
  mutate(groups_lfp1564 = case_when(age %in% c(15:64) ~ "1564"),
         groups_lfp1529 = case_when(age %in% c(15:29) ~ "1529"),
         groups_lfp1520 = case_when(age %in% c(15:20) ~ "1520"),
         groups_mar = case_when(age %in% c(15:20) ~ "1520",
                                age %in% c(21:30) ~ "2130"),
         groups_fert = case_when(age %in% c(15:20) ~ "1520",
                                 age %in% c(21:30) ~ "2130",
                                 age %in% c(31:40) ~ "3040"),
         groups_hc = case_when(age %in% c(5:9) ~ "0509",
                               age %in% c(10:13) ~ "1013",
                               age %in% c(14:19) ~ "1419"))

# Merge with the CROSSWALK data sets
data <- left_join(data, crosswalk_91, by = "geo3_bd1991")

data <- left_join(data, crosswalk_01, by = c("geo3_bd2001", "source", "merged_id"))

data <- left_join(data, crosswalk_11, by = c("geo3_bd2011", "source", "merged_id"))


### Gets you general values

data %>%
  group_by(merged_id, year) %>%
  summarise(pop = n(),
            urban_share = sum(urban == 2)/n(),
            electrification = sum(electric == 1)/n(),
            age_1564_share = sum(age >= 15 & age <=64)/n()) %>%
  mutate(pop = ifelse(year == 2011, (pop/0.05), (pop/0.1)),
         age_1564_pop = age_1564_share * pop) %>%
  ungroup() -> general_data


# FLFP values

# creating an indicator of industrial LFP for ease

data %>%
  mutate(lfp_ind = case_when(
    year == 2011 & ind == 2 ~ 1,
    year == 2011 & ind != 2 ~ 0,
    year != 2011 & ind %in% c(05, 06, 07) ~ 1,
    year != 2011 & ! ind %in% c(05, 06, 07) ~ 0)) -> data

# 15 - 64 year olds

data %>%
  group_by(merged_id, year, sex, groups_lfp1564) %>%
  summarise(lfp = sum(labforce %in% c(2))/ n(),
            lfp_ind = sum(lfp_ind == 1) / n(),
            yrschool1564 = mean(yrschool[yrschool < 50])) %>%
  filter(!is.na(groups_lfp1564)) %>% ungroup() -> lfp_1564


lfp_1564 %>% 
  pivot_wider(names_from = sex, values_from = c("lfp", "lfp_ind", "yrschool1564")) %>%
  rename(m_yrschool1564 = yrschool1564_1, f_yrschool1564 = yrschool1564_2,
         m_lfp1564 = lfp_1, f_lfp1564 = lfp_2, m_lfp_ind1564 =  lfp_ind_1,
         f_lfp_ind1564 = lfp_ind_2) %>% select(-groups_lfp1564) -> lfp_1564

# 15 - 29 year olds

data %>%
  group_by(merged_id, year, sex, groups_lfp1529) %>%
  summarise(lfp = sum(labforce %in% c(2))/ n(),
            lfp_ind = sum(lfp_ind == 1) / n()) %>%
  filter(!is.na(groups_lfp1529)) %>% ungroup() -> lfp_1529

lfp_1529 %>% 
  pivot_wider(names_from = sex, values_from = c("lfp", "lfp_ind")) %>%
  rename(m_lfp1529 = lfp_1, f_lfp1529 = lfp_2, m_lfp_ind1529 =  lfp_ind_1,
         f_lfp_ind_1529 = lfp_ind_2) %>% select(-groups_lfp1529) -> lfp_1529

# 15 - 20 year olds

data %>%
  group_by(merged_id, year, sex, groups_lfp1520) %>%
  summarise(lfp = sum(labforce %in% c(2))/ n(),
            lfp_ind = sum(lfp_ind == 1) / n()) %>%
  filter(!is.na(groups_lfp1520)) %>% ungroup() -> lfp_1520

lfp_1520 %>% 
  pivot_wider(names_from = sex, values_from = c("lfp", "lfp_ind")) %>%
  rename(m_lfp1520 = lfp_1, f_lfp1520 = lfp_2, m_lfp_ind1520 =  lfp_ind_1,
         f_lfp_ind1520 = lfp_ind_2) %>% select(-groups_lfp1520) -> lfp_1520

# obtaining all marriage data

data %>%
  group_by(merged_id, year, groups_mar) %>%
  summarise(married = sum(marst %in% c(2,3,4)) / n()) %>%
  filter(!is.na(groups_mar)) %>% ungroup() -> ever_married

ever_married %>% 
  pivot_wider(names_from = groups_mar, values_from = married,
              names_glue = "married_{groups_mar}") -> ever_married

## fertility data

data %>%
  group_by(merged_id, year, groups_fert) %>%
  summarise(fertility = mean(nchild )) %>%
  filter(!is.na(groups_fert)) %>% ungroup() -> fertility_df

fertility_df %>% 
  pivot_wider(names_from = groups_fert, values_from = fertility,
              names_glue = "fertility_{groups_fert}")-> fertility_df

# Education variables

data %>%
  group_by(merged_id, year, sex, groups_hc) %>%
  summarise(enrollment = sum(school == 1) / n(),
            literacy = sum(lit == 2) / n(),
            yrschool = mean(yrschool[yrschool < 50])) %>%
  filter(!is.na(groups_hc)) %>% ungroup() -> human_cap

human_cap %>% 
  pivot_wider(names_from = c("sex","groups_hc"),  
              values_from = c("enrollment", "literacy", "yrschool")) -> human_cap

colnames(human_cap) <- sub("enrollment_1_", "m_enrollment_", colnames(human_cap))
colnames(human_cap) <- sub("enrollment_2_", "f_enrollment_", colnames(human_cap))
colnames(human_cap) <- sub("literacy_1_", "m_literacy_", colnames(human_cap))
colnames(human_cap) <- sub("literacy_2_", "f_literacy_", colnames(human_cap))
colnames(human_cap) <- sub("yrschool_1_", "m_yrschool_", colnames(human_cap))
colnames(human_cap) <- sub("yrschool_2_", "f_yrschool_", colnames(human_cap))

lfp_data <-   left_join(lfp_1520, lfp_1529, by = c("merged_id", "year")) %>%
  left_join(lfp_1564, by = c("merged_id", "year"))

rm("lfp_1520", "lfp_1529", "lfp_1564")

# add HC
upazila_data <- left_join(lfp_data, human_cap)

rm("human_cap", "lfp_data")

upazila_data <- left_join(upazila_data, general_data)

rm("general_data")

# now fertility data
upazila_data <- left_join(upazila_data, fertility_df)

upazila_data <- left_join(upazila_data, ever_married)

upazila_data <- remove_labels(upazila_data)

# 1991 dataset for area

upazilas91  <- "/Users/kamanga/Documents/Hamilton/Research/with Shihab/BD-RGM/data/geo3_bd1991.shp"

upazilas91 <- st_read(upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
upazilas91 <- st_as_sf(upazilas91) %>% st_transform(3106) %>%
  select(contains("IP")) %>%
  clean_names() %>%
  mutate_if(is.character,as.integer) %>%
  mutate(area91 = st_area(geometry) %>% drop_units(),
         area91 = area91 / 1000^2) %>%
  as.data.frame() %>% select(-geometry) %>%
  rename(geo3_bd1991 = ipum1991)

# Merge with the CROSSWALK data sets
upazilas91 <- left_join(upazilas91, crosswalk_91)

# clean environment by delete unnecessary files
rm(crosswalk_01, crosswalk_11, crosswalk_91, crosswalk)

upazila_data <- left_join(upazila_data, upazilas91)

# remove unnecessary files
rm(upazilas91, ever_married, fertility_df, data)

# save the file
save(upazila_data, file = "/Users/kamanga/Documents/Hamilton/Research/with Shihab/BD-RGM/codes/bartik_new/upazila_data.RData")
