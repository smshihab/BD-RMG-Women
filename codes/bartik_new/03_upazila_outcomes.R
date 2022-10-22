library(pacman)
p_load(tidyverse, conflicted, janitor, sf, sp, labelled, units)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Loading Upazila data and the outcome data in census

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.RData")

# 1991 dataset

factory_upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

factory_upazilas91 <- st_read(factory_upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas91 <- st_as_sf(factory_upazilas91) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())

factory_upazilas <- left_join(
  factory_upazilas, factory_upazilas91 %>% as.data.frame() %>%
    select(area91, ipum1991)) %>%
  mutate(across(-upazila, ~as.integer(.))) %>%
  mutate(area91 = area91 / 1000^2)

# Data is the IPUMS dataset from Bangladesh that covers the variables for parents and spouse in same HH.
load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data.Rdata")

data <- data %>% clean_names() %>%
  select(year, urban, geo3_bd1991, geo3_bd2001, geo3_bd2011, ownership, electric, 
         famsize, nchild, nchlt5, age, sex, marst, religion, school, lit, yrschool, empstat, labforce, ind,
       starts_with(c("age","lit", "yrs", "empstat_", "lab", "ind")))

# Creating grouping variables

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
                               
# Collecting the variables available
#as.data.frame(names(data)) -> variables_in_data

factory_upazilas$ipum1991[factory_upazilas$ipum1991 == 20019072] <- 20019067

# samples to select from IPUMS data

ninetyone_sample <- factory_upazilas$ipum1991 %>% unique()
twentyone_sample <- factory_upazilas$ipum2001 %>% unique()
twenty11_sample <-  factory_upazilas$ipum2011 %>% unique()

### Upazila data in 1991

### Gets you general values

# Selecting sample

fac_up_sample <- data %>%
  filter(geo3_bd1991 %in% ninetyone_sample | geo3_bd2001 %in% twentyone_sample  | geo3_bd2011 %in% twenty11_sample)

fac_up_sample01 <- fac_up_sample %>% filter(year == 2001)

fac_up_sample01 <- fac_up_sample01 %>%
  mutate(ipum2001 = remove_labels(geo3_bd2001))

factory_upazilas %>% 
  select(ipum1991, ipum2001) %>% 
  unique() -> matcher9101

fac_up_sample01 %>%
  left_join(matcher9101, by = "ipum2001") %>%
  select(-ipum2001) -> fac_up_sample01

# since lakshan and comilla sadar merged later



    
# now with 2011
fac_up_sample11 <- fac_up_sample %>% filter(year == 2011)

fac_up_sample11 <- fac_up_sample11 %>%
  mutate(ipum2011 = remove_labels(geo3_bd2011))

factory_upazilas %>% 
  select(ipum1991, ipum2011) %>%
  mutate(ipum1991 = ifelse(ipum1991 == 20019072, 20019067, ipum1991)) %>%
  unique() -> matcher9111

fac_up_sample11 %>%
  left_join(matcher9111, by = "ipum2011") %>%
  select(-ipum2011) -> fac_up_sample11

fac_up_sample %>%
  filter(year == 1991) %>%
  mutate(ipum1991 = remove_labels(geo3_bd1991),
         ipum1991 = ifelse(ipum1991 == 20019072, 20019067, ipum1991)) %>%
  rbind(fac_up_sample01) %>%
  rbind(fac_up_sample11) -> fac_up_sample

data <- fac_up_sample

rm(list = setdiff(ls(), c("data", "factory_upazilas")))

### Gets you general values

data %>%
  group_by(ipum1991, year) %>%
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
  group_by(ipum1991, year, sex, groups_lfp1564) %>%
  summarise(lfp = sum(labforce %in% c(2))/ n(),
            lfp_ind = sum(lfp_ind == 1) / n()) %>%
  filter(!is.na(groups_lfp1564)) %>% ungroup() -> lfp_1564
  

lfp_1564 %>% 
  pivot_wider(names_from = sex, values_from = c("lfp", "lfp_ind")) %>%
  rename(m_lfp1564 = lfp_1, f_lfp1564 = lfp_2, m_lfp_ind1564 =  lfp_ind_1,
         f_lfp_ind1564 = lfp_ind_2) %>% select(-groups_lfp1564) -> lfp_1564

# 15 - 29 year olds

data %>%
  group_by(ipum1991, year, sex, groups_lfp1529) %>%
  summarise(lfp = sum(labforce %in% c(2))/ n(),
            lfp_ind = sum(lfp_ind == 1) / n()) %>%
  filter(!is.na(groups_lfp1529)) %>% ungroup() -> lfp_1529

lfp_1529 %>% 
  pivot_wider(names_from = sex, values_from = c("lfp", "lfp_ind")) %>%
  rename(m_lfp1529 = lfp_1, f_lfp1529 = lfp_2, m_lfp_ind1529 =  lfp_ind_1,
         f_lfp_ind_1529 = lfp_ind_2) %>% select(-groups_lfp1529) -> lfp_1529

# 15 - 20 year olds

data %>%
  group_by(ipum1991, year, sex, groups_lfp1520) %>%
  summarise(lfp = sum(labforce %in% c(2))/ n(),
            lfp_ind = sum(lfp_ind == 1) / n()) %>%
  filter(!is.na(groups_lfp1520)) %>% ungroup() -> lfp_1520

lfp_1520 %>% 
  pivot_wider(names_from = sex, values_from = c("lfp", "lfp_ind")) %>%
  rename(m_lfp1520 = lfp_1, f_lfp1520 = lfp_2, m_lfp_ind1520 =  lfp_ind_1,
         f_lfp_ind1520 = lfp_ind_2) %>% select(-groups_lfp1520) -> lfp_1520

# obtaining all marriage data

data %>%
  group_by(ipum1991, year, groups_mar) %>%
  summarise(married = sum(marst %in% c(2,3,4)) / n()) %>%
  filter(!is.na(groups_mar)) %>% ungroup() -> ever_married

ever_married %>% 
  pivot_wider(names_from = groups_mar, values_from = married,
              names_glue = "married_{groups_mar}") -> ever_married

## fertility data

data %>%
  group_by(ipum1991, year, groups_fert) %>%
  summarise(fertility = mean(nchild )) %>%
  filter(!is.na(groups_fert)) %>% ungroup() -> fertility_df

fertility_df %>% 
  pivot_wider(names_from = groups_fert, values_from = fertility,
              names_glue = "fertility_{groups_fert}")-> fertility_df

# Education variables

data %>%
  group_by(ipum1991, year, sex, groups_hc) %>%
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

lfp_data <-   left_join(lfp_1520, lfp_1529, by = c("ipum1991", "year")) %>%
  left_join(lfp_1564, by = c("ipum1991", "year"))

rm("lfp_1520", "lfp_1529", "lfp_1564")

lfp_data %>% mutate(ipum1991 = as.integer(ipum1991))

# add HC
upazila_data <- left_join(lfp_data, human_cap)

rm("human_cap", "lfp_data")

upazila_data <- left_join(upazila_data, general_data)

rm("general_data")

# now fertility data
upazila_data <- left_join(upazila_data, fertility_df)

upazila_data <- left_join(upazila_data, ever_married)

upazila_data <- remove_labels(upazila_data)

factory_upazilas %>%
  group_by(ipum1991) %>%
  summarise(area91 = mean(area91)) -> factory_upazilas

upazila_data <- left_join(upazila_data,
                          factory_upazilas %>%
                            select(area91, ipum1991))

save(upazila_data, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/upazila_data.RData")