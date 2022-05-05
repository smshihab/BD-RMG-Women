
# First we do the FLFP analysis

p_load(labelled)

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/fr_data.RData")

fr_data <- fr_data %>%
  filter(!is.na(LATNUM))

# Keeping needed variables

dataflfp <- fr_data %>%

  # Keeping below defined variables

  select(id, hhid, id_ind, LATNUM, LONGNUM, v008, v011, v025, v104, v105, v119, v127, v128, v129, v130, v133, v137, v201, v212, v213, v503, v511, v605, v621,
         v501, v704, v714, v715, v716, v717, v719, v720) %>%
  # Rename
  
  rename(year = v008, birth_year = v011, rural = v025, years_lived = v104, prev_place_type = v105, 
         electricity = v119, floor = v127, wall = v128, roof = v129, religion = v130, educ = v133, 
         num_child5 = v137, num_child = v201, preg_survey = v213, married = v501, age_mar = v511, fert_pref = v605,
         fert_pref_hus = v621, husb_worklocalgroup = v704, work = v714, husb_educ = v715, work_localgroup = v716, 
         work_stdgroup = v717, work_for_fam = v719, earn_cash = v720) 
    

# Basic data cleaning

# Codes with 9, 99, 999 and other nineties represent missing values

# For floor, wall and roof - 96 is other, do not know so do not count. 97 represents not a household member. 99 is not in question.

dataflfp <- remove_labels(dataflfp)

dataflfp$floor[dataflfp$floor %in% c(96, 97, 99)] <- NA 

dataflfp$floor[dataflfp$wall %in% c(96, 97, 99)] <- NA # 96 is other, do not know so do not count. 97 represents not a household member. 99 is not in question.

dataflfp$floor[dataflfp$roof %in% c(96, 97, 99)] <- NA # 96 is other, do not know so do not count. 97 represents not a household member. 99 is not in question.


dataflfp$years_lived[dataflfp$years_lived %in% c(96, 97, 98)] <- NA

dataflfp <- dataflfp %>%
  mutate(prev_place_type = as_factor(prev_place_type))

# 7 represents not a household member, making them NA

dataflfp$electricity[dataflfp$electricity %in% c(7,9)] <- NA


# 97 and 99 are NA in educ

dataflfp$educ[dataflfp$educ %in% c(99, 98)] <- NA 


# Husband's education educ
dataflfp$husb_educ[dataflfp$husb_educ %in% c(99, 98)] <- NA 

dataflfp$educ[dataflfp$educ %in% c(99)] <- NA 


# Religion - coding 1 is islam, 0 otherwise

dataflfp$religion[dataflfp$religion %in% c(2,3,4,96)] <- 0



# Fertility preference

# 1 means wants a child within two years

dataflfp <- dataflfp %>% mutate(fert_pref = case_when(fert_pref %in% c(1) ~ 1,
                                                       TRUE ~ 0))


dataflfp$work[dataflfp$work == 9] <- NA

# removing unknown local work types for females and their husbands

dataflfp$work_localgroup[dataflfp$work_localgroup %in% c(9,96,98,99,999,99999)] <- NA 

dataflfp$husb_worklocalgroup[dataflfp$husb_worklocalgroup %in% c(0,61, 62, 64, 65, 96, 98, 99998, 99999)] <- NA # removing unknown local work types 


dataflfp <- dataflfp %>% 
  
  # creating a wealth index by house characteristics in terms of floor, roof and wall. In each category, finished or better is 30 or more, giving a value of 1. So wealthiest is 3, poorest is 0. 
  mutate(floor = ifelse(floor <30, 0, 1), 
         wall = ifelse(wall <30, 0, 1), 
         roof = ifelse(roof <30, 0, 1)) %>%
  mutate(wealth_index = floor + wall + roof) %>% # removing the now redundant variable
  select(-c(floor, wall, roof)) %>% 
  
  # recoding to make rural = 1
  
  mutate(rural = rural -1 ) %>% 
  
  mutate(age = year - birth_year) %>%
  
  # working out dummy based on local (BD) occupation groups 16 - handicrafts, 23 factory, 31 includes tailoring, 54 is small business 
  
  mutate(flfp_garments = as.integer(work_localgroup %in% c(16, 21, 23,31,52))) %>%
  
  # working out in agriculture and domestic 
  
  mutate(flfp_trad = as.integer(work_stdgroup %in% c(4, 5, 6))) %>%
  
  #working out dummy based on standard occupation groups services, manual skilled and unskilled manual
  
  mutate(flfp_poor_nontrad = as.integer(work_stdgroup %in% c(7,8,9))) %>%
  
  mutate(flfp_highskilled = as.integer(work_stdgroup %in% c(1,2,3))) %>%
  
  #coding husbands occupation groups
  
  mutate(husb_worklocalgroup
         = case_when(husb_worklocalgroup %in% c(11, 12, 13, 14, 15, 22) ~ 1, # agriculture
                     husb_worklocalgroup %in% c(21, 52) ~ 2, # service
                     husb_worklocalgroup %in% c(16, 23, 31) ~ 3, # manufcaturing
                     husb_worklocalgroup %in% c(41, 51) ~ 4)) %>% # richer places
  
  select(-c(work_localgroup, work_stdgroup)) %>% #ever divorced dummy = 1, if div and no longer living together
  
  
  mutate(age_group = case_when(age >=40 ~ 0,
                               age >=30 & age <40 ~ 1,
                               age <=30 & age > 20 ~ 2,
                               TRUE ~ 3)) 


# Creating cluster level controls of electrification, wealth and industry structure

temp <- dataflfp %>%
  group_by(id) %>%
  summarise(electrification_rate = mean(electricity, na.rm =T),
            local_wealth = mean(wealth_index, na.rm = T),
            num = n())

dataflfp <- left_join(dataflfp, temp, by = "id")

rm(temp)


#### Autonomy variables

### health move auto

data_autonomy_healthMove <- fr_data %>% select(id_ind, s823a, s823b, s818, s819) %>% remove_labels()

#823a
data_autonomy_healthMove$s823a[data_autonomy_healthMove$s823a %in% c(2, 3, 4)] <- 0
data_autonomy_healthMove$s823a[data_autonomy_healthMove$s823a %in% c(6, 8, 9)] <-NA
#823b
data_autonomy_healthMove$s823b[data_autonomy_healthMove$s823b %in% c(2, 3, 4)] <- 0
data_autonomy_healthMove$s823b[data_autonomy_healthMove$s823b %in% c(6, 8, 9)] <-NA
# 818
data_autonomy_healthMove$s818[data_autonomy_healthMove$s818 %in% c(2, 3, 4)] <- 0
data_autonomy_healthMove$s818[data_autonomy_healthMove$s818 %in% c(6, 8, 9)] <-NA
#819
data_autonomy_healthMove$s819[data_autonomy_healthMove$s819 %in% c(2, 3, 4)] <- 0
data_autonomy_healthMove$s819[data_autonomy_healthMove$s819 %in% c(6, 8, 9)] <-NA


data_autonomy_healthMove <- data_autonomy_healthMove %>%
  mutate(iter1 = case_when(is.na(s823a) ~ s823b,
                           TRUE ~ s823a)) %>%
  
  mutate(iter2 = case_when(is.na(iter1) ~ s818,
                           TRUE ~ iter1)) %>%
  mutate(health_move_auto = case_when(is.na(iter2) ~ s819,
                           TRUE ~ iter2)) %>%
  select(id_ind, health_move_auto)


# General movement auto

data_autonomy_Move <- fr_data %>% select(id_ind, s626a, s626, s815, s816) %>% remove_labels()

#815
data_autonomy_Move$s815[data_autonomy_Move$s815 %in% c(2, 3, 4)] <- 0
data_autonomy_Move$s815[data_autonomy_Move$s815 %in% c(6, 8, 9)] <-NA
#816
data_autonomy_Move$s816[data_autonomy_Move$s816 %in% c(2, 3, 4)] <- 0
data_autonomy_Move$s816[data_autonomy_Move$s816 %in% c(6, 8, 9)] <-NA
#s626 and s626a
data_autonomy_Move$s626[data_autonomy_Move$s626 %in% c(2, 3, 4)] <- 0
data_autonomy_Move$s626[data_autonomy_Move$s626 %in% c(6, 8, 9)] <-NA
data_autonomy_Move$s626a[data_autonomy_Move$s626a %in% c(2, 3, 4)] <- 0
data_autonomy_Move$s626a[data_autonomy_Move$s626a %in% c(6, 8, 9)] <-NA


data_autonomy_Move <- data_autonomy_Move %>%
  mutate(iter1 = case_when(is.na(s815) ~ s816,
                           TRUE ~ s815)) %>%
  
  mutate(iter2 = case_when(is.na(iter1) ~ s626,
                           TRUE ~ iter1)) %>%
  mutate(move_auto = case_when(is.na(iter2) ~ s626a,
                                      TRUE ~ iter2)) %>%
  select(id_ind, move_auto)


final_say_data <- fr_data %>% select(id_ind, starts_with("s812"), starts_with("v74")) 

final_say_data <- remove_labels(final_say_data) 

final_say_data$s812a[final_say_data$s812a %in% c(1)] <- 200
final_say_data$s812a[final_say_data$s812a %in% c(3, 5)] <- 100
final_say_data$s812a[final_say_data$s812a %in% c(2, 4)] <- 0
final_say_data$s812a[final_say_data$s812a %in% c(6, 7, 8, 11, 12)] <-NA

final_say_data$s812b[final_say_data$s812b %in% c(1)] <- 200
final_say_data$s812b[final_say_data$s812b %in% c(3, 5)] <- 100
final_say_data$s812b[final_say_data$s812b %in% c(2, 4)] <- 0
final_say_data$s812b[final_say_data$s812b %in% c(6, 7, 9, 21, 22)] <-NA

final_say_data$s812c[final_say_data$s812c %in% c(1)] <- 200
final_say_data$s812c[final_say_data$s812c %in% c(3, 5)] <- 100
final_say_data$s812c[final_say_data$s812c %in% c(2, 4)] <- 0

final_say_data$s812d[final_say_data$s812d %in% c(1)] <- 200
final_say_data$s812d[final_say_data$s812d %in% c(3, 5)] <- 100
final_say_data$s812d[final_say_data$s812d %in% c(2, 4)] <- 0

final_say_data$s812e[final_say_data$s812e %in% c(1)] <- 200
final_say_data$s812e[final_say_data$s812e %in% c(3, 5)] <- 100
final_say_data$s812e[final_say_data$s812e %in% c(2, 4)] <- 0

final_say_data$s812f[final_say_data$s812f %in% c(1)] <- 200
final_say_data$s812f[final_say_data$s812f %in% c(3, 5)] <- 100
final_say_data$s812f[final_say_data$s812f %in% c(2, 4)] <- 0


dataflfp <- left_join(dataflfp, shocks %>% select(region_id, year, knit_exposure1, knit_exposure3, wov_exposure1, wov_exposure3, export_exposure1, export_exposure3, population, density), by = c("region_id", "year"))




# generate DHS clusters

dhsclusters <- fr %>% select(id, v007, v025, LONGNUM, LATNUM) %>% unique()

names(dhsclusters) <- c("id", "year", "rural", "LONGNUM", "LATNUM")

# Set coordinates
coordinates(dhsclusters) <- c("LONGNUM", "LATNUM")

# Convert to Sf and transform to Mercator Gulshan

# Set projection string
proj4string(dhsclusters) <- CRS("+init=epsg:4326") 

dhsclusters <- st_as_sf(dhsclusters) %>% st_transform(3106)


dhsclusters <- dhsclusters %>%
  mutate(rural = rural - 1,
         region_id = as.integer(st_intersects(geometry, regions))) %>%
  filter(!is.na(region_id))

# Probability of being in a region

dhsclusters_urban <- dhsclusters %>%
  filter(rural == 0) %>%
  st_buffer(1040)

dhsclusters_rural <- dhsclusters %>%
  filter(rural == 1) %>%
  st_buffer(2530)

dhsclusters <- rbind(dhsclusters_urban, dhsclusters_rural) %>%
  mutate(area_prob = st_area(.) %>% as.numeric())

rm(dhsclusters_urban, dhsclusters_rural)

# Prob of being in the region_id

prob <- st_intersection(dhsclusters, regions) %>%
  mutate(area = st_area(.) %>% as.numeric()) %>%
  mutate(prob = area / area_prob) %>% 
  as.data.frame() %>% 
  select(id, region_id, prob)

prob <- prob[!duplicated(prob$id), ]

dhsclusters <- left_join(dhsclusters %>% select(-area_prob), prob %>% select(-region_id), by = "id")

rm(prob)




dataflfp$male_pref[dataflfp$male_pref %in% c(95, 96, 99)] <- NA

dataflfp$female_pref[dataflfp$female_pref %in% c(95, 96, 99)] <- NA

