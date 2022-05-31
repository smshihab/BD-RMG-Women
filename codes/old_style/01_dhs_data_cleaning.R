
#### Loading packages and data ####

library(pacman)

# Non-geo packages

p_load(readxl, conflicted, openxlsx, devtools, plm, tidyverse, knitr, memoise, Rcpp, labelled, 
       RhpcBLASctl, gridExtra, stargazer, broom, magick, cowplot, ggplot2, ggrepel)

# geo packages
p_load(sf, sp, raster, rgdal, ggmap, rgeos) 

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# First we clean up the FR data

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/fr_data.RData")

fr_data <- fr_data %>%
  filter(!is.na(LATNUM)) %>% 
  # Taking out Ws since they are all non-NAs
    select(-starts_with("w"))



# Separating birth versus non-fertility data

fr_birth <- fr_data %>% select(id_ind, v008_cmc, v011_cmc, starts_with("b"))

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
            mfg_share = sum(husb_worklocalgroup == 3) / n(), 
            agro_share = sum(husb_worklocalgroup == 1) / n() )

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
  # s823b and s819 is asking whether the person CAN go alone, in an ability sense
    mutate(can_health_alone = case_when(is.na(s823b) ~ s819,
                           TRUE ~ s823b)) %>%
  # s823a and s818 is asking whether the person goes alone
  mutate(does_health_alone = case_when(is.na(s819) ~ s818,
                           TRUE ~ s819)) %>%
  mutate(health_move_auto = can_health_alone + does_health_alone) %>%
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
  # s626a and s816 is asking whether the person CAN go alone, in an ability sense
  mutate(can_move_alone = case_when(is.na(s626a) ~ s816,
                                      TRUE ~ s626a)) %>%
  # s626 and s815 is asking whether the person goes alone
  mutate(does_move_alone = case_when(is.na(s626) ~ s815,
                                       TRUE ~ s626)) %>%
  mutate(gen_move_auto = can_move_alone + does_move_alone) %>%
  select(id_ind, gen_move_auto)

move_auto <- left_join(data_autonomy_Move, data_autonomy_healthMove, by = "id_ind")


rm(data_autonomy_Move, data_autonomy_healthMove)




### Decision making autonomy

final_say_data <- fr_data %>% select(id_ind, starts_with("s812"), starts_with("v74")) %>% 
  remove_labels(final_say_data) 

# own health care decision

final_say_data$s812a[final_say_data$s812a %in% c(1)] <- 200
final_say_data$s812a[final_say_data$s812a %in% c(3, 5)] <- 100
final_say_data$s812a[final_say_data$s812a %in% c(2, 4)] <- 0
final_say_data$s812a[final_say_data$s812a %in% c(6, 7, 8, 11, 12)] <-NA

final_say_data$v743a[final_say_data$v743a %in% c(1)] <- 200
final_say_data$v743a[final_say_data$v743a %in% c(2,3)] <- 100
final_say_data$v743a[final_say_data$v743a %in% c(4,5)] <- 0
final_say_data$v743a[final_say_data$v743a %in% c(6,9)] <- NA

final_say_data <- final_say_data %>%
  mutate(own_health_auto = (1/100)*case_when(is.na(s812a) ~ v743a,
                                     TRUE ~ s812a)) %>% 
  select(-c(s812a, v743a))

# child health auto not added cause of many missing values

final_say_data$s812b[final_say_data$s812b %in% c(1)] <- 200
final_say_data$s812b[final_say_data$s812b %in% c(3, 5)] <- 100
final_say_data$s812b[final_say_data$s812b %in% c(2, 4)] <- 0
final_say_data$s812b[final_say_data$s812b %in% c(6, 7, 9, 21, 22)] <-NA

# large purchase auto


final_say_data$s812c[final_say_data$s812c %in% c(1)] <- 200
final_say_data$s812c[final_say_data$s812c %in% c(3, 5)] <- 100
final_say_data$s812c[final_say_data$s812c %in% c(2, 4)] <- 0

final_say_data$v743b[final_say_data$v743b %in% c(1)] <- 200
final_say_data$v743b[final_say_data$v743b %in% c(2,3)] <- 100
final_say_data$v743b[final_say_data$v743b %in% c(4,5)] <- 0
final_say_data$v743b[final_say_data$v743b %in% c(6,9)] <- NA


final_say_data <- final_say_data %>%
  mutate(larg_buy_auto = (1/100)*case_when(is.na(s812c) ~ v743b,
                                             TRUE ~ s812c)) %>% 
  select(-c(s812c, v743b))


# small purchase auto

final_say_data$s812d[final_say_data$s812d %in% c(1)] <- 200
final_say_data$s812d[final_say_data$s812d %in% c(3, 5)] <- 100
final_say_data$s812d[final_say_data$s812d %in% c(2, 4)] <- 0


final_say_data$v743c[final_say_data$v743c %in% c(1)] <- 200
final_say_data$v743c[final_say_data$v743c %in% c(2,3)] <- 100
final_say_data$v743c[final_say_data$v743c %in% c(4,5)] <- 0
final_say_data$v743c[final_say_data$v743c %in% c(6,9)] <- NA


final_say_data <- final_say_data %>%
  mutate(small_buy_auto = (1/100)*case_when(is.na(s812d) ~ v743c,
                                           TRUE ~ s812d)) %>% 
  select(-c(s812d, v743c))

# visitor_auto

final_say_data$s812e[final_say_data$s812e %in% c(1)] <- 200
final_say_data$s812e[final_say_data$s812e %in% c(3, 5)] <- 100
final_say_data$s812e[final_say_data$s812e %in% c(2, 4)] <- 0

final_say_data$v743d[final_say_data$v743d %in% c(1)] <- 200
final_say_data$v743d[final_say_data$v743d %in% c(2,3)] <- 100
final_say_data$v743d[final_say_data$v743d %in% c(4,5)] <- 0
final_say_data$v743d[final_say_data$v743d %in% c(6,9)] <- NA


final_say_data <- final_say_data %>%
  mutate(visitor_auto = (1/100)*case_when(is.na(s812e) ~ v743d,
                                            TRUE ~ s812e)) %>% 
  select(-c(s812e, v743d))

# cooking auto

final_say_data$s812f[final_say_data$s812f %in% c(1)] <- 200
final_say_data$s812f[final_say_data$s812f %in% c(3, 5)] <- 100
final_say_data$s812f[final_say_data$s812f %in% c(2, 4)] <- 0


final_say_data$v743e[final_say_data$v743e %in% c(1)] <- 200
final_say_data$v743e[final_say_data$v743e %in% c(2,3)] <- 100
final_say_data$v743e[final_say_data$v743e %in% c(4,5)] <- 0
final_say_data$v743e[final_say_data$v743e %in% c(6,9)] <- NA


final_say_data <- final_say_data %>%
  mutate(cook_auto = (1/100)*case_when(is.na(s812f) ~ v743e,
                                          TRUE ~ s812f)) %>% 
  select(-c(s812f, v743e))


final_say_data <- final_say_data %>% select(id_ind, contains("auto"))


autonomy <- left_join(move_auto, final_say_data, by = "id_ind")

dataflfp <- left_join(dataflfp, autonomy, by = "id_ind")

rm(autonomy, final_say_data, move_auto)




### Getting the PR data ready





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

