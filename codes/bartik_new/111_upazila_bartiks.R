library(pacman)

p_load(tidyverse, conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.Rdata") 

factory_upazilas %>%
  mutate(upaz2011 = as.numeric(upaz2011), upaz2001 =as.numeric(upaz2001), 
         upaz1991 = as.numeric(upaz1991)) -> factory_upazilas

# Lakshan and comilla sadar of 1991 is united later on

factory_upazilas$upaz1991[factory_upazilas$upaz1991 == 201972] <- 201967


load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factories.Rdata")

matched_data_01 <- matched_data_01 %>% 
  left_join(factory_upazilas %>% select(upaz2011, upaz2001, upaz1991))

matched_data_09 <- matched_data_09 %>% 
  left_join(factory_upazilas %>% select(upaz2011, upaz2001, upaz1991))


## First, shares in knit versus woven importance in an area way

matched_data_01 %>% group_by(upaz1991) %>%
  summarise(
    total91 = sum(machine*exist91, na.rm = T),
    total01 = sum(machine*exist01, na.rm = T),
    knit91 = sum(machine*exist91*fac_type, na.rm = T),
    knit01 = sum(machine*exist01*fac_type, na.rm = T),
    wove91 = total91 - knit91,
    wove01 = total01 - knit01) %>%
  mutate(knit_share91 = knit91 / total91,
         knit_share01 = knit01 / total01,
         wove_share91 = wove91 / total91,
         wove_share01 = wove01 / total01) %>%
  mutate_all(~replace(., is.nan(.), 0)) -> usual_bartik_shares01


matched_data_09 %>% group_by(upaz1991) %>%
  summarise(
    total06 = sum(machine*exist06, na.rm = T),
    knit06 = sum(machine*exist06*fac_type, na.rm = T),
    wove06 = total06 - knit06) %>%
  mutate(knit_share06 = knit06 / total06,
         wove_share06 = wove06 / total06) %>%
  mutate_all(~replace(., is.nan(.), 0)) -> usual_bartik_shares06


bartik_shares <- full_join(usual_bartik_shares06, usual_bartik_shares01, by ="upaz1991")

rm("usual_bartik_shares06", "usual_bartik_shares01")