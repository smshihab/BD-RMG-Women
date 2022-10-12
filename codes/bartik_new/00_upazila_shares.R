library(pacman)

p_load(tidyverse, conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.Rdata") 

factory_upazilas %>%
  mutate(upaz2011 = as.numeric(upaz2011), upaz2001 =as.numeric(upaz2001), 
         upaz1991 = as.numeric(upaz1991)) -> factory_upazilas

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
    total05 = sum(machine*exist05, na.rm = T),
    knit05 = sum(machine*exist05*fac_type, na.rm = T),
    wove05 = total05 - knit05) %>%
    mutate(knit_share05 = knit05 / total05,
           wove_share05 = wove05 / total05) %>%
  mutate_all(~replace(., is.nan(.), 0)) -> usual_bartik_shares05


bartik_shares <- full_join(usual_bartik_shares05, usual_bartik_shares01, by ="upaz1991")

rm("usual_bartik_shares05", "usual_bartik_shares01")

## Autor et al way of share
total_knit91 <- sum(matched_data_01$machine*matched_data_01$fac_type*matched_data_01$exist91)
total_knit01 <- sum(matched_data_01$machine*matched_data_01$fac_type*matched_data_01$exist01)
total_wove91 <- sum(matched_data_01$machine*(-matched_data_01$fac_type+1)*matched_data_01$exist91)
total_wove01 <- sum(matched_data_01$machine*(-matched_data_01$fac_type+1)*matched_data_01$exist01)


matched_data_01 %>%
  group_by(upaz1991) %>%
  summarise(knit91_share = (sum(machine*fac_type*exist91))/total_knit91,
            wove91_share = (sum(machine*(-fac_type+1)*exist91))/total_wove91,
            knit01_share = (sum(machine*fac_type*exist01))/total_knit01,
            wove01_share = (sum(machine*(-fac_type+1)*exist01))/total_wove01) -> autor_shares01


total_knit05 <- sum(matched_data_09$machine*matched_data_09$fac_type*matched_data_09$exist05)
total_wove05 <- sum(matched_data_09$machine*(-matched_data_09$fac_type+1)*matched_data_09$exist05)

matched_data_09 %>%
  group_by(upaz1991) %>%
  summarise(knit05_share = (sum(machine*fac_type*exist05))/total_knit91,
            wove05_share = (sum(machine*(-fac_type+1)*exist05))/total_wove05) -> autor_shares05

autor_shares <- full_join(autor_shares05, autor_shares01, by ="upaz1991")

rm(list = setdiff(ls(), c("bartik_shares", "autor_shares")))

save(list = c("autor_shares", "bartik_shares"), file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/shares.RData")


