library(pacman)

p_load(tidyverse, conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

factory <- read.csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/04_final_data.csv") %>%
  select(-c(capacity, bgmea_num, parent, ipum2011))


## First, shares in knit versus woven importance in an area way

factory %>% group_by(upazila) %>%
  summarise(
    upaz2011 = mean(upaz2011),
    total91 = sum(machine*exist91, na.rm = T),
    total00 = sum(machine*exist00, na.rm = T),
    knit91 = sum(machine*exist91*fac_type, na.rm = T),
    knit00 = sum(machine*exist00*fac_type, na.rm = T),
    wove91 = total91 - knit91,
    wove00 = total00 - knit00) %>%
  mutate(knit_share91 = knit91 / total91,
         knit_share00 = knit00 / total00,
         wove_share91 = wove91 / total91,
         wove_share00 = wove00 / total00) %>%
  mutate_all(~replace(., is.nan(.), 0)) -> usual_bartik_shares


## Autor et al way of share

total_knit91 <- sum(factory$machine*factory$fac_type*factory$exist91)
total_knit00 <- sum(factory$machine*factory$fac_type*factory$exist00)
total_wove91 <- sum(factory$machine*(-factory$fac_type+1)*factory$exist91)
total_wove00 <- sum(factory$machine*(-factory$fac_type+1)*factory$exist00)


factory %>%
  group_by(upazila) %>%
  summarise(knit91 = (sum(machine*fac_type*exist91))/total_knit91,
            wove91 = (sum(machine*(-fac_type+1)*exist91))/total_wove91,
            knit00 = (sum(machine*fac_type*exist00))/total_knit00,
            wove00 = (sum(machine*(-fac_type+1)*exist00))/total_wove00) -> shares_autor

rm(list = setdiff(ls(), c("factory", "shares_autor", "usual_bartik_shares")))


## Find Factory numbers per upazila and date of establishment


upaz81_data <- read_csv("C:/Users/smshi/OneDrive/Documents/large_datasets/BBS reports/Census reports/Census 1981/upaz81.csv")

upaz81_data %>%
  mutate(mfg_share = occ_mfg81 / (occ_hh81 + occ_culti81 + occ_mfg81),
         road_metal_share = metalledroad81 / kutcha81,
         madrasa_prev = madrasa81 / primary81,
         density81 = pop81 / nonRiverareaSQKM,
         family_size = pop81 / hh81,
         urban_rate = urbanpop81 / pop81,
         wealth_index = (roof_bamboo81 + agrolandowner81)/(2*pop81)) -> upaz81_data

p_load(broom)   

lm(factory ~ literacy81 + urban_rate, upaz81_data) %>% summary()




