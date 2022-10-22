library(pacman)

p_load(tidyverse, conflicted, janitor)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.Rdata") 
#, upaz2001 =as.integer(upaz2001),  upaz1991 = as.integer(upaz1991))

factory_upazilas %>%
  mutate(ipum1991 = as.integer(ipum1991), 
         upaz2011 =as.integer(upaz2011)) -> factory_upazilas

# Laksham and comilla sadar of 1991 is united later on

factory_upazilas$ipum1991[factory_upazilas$ipum1991 == 20019072] <- 20019067

load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factories.Rdata")

matched_data_01 <- matched_data_01 %>%
  filter(!is.na(fac_type)) %>%
  filter(!is.na(machine)) %>%
  left_join(factory_upazilas %>% select(upaz2011, ipum1991))

## Autor et al way of share

total_knit91 <- sum(matched_data_01$machine*matched_data_01$fac_type*matched_data_01$exist91)
total_knit01 <- sum(matched_data_01$machine*matched_data_01$fac_type*matched_data_01$exist01)
total_wove91 <- sum(matched_data_01$machine*(-matched_data_01$fac_type+1)*matched_data_01$exist91)
total_wove01 <- sum(matched_data_01$machine*(-matched_data_01$fac_type+1)*matched_data_01$exist01)

matched_data_01 %>%
  group_by(ipum1991) %>%
  summarise(knit91_share = (sum(machine*fac_type*exist91))/total_knit91,
            wove91_share = (sum(machine*(-fac_type+1)*exist91))/total_wove91,
            knit01_share = (sum(machine*fac_type*exist01))/total_knit01,
            wove01_share = (sum(machine*(-fac_type+1)*exist01))/total_wove01) %>%
  ungroup()-> autor_shares01

autor_shares <- data.frame(ipum1991 = factory_upazilas$ipum1991 %>% unique())

autor_shares <- autor_shares %>%
  left_join(autor_shares01, by ="ipum1991") %>%
  mutate_all(~replace(., is.na(.), 0))

autor_shares %>% select(contains("knit"), ipum1991) %>%
  pivot_longer(!ipum1991, names_to = "year", values_to = "knit_share") %>%
  mutate(year = case_when(year == "knit91_share" ~ 1991,
                          year == "knit01_share" ~ 2001))-> autor_shares_knit

autor_shares %>% select(contains("wov"), ipum1991) %>%
  pivot_longer(!ipum1991, names_to = "year", values_to = "woven_share") %>%
  mutate(year = case_when(year == "wove91_share" ~ 1991,
                          year == "wove01_share" ~ 2001)) -> autor_shares_woven

autor_shares <- left_join(autor_shares_knit, autor_shares_woven, by = c("ipum1991", "year"))
  
rm(list = setdiff(ls(), c("autor_shares")))

### exports
exports <- read_csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/exports.csv") %>%
  clean_names() %>% select(-flow) %>%
  filter(year %in% c(1991, 2001, 2011)) %>%
  rename(export_val = value)

imports_knit <- read_csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/imports_wov_rest_knit.csv") %>%
  clean_names() %>%
  filter(code == 60) %>%
  filter(year %in% c(1991, 2001, 2011)) %>%
  mutate(code = 61) %>%
  rename(import_val = value)

imports_wov1 <- read_csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/imports_wov_rest_knit.csv") %>%
  clean_names() %>%
  filter(code != 60) %>%
  filter(year %in% c(1991, 2001, 2011)) %>%
  group_by(year) %>%
  summarise(code = 62,
            import_val = sum(value)) %>% ungroup()

imports_wov2 <- read_csv("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/imports_wov50_54.csv") %>%
  clean_names() %>%
  filter(code != 60) %>%
  filter(year %in% c(1991, 2001, 2011)) %>%
  group_by(year) %>%
  summarise(code = 62,
            import_val = sum(value)) %>% ungroup()

imports_wov <- rbind(imports_wov1, imports_wov2) %>%
  group_by(year, code) %>%
  summarise(import_val = sum(import_val)) %>% ungroup()

imports <- rbind(imports_wov, imports_knit)

rm("imports_wov", "imports_wov1", "imports_wov2", "imports_knit")

trade <- left_join(exports, imports, by = c("year", "code"))

rm("imports", "exports")

trade %>%
  mutate(value = export_val - import_val) -> trade

trade %>% select(year, code, value) -> trade

trade %>%
  mutate(code = as.character(code)) %>%
  mutate(code = case_when(code == "61" ~ "knit",
                   code == "62" ~ "woven")) %>%
pivot_wider(names_from = code) -> trade
  name

save(list = c("autor_shares", "trade"), file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/shift_shares.RData")


