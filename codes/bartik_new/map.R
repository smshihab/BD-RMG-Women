upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

upazilas91 <- st_read(upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
upazilas91 <- st_as_sf(upazilas91) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), contains("IP"), PARENT) %>%
  clean_names()

matched_data_01 %>%
  filter(exist91 == 1) %>%
  select(upaz2011) %>%
  left_join(factory_upazilas %>%
              select(upaz2011, ipum1991)) %>%
  select(ipum1991) %>%
  unique() -> locs91

locs91$ipum1991 %>% unique() -> locs91

matched_data_01 %>%
  filter(exist01 == 1) %>%
  select(upaz2011) %>%
  left_join(factory_upazilas %>%
              select(upaz2011, ipum1991)) %>%
  select(ipum1991) %>%
  unique() -> locs01

locs01$ipum1991 %>% unique() -> locs01

ninetyone_sample %>% unique() -> locs06

upazilas91 %>%
  select(ipum1991) %>%
  mutate(ipum1991 = as.integer(ipum1991)) %>%
  mutate(has91 = ifelse(ipum1991 %in% locs91, 1, 0),
         has01 = ifelse(ipum1991 %in% locs01, 1, 0),
         has06 = ifelse(ipum1991 %in% locs06, 1, 0)) -> fac_locs
    


# Loading data


# Factory Map

factory_upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

factory_upazilas91 <- st_read(factory_upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas91 <- st_as_sf(factory_upazilas91) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower)

# Bangladesh zones 0 - country map, 1 - div, 2- district, 3-sub-district, 4 - smaller
geo_bd0 <- "C:/Users/smshi/Dropbox/University of Oregon/Research/Bangladesh FLFPR/Data/Bd admin and roads/bgd_adm_bbs_20201113_shp/bgd_admbnda_adm0_bbs_20201113.shp"

bd_map0 <- st_read(geo_bd0, quiet = TRUE)
bd_map0 <- bd_map0 %>% st_transform(3106)

p1 <- ggplot(fac_locs) +
  geom_sf(aes(as.factor(has91)) %>% select(has91)))+
  theme_map() + labs(title="Factories in 1991 99-00")

p2 <- ggplot(bd_map0) +
  geom_sf() +
  geom_sf(data = filter(factory, date_est<=2000), color = "black") +
  geom_sf(data = filter(factory, date_est > 2000 & date_est <= 2004), color = "red")+
  theme_map() + labs(title="Factories betwen 00-04")

p3 <- ggplot(bd_map0) +
  geom_sf() +
  geom_sf(data = filter(factory, date_est<=2004), color = "black") +
  geom_sf(data = filter(factory, date_est > 2004 & date_est <= 2007), color = "red")+
  theme_map() + labs(title="Factories betwen 04-07")


grid.arrange(p1, p2, p3, nrow =3)

fac_locs %>% select(-ipum1991) %>% plot()