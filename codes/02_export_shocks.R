
#### Here, we estimate export exposure across different regions ####

## The idea is to estimate the the intensity of treatment in a 
## context of overallping catchment area

# Using factory panel from before
factory_panel <- factory_panel %>%
  mutate(woven = 1-knit)

# Estimating shocks to areas

temp <- factory_panel %>% filter(year >=1989 & year <=2018)

# Estimating total knit and woven capacity in BD

bd_capacity <- temp %>%
  data.frame() %>%
  group_by(year) %>%
  summarise(
    knit_bd = sum(workers*knit*exist, na.rm = T),
    wov_bd = sum(workers*woven*exist, na.rm = T)) %>%
  ungroup()

# Estimating a region's share of knit and woven

temp <- left_join(temp, bd_capacity, by = "year")

rm(bd_capacity)

temp <- temp %>%
  mutate(knit_coef = ((exist*workers*knit)/knit_bd),
         wov_coef = ((exist*workers*woven)/wov_bd))

# Adding in Export data

exports <- read_excel("C:/Users/smshi/Dropbox/Research/Data/BD RMG factory data/exports_bgmea.xlsx") %>%
  select(year, knit, woven) %>%
  mutate(knit_export = 1000000*knit, woven_export = 1000000*woven) %>%
  select(-c(knit, woven)) %>%
  mutate(ch_knit1 = knit_export - lag(knit_export,1), ch_wov1 = woven_export - lag(woven_export,1),
         ch_knit3 = knit_export - lag(knit_export,3), ch_wov3 = woven_export - lag(woven_export,3))

temp <- left_join(temp, exports, by = "year")

rm(exports)


## Estimating export shocks - contemporaneous and 3 over three year instrumented with 5 year lagged pop and knit and wov share


# generating export shocks

shocks1686 <- temp %>%
  mutate(fac_id = as.integer(fac_id)) %>%
  mutate(knit_exposure1 = (knit_coef * ch_knit1)/population1686, 
         wov_exposure1 = (wov_coef * ch_wov1)/population1686,
         export_exposure1 = knit_exposure1 + wov_exposure1) %>%
  mutate(knit_exposure3 = (lag(knit_coef, 3) * ch_knit3) / population1686,
         wov_exposure3 = (lag(wov_coef, 3) * ch_wov3) / population1686,
         export_exposure3 = knit_exposure3 + wov_exposure3)


shocks3372 <- temp %>%
  mutate(fac_id = as.integer(fac_id)) %>%
  mutate(knit_exposure1 = (knit_coef * ch_knit1)/population3372, 
         wov_exposure1 = (wov_coef * ch_wov1)/population3372,
         export_exposure1 = knit_exposure1 + wov_exposure1) %>%
  mutate(knit_exposure3 = (lag(knit_coef, 3) * ch_knit3) / population3372,
         wov_exposure3 = (lag(wov_coef, 3) * ch_wov3) / population3372,
         export_exposure3 = knit_exposure3 + wov_exposure3)

### Taking out all exist = 0 values for making intersection easier

shocks1686 <- shocks1686 %>% filter(exist == 1)

shocks3372 <- shocks3372 %>% filter(exist == 1)


##  ##

shocks1686 <- shocks1686 %>%
  select(fac_id, year, knit, long, lat, contains("exposure"))

## geo_coding shocks ##

# Set coordinates
coordinates(shocks1686) <- c("long", "lat")

# Set projection string
proj4string(shocks1686) <- CRS("+init=epsg:4326")

# Convert to Sf and transform to Mercator Gulshan
shocks1686 <- st_as_sf(shocks1686) %>% st_transform(3106)


## Find the intersections ###

shocks1686_intersected <- st_intersection(shocks1686, shocks1686)

