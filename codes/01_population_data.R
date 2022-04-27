
#### Creating population panel for catchment areas 1686 and 3372 meters ####

## geo_coding factory data ##

factory_geo <- factory %>% select(fac_id, long, lat)

# Set coordinates
coordinates(factory_geo) <- c("long", "lat")

# Set projection string
proj4string(factory_geo) <- CRS("+init=epsg:4326")

# Convert to Sf and transform to Mercator Gulshan
factory_geo <- st_as_sf(factory_geo) %>% st_transform(3106)



### Catchment area creation ###

# 1686 is motivated by the literature
catchment_1686 <- st_buffer(factory_geo, 1686)

# 3372 = 2 x 1686, so double of initial catchment area
catchment_3372 <- st_buffer(factory_geo, 3372)


## Obtaining population data given the catchment areas


pop_data_1686 <- factory_geo %>% data.frame() %>% select(fac_id)

pop_data_3372 <- factory_geo %>% data.frame() %>% select(fac_id)


## Loading needed spatial packages

p_load(raster, stars)

  # for 1990 - 2000, data source is 

  ## https://sedac.ciesin.columbia.edu/data/set/gpw-v3-population-density/data-download    , 2.5 minute resolutions <=> 5 KM at the equation

  ## We use UN-adjusted values after downloading


  # Raster data sources
  
raster_list <- c("density_till2000//bgdds90ag.bil",
                 "density_till2000//bgdds95ag.bil")


for(i in c(1,2)){
  
  # Reading in Raster data
  
  setwd("C:/Users/smshi/OneDrive/Documents/large_datasets/BD Geo-spatial/gpw_data")

    rasterdata <- read_stars(raster_list[i])
  
    rasterdata <- rasterdata %>%
      st_as_sf() %>%
      st_set_crs(4326) %>%
      st_transform(3106)
  
  # Finding intersection of catchment area and the density raster 
  
    temp_data <- st_intersection(catchment_1686, rasterdata)
  
    temp_data <- temp_data %>%
      mutate(area = st_area(.) %>% as.numeric() / 1000^2)
  
    # Changeing names of columns 
    
    names(temp_data) <- c("fac_id", "density", "geometry", "area")
  
    temp_data <- as.data.frame(temp_data) %>%
      select(-geometry) %>%
      group_by(fac_id) %>%
      summarize(density = weighted.mean(density, area, na.rm = T))
  
    pop_data_1686 <- left_join(pop_data_1686, temp_data, by = "fac_id")
}


# Moving onto the higher resolution tif files available in later years

# Raster data sources

raster_list <- c("density2000//density2000.tif",
                 "density2005//density2005.tif",
                 "density2010//density2010.tif",
                 "density2015//density2015.tif",
                 "density2020//density2020.tif")

for(i in c(1, 2, 3, 4, 5)){
  
  # Reading in Raster data   
  
  setwd("C:/Users/smshi/OneDrive/Documents/large_datasets/BD Geo-spatial/gpw_data")
  
  rasterdata <- read_stars(raster_list[i])
  
  rasterdata <- st_crop(rasterdata, catchment_1686 %>% st_transform(4326) %>% st_bbox())
  
  rasterdata <- rasterdata %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(3106)
  
  # Finding intersection of catchment area and the density raster
  
  temp_data <- st_intersection(catchment_1686, rasterdata)
  
  temp_data <- temp_data %>%
    mutate(area = st_area(.) %>% as.numeric() / 1000^2)
  
  #Changing names of columns 
  
  
  names(temp_data) <- c("fac_id", "density", "geometry", "area")
  
  
  temp_data <- as.data.frame(temp_data) %>%
    select(-geometry) %>%
    group_by(fac_id) %>%
    summarize(density = weighted.mean(density, area, na.rm = T))
  
  pop_data_1686 <- left_join(pop_data_1686, temp_data, by = "fac_id")
  
}

# Renaming columns
names(pop_data_1686) <- c("fac_id", "1990", "1995", "2000", "2005", "2010", "2015", "2020")


rm(raster_list, rasterdata, i, temp_data)

# Pivot longer for a usual panel setup

pop_data_1686 <- pop_data_1686 %>%
  pivot_longer(-fac_id, names_to = "year", values_to = "density") %>%
  mutate(year = as.integer(year)) %>%
  arrange(fac_id, year)

# Fill in gaps

pop_data_1686 <- pop_data_1686 %>%
  group_by(fac_id) %>%
  mutate(change = (density - dplyr::lag(density, 1))/5) %>%
  ungroup()


# Fill in gaps

pop_data_1686 <- left_join(
  data.frame(
    fac_id = rep(c(1:length(factory$fac_id)),32), year = c(1989:2020)),
  pop_data_1686, by = c("fac_id", "year")) %>%
  arrange(fac_id, year) %>%  
  group_by(fac_id) %>%
  fill(change, .direction = "up") %>%
  fill(density, .direction = "down") %>%
  fill(density, .direction = "up") %>%
  ungroup() %>%
  mutate(change = ifelse(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020), 0, change),
         change = ifelse(year == 1989, -change, change)) %>%
  filter(year != 2020) %>%
  left_join(rbind(data.frame(year = c(1990:2019), factor = rep(c(0:4), 6)), data.frame(year = 1989, factor = 1))
  ) %>%
  mutate(density = density + change * factor) %>%
  select(-c(change,factor))


catchment_1686 <- catchment_1686 %>%
  mutate(area = st_area(.) %>% as.numeric() / 1000^2)

pop_data_1686 <- left_join(pop_data_1686, data.frame(catchment_1686) %>% select(-geometry), by= "fac_id")

pop_data_1686 <- pop_data_1686 %>%
  mutate(population= density*area)



#### Changing variable names to reflect which catchment area to consider ####


pop_data_1686 <- pop_data_1686 %>% rename(density1686 = density, population1686 = population)


factory_panel <- left_join(factory_panel, pop_data_1686 %>% select(-area), by = c("fac_id", "year"))


### Catchment area ###


# Raster data sources

raster_list <- c("density_till2000//bgdds90ag.bil",
                 "density_till2000//bgdds95ag.bil")


for(i in c(1,2)){
  
  # Reading in Raster data
  
  setwd("C:/Users/smshi/OneDrive/Documents/large_datasets/BD Geo-spatial/gpw_data")
  
  rasterdata <- read_stars(raster_list[i])
  
  rasterdata <- rasterdata %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(3106)
  
  # Finding intersection of catchment area and the density raster 
  
  temp_data <- st_intersection(catchment_3372, rasterdata)
  
  temp_data <- temp_data %>%
    mutate(area = st_area(.) %>% as.numeric() / 1000^2)
  
  # Changeing names of columns 
  
  names(temp_data) <- c("fac_id", "density", "geometry", "area")
  
  temp_data <- as.data.frame(temp_data) %>%
    select(-geometry) %>%
    group_by(fac_id) %>%
    summarize(density = weighted.mean(density, area, na.rm = T))
  
  pop_data_3372 <- left_join(pop_data_3372, temp_data, by = "fac_id")
}


# Moving onto the higher resolution tif files available in later years

# Raster data sources

raster_list <- c("density2000//density2000.tif",
                 "density2005//density2005.tif",
                 "density2010//density2010.tif",
                 "density2015//density2015.tif",
                 "density2020//density2020.tif")

for(i in c(1, 2, 3, 4, 5)){
  
  # Reading in Raster data   
  
  setwd("C:/Users/smshi/OneDrive/Documents/large_datasets/BD Geo-spatial/gpw_data")
  
  rasterdata <- read_stars(raster_list[i])
  
  rasterdata <- st_crop(rasterdata, catchment_3372 %>% st_transform(4326) %>% st_bbox())
  
  rasterdata <- rasterdata %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(3106)
  
  # Finding intersection of catchment area and the density raster
  
  temp_data <- st_intersection(catchment_3372, rasterdata)
  
  temp_data <- temp_data %>%
    mutate(area = st_area(.) %>% as.numeric() / 1000^2)
  
  #Changing names of columns 
  
  
  names(temp_data) <- c("fac_id", "density", "geometry", "area")
  
  
  temp_data <- as.data.frame(temp_data) %>%
    select(-geometry) %>%
    group_by(fac_id) %>%
    summarize(density = weighted.mean(density, area, na.rm = T))
  
  pop_data_3372 <- left_join(pop_data_3372, temp_data, by = "fac_id")
  
}

# Renaming columns
names(pop_data_3372) <- c("fac_id", "1990", "1995", "2000", "2005", "2010", "2015", "2020")


rm(raster_list, rasterdata, i, temp_data)

# Pivot longer for a usual panel setup

pop_data_3372 <- pop_data_3372 %>%
  pivot_longer(-fac_id, names_to = "year", values_to = "density") %>%
  mutate(year = as.integer(year)) %>%
  arrange(fac_id, year)

# Fill in gaps

pop_data_3372 <- pop_data_3372 %>%
  group_by(fac_id) %>%
  mutate(change = (density - dplyr::lag(density, 1))/5) %>%
  ungroup()



# Fill in gaps in population estimates
# Fill in gaps


pop_data_3372 <- left_join(
  data.frame(
    fac_id = rep(c(1:length(factory$fac_id)),32), year = c(1989:2020)),
  pop_data_3372, by = c("fac_id", "year")) %>%
  arrange(fac_id, year) %>%  
  group_by(fac_id) %>%
  fill(change, .direction = "up") %>%
  fill(density, .direction = "down") %>%
  fill(density, .direction = "up") %>%
  ungroup() %>%
  mutate(change = ifelse(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020), 0, change),
         change = ifelse(year == 1989, -change, change)) %>%
  filter(year != 2020) %>%
  left_join(rbind(data.frame(year = c(1990:2019), factor = rep(c(0:4), 6)), data.frame(year = 1989, factor = 1))
  ) %>%
  mutate(density = density + change * factor) %>%
  select(-c(change,factor))


catchment_3372 <- catchment_3372 %>%
  mutate(area = st_area(.) %>% as.numeric() / 1000^2)

pop_data_3372 <- left_join(pop_data_3372, data.frame(catchment_3372) %>% select(-geometry), by= "fac_id")

pop_data_3372 <- pop_data_3372 %>%
  mutate(population= density*area)


#### Changing variable names to reflect which catchment area to consider ####


pop_data_3372 <- pop_data_3372 %>% rename(density3372 = density, population3372 = population)


factory_panel <- left_join(factory_panel, pop_data_3372 %>% select(-area), by = c("fac_id", "year"))

rm("pop_data_1686", "pop_data_3372")
rm("pop_data_1686", "pop_data_3372")

