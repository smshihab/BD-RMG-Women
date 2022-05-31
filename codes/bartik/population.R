factory_upazilas  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/factories_ipums/factories_ipums.shp"

factory_upazilas <- st_read(factory_upazilas, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas <- st_as_sf(factory_upazilas) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names()

factory_upazilas[11,3] <- "020015061"                                                
factory_upazilas[11,4] <- "020015061"

factory_upazilas[11,6] <- "201561"
factory_upazilas[11,7] <- "201561"

factory_upazilas[25,3] <- "030026072"
factory_upazilas[25,4] <- "030026072"

factory_upazilas[25,6] <- "302672"
factory_upazilas[25,7] <- "302672"


factory_upazilas[27,3] <- "030033032"
factory_upazilas[27,4] <- "030033032"

factory_upazilas[27,6] <- "303332"
factory_upazilas[27,7] <- "303332"


factory_upazilas[factory_upazilas$admin_name == "Uttara", 3]  <- "030026095"
factory_upazilas[factory_upazilas$admin_name == "Uttara", 4]  <- "030026095"

factory_upazilas[factory_upazilas$admin_name == "Uttara", 6]  <- "302695"
factory_upazilas[factory_upazilas$admin_name == "Uttara", 7]  <- "302695"


factory_upazilas <- factory_upazilas %>% select(ipum1991) %>% unique()

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/upazila_data.RData")

population <- factory_upazilas %>% data.frame() %>% select(ipum1991)

## Loading needed spatial packages

p_load(raster, stars)

# for 1990 - 2000, data source is 

## https://sedac.ciesin.columbia.edu/data/set/gpw-v3-population-density/data-download    , 2.5 minute resolutions <=> 5 KM at the equation

## We use UN-adjusted values after downloading


rasterdata <- read_stars("C:/Users/smshi/OneDrive/Documents/large_datasets/BD Geo-spatial/gpw_data/density_till2000//bgdds90ag.bil")
  
rasterdata <- rasterdata %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(3106)
  
  # Finding intersection of catchment area and the density raster 
  
  temp_data <- st_intersection(factory_upazilas, rasterdata)
  
  temp_data <- temp_data %>%
    mutate(area = st_area(.) %>% as.numeric() / 1000^2)
  
  # Changeing names of columns 
  
  names(temp_data) <- c("ipum1991", "density", "geometry", "area")
  
  temp_data <- as.data.frame(temp_data) %>%
    select(-geometry) %>%
    group_by(ipum1991) %>%
    summarize(density = weighted.mean(density, area, na.rm = T))
  
population <- left_join(population, temp_data, by = "ipum1991")


# Moving onto the higher resolution tif files available in later years

# Raster data sources

raster_list <- c("C:/Users/smshi/OneDrive/Documents/large_datasets/BD Geo-spatial/gpw_data/density2000//density2000.tif",
                 "C:/Users/smshi/OneDrive/Documents/large_datasets/BD Geo-spatial/gpw_data/density2010//density2010.tif")

for(i in c(1, 2)){
  
  # Reading in Raster data   
  
  rasterdata <- read_stars(raster_list[i])
  
  rasterdata <- st_crop(rasterdata, factory_upazilas %>% st_transform(4326) %>% st_bbox())
  
  rasterdata <- rasterdata %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(3106)
  
  # Finding intersection of catchment area and the density raster
  
  temp_data <- st_intersection(factory_upazilas, rasterdata)
  
  temp_data <- temp_data %>%
    mutate(area = st_area(.) %>% as.numeric() / 1000^2)
  
  #Changing names of columns 
  
  
  names(temp_data) <- c("ipum1991", "density", "geometry", "area")
  
  
  temp_data <- as.data.frame(temp_data) %>%
    select(-geometry) %>%
    group_by(ipum1991) %>%
    summarize(density = weighted.mean(density, area, na.rm = T))
  
  population <- left_join(population, temp_data, by = "ipum1991")
  
}

names(population) <- c("ipum1991", "1991", "2001", "2011")

population <- population %>%
  pivot_longer(-ipum1991, names_to = "year", values_to = "density") %>%
  mutate(year = as.integer(year)) %>%
  arrange(ipum1991, year)


population <- left_join(factory_upazilas, population, by = "ipum1991")

population <- population %>%
  mutate(area = st_area(geometry)) %>%
  mutate(area = as.numeric(area) / 1000^2) %>%
  mutate(population = density * area) %>%
  as.data.frame() %>% select(-geometry)


save(population, file = "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/IPUMS-I/population.RData")