load("C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factory_bgmea.RData")

#### Panelizing Factory data ####

factory %>% mutate(fac_id = row.names(factory)) -> factory

# number of years
t <- max(factory$date_est) - min(factory$date_est) + 1

# number of fac
n <- length(factory$fac_id)

#forming panel to hold data on fac_id, time, machine, knitfac, wovenfac, Latitude, longitude, existence

factory_panel <- matrix(nrow = t * n, ncol = 7)

# creating a unique fac_id-year part of data

temp <- expand(factory, fac_id, year = c(min(factory$date_est):max(factory$date_est))) %>% as.matrix()

factory_panel[,c(1,2)] <- temp[,c(1,2)]


factory <- factory %>% select(fac_id, date_est, fac_type, machines, ipum1991) %>%
  rename(knitfac = fac_type)

# Extract all data from factory in a panelized form. To be multiplied with existence dummy for final data

factory <- factory %>% mutate(fac_id = as.integer(fac_id), knitfac = as.numeric(knitfac), machines = as.integer(machines))
    
  
  for(columns in c(2,3,4,5)){
    
    temp <- matrix(nrow = t*n, ncol = 1)
    
    for(i in 1:n){
      # first find the value
      value <- as.numeric(factory[i,columns])
      for(j in 1:t){
        k = (i-1)*t + j
        temp[k,1] <- value
      }
      factory_panel[,columns+1] <- temp
    }
  }
  
factory_panel <- data.frame(factory_panel)

names(factory_panel) <- c("fac_id", "year", "date_est", "knit", "machines", "hi1", "existence")

factory_panel <- factory_panel %>% mutate(
  fac_id = as.integer(fac_id), 
  year = as.integer(year), 
  machines = as.integer(machines))

factory_panel <- left_join(factory_panel, factory %>% select(fac_id, ipum1991), by = "fac_id")

factory_panel <- factory_panel %>% select(-hi1)


factory_panel <- factory_panel %>%
  mutate(existence = ifelse(year < date_est, 0, 1))

rm(list = ls.str(mode = 'numeric'))

save(factory_panel, file="C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factory_panel.RData")



