# Assuming previous rdhs libraries

p_load(rdhs, stringr) 

### This code chunnk loads the DHS data

bd_fert <- set_rdhs_config(
  email = "smshihab@uoregon.edu",
  project = "Fertility and FLP in Bangladesh in a Macroeconomic context",
  cache_path = "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH data/DHS/Surveys",
  config_path = NULL,
  global = TRUE,
  verbose_download = FALSE,
  verbose_setup = TRUE,
  data_frame = NULL,
  timeout = 30,
  password_prompt = TRUE,
  prompt = TRUE
)


# What datasets are available?
avail_survys <- get_available_datasets(clear_cache = FALSE) %>%
  filter(DHS_CountryCode =="BD")

# List of all bd surveys

survey_list <- dhs_surveys(countryIds = "BD")


# Defining the datasets

# persons recode dataset
pr_datasetsdef <- dhs_datasets(surveyIds = survey_list$SurveyId, fileFormat = "FL", fileType = "PR")

# household recode dataset
hr_datasetsdef <- dhs_datasets(surveyIds = survey_list$SurveyId, fileFormat = "FL", fileType = "HR")

#female recode dataset
fr_datasetsdef <- dhs_datasets(surveyIds = survey_list$SurveyId, fileFormat = "FL", fileType = "IR")

## Special survey

fr_raw2001 <- dhs_datasets(surveyIds = "BD2001OTH", fileFormat = "FL", fileType = "IQ")

hr_raw2001 <- dhs_datasets(surveyIds = "BD2001OTH", fileFormat = "FL", fileType = "HH")


# Download datasets

## Regulars

pr_dataset <- get_datasets(pr_datasetsdef$FileName, clear_cache=FALSE)

hr_dataset <- get_datasets(hr_datasetsdef$FileName, clear_cache=FALSE)

fr_dataset <- get_datasets(fr_datasetsdef$FileName, clear_cache=FALSE)

## Specials

fr_raw2001 <- get_datasets(fr_raw2001)

hr_raw2001 <- get_datasets(hr_raw2001)

rm("avail_survys", "bd_fert", "fr_datasetsdef", "hr_datasetsdef", "pr_datasetsdef", "survey_list")


### First start with fr_dataset and obtain labor force related data

# list of all available variables from usual DHS

avail_frvars <- get_variable_labels(fr_dataset) %>% select(variable, description) %>% unique()

# First lines are ID vars, second are education and age of husband and wife, third are locations, 
# fourth are reproductive behavior, fifth are the autonomy variables, and sixth are various controls

frvars <- avail_frvars %>% filter(grepl("v001|v002|v003|v008|v011|
                                        v107|v108|v133|v704|v714|v716|v719|v720|v702|v715|
                                        v102|v103|v104|v105|v119|
                                        v501|v503|v511|v312|v364|v602|v605|v621|v613|v627|v628|^b0_|^b3_|^b4_|^b5_|^b7_|
                                        v739|s626|s823a|s823b|s814|s826b|s812c|^w104|^w105|^s81|^v743
                                        v137|v130|v127|v128|v129", variable)) %>%
                        select(variable) %>% unlist()

## Search

temp_vars <- search_variables(names(fr_dataset), variables = frvars)

## extract variables

fr_data <- extract_dhs(temp_vars, add_geo = FALSE)

## Bind them together

fr_data <- bind_rows(fr_data)

save(fr_data, file = "fr_data.RData")


avail_prvars <- get_variable_labels(pr_dataset) %>% select(variable, description) %>% unique()


prvars <- avail_prvars %>% filter(grepl("hv001|hv002|hv003|hv005|sh26i|sh25|sh31|^hv10|sh14|sh27a|sh19|hv116", variable)) %>%
  select(variable) %>% unlist()


temp_vars <- search_variables(names(pr_dataset), variables = prvars)

## extract variables

pr_data <- extract_dhs(temp_vars, add_geo = FALSE)

## Bind them together

pr_data <- bind_rows(pr_data)

save(pr_data, file = "pr_data.RData")


### GPS data


gps_data <- c("v001", "v002", "v003", "v008")

# Search and extract the variables
gps_data <- search_variables(names(fr_dataset), variables = gps_data, reformat=FALSE)

gps_data <- extract_dhs(gps_data, add_geo = TRUE)


gps_data <- bind_rows(gps_data)

gps_data <- gps_data %>% select(-c(ALT_DEM, ADM1NAME, DHSREGNA, SurveyId))

gps_data <- gps_data %>% filter(!is.na(LATNUM))




#Function to convert cmc to year
cmc_year_conv <- function(x, na.rm = FALSE)(1900 + as.integer((x - 1)/12))

gps_data <- gps_data %>%
  mutate(v008 = cmc_year_conv(v008))  %>%
  mutate(id = as.integer(paste0(as.character(v008), formatC(v001, width=3, flag="0")))) %>%
  mutate(hhid = as.integer(paste0(as.character(id), formatC(v002, width=3, flag="0")))) %>%
  mutate(id_ind = as.character(paste0(as.character(hhid), formatC(v003, width=2, flag="0")))) %>%
  filter(!is.na(hhid)) # removes the one hhid is na observation


gps_data <- gps_data %>% 
  select(LATNUM, LONGNUM, id, v008) %>%
  unique()

save(gps_data, file = "gps_data.RData")