# ==============================================================================
# Data Cleaning of the imported additional TAPE data (extra data sheets)
#===============================================================================
# This script only executes minimal data cleaning. Please be aware, that
# every data set needs to be checked carefully for possible outliers and 
# impossible data values!!

## Data cleaning for male youth data (replace -99 with NA)
data_Youth_Males <- data_Youth_Males %>% 
  mutate_all(., ~replace(., . == -99, NA)) %>%
  mutate(y_occup_m = ifelse(is.na(y_occup_m), 0, y_occup_m))

## Data cleaning for female youth data (replace -99 with NA)
data_Youth_Females <- data_Youth_Females %>% 
  mutate_all(., ~replace(., . == -99, NA)) %>%
  mutate(y_occup_f = ifelse(is.na(y_occup_f), 0, y_occup_f))

## Data cleaning for crops
# cland -> replace -99 with NA & if missing value replace it with median value
data_Crops <- data_Crops %>%
  mutate_all(., ~replace(., . == -99, NA))

# if missing cland, replace it with median value
cland_med <- median(data_Crops$cland, na.rm = T)
data_Crops <- data_Crops %>%
  mutate(cland = ifelse(is.na(cland), cland_med, cland))
rm(cland_med)

## Data cleaning Animal
# remove entries that have an_id, aborn, adied, & arais as NA
data_Animals <- data_Animals %>%
  filter(!(is.na(an_id) & is.na(arais) & is.na(aborn) & is.na(adied))) %>%
  mutate(an_id = ifelse(is.na(an_id), 0, an_id)) # Replace NA in an_id to 0 --> is a Cow / Bull

## Data cleaning Chemical Pesticide
data_Chemical_Pesticide <- data_Chemical_Pesticide %>%
  mutate_all(., ~replace(., . == -99, NA)) %>%
  mutate(cpused = ifelse((cpused >= 100 & cparea_ha < 20), cpused/1000, cpused)) %>% # assume max 50 liters per ha (https://ourworldindata.org/pesticides)
  mutate(cpspray = ifelse(cpspray > 365, NA, cpspray)) # assume max 1 spraying per day
  
## Data cleaning Organic Pesticides
data_Organic_Pesticides <- data_Organic_Pesticides %>%
  mutate_all(., ~replace(., . == -99, NA)) %>%
  mutate(coused1 = ifelse((coused1 >= 100 & coarea1_ha < 20), coused1/1000, coused1)) %>% # assume max 50 liters per ha
  mutate(cospray = ifelse(cospray > 365, NA, cospray)) # assume max 1 spraying per day

## Data cleaning Crop Products
# remove entries that have cfpprod, cfpqsold, cfpqgift, and cfppg == 0 --> basically no data 
data_Crop_Products <- data_Crop_Products %>%
  filter(!(is.na(cfpprod) & is.na(cfpqsold) & is.na(cfpgift) & is.na(cfppg))) %>%
  mutate_all(., ~replace(., . == -99, NA))

## Data cleaning Activities
data_Activities <- data_Activities %>%
  mutate_all(., ~replace(., . == -99, NA))

## Data cleaning Machines
data_Machines <- data_Machines %>%
  mutate_all(., ~replace(., . == -99, NA))

## Data cleaning Animal Products
# remove entries that have approd, apqsold, apgift, and appg == 0 --> basically no data 
data_Animal_Products <- data_Animal_Products %>%
  filter(!(is.na(approd) & is.na(apqsold) & is.na(apgift) & is.na(appg))) %>%
  mutate_all(., ~replace(., . == -99, NA))

