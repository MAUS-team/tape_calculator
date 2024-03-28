# ==============================================================================
# Data Cleaning of the imported main TAPE data
#===============================================================================
# This script only executes minimal data cleaning. Please be aware, that
# every data set needs to be checked carefully for possible outliers and 
# impossible data values!!

## HH characteristicts
# cannot be NA and missing are coded as -99
hh <- c('hh_men', 'hh_women', 'hh_myoung', 'hh_fyoung', 'hh_children')
data <- data %>%
  mutate_at(vars(all_of(hh)), ~replace(., .== -99, NA))
rm(hh)

working_ag <- c('ag_men', 'ag_myoung', 'ag_fyoung', 'ag_children')
data <- data %>%
  mutate_at(vars(all_of(working_ag)), ~replace(., .== -99, 0))
rm(working_ag)

## Areas (-99 is NA, but due to conversion into hectares it could also be -40.06 or -38.84999847 --> all negative values are set to 0)
areas <- c('area_natural_veg', 'area_natural_veg_ha', 'area_permanent_pasture', 'area_permanent_pasture_ha', 'area_common_pasture', 'area_common_pasture_ha')
data <- data %>%
  mutate_at(vars(all_of(areas)), ~replace(., . < 0, 0))
rm(areas)

## CAET (-99 is NA)
caet <- c('crops', 'animals', 'trees', 'div_activ', 'cla_int', 's_plant', 'tree_int', 'connectivity', 'rec_biomass', 'waste', 'water', 'ren_energy',
          'ext_inp', 'soil_fert', 'pest_dis', 'emergingefficiency', 'vuln', 'indebt', 'averdiv', 'averself_suff_empowerment', 'diet', 'food_self_suff', 
          'food_heritage', 'seeds_breeds', 'ae_know', 'platforms', 'partic_orgs', 'women', 'labour', 'youth', 'animalwel', 'coalanwel', 'mkt_local', 'networks', 
          'local_fs', 'prod_empow', 'prod_orgs', 'partic_prod')
data <- data %>%
  mutate_at(vars(all_of(caet)), ~replace(., . == -99, NA))
rm(caet)

## Dietary Diversity (-99 is NA)
dietdiv <- c('grains_a',	'grains_b',	'pulses',	'nuts',	'dairy_e',	'dairy_f',	'meat_h',	'meat_i',	'meat_j',	'meat_k',	'meat_l',	'eggs',	'darkgreen',
             'darkyellow_n',	'darkyellow_o', 'otherveg',	'otherfruit',	'fried_salty_1',	'fried_salty_2',	'fried_salty_3',	'fried_salty_4',	'sweet_foods',
             'sweet_beverages_1',	'sweet_beverages_2')
data <- data %>%
  mutate_at(vars(all_of(dietdiv)), ~replace(., . == -99, NA))
rm(dietdiv)

## Soil health (-99 is NA & missing values are replaced with median value)
soilhealth <- c('structure', 'compaction', 'depth', 'residues', 'color', 'water_ret', 'cover', 'erosion', 'invertebrates', 'microbio')
data <- data %>%
  mutate_at(vars(all_of(soilhealth)), ~replace(., .== -99, NA))

for(i in soilhealth){
  median_i <- median(data[[i]], na.rm = T)
  data[[i]][is.na(data[[i]])] <- median_i # replace missing values with median value
}
rm(soilhealth)

## Other soil indicators (-99 is NA)
soilind <- c('soil_conducted', 'field_size_ok',	'subplot_distance',	'soil_depth_sub1',	'soil_depth_sub2',	'soil_depth_sub3',	'soil_depth_sub4',	'soil_field_area',	
             'soil_field_area_unit',	'soil_year_first_cultivation',	'soil_crop_rotation',	'soil_intercropping',	'soil_manure',	'soil_inorganic_fert',	'soil_burned')
data <- data %>%
  mutate_at(vars(all_of(soilind)), ~replace(., . == -99, NA))
rm(soilind)

## Land Tenure (-99 is NA but also 77 or 88 (for doc_men and doc_women))
landtenure <- c('recland_men',	'recland_women',	'doc_men',	'doc_women',	'doc_womenoth',	'name_men',	'name_women',
                'ltperc_men',	'ltperc_women',	'sell_men',	'sell_women',	'beq_men',	'beq_women',	'inh_men',	'inh_women')
data <- data %>%
  mutate_at(vars(all_of(landtenure)), ~replace(., . == -99, NA)) %>%
  mutate(doc_men = ifelse(doc_men == 77 | doc_men == 88, NA, doc_men),
         doc_women = ifelse(doc_women == 77| doc_men == 88, NA, doc_women))
rm(landtenure)

## Women empowerment
# if no women or men in household --> wtime has to be 0
data <- data %>%
  mutate(wtime_ag_women = ifelse(hh_women == 0 & hh_fyoung == 0, 0, wtime_ag_women),
         wtime_dom_women = ifelse(hh_women == 0 & hh_fyoung == 0, 0, wtime_dom_women),
         wtime_otgain_women = ifelse(hh_women == 0 & hh_fyoung == 0, 0, wtime_otgain_women)) %>%
  mutate(wtime_ag_men = ifelse(hh_men == 0 & hh_myoung == 0, 0, wtime_ag_men),
         wtime_dom_men = ifelse(hh_men == 0 & hh_myoung == 0, 0, wtime_dom_men),
         wtime_otgain_men = ifelse(hh_men == 0 & hh_myoung == 0, 0, wtime_otgain_men))

# replace -99 & 88 with NA
wemp <- c('wtime_ag_men',	'wtime_ag_women',	'wtime_dom_men',	'wtime_dom_women',	'wtime_otgain_men',	'wtime_otgain_women',	'owcrop',	'owanim',
          'owhouse',	'owotact',	'deccrop',	'decmajor',	'decanim',	'decotact', 'dec_rev_crop',	'dec_rev_anim',	'dec_rev_oth',	'credit_men',
          'credit_women',	'involv_agri_men',	'involv_agri_wom',	'involv_othe_men',	'involv_othe_wom', 'answom', 'manref')
data <- data %>%
  mutate_at(vars(all_of(wemp)), ~replace(., . == -99, NA))
data <- data %>%
  mutate_at(vars(all_of(wemp)), ~replace(., . == 88, NA))
rm(wemp)

# if no ag_men or ag_women then wtime_ag has to be 0, and if wtime_ag missing --> replace with median
wtime_ag_men_med <- median(data$wtime_ag_men, na.rm = T)
wtime_ag_women_med <- median(data$wtime_ag_women, na.rm = T)

data <- data %>%
  rowwise() %>%
  mutate(tmp_men = sum(c(ag_men, ag_myoung), na.rm = T),
         tmp_women = sum(c(ag_women, ag_fyoung), na.rm = T)) %>%
  mutate(wtime_ag_men = ifelse(tmp_men == 0, 0, wtime_ag_men),
         wtime_ag_women = ifelse(tmp_women == 0, 0, wtime_ag_women)) %>%
  select(-c('tmp_men', 'tmp_women'))
rm(wtime_ag_men_med, wtime_ag_women_med)

## Educational level (replace edulev with NA if no men or women are in household)
data <- data %>%
  rowwise() %>%
  mutate(tmp_men = sum(c(hh_men, hh_myoung), na.rm = T),
         tmp_women = sum(c(hh_women, hh_fyoung), na.rm = T)) %>%
  mutate(edulev_men = ifelse(tmp_men == 0, NA, edulev_men),
         edulev_women = ifelse(tmp_women == 0, NA, edulev_women)) %>%
  select(-c('tmp_men', 'tmp_women'))

## External Farm Workers (replace -99 with NA)
extw <- c('if_extworkers_year', 'if_extworkers_season', 'num_extworkers_year', 'wage_year', 'num_extworkers_season', 'days_ext', 'wage_season')
data <- data %>%
  mutate_at(vars(all_of(extw)), ~replace(., . == -99, NA))
rm(extw)

data <- data %>%
  mutate(if_extworkers_year = ifelse(if_extworkers == 0, 0, if_extworkers_year),
         if_extworkers_season = ifelse(if_extworkers == 0, 0, if_extworkers_season),
         num_extworkers_year = ifelse(if_extworkers_year == 0, 0, num_extworkers_year),
         num_extworkers_season = ifelse(if_extworkers_season == 0, 0, num_extworkers_season),
         wage_year = ifelse(if_extworkers_year == 0, 0, wage_year),
         wage_season = ifelse(if_extworkers_season == 0, 0, wage_season),
         days_ext = ifelse(if_extworkers_season == 0, 0, days_ext))

## Expenditures (replace -99 with NA)
exp <- c('foodexp', 'seedsexp', 'fertexp', 'fuelexp', 'enerexp', 'machrentexp', 'transpexp', 'machexp', 'livexp', 'rentcost', 'op_exp', 'cp_exp')
data <- data %>%
  mutate_at(vars(all_of(exp)), ~replace(., . == -99, NA))
rm(exp)

## FIES & Pesticide (replace -99 with NA)
fies <- c('worried',	'healthy',	'fewfoods',	'skipped',	'ateless',	'ranout',	'hungry',	'wholeday', 'cpestfuture')
data <- data %>%
  mutate_at(vars(all_of(fies)), ~replace(., . == -99, NA))
rm(fies)
