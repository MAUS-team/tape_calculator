
#########################################
# functions TAPE
# Stevan Paunovic
# 08082023


##############################################################################
#---------------------calculate CAET // Step 1-------------------------------#
##############################################################################


#function for calculating Diversity for CAET

caet_diversity <- function(){
  Diversity <- data[,c('key', 'location1','sys_name', 'crops', 'animals', 'trees', 'div_activ')]
  Diversity <- Diversity %>%
    rowwise() %>%  #cleans data so there only is one number for crops animals trees ...
    mutate(div_sum = sum(c(crops, animals, trees, div_activ), na.rm = TRUE),
           div_aver = mean(c(crops ,animals ,trees ,div_activ),na.rm = TRUE),  # Is later used for Resilience
           div_score = round((div_sum/16)*100,6)) # calculates score
  return(Diversity)
}


#function for calculating Synergies for CAET

caet_synergies <- function(){
  Synergies <- data[,c('key', 'location1','sys_name', 'cla_int', 's_plant', 'tree_int', 'connectivity')]
  Synergies <- Synergies %>% #creates dataframe to calculate indicator
    rowwise() %>%
    mutate(syn_sum = sum(c(cla_int ,s_plant ,tree_int ,connectivity),na.rm = TRUE),
           syn_score = round((syn_sum/16)*100,6))
  return(Synergies)
}


#function for calculating Efficiency for CAET

caet_efficiency <- function(){
  Efficiency <- data[,c('key', 'location1','sys_name', 'ext_inp', 'soil_fert', 'pest_dis', 'productivity')]
  Efficiency <- Efficiency %>%
    rowwise() %>%
    mutate(eff_sum = sum(c(ext_inp, soil_fert, pest_dis, productivity), na.rm = TRUE),
           eff_score = round((eff_sum/16)*100,6))
  return(Efficiency)
}


#function for calculating Recycling for CAET 

caet_recycling <- function(){
  Recycling <- data[,c('key', 'location1', 'sys_name', 'rec_biomass', 'water', 'seeds_breeds', 'ren_energy')]
  Recycling <- Recycling %>%
    rowwise() %>%
    mutate(rec_sum = sum(c(rec_biomass,water,seeds_breeds,ren_energy),na.rm = TRUE),
           rec_score = round((rec_sum/16)*100,6))
  return(Recycling)
}

#function for calculating Resilience for CAET

caet_resilience <- function(){
  Resilience <- data[,c('key', 'location1', 'sys_name', 'stab', 'vuln', 'indebt','crops', 'animals', 'trees', 'div_activ')]
  Resilience <- Resilience %>%
    rowwise() %>%
    mutate(div_aver = mean(c(crops ,animals ,trees ,div_activ),na.rm = TRUE),
           res_sum = sum(c(stab, vuln, indebt, div_aver), na.rm = TRUE),
           res_score = round((res_sum/16)*100,6))
  return(Resilience)
}

#function for calculating Culture and food traditions for CAET

caet_culture <- function(){
  Culture <- data[,c('key', 'location1', 'sys_name', 'diet', 'local_id', 'local_var')]
  Culture <- Culture %>%
    rowwise() %>%
    mutate(cultf_sum = sum(c(diet,local_id,local_var),na.rm = TRUE),
           cultf_score = round((cultf_sum/12)*100,6))
  return(Culture)
}

#function for calculating Co-creation for CAET

caet_cocreation <- function(){
  Cocreation <- data[,c('key', 'location1', 'sys_name', 'platforms', 'ae_know', 'partic_orgs')]
  Cocreation <- Cocreation %>%
    rowwise() %>%
    mutate(cocr_sum = sum(c(platforms,ae_know,partic_orgs),na.rm = TRUE),
           cocr_score = round((cocr_sum/12)*100,6))
  return(Cocreation)
}

#function for calculating Human & Social values for CAET

caet_humanvalues <- function(){
  Humanvalues <- data[,c('key', 'location1', 'sys_name', 'women', 'labour', 'youth', 'animalwel')]
  Humanvalues <- Humanvalues %>%
    rowwise() %>%
    mutate(human_sum = sum(c(women,labour,youth ,animalwel),na.rm = TRUE),
           human_score = round((human_sum/16)*100,6),
           human_score = ifelse(is.na(animalwel),round((human_sum/12)*100,6),human_score))
  return(Humanvalues)
}

#function for calculating Circular and solidarity economy values for CAET

caet_circular <- function(){
  Circular <- data[,c('key', 'location1', 'sys_name', 'mkt_local', 'networks', 'local_fs')]
  Circular <- Circular %>%
    rowwise() %>%
    mutate(circ_sum = sum(c(mkt_local ,networks ,local_fs),na.rm = TRUE),
           circ_score = round((circ_sum/12)*100,6))
  return(Circular)
}

#function for calculating Responsible Governance values for CAET

caet_governance <- function(){
  Governance <- data[,c('key', 'location1', 'sys_name', 'prod_empow', 'prod_orgs', 'partic_prod')]
  Governance <- Governance %>%
    rowwise() %>%
    mutate(respg_sum = sum(c(prod_empow,prod_orgs,partic_prod),na.rm = TRUE),
           respg_score = round((respg_sum/12)*100,6))
  return(Governance)
}

#function for calculating all of the CAET values

caet_CAET <- function(){
  calc_CAET <- data[,c("key",'region', 'country', 'location1', 'location2', '_gps_loc_latitude', '_gps_loc_longitude', 'sys_type', 'sys_name',
                       'crops', 'animals', 'trees', 'div_activ','cla_int', 's_plant', 'tree_int', 'connectivity',
                       'ext_inp', 'soil_fert', 'pest_dis', 'productivity', 'rec_biomass', 'water', 'seeds_breeds', 'ren_energy',
                       'stab', 'vuln', 'indebt','diet', 'local_id', 'local_var','platforms', 'ae_know', 'partic_orgs',
                       'women', 'labour', 'youth', 'animalwel','mkt_local', 'networks', 'local_fs','prod_empow', 'prod_orgs', 'partic_prod')]
  calc_CAET <- calc_CAET %>%
    rowwise() %>%
    mutate(div_sum = sum(c(crops, animals, trees, div_activ), na.rm = TRUE),
           div_aver = mean(c(crops ,animals ,trees ,div_activ),na.rm = TRUE),  # Is later used for Resilience
           div_score = round((div_sum/16)*100,6),
           syn_sum = sum(c(cla_int ,s_plant ,tree_int ,connectivity),na.rm = TRUE),
           syn_score = round((syn_sum/16)*100,6),
           eff_sum = sum(c(ext_inp, soil_fert, pest_dis, productivity), na.rm = TRUE),
           eff_score = round((eff_sum/16)*100,6),
           rec_sum = sum(c(rec_biomass,water,seeds_breeds,ren_energy),na.rm = TRUE),
           rec_score = round((rec_sum/16)*100,6),
           res_sum = sum(c(stab, vuln, indebt, div_aver), na.rm = TRUE),
           res_score = round((res_sum/16)*100,6),
           cultf_sum = sum(c(diet,local_id,local_var),na.rm = TRUE),
           cultf_score = round((cultf_sum/12)*100,6),
           cocr_sum = sum(c(platforms,ae_know,partic_orgs),na.rm = TRUE),
           cocr_score = round((cocr_sum/12)*100,6),
           human_sum = sum(c(women,labour,youth ,animalwel),na.rm = TRUE),
           human_score = round((human_sum/16)*100,6),
           human_score = ifelse(is.na(animalwel),round((human_sum/12)*100,6),human_score),
           circ_sum = sum(c(mkt_local ,networks ,local_fs),na.rm = TRUE),
           circ_score = round((circ_sum/12)*100,6),
           respg_sum = sum(c(prod_empow,prod_orgs,partic_prod),na.rm = TRUE),
           respg_score = round((respg_sum/12)*100,6),
           caet_tot = mean(c(div_score,syn_score,eff_score,rec_score,res_score,cultf_score,cocr_score,human_score,circ_score,respg_score),na.rm = TRUE))
  
  CAET <- calc_CAET[,c('key', 'region', 'country', 'location1', 'location2', '_gps_loc_latitude', '_gps_loc_longitude', 'sys_type', 'sys_name',
                       'div_score','syn_score','eff_score','rec_score','res_score','cultf_score','cocr_score','human_score',
                       'circ_score','respg_score',"caet_tot")]
  return(CAET)
}

##############################################################################
#--------------------------------Step 2--------------------------------------#
##############################################################################

#-------------------------------------------------------------------------------
# 2.1 Dietary Diversity function
#-------------------------------------------------------------------------------

step2_Dietary_Diversity <- function(){
  DietaryDiv <- data[,c('key', 'grains_A', 'grains_B', 'pulses', 'nuts', 'dairy_E', 'dairy_F', 'meat_H', 'meat_I', 'meat_J', 'meat_K', 'meat_L',
                        'eggs', 'darkgreen', 'darkyellow_N', 'darkyellow_O', 'otherveg', 'otherfruit', 'fried_salty_1', 'fried_salty_2', 'fried_salty_3',
                        'sweet_foods', 'sweet_beverages_1', 'sweet_beverages_2')]
  DietaryDiv <- DietaryDiv %>%
    rowwise() %>%
    mutate(grains = ifelse(grains_A == 1 | grains_B == 1, 1, 0),
           dairy = ifelse(dairy_E == 1 | dairy_F == 1, 1, 0),
           meat = ifelse(meat_H == 1 | meat_I == 1 | meat_J == 1 | meat_K == 1 | meat_L == 1, 1, 0),
           darkyellow = ifelse(darkyellow_N == 1 | darkyellow_O == 1, 1, 0)) %>%
    rowwise() %>%
    mutate(dietary_diversity = sum(c(grains, pulses, nuts, dairy, meat, eggs, darkgreen, darkyellow, otherveg, otherfruit), na.rm = T)) %>%
    mutate(dietary_diversity = dietary_diversity*10) %>%
    mutate(dietary_diversity = ifelse(is.na(dietary_diversity), 0, dietary_diversity)) %>%
    return(DietaryDiv)
}


#-------------------------------------------------------------------------------
# 2.2 Soil Health function
#-------------------------------------------------------------------------------

step2_SoilHealth <- function(){
  SoilHealth <- data[,c('key', 'structure', 'compaction', 'depth', 'residues', 'color', 'water_ret', 'cover', 'erosion', 'invertebrates', 'microbio')]
  SoilHealth <- SoilHealth %>%
    rowwise() %>%
    mutate(soil_health = mean(c(structure,compaction,depth,residues,color,water_ret,cover,erosion,invertebrates,microbio),na.rm = T)) %>%
    mutate(soil_health = (soil_health - 1) * 25)
  return(SoilHealth)
}

#-------------------------------------------------------------------------------
# 2.3 Secure Land Tenure
#-------------------------------------------------------------------------------

step2_LandTenure <- function(){
  Landtenure <- data[,c('key', 'yn_men', 'name_men', 'ltperc_men', 'sell_men', 'beq_men', 'inh_men', 'yn_women', 'name_women', 'ltperc_women', 'sell_women', 'beq_women', 'inh_women')]
  
  Landtenure <- Landtenure %>%
    rowwise() %>%
    mutate(tmp_men = sum(c(sell_men, beq_men, inh_men), na.rm = T)) %>%
    mutate(landtenure_men = 0) %>%
    mutate(landtenure_men = case_when(yn_men == 1 & name_men == 1 & ltperc_men == 1 & tmp_men > 0 ~ 100,
                                      yn_men == 1 & name_men == 0 ~ 50,
                                      yn_men == 0 & ltperc_men == 1 & tmp_men > 0 ~ 50,
                                      yn_men == 1 & name_men == 1 & ltperc_men == 0 ~ 50,
                                      yn_men == 1 & name_men == 1 & ltperc_men != 1 & tmp_men > 0 ~ 50,
                                      yn_men == 0 & ltperc_men == 0 ~ 0,
                                      yn_men == 0 & tmp_men == 0 ~ 0)) %>%
    rowwise() %>%
    mutate(tmp_women = sum(c(sell_women, beq_women, inh_women), na.rm = T)) %>%
    mutate(landtenure_women = 0) %>%
    mutate(landtenure_women = case_when(yn_women == 1 & name_women == 1 & ltperc_women == 1 & tmp_women > 0 ~ 100,
                                        yn_women == 1 & name_women == 0 ~ 50,
                                        yn_women == 0 & ltperc_women == 1 & tmp_women > 0 ~ 50,
                                        yn_women == 1 & name_women == 1 & ltperc_women == 0 ~ 50,
                                        yn_women == 1 & name_women == 1 & ltperc_women != 1 & tmp_women > 0 ~ 50,
                                        yn_women == 0 & ltperc_women == 0 ~ 0,
                                        yn_women == 0 & tmp_women == 0 ~ 0))
  Landtenure <- Landtenure %>%
    mutate(landtenure = mean(c(landtenure_women, landtenure_men), na.rm = T))
  
  return(Landtenure)
}
#-------------------------------------------------------------------------------
# 2.4 Women Empowerment
#-------------------------------------------------------------------------------

step2_WomensEmpowerment <- function(){
  ProductiveDecision <- data[, c('key', 'deccrop', 'decanim', 'decotact', # About crops, animals, and other economic activities
                                 'decmajor', 'decminor', # About Major and minor hh expenditures (codifico e poi media)
                                 'perc_dec_crop', 'perc_dec_anim' , 'perc_dec_otact', 'perc_dec_major', 'perc_dec_minor')] # Perception for crops, animals and other economic activities (codifico e poi media): 
  # Decisions about crops, animals and other economic activities
  ProductiveDecision <- ProductiveDecision %>%
    mutate(prodec_c = case_when(deccrop %in% c(1,3) ~ 1,
                                deccrop %in% c(2,4) ~ 0),
           prodec_a = case_when(decanim %in% c(1,3) ~ 1,
                                decanim %in% c(2,4) ~ 0),
           prodec_oa = case_when(decotact %in% c(1,3) ~ 1,
                                 decotact %in% c(2,4) ~ 0),
           prodec_men_c = case_when(deccrop %in% c(2,3) ~ 1, # For Men Index
                                    deccrop %in% c(1,4) ~ 0),
           prodec_men_a = case_when(decanim %in% c(2,3) ~ 1, # For Men Index
                                    decanim %in% c(1,4) ~ 0),
           prodec_men_oa = case_when(decotact %in% c(2,3) ~ 1, # For Men Index
                                     decotact %in% c(1,4) ~ 0)) %>%
    rowwise() %>%
    mutate(prodec_caoa = mean(c(prodec_c, prodec_a, prodec_oa), na.rm = T)) %>%
    rowwise() %>%
    mutate(prodec_men_caoa = mean(c(prodec_men_c, prodec_men_a, prodec_men_oa), na.rm = T)) # For Men Index
  
  # Decisions about major and minor household expenditures
  ProductiveDecision <- ProductiveDecision %>%
    mutate(prodec_maj = case_when(decmajor %in% c(1,3) ~ 1,
                                  decmajor %in% c(2,4) ~ 0),
           prodec_min = case_when(decminor %in% c(1,3) ~ 1,
                                  decminor %in% c(2,4) ~ 0),
           prodec_men_maj = case_when(decmajor %in% c(2,3) ~ 1, # For Men Index
                                      decmajor %in% c(1,4) ~ 0), 
           prodec_men_min = case_when(decminor %in% c(2,3) ~ 1, # For Men Index
                                      decminor %in% c(1,4) ~ 0)) %>%
    rowwise() %>%
    mutate(prodec_mm = mean(c(prodec_maj, prodec_min), na.rm = TRUE)) %>%
    rowwise() %>%
    mutate(prodec_men_mm = mean(c(prodec_men_maj, prodec_men_min), na.rm = TRUE)) # For Men Index
  
  # Perception for crops, animals and other economic activities
  ProductiveDecision <- ProductiveDecision %>%
    mutate(perc_c = case_when(perc_dec_crop == 4 ~ 1,
                              perc_dec_crop == 3 ~ 0.66,
                              perc_dec_crop == 2 ~ 0.33,
                              perc_dec_crop == 1 ~ 0),
           perc_a = case_when(perc_dec_anim == 4 ~ 1,
                              perc_dec_anim == 3 ~ 0.66,
                              perc_dec_anim == 2 ~ 0.33,
                              perc_dec_anim == 1 ~ 0),
           perc_oa = case_when(perc_dec_otact == 4 ~ 1,
                               perc_dec_otact == 3 ~ 0.66,
                               perc_dec_otact == 2 ~ 0.33,
                               perc_dec_otact == 1 ~ 0)) %>%
    mutate(perc_men_c = 1 - perc_c, # For Men Index
           perc_men_a = 1 - perc_a,
           perc_men_oa = 1 - perc_oa) %>%
    rowwise() %>%
    mutate(perc_caoa = mean(c(perc_c , perc_a , perc_oa), na.rm = T)) %>%
    rowwise() %>%
    mutate(perc_men_caoa = mean(c(perc_men_c, perc_men_a, perc_men_oa), na.rm = T)) %>% # For the men Index
    mutate(perc_maj = case_when(perc_dec_major == 4 ~ 1,
                                perc_dec_major == 3 ~ 0.66,
                                perc_dec_major == 2 ~ 0.33,
                                perc_dec_major == 1 ~ 0),
           perc_min = case_when(perc_dec_minor == 4 ~ 1,
                                perc_dec_minor == 3 ~ 0.66,
                                perc_dec_minor == 2 ~ 0.33,
                                perc_dec_minor == 1 ~ 0)) %>%
    mutate(perc_men_maj = 1 - perc_maj, # For Men Index
           perc_men_min = 1 - perc_min) %>%
    rowwise() %>%
    mutate(perc_mm = mean(c(perc_maj, perc_min), na.rm = T)) %>% 
    rowwise() %>%
    mutate(perc_men_mm = mean(c(perc_men_maj, perc_men_min), na.rm = T)) %>% # For MEn Index
    rowwise() %>%
    mutate(prodec_women = mean(c(prodec_caoa, prodec_mm, perc_caoa, perc_mm), na.rm = T)) %>% #Total Productive Decisions score for Women
    rowwise() %>%
    mutate(prodec_men = mean(c(prodec_men_caoa, prodec_men_mm, perc_men_caoa, perc_men_mm), na.rm = T)) # Total Productive Decision score for Men
  ProductiveDecision <- rapply(ProductiveDecision, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  ## 2.4.2 Acces To and decision-making power over productive resources
  
  DecisionMaking <- data[,c('key', 'credit_women', 'credit_men',
                            'owcrop', 'owanim', 'owotact', 'owmajor', 'owminor')]
  
  Landtenure <- step2_LandTenure()
  
  DecisionMaking <- DecisionMaking %>%
    left_join(Landtenure[,c('key', 'landtenure_women', 'landtenure_men')], by = 'key') %>%
    mutate(landtenure_women = landtenure_women * 0.01) %>%
    mutate(landtenure_men = landtenure_men * 0.01)
  
  # Access to credit
  DecisionMaking <- DecisionMaking %>%
    mutate(credit_score = credit_men) %>%
    mutate(credit_score = case_when(credit_women == 1 ~ 1,
                                    credit_women == 2 & credit_men == 3 ~ 0.8,
                                    credit_women == 2 & credit_men == 2 ~ 0.75,
                                    credit_women == 2 & credit_men == 1 ~ 0.5,
                                    credit_women == 3 & credit_men == 3 ~ 0.25,
                                    credit_women == 3 & credit_men == 2 ~ 0.1,
                                    credit_women == 3 & credit_men == 1 ~ 0,
                                    credit_women == 2 & is.na(credit_men) ~ 0.5,
                                    credit_women == 3 & is.na(credit_men) ~ 0)) %>%
    mutate(credit_score_men = case_when(credit_men == 1 ~ 1,
                                        credit_men == 2 & credit_women == 3 ~ 0.8,
                                        credit_men == 2 & credit_women == 2 ~ 0.75,
                                        credit_men == 2 & credit_women == 1 ~ 0.5,
                                        credit_men == 3 & credit_women == 3 ~ 0.25,
                                        credit_men == 3 & credit_women == 2 ~ 0.1,
                                        credit_men == 3 & credit_women == 1 ~ 0,
                                        credit_men == 2 & is.na(credit_women) ~ 0.5,
                                        credit_men == 3 & is.na(credit_women) ~ 0))
  
  # ownership of crops, animals, other activities
  DecisionMaking <- DecisionMaking %>%
    mutate(own_c = case_when(owcrop %in% c(1,3) ~ 1,
                             owcrop %in% c(2,4) ~ 0),
           own_a = case_when(owanim %in% c(1,3) ~ 1,
                             owanim %in% c(2,4) ~ 0),
           own_oa = case_when(owotact %in% c(1,3) ~ 1,
                              owotact %in% c(2,4) ~ 0)) %>%
    mutate(own_men_c = case_when(owcrop %in% c(2,3) ~ 1, # For Men Index
                                 owcrop %in% c(1,4) ~ 0),
           own_men_a = case_when(owanim %in% c(2,3) ~ 1,
                                 owanim %in% c(1,4) ~ 0),
           own_men_oa = case_when(owanim %in% c(2,3) ~ 1,
                                  owanim %in% c(1,4) ~ 0)) %>%
    rowwise() %>%
    mutate(own_caoa = mean(c(own_c, own_a, own_oa), na.rm = T)) %>%
    rowwise() %>%
    mutate(own_men_caoa = mean(c(own_men_c, own_men_a, own_men_oa), na.rm = T))
  
  # Ownership of major & minor hh assets
  DecisionMaking <- DecisionMaking %>%
    mutate(own_maj = case_when(owmajor %in% c(1,3) ~ 1,
                               owmajor %in% c(2,4) ~ 0),
           own_min = case_when(owminor %in% c(1,3) ~ 1, 
                               owminor %in% c(2,4) ~ 0)) %>%
    mutate(own_men_maj = case_when(owmajor %in% c(2,3) ~ 1, # For Men Index
                                   owmajor %in% c(1,4) ~ 0),
           own_men_min = case_when(owminor %in% c(2,3) ~ 1,
                                   owminor %in% c(1,4) ~ 0)) %>%
    rowwise() %>%
    mutate(own_mm = mean(c(own_maj, own_min), na.rm = T)) %>%
    rowwise() %>%
    mutate(own_men_mm = mean(c(own_men_maj, own_men_min), na.rm = T))
  
  # Total Access To and Decision-Making score
  DecisionMaking <- DecisionMaking %>%
    rowwise() %>%
    mutate(cred_decmak_women = mean(c(landtenure_women, credit_score, own_caoa, own_mm), na.rm = T)) %>%
    rowwise() %>%
    mutate(cred_decmak_men = mean(c(landtenure_men, credit_score_men, own_men_caoa, own_men_mm), na.rm = T))
  DecisionMaking <- rapply(DecisionMaking, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  ## 2.4.3 Use of Income
  IncomeUse <- data[, c('key', 'dec_rev_crop', 'dec_rev_anim', 'dec_rev_oth')] 
  
  IncomeUse <- IncomeUse %>%
    mutate(decinc_c = case_when(dec_rev_crop %in% c(1,3) ~ 1,
                                dec_rev_crop %in% c(2,4) ~ 0),
           decinc_a = case_when(dec_rev_anim %in% c(1,3) ~ 1,
                                dec_rev_anim %in% c(2,4) ~ 0),
           decinc_oa = case_when(dec_rev_oth %in% c(1,3) ~ 1,
                                 dec_rev_oth %in% c(2,4) ~ 0)) %>%
    mutate(decinc_men_c = case_when(dec_rev_crop %in% c(2,3) ~ 1,
                                    dec_rev_crop %in% c(1,4) ~ 0),
           decinc_men_a = case_when(dec_rev_anim %in% c(2,3) ~ 1,
                                    dec_rev_anim %in% c(1,4) ~ 0),
           decinc_men_oa = case_when(dec_rev_oth %in% c(2,3) ~ 1,
                                     dec_rev_oth %in% c(1,4) ~ 0)) %>%
    rowwise() %>%
    mutate(decinc_women = mean(c(decinc_c, decinc_a, decinc_oa), na.rm = T)) %>%
    rowwise() %>%
    mutate(decinc_men = mean(c(decinc_men_c, decinc_men_a, decinc_men_oa), na.rm = T))
  IncomeUse <- rapply(IncomeUse, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  ## 2.4.4 Leadership in the community
  Leadership <- data[,c('key', 'involv_agri_wom', 'involv_othe_wom', 'involv_agri_men', 'involv_othe_men')]
  # Calculation according to STATA file
  Leadership <- Leadership %>%
    mutate(lead_wom_agri = (involv_agri_wom - 1)*0.25,
           lead_wom_othe = (involv_othe_wom - 1)*0.25) %>%
    mutate(lead_men_agri = (involv_agri_men - 1)*0.25,
           lead_men_othe = (involv_othe_men - 1)*0.25) %>%
    rowwise() %>%
    mutate(leadership_score_women = max(lead_wom_agri, lead_wom_othe, na.rm = T)) %>%
    rowwise() %>%
    mutate(leadership_score_men = max(lead_men_agri, lead_men_othe, na.rm = T))
  Leadership <- rapply(Leadership, f = function(x) ifelse(is.infinite(x), NA, x), how = "replace" )
  
  ## 2.4.5 Time use (average between the two scores)
  Timeuse <- data[,c('key', 'wtime_ag_women', 'wtime_dom_women', 'wtime_otgain_women', 'wtime_ag_men', 'wtime_dom_men', 'wtime_otgain_men')]
  
  
  # More than 10.5 hours per day:       
  Timeuse <- Timeuse %>%
    rowwise() %>%
    mutate(tmp_wtime_women = sum(c(wtime_ag_women, wtime_dom_women, wtime_otgain_women)),
           tmp_wtime_men = sum(c(wtime_ag_men, wtime_dom_men, wtime_otgain_men))) %>% 
    mutate(hrs_women = case_when(tmp_wtime_women <= 10.5 & tmp_wtime_women >= 0 ~ 1,
                                 is.na(tmp_wtime_women) | is.na(tmp_wtime_men) ~ NA,
                                 TRUE ~ 0),
           hrs_men = case_when(tmp_wtime_men <= 10.5 &  tmp_wtime_men >= 0 ~ 1,
                               is.na(tmp_wtime_women) | is.na(tmp_wtime_men) ~ NA,
                               TRUE ~ 0))
  
  # Time spent in Agricultural activities + food preparation & domestic works + other gainful activities
  Timeuse <- Timeuse %>%
    mutate(worktime_women = case_when(tmp_wtime_women > tmp_wtime_men ~ 0,
                                      tmp_wtime_women == 0 & tmp_wtime_men == 0 ~ 0,
                                      tmp_wtime_women <= tmp_wtime_men ~ 1,
                                      is.na(tmp_wtime_women) | is.na(tmp_wtime_men) ~ NA),
           worktime_men = case_when(tmp_wtime_men > tmp_wtime_women ~ 0,
                                    tmp_wtime_men == 0 & tmp_wtime_women == 0 ~ 0,
                                    tmp_wtime_men <= tmp_wtime_women ~ 1,
                                    is.na(tmp_wtime_women) | is.na(tmp_wtime_men) ~ NA))
  
  # Total Time Use score:
  Timeuse <- Timeuse  %>% 
    rowwise() %>%
    mutate(timeuse_women = mean(c(hrs_women, worktime_women), na.rm = T)) %>% 
    mutate(timeuse_men = mean(c(hrs_men, worktime_men), na.rm = T)) 
  Timeuse <- rapply(Timeuse, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  
  ## 2.4.6 Total Women Empowerment Score
  WomenEmpowerment <- ProductiveDecision %>%
    select(c('key', 'prodec_women', 'prodec_men')) %>%
    left_join(DecisionMaking[,c('key', 'cred_decmak_women', 'cred_decmak_men')], by = 'key') %>%
    left_join(IncomeUse[,c('key', 'decinc_women', 'decinc_men')], by = 'key') %>%
    left_join(Leadership[,c('key', 'leadership_score_women', 'leadership_score_men')], by = 'key') %>%
    left_join(Timeuse[,c('key', 'timeuse_women', 'timeuse_men')], by = 'key') %>%
    rowwise() %>%
    mutate(wemp_score = mean(c(prodec_women, cred_decmak_women, decinc_women, leadership_score_women, timeuse_women), na.rm = T)*100) %>%
    rowwise() %>%
    mutate(memp_score = mean(c(prodec_men, cred_decmak_men, decinc_men, leadership_score_men, timeuse_men), na.rm = T)*100)
  WomenEmpowerment <- rapply(WomenEmpowerment, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  rm(ProductiveDecision, DecisionMaking, IncomeUse, Leadership, Timeuse)
  
  # Replace the result with missing value if there are no women in the household:
  WomenEmpowerment <- WomenEmpowerment %>% 
    left_join(data[,c('key', 'hh_fem' )], by = 'key') %>%
    mutate(wemp_score = ifelse(hh_fem == 0, NA, wemp_score))
  
  #implement GPI score
  
  WomenEmpowerment <- WomenEmpowerment %>%
    mutate(GPI = wemp_score/memp_score * 100)
  return(WomenEmpowerment)
}

#-------------------------------------------------------------------------------
# 2.5 Youth Employment and Emigration
#-------------------------------------------------------------------------------

step2_YouthEmploymentEmigration <- function(){
  data_y <- data_youngsters %>%
    rename(key = '_parent_index') %>%
    left_join(data[,c('key', 'ymembers')], 'key')
  
  # Emigrated youth due to lack of employment
  data_y <- data_y %>%
    mutate(y_occup = ifelse(y_emigrated == 1,0,y_occup)) %>%
    mutate(y_future_farm = ifelse(y_emigrated == 1,0,y_future_farm)) %>%
    mutate(y_keen_emigr = ifelse(y_emigrated == 1,1,y_keen_emigr))
  
  
  # Employment
  data_y <- data_y %>%
    mutate(y_good = ifelse(y_occup %in% c(1,4,5,6),1,0),
           y_med = ifelse(y_occup %in% c(2),1,0),
           y_bad = ifelse(y_occup %in% c(0,3),1,0))
  
  data_y <- data_y %>%
    group_by(key) %>%
    mutate(good_sum = sum(y_good,na.rm = TRUE),
           med_sum = sum(y_med,na.rm = TRUE),
           bad_sum = sum(y_bad,na.rm = TRUE),
           ymembers = sum(c(y_good,y_bad,y_med), na.rm = TRUE)) # what to do?
  
  data_y <- data_y %>%
    mutate(good_perc = good_sum*100/ymembers,
           med_perc = med_sum*100/ymembers,
           bad_perc = bad_sum*100/ymembers) %>% 
    mutate(youth_employ = good_perc + 0.5*med_perc)
  
  # Emigration
  data_y <- data_y %>%
    mutate(g1 = ifelse(y_future_farm == 1 & y_keen_emigr == 0,1,0), 
           g2 = ifelse(y_future_farm == 0 & y_keen_emigr == 1,1,0),
           g3 = ifelse(y_future_farm == 1 & y_keen_emigr == 1,1,0),
           g4 = ifelse(y_future_farm == 0 & y_keen_emigr == 0,1,0))
  
  data_y <- data_y %>%
    group_by(key) %>%
    mutate(g1_sum = sum(g1, na.rm = TRUE),
           g2_sum = sum(g2, na.rm = TRUE),
           g3_sum = sum(g3, na.rm = TRUE),
           g4_sum = sum(g4, na.rm = TRUE)) %>% 
    mutate(g1_perc = g1_sum/ymembers*100,
           g2_perc = g2_sum/ymembers*100,
           g3_perc = g3_sum/ymembers*100,
           g4_perc = g4_sum/ymembers*100) %>% 
    mutate(youth_emigr = g1_perc + 0.5*g3_perc + 0.5*g4_perc) 
  
  # calculate youth score
  data_y <- data_y %>% 
    rowwise() %>%
    mutate(youth_score = mean(c(youth_employ , youth_emigr),na.rm = T)) %>% 
    mutate(youth_score = ifelse(ymembers == 0,NA,youth_score))
  
  YouthScore <- data_y %>%
    select(youth_employ, youth_emigr, youth_score, key) %>%
    distinct()
  return(YouthScore)
  
}

#-------------------------------------------------------------------------------
# 2.6 Agricultural Biodiversity
#-------------------------------------------------------------------------------

step2_Agricultural_Biodiversity <- function(){
  
  #-------------------------------------------------------------------------------
  # Old score
  #-------------------------------------------------------------------------------
  
  if (cfg$Indicator == "Agrobiodiversity"){
    
    ## Crops
    c1 <- data_c1 %>%
      rename(key = '_parent_index')
    
    c1 <- c1 %>% 
      group_by(cname_label, key) %>%
      mutate(cprod = sum(cprod, na.rm = TRUE),
             cqsold = sum(cqsold, na.rm = TRUE),
             cpg = sum(cpg, na.rm = TRUE),
             cgift = sum(cgift, na.rm = TRUE),
             cland = sum(cland, na.rm = TRUE),
             cvar = sum(cvar, na.rm = TRUE)) %>%  
      ungroup() %>%
      select(cname_label, key, cprod, cqsold, cpg, cgift, cland, cvar) %>%
      arrange(cname_label) %>% 
      distinct() %>% as.data.frame()
    
    c1 <- c1 %>%
      mutate(cvar = ifelse(cvar > 50, 1, cvar)) %>%
      group_by(key) %>%
      mutate(cland_sum = sum(cland, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(key) %>%
      mutate(p2_ = (cland/cland_sum)^2/cvar)
    
    c1 <- c1 %>%
      group_by(key) %>%
      mutate(shannon = sum(p2_, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(shannon = ifelse(is.na(shannon), 1, shannon)) %>%
      mutate(GSIndex_crops = 100*(1 - shannon)) %>%
      select(!c("p2_"))
    
    GSIndex_crops <- c1 %>%
      group_by(key, shannon, GSIndex_crops) %>%
      distinct(key)
    rm(c1)
    
    ## Animals
    lsucv_cattle <- cfg$lsucv_cattle
    lsucv_sheep <- cfg$lsucv_sheep
    lsucv_pig <- cfg$lsucv_pig
    
    a1 <- data_a1 %>%
      rename(key = '_parent_index',) 
    a1 <- a1 %>% 
      group_by(aname,key) %>%
      mutate(arais = sum(arais, na.rm = TRUE),
             aborn = sum(aborn, na.rm = TRUE),
             adied = sum(adied, na.rm = TRUE),
             abreed = sum(abreed, na.rm = TRUE),
             aqsold = sum(aqsold, na.rm = TRUE),
             apg = sum(apg, na.rm = TRUE),
             aqgift = sum(aqgift, na.rm = TRUE)) %>% 
      ungroup() %>%
      select(aname, key, arais, aborn, adied, abreed, aqsold, apg, aqgift) %>%
      arrange(aname) %>% distinct() %>% as.data.frame()
    
    # We convert in livestok standard units:
    a1 <- a1 %>%
      mutate(livunit = case_when(aname == 0 ~ lsucv_cattle,
                                 aname %in% c(1,2,3,4) ~ 0.8,
                                 aname %in% c(5,6) ~ 0.3,
                                 aname == 7 ~ lsucv_sheep,
                                 aname %in% c(8, 23) ~ 0.1,
                                 aname == 9 ~ lsucv_pig,
                                 aname == 10 ~ 1,
                                 aname == 11 ~ 0.17,
                                 aname == 12 ~ 0.02,
                                 aname %in% c(13, 19) ~ 0.01,
                                 aname %in% c(14, 15, 16, 22) ~ 0.03,
                                 aname == 17 ~ 0.005,
                                 aname == 18 ~ 0.35,
                                 aname == 20 ~ 0.003,
                                 aname %in% c(21, 77) ~ 0.002)) %>%
      mutate(nliv = livunit * arais) # Multiplying livestock units per number of individuals
    
    a1 <- a1 %>%
      group_by(key) %>%
      mutate(nliv_sum = sum(nliv, na.rm = TRUE)) %>%  # total livestock size in SLU/TLU
      ungroup() %>%
      mutate(p2_ = (nliv/nliv_sum)^2/abreed) %>%   # weighted shares
      group_by(key) %>%
      mutate(shannon_a = sum(p2_, na.rm = TRUE)) %>% # shannon index
      ungroup() %>%
      mutate(shannon_a = ifelse(is.na(shannon_a), 1, shannon_a)) %>% 
      mutate(GSIndex_animals = 100*(1 - shannon_a)) %>%
      mutate(GSIndex_animals = ifelse(nliv == nliv_sum & abreed == 1, 10, GSIndex_animals)) %>% # Farms with only one animal category and 1 breed get a value of 10 instead of 0
      select(!c("p2_")) %>%
      filter(!nliv == 0)
    
    GSIndex_animals <- a1 %>%
      group_by(key, shannon_a, GSIndex_animals) %>%
      distinct(key)
    rm(a1)
    
    ## Beekeping & other
    data_b <- data[,c('key', 'nat_veg', 'bee', 'poll')]
    data_b <- data_b %>%
      mutate(natveg_score = case_when(nat_veg == 1 ~ 0,
                                      nat_veg == 2 ~ 0.33,
                                      nat_veg == 3 ~ 0.66,
                                      nat_veg == 4 ~ 1),
             bee_score = case_when(bee == 3 ~ 0,
                                   bee == 2 ~ 0.5,
                                   bee == 1 ~ 1),
             poll_score = case_when(poll == 1 ~ 1,
                                    poll == 2 ~ 0.66,
                                    poll == 3 ~ 0.33,
                                    poll == 4 ~ 0))
    
    data_b <- data_b %>%
      rowwise() %>%
      mutate(b_gini = mean(c(natveg_score, bee_score, poll_score), na.rm = T)) %>%
      mutate(GSI_other = b_gini*100)
    
    ## OVERALL GINI-SIMPSON DIVERSITY INDEX
    GSI_overall <- GSIndex_crops %>%
      left_join(GSIndex_animals, by = 'key') %>%
      left_join(data_b[,c('key', 'GSI_other')], by = 'key') %>%
      rowwise() %>%
      mutate(Agricultural_diversity_tape = mean(c(GSIndex_crops, GSIndex_animals, GSI_other), na.rm = T))
    rm(GSIndex_crops, GSIndex_animals, data_b)
    
    Agricultural_biodiversity <- GSI_overall
    
    return(Agricultural_biodiversity)
    
  }
  else if (cfg$Indicator == "Biodiversity"){
    
    #-------------------------------------------------------------------------------
    # New score
    #-------------------------------------------------------------------------------
    
    
    
    ## Crops
    c1 <- data_c1 %>%
      rename(key = '_parent_index')
    
    c1 <- c1 %>% 
      group_by(cname_label, key) %>%
      mutate(cprod = sum(cprod, na.rm = TRUE),
             cqsold = sum(cqsold, na.rm = TRUE),
             cpg = sum(cpg, na.rm = TRUE),
             cgift = sum(cgift, na.rm = TRUE),
             cland = sum(cland, na.rm = TRUE),
             cvar = sum(cvar, na.rm = TRUE)) %>%  
      ungroup() %>%
      select(cname_label, key, cprod, cqsold, cpg, cgift, cland, cvar) %>%
      arrange(cname_label) %>% 
      distinct() %>% as.data.frame()
    
    c1 <- c1 %>%
      mutate(cvar = ifelse(cvar > 50, 1, cvar)) %>%
      group_by(key) %>%
      mutate(cland_sum = sum(cland, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(key) %>%
      mutate(p2_ = (cland/cland_sum)^2/cvar)
    
    c1 <- c1 %>%
      group_by(key) %>%
      mutate(shannon = sum(p2_, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(shannon = ifelse(is.na(shannon), 1, shannon)) %>%
      mutate(GSIndex_crops = 100*(1 - shannon)) %>%
      select(!c("p2_"))
    
    GSIndex_crops <- c1 %>%
      group_by(key, shannon, GSIndex_crops) %>%
      distinct(key)
    rm(c1)
    
    ## Animals
    lsucv_cattle <- cfg$lsucv_cattle
    lsucv_sheep <- cfg$lsucv_sheep
    lsucv_pig <- cfg$lsucv_pig
    
    a1 <- data_a1 %>%
      rename(key = '_parent_index',) 
    a1 <- a1 %>% 
      group_by(aname,key) %>%
      mutate(arais = sum(arais, na.rm = TRUE),
             aborn = sum(aborn, na.rm = TRUE),
             adied = sum(adied, na.rm = TRUE),
             abreed = sum(abreed, na.rm = TRUE),
             aqsold = sum(aqsold, na.rm = TRUE),
             apg = sum(apg, na.rm = TRUE),
             aqgift = sum(aqgift, na.rm = TRUE)) %>% 
      ungroup() %>%
      select(aname, key, arais, aborn, adied, abreed, aqsold, apg, aqgift) %>%
      arrange(aname) %>% distinct() %>% as.data.frame()
    
    # We convert in livestok standard units:
    a1 <- a1 %>%
      mutate(livunit = case_when(aname == 0 ~ lsucv_cattle,
                                 aname %in% c(1,2,3,4) ~ 0.8,
                                 aname %in% c(5,6) ~ 0.3,
                                 aname == 7 ~ lsucv_sheep,
                                 aname %in% c(8, 23) ~ 0.1,
                                 aname == 9 ~ lsucv_pig,
                                 aname == 10 ~ 1,
                                 aname == 11 ~ 0.17,
                                 aname == 12 ~ 0.02,
                                 aname %in% c(13, 19) ~ 0.01,
                                 aname %in% c(14, 15, 16, 22) ~ 0.03,
                                 aname == 17 ~ 0.005,
                                 aname == 18 ~ 0.35,
                                 aname == 20 ~ 0.003,
                                 aname %in% c(21, 77) ~ 0.002)) %>%
      mutate(nliv = livunit * arais) # Multiplying livestock units per number of individuals
    
    a1 <- a1 %>%
      group_by(key) %>%
      mutate(nliv_sum = sum(nliv, na.rm = TRUE)) %>%  # total livestock size in SLU/TLU
      ungroup() %>%
      mutate(p2_ = (nliv/nliv_sum)^2/abreed) %>%   # weighted shares
      group_by(key) %>%
      mutate(shannon_a = sum(p2_, na.rm = TRUE)) %>% # shannon index
      ungroup() %>%
      mutate(shannon_a = ifelse(is.na(shannon_a), 1, shannon_a)) %>% 
      mutate(GSIndex_animals = 100*(1 - shannon_a)) %>%
      mutate(GSIndex_animals = ifelse(nliv == nliv_sum & abreed == 1, 10, GSIndex_animals)) %>% # Farms with only one animal category and 1 breed get a value of 10 instead of 0
      select(!c("p2_")) %>%
      filter(!nliv == 0)
    
    GSIndex_animals <- a1 %>%
      group_by(key, shannon_a, GSIndex_animals) %>%
      distinct(key)
    rm(a1)
    
    GSI_overall <- GSIndex_crops %>%
      left_join(GSIndex_animals, by = 'key')
    
    ## 2.6.1  Agricultural diversity
    Agricultural_biodiversity <- GSI_overall %>%
      mutate(Agricultural_diversity = (GSIndex_crops + GSIndex_animals)/2) 
    
    
    ### New Agrobiodiversity Index
    ## Total agricultural Area (total area under agricultural production + total grazing area + area semi natural habitats)
    
    snh_data <- data_snh %>%
      rename(key = '_parent_index') %>%
      mutate(SNH_area_ha = ifelse(is.na(sizeha_snh), length_snh*width_snh*0.0001,sizeha_snh)) %>%
      group_by(key) %>%
      summarise_at(vars(SNH_area_ha), sum, na.rm = T)
    
    Area_total <- data[, c('key', 'area', 'area_g')] %>%
      left_join(snh_data, by = 'key') %>%
      rowwise() %>%
      mutate(area_total = sum(area, area_g, SNH_area_ha, na.rm = T))
    
    
    
    ## 2.6.2 Patch size
    # only crop fields are considered
    Patch_size <- data[,c('key', 'crops_fields', 'crops_fields_area')]
    Patch_size <- Patch_size %>%
      mutate(Patch_size_n = crops_fields_area/crops_fields, # Medium patch size for crops
             Patch_size = case_when(Patch_size_n < 0.5 ~ 100,
                                    Patch_size_n >= 0.5 & Patch_size_n < 3 ~ 75,
                                    Patch_size_n >= 3 & Patch_size_n < 16 ~ 50,
                                    Patch_size_n >= 16 & Patch_size_n < 100 ~ 25,
                                    Patch_size_n >= 100 ~ 0))
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(Patch_size, by = 'key')
    rm(Patch_size)
    
    ## 2.6.3 Tree habitat
    Tree <- data_tree %>%
      rename(key = '_parent_index') %>%
      group_by(key) %>%
      summarise_at(vars(sizeha), sum, na.rm = T)
    
    Tree_habitat <- data[,c('key', 'width', 'length')]
    Tree_habitat <- Tree_habitat %>%
      left_join(Tree, by = 'key') %>%
      mutate(tree_lines_ha = length * width * 0.0001) %>% # length and width are in m
      rowwise() %>%
      mutate(tree_area_total = sum(tree_lines_ha, sizeha, na.rm = T)) %>%
      left_join(Area_total, by = 'key') %>%
      mutate(Tree_habitat = (tree_area_total/area_total)*100)
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(Tree_habitat[,c('key', 'sizeha', 'tree_lines_ha', 'Tree_habitat')], by = 'key')
    rm(Tree_habitat, Tree)
    
    ## 2.6.4 Semi-natural Habitats
    SNH <- data_snh %>%
      rename(key = '_parent_index',)
    # diversity of SNH 
    SNH_div <- SNH %>%
      group_by(key) %>%
      summarise(count = n_distinct(snhname)) %>%
      mutate_all(., as.numeric)
    
    SemiNaturalHabitats <- Area_total %>%
      left_join(SNH_div, by = 'key') %>%
      mutate(SNH_area_ind =  SNH_area_ha/area_total * 4) %>%# x 4, so that 25% of SNH gives max points
      mutate(SNH_area_ind = ifelse(SNH_area_ind >= 1, 1, SNH_area_ind)) %>%
      mutate(SNH_div_ind = case_when(count >= 5 ~ 1,
                                     count == 4 ~ 0.75,
                                     count == 3 ~ 0.5,
                                     count == 2 ~ 0.25,
                                     count < 2 ~ 0)) %>%
      mutate(Semi_natural_Habitats = (SNH_area_ind + SNH_div_ind)/2) %>%
      mutate(Semi_natural_Habitats = Semi_natural_Habitats * 100) %>%
      mutate(Semi_natural_Habitats = ifelse(is.na(Semi_natural_Habitats), 0, Semi_natural_Habitats))
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(SemiNaturalHabitats[,c('key', 'SNH_area_ind', 'SNH_div_ind', 'Semi_natural_Habitats')], by = 'key')
    rm(SNH, SNH_div, SemiNaturalHabitats)
    
    ## 2.6.5 Nitrogen application
    N_Fert <- data[, c('key', 'fert_ha', 'fert_intensity')] %>%
      left_join(Area_total, by = 'key') %>%
      mutate(FertArea = 1 - (fert_ha/area_total)) %>%
      mutate(N_input = case_when(fert_intensity == 1 ~ 1,
                                 fert_intensity == 2 ~ 0.75,
                                 fert_intensity == 3 ~ 0.5,
                                 fert_intensity == 4 ~ 0.25,
                                 fert_intensity == 5 ~ 0)) %>%
      mutate(N_application = (FertArea + N_input)/2) %>%
      mutate(N_application = N_application * 100)
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(N_Fert[,c('key', 'FertArea', 'N_input', 'N_application')], by = 'key')
    rm(N_Fert)
    
    ## 2.6.6 Pesticide application
    ## Pesticide application
    
    cp <- data_cp %>%
      rename(key = '_parent_index') %>%
      group_by(key) %>%
      summarise_at(vars(cpapp_num), sum, na.rm = T) 
    
    ## Toxiocology
    cp_tox <- data_cp %>%
      rename(key = '_parent_index') %>%
      mutate(toxicity = ifelse(cptox <= cptoxenv, cptox, cptoxenv)) %>%
      group_by(key) %>%
      summarise_at(vars(toxicity), min, na.rm = T)
    
    ## Area
    pesticides <- data[,c('key', 'cpestareaf')] %>%
      left_join(Area_total, by = 'key') %>%
      left_join(cp, by = 'key') %>%
      left_join(cp_tox, by = 'key') %>%
      mutate(nr_Appl_ha = cpapp_num/area_total) %>%
      mutate(cp = case_when(nr_Appl_ha >= 10 ~ 0,
                            nr_Appl_ha < 10 & nr_Appl_ha >= 9 ~ 0.1,
                            nr_Appl_ha < 9 & nr_Appl_ha >= 8 ~ 0.2,
                            nr_Appl_ha < 8 & nr_Appl_ha >= 7 ~ 0.3, 
                            nr_Appl_ha < 7 & nr_Appl_ha >= 6 ~ 0.4,
                            nr_Appl_ha < 6 & nr_Appl_ha >= 5 ~ 0.5,
                            nr_Appl_ha < 5 & nr_Appl_ha >= 4 ~ 0.6,
                            nr_Appl_ha < 4 & nr_Appl_ha >= 3 ~ 0.7,
                            nr_Appl_ha < 3 & nr_Appl_ha >= 2 ~ 0.8,
                            nr_Appl_ha < 2 & nr_Appl_ha >= 1 ~ 0.9,
                            nr_Appl_ha < 1 ~ 1)) %>%
      mutate(cp = replace_na(cp, 1)) %>%
      mutate(cp_area = 1 - (cpestareaf/area_total)) %>%
      mutate(cp_area = ifelse(cp_area < 0, 0, cp_area)) %>%
      mutate(cp_ecotox = case_when(is.na(toxicity) ~ 1,
                                   toxicity == 3 ~ 0.67,
                                   toxicity == 2 ~ 0.33,
                                   toxicity == 1 ~ 0)) %>%
      rowwise() %>%
      mutate(cp_ind = mean(c(cp, cp_area, cp_ecotox))) %>%
      mutate(cp_ind = cp_ind * 100)
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(pesticides[,c('key', 'cp', 'cp_area', 'cp_ecotox', 'cp_ind')], by = 'key')
    rm(cp, cp_tox, pesticides)
    
    ### 2.6.7 Mechanical Field operations
    Fieldoperations <- data[,c('key', 'mechz_fieldops')] %>%
      rename(field_op_nr = `mechz_fieldops`) %>%
      mutate(FieldOP = case_when(field_op_nr == 1 ~ 100,
                                 field_op_nr == 2 ~ 75,
                                 field_op_nr == 3 ~ 50,
                                 field_op_nr == 4 ~ 25,
                                 field_op_nr == 5 ~ 0))
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(Fieldoperations[,c('key', 'FieldOP')], by = 'key')
    rm(Fieldoperations)
    
    ### 2.6.8 Stocking rate
    # Number of animals converted to livestock units (lU/ha)
    Stockingrate <- data_a1 %>%
      rename(key = '_parent_index') 
    
    Stockingrate <- Stockingrate %>% 
      group_by(aname, key, onfarm) %>%
      mutate(arais = sum(arais, na.rm = TRUE)) %>% 
      ungroup() %>%
      select(aname, key, arais, onfarm) %>%
      distinct() %>% as.data.frame()
    
    # We convert in livestok standard units:
    Stockingrate <- Stockingrate %>%
      mutate(livunit = case_when(aname == 0 ~ lsucv_cattle,
                                 aname %in% c(1,2,3,4) ~ 0.8,
                                 aname %in% c(5,6) ~ 0.3,
                                 aname == 7 ~ lsucv_sheep,
                                 aname %in% c(8, 23) ~ 0.1,
                                 aname == 9 ~ lsucv_pig,
                                 aname == 10 ~ 1,
                                 aname == 11 ~ 0.17,
                                 aname == 12 ~ 0.02,
                                 aname %in% c(13, 19) ~ 0.01,
                                 aname %in% c(14, 15, 16, 22) ~ 0.03,
                                 aname == 17 ~ 0.005,
                                 aname == 18 ~ 0.35,
                                 aname == 20 ~ 0.003,
                                 aname %in% c(21, 77) ~ 0.002)) %>%
      mutate(nliv = livunit * arais) %>% # Multiplying livestock units per number of individuals
      group_by(key) %>%
      summarise_at(vars(nliv), sum, na.rm = T)
    
    Stockingrate <- Stockingrate %>%
      left_join(Area_total, by = 'key') %>%
      mutate(stocking_rate = nliv/area_total) %>%
      mutate(AvStock = case_when(stocking_rate > 4 ~ 0,
                                 stocking_rate <= 4 ~ (100-(25 * stocking_rate))))
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(Stockingrate[,c('key', 'AvStock')], by = 'key')
    rm(Stockingrate)
    
    ### 2.6.9 Grazing intensity
    Grazing_intensity <- data_a1 %>%
      rename(key = '_parent_index') 
    
    Grazing_intensity <- Grazing_intensity %>% 
      group_by(aname, key, onfarm) %>%
      mutate(arais = sum(arais, na.rm = TRUE)) %>% 
      ungroup() %>%
      select(aname, key, arais, onfarm) %>%
      distinct() %>% as.data.frame()
    
    # We convert in livestok standard units:
    Grazing_intensity <- Grazing_intensity %>%
      mutate(livunit = case_when(aname == 0 ~ lsucv_cattle,
                                 aname %in% c(1,2,3,4) ~ 0.8,
                                 aname %in% c(5,6) ~ 0.3,
                                 aname == 7 ~ lsucv_sheep,
                                 aname %in% c(8, 23) ~ 0.1,
                                 aname == 9 ~ lsucv_pig,
                                 aname == 10 ~ 1,
                                 aname == 11 ~ 0.17,
                                 aname == 12 ~ 0.02,
                                 aname %in% c(13, 19) ~ 0.01,
                                 aname %in% c(14, 15, 16, 22) ~ 0.03,
                                 aname == 17 ~ 0.005,
                                 aname == 18 ~ 0.35,
                                 aname == 20 ~ 0.003,
                                 aname %in% c(21, 77) ~ 0.002)) %>%
      mutate(nliv = livunit * arais) %>% # Multiplying livestock units per number of individuals
      mutate(pasture_d = nliv * onfarm/100) %>% # livestock times percentage on farm
      group_by(key) %>%
      summarise_at(vars(pasture_d, nliv, livunit), sum, na.rm = T)
    
    Grazing_intensity <- Grazing_intensity %>%
      left_join(Area_total, by = 'key') %>%
      mutate(GI = pasture_d/area_g) %>%
      mutate(Grazing_intensity = case_when(GI > 2 ~ 0,
                                           GI <= 2 & GI > 1 ~ 25,
                                           GI <= 1 & GI > 0.5 ~ 50,
                                           GI <= 0.5 & GI > 0.25 ~ 75,
                                           GI <= 0.25 ~ 100))
    Grazing_intensity <- Grazing_intensity %>%
      mutate(Grazing_intensity = ifelse(area_g == 0, NA, Grazing_intensity)) # Farmers that have no On-farm pastures have grazing intensity NA
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(Grazing_intensity[,c('key', 'Grazing_intensity')], by = 'key')
    
    ### 2.6.10 Land use change
    LUC <- data[,c('key', 'landuse_change')] %>%
      left_join(Area_total, by = 'key') %>%
      mutate(LUC_SNH = 1 - (landuse_change/area_total)) %>%
      mutate(LUC_SNH = LUC_SNH * 100) %>%
      mutate(LUC_SNH = ifelse(is.na(LUC_SNH), 100, LUC_SNH))
    
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      left_join(LUC[,c('key', 'LUC_SNH')], by = 'key') 
    rm(LUC)
    
    ### Calculation of total Biodiversity Indicator
    Agricultural_biodiversity <- Agricultural_biodiversity %>%
      rowwise() %>%
      mutate(Biodiversity_indicator = mean(c(Agricultural_diversity, LUC_SNH, Grazing_intensity, AvStock, FieldOP, cp_ind, N_application, Semi_natural_Habitats, Tree_habitat, Patch_size), na.rm = T)) %>%
      rename( "cp_appl" = "cp") 
    
    return(Agricultural_biodiversity)
  } else {
    cat("Ups... something went wrong. Please check if your config.yml file contains the values Biodiversity or Agrobiodiversity for the Indicator!")
  }
  
  
}


#-------------------------------------------------------------------------------
# 2.7 Exposure to Pesticides
#-------------------------------------------------------------------------------

step2_pesticides <- function(){
  ## Organic Pesticides
  
  co <- data_co %>%
    rename(key = '_parent_index') %>%
    mutate(coused1 = ifelse(is.na(coused1),NA,
                            ifelse(is.na(comeas1),coused1,
                                   ifelse(comeas1 %in% c('Grams', 'g'), coused1*0.001, coused1))))
  
  co <- co %>%
    group_by(key) %>%
    mutate(coused1 = sum(coused1, na.rm = T)) %>% 
    ungroup() %>%
    select(key, coused1) %>%
    distinct()
  
  ## Chemical Pesticides
  
  cp <- data_cp %>%
    rename(key = '_parent_index') %>% # Is human toxicity
    mutate(cpused = ifelse(cpmeas %in% c('Grams', 'g'), cpused*0.001, cpused)) %>% 
    mutate(tox1 = ifelse(cptox == 1,1,0),
           tox2 = ifelse(cptox == 2,1,0),
           tox3 = ifelse(cptox == 3,1,0))
  cp <- cp %>%
    group_by(key) %>%
    mutate(cpused = sum(cpused, na.rm = T),
           tox1 = ifelse(!is.na(tox1), max(tox1, na.rm = T), NA),
           tox2 = ifelse(!is.na(tox2), max(tox2, na.rm = T), NA),
           tox3 = ifelse(!is.na(tox3), max(tox3, na.rm = T), NA)) %>% 
    select(key, cpused, tox1, tox2, tox3) %>% distinct()
  
  ## Number of mitigation strategies
  mitigation <- data %>%
    select(starts_with('mitig')) %>%
    select(!c('mitig')) %>%
    apply(1,function(x) sum(x,na.rm = T)) %>%
    data.frame() %>%
    rename(mitigation = '.')
  
  ecoman_num <- data %>%
    select(starts_with('ecoman')) %>%
    select(!c('ecoman')) %>%
    apply(1,function(x) sum(x,na.rm = T)) %>%
    data.frame() %>%
    rename(ecoman_num = ".")
  
  ## Combining
  Pesticides <- data %>%
    select('key', 'opestnum') %>%
    left_join(co, by = 'key') %>%
    left_join(cp, by = 'key')
  
  Pesticides <- cbind(Pesticides, mitigation, ecoman_num)
  rm(co, cp, mitigation, ecoman_num)
  
  
  # New pest score proposed by Anina (4 scores + overall score as mean of the 4 scores)
  Pesticides <- Pesticides %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    # Score 1: Organic vs. synthetic pesticide use
    mutate(pest_use = case_when(cpused > 0 & coused1 == 0 ~ 0,
                                cpused > 0 & coused1 > 0 ~ 50,
                                coused1 > cpused & cpused == 0 ~ 100)) %>%
    # Score 2: Toxicity
    mutate(pest_tox = case_when(tox1 == 1 ~ 0,
                                tox2 == 1 & tox1 == 0 ~ 50,
                                tox3 == 1 & tox2 == 0 & tox1 == 0 ~ 100,
                                tox3 == 0 & tox2 == 0 & tox1 == 0 ~ 100)) %>%
    # Score 3: Number of mitigation measures
    mutate(pest_mitig = case_when(mitigation == 0 ~ 0,
                                  mitigation == 1 ~ 25,
                                  mitigation == 2 ~ 50,
                                  mitigation == 3 ~ 75,
                                  mitigation >= 4 ~ 100)) %>%
    # Score 4: Number of ecological management measures
    mutate(pest_ecoman = case_when(ecoman_num == 0 ~ 0,
                                   ecoman_num == 1 ~ 25,
                                   ecoman_num == 2 ~ 50,
                                   ecoman_num == 3 ~ 75,
                                   ecoman_num >= 4 ~ 100)) %>%
    # Overall pest-score
    mutate(pest_score = case_when(cpused == 0 & coused1 == 0 ~ 100, # no pesticides used --> 100 % even if no ecoman and mitigation!
                                  TRUE ~ (pest_use + pest_tox + pest_mitig + pest_ecoman)/4))
  return(Pesticides)
}

#-------------------------------------------------------------------------------
# 2.8 Productivity - Income - Added Value
#-------------------------------------------------------------------------------

step2_economy <- function(){
  
  SNH <- data_snh %>%
    rename(key = '_parent_index') %>%
    mutate(SNH_area_ha = ifelse(is.na(sizeha_snh), length_snh*width_snh*0.0001,sizeha_snh)) %>%
    group_by(key) %>%
    summarise_at(vars(SNH_area_ha), sum, na.rm = T)
  
  
  
  Area_total <- data[, c('key', 'area', 'area_g')] %>%
    left_join(SNH, by = 'key') %>%
    rowwise() %>%
    mutate(area_total = sum(area, area_g, SNH_area_ha, na.rm = T))
  
  
  ## Family of workers
  data_workers <- data[,c('key', 'ag_men', 'ag_women', 'ag_myoung', 'ag_fyoung', 'ag_children')]
  data_workers <- data_workers %>%
    left_join(Area_total, by = 'key') %>%
    mutate(nag_children = ag_children/2) %>%
    rowwise() %>%
    mutate(fam_workers = sum(c(ag_men, ag_women, ag_myoung, ag_fyoung, nag_children), na.rm = T))
  
  # External workers
  ext_workers <- data_ext_workers %>%
    rename(key = '_parent_index')
  
  ext_workers <- ext_workers %>%
    select(key, days_ext, wage) %>%
    group_by(key) %>%
    mutate(days_ext = sum(days_ext, na.rm = T),
           wage = sum(wage, na.rm = T)) %>% 
    ungroup() %>%
    mutate(ext_workers = days_ext/260) %>% # on a basis of 5 days worked per week
    distinct()
  
  data_workers <- data_workers %>%
    left_join(ext_workers, by = 'key') %>%
    rowwise() %>%
    mutate(num_workers = sum(c(fam_workers, ext_workers), na.rm = T))
  
  
  ## Crop productivity
  c1 <- data_c1 %>%
    rename(key = '_parent_index')
  
  # Make a check for price at the gate (can't be 0 or too low)
  cpg <- c1 %>%
    select(c('cname_label', 'other_crop', 'cpg')) %>%
    mutate(cname_label = ifelse(cname_label == 'Other', other_crop, cname_label)) %>%
    group_by(cname_label) %>%
    summarise_at(vars(cpg), median, na.rm = T) %>%
    rename(cpg_median = cpg)
  
  c1 <- c1 %>%
    left_join(cpg, by = 'cname_label') %>%
    # corrections for cpg if = 0 or NA or too low
    mutate(cpg_corr = ifelse(cpg == 0, cpg_median, cpg)) %>% 
    mutate(cpg_corr = ifelse(is.na(cpg), cpg_median, cpg)) %>%
    mutate(cpg_corr = ifelse(cpg/cpg_median < 0.1, cpg_median, cpg)) %>%
    mutate(cpg_corr = ifelse(cpg/cpg_median > 10, cpg_median, cpg)) %>%
    mutate(cpg_corr = ifelse(is.na(cpg_corr), cpg, cpg_corr)) 
  rm(cpg)
  
  c1 <- c1 %>%
    mutate(crop_prod = cprod * cpg_corr,
           crop_sales = cqsold * cpg_corr) %>%
    group_by(key) %>%
    mutate(crop_prod = sum(crop_prod, na.rm = TRUE),
           crop_sales = sum(crop_sales, na.rm = TRUE)) %>%
    select(key,crop_prod,crop_sales) %>% 
    distinct()
  
  ## Crops and forest products productivity
  cfp <- data_cfp %>%
    rename(key = '_parent_index') 
  
  cfp <- cfp %>%
    mutate(cfp_prod = cfpprod * cfppg,
           cfp_sales = cfpqsold * cfppg) %>%
    group_by(key) %>%
    mutate(cfp_prod = sum(cfp_prod, na.rm = T),
           cfp_sales = sum(cfp_sales, na.rm = T)) %>%
    select(key, cfp_prod, cfp_sales) %>%
    distinct()
  
  
  ## Animal productivity
  a1 <- data_a1 %>%
    rename(key = '_parent_index')
  # Make a check for price at the gate (can't be 0 or too low)
  apg <- a1 %>%
    select(c('aname', 'aname_other', 'apg')) %>%
    mutate(aname = ifelse(aname == 'Other', aname_other, aname)) %>%
    group_by(aname) %>%
    summarise_at(vars(apg), median, na.rm = T) %>%
    rename(apg_median = apg)
  
  a1 <- a1 %>%
    left_join(apg, by = 'aname') %>%
    # corrections for cpg if = 0 or NA or too low
    mutate(apg_corr = ifelse(apg == 0, apg_median, apg)) %>% 
    mutate(apg_corr = ifelse(is.na(apg), apg_median, apg)) %>%
    mutate(apg_corr = ifelse(apg/apg_median < 0.1, apg_median, apg)) %>%
    mutate(apg_corr = ifelse(apg/apg_median > 10, apg_median, apg)) %>%
    mutate(apg_corr = ifelse(is.na(apg_corr), apg, apg_corr)) 
  rm(apg)
  
  a1 <- a1 %>%
    mutate(anim_change = (aborn - adied) * apg_corr,
           anim_sales = aqsold * apg_corr)
  
  a1 <- a1 %>%
    group_by(key) %>%
    mutate(anim_change = sum(anim_change, na.rm = T),
           anim_sales = sum(anim_sales, na.rm = T)) %>%
    select(key, anim_change, anim_sales) %>%
    distinct()
  
  a1 <- a1 %>%
    left_join(data[, c('key', 'livexp')], by = 'key') %>%
    rowwise() %>%
    mutate(anim_prod = sum(c(anim_sales, -livexp, anim_change), na.rm = T))
  
  ## Animal Products Productivity
  ap <- data_ap %>%
    rename(key = '_parent_index') 
  
  # Make a check for price at the gate (can't be 0 or too low)
  appg <- ap %>%
    select(c('apname', 'apname_other', 'appg')) %>%
    mutate(apname = ifelse(apname == 'Other', apname_other, apname)) %>%
    group_by(apname) %>%
    summarise_at(vars(appg), median, na.rm = T) %>%
    rename(appg_median = appg)
  
  ap <- ap %>%
    left_join(appg, by = 'apname') %>%
    # corrections for cpg if = 0 or NA or too low
    mutate(appg_corr = ifelse(appg == 0, appg_median, appg)) %>% 
    mutate(appg_corr = ifelse(is.na(appg), appg_median, appg)) %>%
    mutate(appg_corr = ifelse(appg/appg_median < 0.1, appg_median, appg)) %>%
    mutate(appg_corr = ifelse(appg/appg_median > 10, appg_median, appg)) %>%
    mutate(appg_corr = ifelse(is.na(appg_corr), appg, appg_corr)) 
  rm(appg)
  
  ap <- ap %>%
    mutate(anpr_prod = approd * appg_corr,
           anpr_sales = apqsold * appg_corr) 
  
  ap <- ap %>%
    group_by(key) %>%
    mutate(anpr_prod = sum(anpr_prod, na.rm = TRUE),
           anpr_sales = sum(anpr_sales, na.rm = TRUE)) %>%
    select(key, anpr_prod, anpr_sales) %>% 
    distinct()
  
  Economy <- c1 %>%
    left_join(cfp, by = 'key') %>%
    left_join(a1, by = 'key') %>%
    left_join(ap, by = 'key') %>%
    rowwise() %>%
    mutate(total_output = sum(c(crop_prod, cfp_prod, anim_prod, anpr_prod), na.rm = T))
  rm(c1, a1, ap)
  
  Economy <- Economy %>%
    left_join(data_workers, by = 'key') %>%
    mutate(productivity_ha = total_output/area_total, 
           productivity_pers = total_output/num_workers)
  
  ## Subsidies, Cost of Inputs, Taxes, interest on loans, cost of renting land
  Expenditures <- data %>%
    select(c('key', 'subs', 'taxes', 'inter', 'rentcost'), ends_with('exp')) %>%
    select(!c('foodexp'))
  
  ## Cost of hired labor
  hired_labor <- data_ext_workers %>%
    rename(key = '_parent_index') %>%
    select(c('key', 'days_ext', 'wage')) %>%
    group_by(key) %>%
    summarise_at(vars(days_ext, wage), sum, na.rm = T)
  
  Expenditures <- Expenditures %>%
    left_join(hired_labor, by = 'key')
  
  ## Depreciation
  machine <- data_m %>%
    rename(key = '_parent_index')
  
  machine <- machine %>%
    mutate(init_v = mowned * mprice,
           res_v = (init_v/100)*10,
           depreciation = (init_v - res_v)/((myused + myplan) * (myused + myplan + 1)/2) * myplan)
  
  machine <- machine %>%
    group_by(key) %>%
    mutate(depreciation = sum(depreciation, na.rm = T)) %>%
    select(key, depreciation) %>%
    distinct()
  
  Expenditures <- Expenditures %>%
    left_join(machine, by = 'key') %>%
    mutate(depreciation = ifelse(is.na(depreciation), 0, depreciation))
  
  
  Economy <- Economy %>%
    rowwise() %>%
    mutate(farm_revenue = sum(crop_sales, cfp_sales, anim_sales, anpr_sales, na.rm = T)) 
  
  Expenditures <- Expenditures %>%
    left_join(Economy[,c('key', 'farm_revenue')], by = 'key') %>%
    mutate(cost_of_inputs = rowSums(select(.,ends_with('exp')), na.rm = T)) %>%
    replace(is.na(.), 0) %>%
    mutate(Income = farm_revenue + subs - cost_of_inputs - wage - inter - taxes - rentcost - depreciation)
  
  Economy <- Economy %>%
    left_join(Expenditures, by = c('key','days_ext','farm_revenue','livexp','wage'))
  
  food_expenses <- data %>%
    select(c('key', 'foodexp', 'hh_men', 'hh_women', 'hh_myoung', 'hh_fyoung', 'hh_children')) %>%
    mutate_all(., as.numeric) %>%
    mutate(nhh_children = hh_children/2) %>%
    rowwise() %>%
    mutate(hh_adults = sum(c(hh_men, hh_women, hh_myoung, hh_fyoung, nhh_children), na.rm = T)) %>%
    mutate(food_exp_capita = foodexp/hh_adults)
  
  Economy <- Economy %>%
    left_join(food_expenses[c('food_exp_capita', 'foodexp','key')], by = 'key') 
  
  Economy <- Economy %>%
    mutate(ValueAdded = total_output - cost_of_inputs - depreciation )
  
  #scaling
  median_Productivity <- median(Economy$total_output)
  median_Income <- median(Economy$Income)
  median_ValueAdded <- median(Economy$ValueAdded)
  
  Economy <- Economy %>%
    rowwise() %>%
    mutate(Productivity_scaled = ((total_output - median_Productivity)/ (2 * max(Economy$total_output - median_Productivity)) + 0.5)*100) %>%
    mutate(Income_scaled = ((Income - median_Income) / (2 * max(Economy$Income - median_Income)) + 0.5)*100) %>%
    mutate( ValueAdded_scaled = ((ValueAdded - median_ValueAdded) / (2 * max(Economy$ValueAdded - median_ValueAdded)) + 0.5)*100) 
  
  
  
  
  
  
  
  
  Economy <- Economy %>%
    select(!c('ag_men', 'ag_women', 'ag_myoung', 'ag_fyoung', 'ag_children', 'area', 'area_g', 'SNH_area_ha', 'nag_children', 
              'fam_workers', 'days_ext' , 'seedsexp', 'fertexp', 'feedexp', 'vetexp', 'machexp', 'fuelexp', 'enerexp', 'transpexp', 'cp_exp', 'op_exp'))
  
  return(Economy)
}

#===============================================================================
# Generate Output Excel-Table
#===============================================================================

all_TAPE <- function(){
  
  ###########################################
  # call functions and calculate everything
  ###########################################
  
  CAET <- caet_CAET()
  DietaryDiv <- step2_Dietary_Diversity()
  SoilHealth <- step2_SoilHealth()
  Landtenure <- step2_LandTenure()
  WomenEmpowerment <- step2_WomensEmpowerment()
  YouthScore <- step2_YouthEmploymentEmigration()
  Agricultural_biodiversity <- step2_Agricultural_Biodiversity()
  Pesticides <- step2_pesticides()
  Economy <- step2_economy()
  
  ########################################
  # format
  #######################################
  
  ## Location
  TAPE_final <- data %>%
    select(c('key','country', 'location1', 'location2', 'sys_name'))
  
  ## Characterization of Agroecological transition
  TAPE_final <- TAPE_final %>%
    left_join(CAET[,c('key', 'caet_tot', 'div_score', 'syn_score', 'eff_score', 'rec_score', 'res_score', 'cultf_score', 'cocr_score', 'human_score', 'circ_score', 'respg_score')], by = 'key' )
  
  
  ## Dietary diversity
  TAPE_final <- TAPE_final %>%
    left_join(DietaryDiv[,c('key', 'dietary_diversity')], by = 'key') 
  
  ## Food Insecurity Experience scale
  TAPE_final <- TAPE_final %>%
    left_join(Economy[,c('key', 'food_exp_capita')], by = 'key')
  
  ## Soil Health
  TAPE_final <- TAPE_final %>%
    left_join(SoilHealth[, c('key', 'soil_health')], by = 'key')
  
  ## Pesticide, Land tenure score 
  TAPE_final <- TAPE_final %>%
    left_join(Landtenure[, c('key','landtenure')], by = 'key') %>%
    left_join(Pesticides[, c('key', 'pest_score')], by = 'key')
  
  ## Agrobiodiversity
  if (cfg$Indicator == "Biodiversity") {
    TAPE_final <- TAPE_final %>%
      left_join(Agricultural_biodiversity[c('key','Biodiversity_indicator','Agricultural_diversity', 'LUC_SNH', 'Grazing_intensity', 'AvStock', 'FieldOP', 'cp_ind', 'N_application', 'Semi_natural_Habitats', 'Tree_habitat', 'Patch_size')], by = 'key')
  }else if (cfg$Indicator == "Agrobiodiversity") {
    TAPE_final <- TAPE_final %>%
      left_join(Agricultural_biodiversity[c( 'key' ,'Agricultural_diversity_tape','GSIndex_crops', 'GSIndex_animals', 'GSI_other')], by = 'key')
  }
  
  ## Economic Indicators
  TAPE_final <- TAPE_final %>%
    left_join(Economy[, c('key','total_output', 'Income' , 'ValueAdded','productivity_ha','productivity_pers','ValueAdded_scaled','Productivity_scaled','Income_scaled')], by = 'key') 
  
  
  ## Youth
  TAPE_final <- TAPE_final %>%
    left_join(YouthScore, by = 'key')
  
  ## Women's Empowerment
  TAPE_final <- TAPE_final %>%
    left_join(WomenEmpowerment[,c('key', 'prodec_women', 'cred_decmak_women', 'decinc_women', 'leadership_score_women', 'timeuse_women', 'wemp_score', 'GPI')], by = 'key')
  
  
  return(TAPE_final)
}


########################################################################################################
# transform to excel
###########################################################################################################

TAPE_excel <- function(){
  TAPE <- all_TAPE()
  if (cfg$Indicator == "Biodiversity") {
    TAPE_final_Excel <- TAPE %>%
      rename("Region, Province" = "location1",
             "Municipality, District" = "location2",
             "Name of the system" = "sys_name",
             CAET = "caet_tot",
             Diversity = "div_score",
             Synergy = "syn_score",
             Efficiency = "eff_score",
             Recycling = "rec_score",
             Resilience = "res_score",
             'Culture and food traditions' = "cultf_score",
             'Co-creation and sharing of knowledge' = "cocr_score",
             'Human and social values' = "human_score",
             'Circular and solidarity economy' = "circ_score",
             'Responsible Governance' = "respg_score",
             'Dietary Diversity' = "dietary_diversity",
             'Food expenses per person' = "food_exp_capita",
             'Soil Health' = "soil_health",
             Landtenure = "landtenure",
             'Exposure to Pesticides' = "pest_score",
             'Biodiversity Indicator' = "Biodiversity_indicator",
             'Agricultural Biodiversity' = "Agricultural_diversity",
             'Land use change' = "LUC_SNH",
             'Grazing intensity' = "Grazing_intensity",
             'Stocking rate' = "AvStock",
             'Field operations' = "FieldOP",
             'Pesticide application' = "cp_ind",
             'Nitrogen application' = "N_application",
             'Semi natural Habitats' = "Semi_natural_Habitats",
             'Tree habitat' = "Tree_habitat",
             'Patch size' = "Patch_size",
             Production = "total_output",
             'Value Added' = "ValueAdded",
             'Productivity per hectare' = "productivity_ha",
             'Productivity per person' = "productivity_pers",
             'Youth Employment' = "youth_employ",
             'Youth Emigration' = "youth_emigr",
             'Youth Empowerment' = "youth_score",
             'Productive Decisions' = "prodec_women",
             'Decision Making' = "cred_decmak_women",
             'Income Use' = "decinc_women",
             'Leadership' = "leadership_score_women",
             'Time use' = "timeuse_women",
             'Productivity scaled' = 'Productivity_scaled' ,
             'Income scaled' = 'Income_scaled'  ,
             'Value added scaled' = 'ValueAdded_scaled',
             'Womens Empowerment' = "wemp_score")  }else if (cfg$Indicator == "Agrobiodiversity") {
               TAPE_final_Excel <- TAPE %>%
                 rename("Region, Province" = "location1",
                        "Municipality, District" = "location2",
                        "Name of the system" = "sys_name",
                        CAET = "caet_tot",
                        Diversity = "div_score",
                        Synergy = "syn_score",
                        Efficiency = "eff_score",
                        Recycling = "rec_score",
                        Resilience = "res_score",
                        'Culture and food traditions' = "cultf_score",
                        'Co-creation and sharing of knowledge' = "cocr_score",
                        'Human and social values' = "human_score",
                        'Circular and solidarity economy' = "circ_score",
                        'Responsible Governance' = "respg_score",
                        'Dietary Diversity' = "dietary_diversity",
                        'Food expenses per person' = "food_exp_capita",
                        'Soil Health' = "soil_health",
                        Landtenure = "landtenure",
                        'Exposure to Pesticides' = "pest_score",
                        'Biodiversity Indicator' = "Agricultural_diversity_tape",
                        'GSI crops' = "GSIndex_crops",
                        'GSI animals' = "GSIndex_animals",
                        'GSI other' = "GSI_other",
                        Production = "total_output",
                        'Value Added' = "ValueAdded",
                        'Productivity per hectare' = "productivity_ha",
                        'Productivity per person' = "productivity_pers",
                        'Youth Employment' = "youth_employ",
                        'Youth Emigration' = "youth_emigr",
                        'Youth Empowerment' = "youth_score",
                        'Productive Decisions' = "prodec_women",
                        'Decision Making' = "cred_decmak_women",
                        'Income Use' = "decinc_women",
                        'Leadership' = "leadership_score_women",
                        'Time use' = "timeuse_women",
                        'Womens Empowerment' = "wemp_score",
                        'Productivity scaled' = 'Productivity_scaled' ,
                        'Income scaled' = 'Income_scaled'  ,
                        'Value added scaled' = 'ValueAdded_scaled')
               
             }
  return(TAPE_final_Excel)
}




