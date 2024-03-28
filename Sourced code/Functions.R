
#########################################
# functions TAPE
# Stevan Paunovic
# 08082023


##############################################################################
#---------------------calculate CAET // Step 1-------------------------------#
##############################################################################


#function for calculating Diversity for CAET

caet_diversity <- function(){
  Diversity <- data[,c('key', 'crops', 'animals', 'trees', 'div_activ')]
  Diversity <- Diversity %>%
    rowwise() %>%  #cleans data so there only is one number for crops animals trees ...
    mutate(div_sum = sum(c(crops, animals, trees, div_activ), na.rm = TRUE),
           div_aver = mean(c(crops ,animals ,trees ,div_activ),na.rm = TRUE),  # Is later used for Resilience
           div_score = round((div_sum/16)*100,6)) # calculates score
  return(Diversity)
}


#function for calculating Synergies for CAET

caet_synergies <- function(){
  Synergies <- data[,c('key', 'cla_int', 's_plant', 'tree_int', 'connectivity')]
  Synergies <- Synergies %>% #creates dataframe to calculate indicator
    rowwise() %>%
    mutate(syn_sum = sum(c(cla_int ,s_plant ,tree_int ,connectivity),na.rm = TRUE),
           syn_score = round((syn_sum/16)*100,6))
  return(Synergies)
}


#function for calculating Efficiency for CAET

caet_efficiency <- function(){
  Efficiency <- data[,c('key', 'ext_inp', 'soil_fert', 'pest_dis', 'emergingefficiency')]
  Efficiency <- Efficiency %>%
    rowwise() %>%
    mutate(eff_sum = sum(c(ext_inp, soil_fert, pest_dis, emergingefficiency), na.rm = TRUE),
           eff_score = round((eff_sum/16)*100,6))
  return(Efficiency)
}


#function for calculating Recycling for CAET 

caet_recycling <- function(){
  Recycling <- data[,c('key', 'rec_biomass', 'waste', 'water', 'ren_energy')]
  Recycling <- Recycling %>%
    rowwise() %>%
    mutate(rec_sum = sum(c(rec_biomass, waste, water, ren_energy),na.rm = TRUE),
           rec_score = round((rec_sum/16)*100,6))
  return(Recycling)
}

#function for calculating Resilience for CAET

caet_resilience <- function(){
  Resilience <- data[,c('key', 'crops', 'animals', 'trees', 'div_activ', 'ext_inp', 'seeds_breeds', 'local_fs', 'prod_empow', 'vuln', 'indebt')]
  Resilience <- Resilience %>%
    rowwise() %>%
    mutate(div_aver = mean(c(crops, animals, trees, div_activ), na.rm = TRUE), # emerging resilience from diversity
           aver_suff_emp = mean(c(ext_inp, seeds_breeds, local_fs, prod_empow), na.rm = T), # average score of the indices measuring self-sufficiency and empowerment
           res_sum = sum(c(vuln, indebt, div_aver, aver_suff_emp), na.rm = TRUE),
           res_score = round((res_sum/16)*100,6))
  return(Resilience)
}

#function for calculating Culture and food traditions for CAET

caet_culture <- function(){
  Culture <- data[,c('key', 'diet', 'food_self_suff', 'food_heritage', 'seeds_breeds')]
  Culture <- Culture %>%
    rowwise() %>%
    mutate(cultf_sum = sum(c(diet, food_self_suff, food_heritage, seeds_breeds),na.rm = TRUE),
           cultf_score = round((cultf_sum/12)*100,6))
  return(Culture)
}

#function for calculating Co-creation for CAET

caet_cocreation <- function(){
  Cocreation <- data[,c('key', 'platforms', 'ae_know', 'partic_orgs')]
  Cocreation <- Cocreation %>%
    rowwise() %>%
    mutate(cocr_sum = sum(c(platforms, ae_know, partic_orgs),na.rm = TRUE),
           cocr_score = round((cocr_sum/12)*100,6))
  return(Cocreation)
}

#function for calculating Human & Social values for CAET

caet_humanvalues <- function(){
  Humanvalues <- data[,c('key', 'women', 'labour', 'youth', 'animalwel')]
  Humanvalues <- Humanvalues %>%
    rowwise() %>%
    mutate(human_sum = sum(c(women, labour, youth, animalwel),na.rm = TRUE),
           human_score = round((human_sum/16)*100,6),
           human_score = ifelse(is.na(animalwel),round((human_sum/12)*100,6),human_score))
  return(Humanvalues)
}

#function for calculating Circular and solidarity economy values for CAET

caet_circular <- function(){
  Circular <- data[,c('key', 'mkt_local', 'networks', 'local_fs')]
  Circular <- Circular %>%
    rowwise() %>%
    mutate(circ_sum = sum(c(mkt_local, networks, local_fs),na.rm = TRUE),
           circ_score = round((circ_sum/12)*100,6))
  return(Circular)
}

#function for calculating Responsible Governance values for CAET

caet_governance <- function(){
  Governance <- data[,c('key', 'prod_empow', 'prod_orgs', 'partic_prod')]
  Governance <- Governance %>%
    rowwise() %>%
    mutate(respg_sum = sum(c(prod_empow, prod_orgs, partic_prod),na.rm = TRUE),
           respg_score = round((respg_sum/12)*100,6))
  return(Governance)
}

#function for calculating all of the CAET values

caet_CAET <- function(){
  
  Diversity <- caet_diversity()
  Synergies <- caet_synergies()
  Efficiency <- caet_efficiency()
  Recycling <- caet_recycling()
  Resilience <- caet_resilience()
  Culture <- caet_culture()
  Cocreation <- caet_cocreation()
  Humanvalues <- caet_humanvalues()
  Circular <- caet_circular()
  Governance <- caet_governance()
  
  CAET <- Diversity %>%
    select(key, div_score) %>%
    left_join(Synergies[,c('key', 'syn_score')], by = 'key') %>%
    left_join(Efficiency[,c('key', 'eff_score')], by = 'key') %>%
    left_join(Recycling[,c('key', 'rec_score')], by = 'key') %>%
    left_join(Resilience[,c('key', 'res_score')], by = 'key') %>%
    left_join(Culture[,c('key', 'cultf_score')], by = 'key') %>%
    left_join(Cocreation[,c('key', 'cocr_score')], by = 'key') %>%
    left_join(Humanvalues[,c('key', 'human_score')], by = 'key') %>%
    left_join(Circular[,c('key', 'circ_score')], by = 'key') %>%
    left_join(Governance[,c('key', 'respg_score')], by = 'key') %>%
    mutate(caet_tot = mean(c(div_score,syn_score,eff_score,rec_score,res_score,cultf_score,cocr_score,human_score,circ_score,respg_score), na.rm = TRUE))
  
  return(CAET)
}

##############################################################################
#--------------------------------Step 2--------------------------------------#
##############################################################################

#-------------------------------------------------------------------------------
# 2.1 Dietary Diversity function
#-------------------------------------------------------------------------------

step2_Dietary_Diversity <- function(){
  DietaryDiv <- data[,c('key', 'grains_a', 'grains_b', 'pulses', 'nuts', 'dairy_e', 'dairy_f', 'meat_h', 'meat_i', 'meat_j', 'meat_k', 'meat_l',
                        'eggs', 'darkgreen', 'darkyellow_n', 'darkyellow_o', 'otherveg', 'otherfruit', 'fried_salty_1', 'fried_salty_2', 'fried_salty_3', 'fried_salty_4',
                        'sweet_foods', 'sweet_beverages_1', 'sweet_beverages_2')]
  DietaryDiv <- DietaryDiv %>%
    rowwise() %>%
    mutate(grains = ifelse(grains_a == 1 | grains_b == 1, 1, 0),
           dairy = ifelse(dairy_e == 1 | dairy_f == 1, 1, 0),
           meat = ifelse(meat_h == 1 | meat_i == 1 | meat_j == 1 | meat_k == 1 | meat_l == 1, 1, 0),
           darkyellow = ifelse(darkyellow_n == 1 | darkyellow_o == 1, 1, 0),
           fried_salty = ifelse(fried_salty_1 == 1 | fried_salty_2 == 1 | fried_salty_3 == 1 | fried_salty_4 == 1, 1, 0), # is not included for dietary diversity score, get's calculated anyways.
           sweet_beverages = ifelse(sweet_beverages_1 == 1 |  sweet_beverages_2 == 1, 1, 0)) %>% # is not included for dietary diversity score, get's calculated anyways.
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
  Landtenure <- data[,c('key', 'recland_men', 'doc_men', 'doc_menoth', 'name_men', 'ltperc_men', 'sell_men', 'beq_men', 'inh_men', 'recland_women', 'doc_women', 'doc_womenoth', 'name_women', 'ltperc_women', 'sell_women', 'beq_women', 'inh_women')]
  
  Landtenure <- Landtenure %>%
    rowwise() %>%
    mutate(tmp_men = sum(c(sell_men, beq_men, inh_men), na.rm = T)) %>%
    mutate(landtenure_men = 0) %>%
    mutate(landtenure_men = case_when(recland_men == 1 & name_men == 1 & ltperc_men == 1 & tmp_men > 0 ~ 100,
                                      recland_men == 1 & name_men == 0 ~ 50,
                                      recland_men == 0 & ltperc_men == 1 & tmp_men > 0 ~ 50,
                                      recland_men == 1 & name_men == 1 & ltperc_men == 0 ~ 50,
                                      recland_men == 1 & name_men == 1 & ltperc_men != 1 & tmp_men > 0 ~ 50,
                                      recland_men == 0 & ltperc_men == 0 ~ 0,
                                      recland_men == 0 & tmp_men == 0 ~ 0)) %>%
    rowwise() %>%
    mutate(tmp_women = sum(c(sell_women, beq_women, inh_women), na.rm = T)) %>%
    mutate(landtenure_women = 0) %>%
    mutate(landtenure_women = case_when(recland_women == 1 & name_women == 1 & ltperc_women == 1 & tmp_women > 0 ~ 100,
                                        recland_women == 1 & name_women == 0 ~ 50,
                                        recland_women == 0 & ltperc_women == 1 & tmp_women > 0 ~ 50,
                                        recland_women == 1 & name_women == 1 & ltperc_women == 0 ~ 50,
                                        recland_women == 1 & name_women == 1 & ltperc_women != 1 & tmp_women > 0 ~ 50,
                                        recland_women == 0 & ltperc_women == 0 ~ 0,
                                        recland_women == 0 & tmp_women == 0 ~ 0))
  Landtenure <- Landtenure %>%
    mutate(landtenure = mean(c(landtenure_women, landtenure_men), na.rm = T))
  
  return(Landtenure)
}

#-------------------------------------------------------------------------------
# 2.4 Women Empowerment
#-------------------------------------------------------------------------------

step2_WomensEmpowerment <- function(){
  ProductiveDecision <- data[, c('key', 'deccrop', 'decanim', 'decotact', # About crops, animals, and other economic activities
                                 'decmajor')] # About Major and minor hh expenditures (codifico e poi media)
                    
  # 2.4.1 Decisions about crops, animals and other economic activities
  ProductiveDecision <- ProductiveDecision %>%
    mutate(prodec_wom_c = case_when(deccrop %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                    deccrop == 2 ~ 0.25, # mostly the man
                                    deccrop == 3 ~ 0.5, # both man and woman
                                    deccrop == 4 ~ 0.75, # mostly the woman
                                    deccrop == 5 ~ 1, # completely the woman
                                    deccrop == 7 ~ NA), 
           prodec_wom_a = case_when(decanim %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                    decanim == 2 ~ 0.25, # mostly the man
                                    decanim == 3 ~ 0.5, # both man and woman
                                    decanim == 4 ~ 0.75, # mostly the woman
                                    decanim == 5 ~ 1, # completely the woman
                                    decanim == 7 ~ NA), 
           prodec_wom_oa = case_when(decotact %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                     decotact == 2 ~ 0.25, # mostly the man
                                     decotact == 3 ~ 0.5, # both man and woman
                                     decotact == 4 ~ 0.75, # mostly the woman
                                     decotact == 5 ~ 1, # completely the woman
                                     decotact == 7 ~ NA),
           # For Men-Index
           prodec_men_c = case_when(deccrop == 1 ~ 1, # completely the man
                                    deccrop == 2 ~ 0.75, # mostly the man
                                    deccrop == 3 ~ 0.5, # both man and woman
                                    deccrop == 4 ~ 0.25, # mostly the woman
                                    deccrop %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                    deccrop == 7 ~ NA), 
           prodec_men_a = case_when(decanim == 1 ~ 1, # completely the man
                                    decanim == 2 ~ 0.75, # mostly the man
                                    decanim == 3 ~ 0.5, # both man and woman
                                    decanim == 4 ~ 0.25, # mostly the woman
                                    decanim %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                    decanim == 7 ~ NA),
           prodec_men_oa = case_when(decotact == 1 ~ 1, # completely the man
                                     decotact == 2 ~ 0.75, # mostly the man
                                     decotact == 3 ~ 0.5, # both man and woman
                                     decotact == 4 ~ 0.25, # mostly the woman
                                     decotact %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                     decotact == 7 ~ NA)) %>%
    rowwise() %>%
    mutate(prodec_wom_caoa = mean(c(prodec_wom_c, prodec_wom_a, prodec_wom_oa), na.rm = T)) %>%
    rowwise() %>%
    mutate(prodec_men_caoa = mean(c(prodec_men_c, prodec_men_a, prodec_men_oa), na.rm = T)) # For Men Index
  
  # Decisions household expenditures
  ProductiveDecision <- ProductiveDecision %>%
    mutate(prodec_wom_maj = case_when(decmajor %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                      decmajor == 2 ~ 0.25, # mostly the man
                                      decmajor == 3 ~ 0.5, # both man and woman
                                      decmajor == 4 ~ 0.75, # mostly the woman
                                      decmajor == 5 ~ 1, # completely the woman
                                      decmajor == 7 ~ NA),
           # For Men-Index
           prodec_men_maj = case_when(decmajor == 1 ~ 1, # completely the man
                                      decmajor == 2 ~ 0.75, # mostly the man
                                      decmajor == 3 ~ 0.5, # both man and woman
                                      decmajor == 4 ~ 0.25, # mostly the woman
                                      decmajor %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                      decmajor == 7 ~ NA)) %>%
    # Total Productive Decisions score:
    rowwise() %>%
    mutate(prodec_wom = mean(c(prodec_wom_caoa, prodec_wom_maj), na.rm = TRUE)) %>% #Total Productive Decisions score for Women
    rowwise() %>%
    mutate(prodec_men = mean(c(prodec_men_caoa, prodec_men_maj), na.rm = TRUE)) # Total Productive Decision score for Men
  
  ProductiveDecision <- rapply(ProductiveDecision, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  ## 2.4.2 Acces To and decision-making power over productive resources
  
  DecisionMaking <- data[,c('key', 'credit_women', 'credit_men',
                            'owcrop', 'owanim', 'owotact', 'owhouse')]
  
  Landtenure <- step2_LandTenure()
  
  DecisionMaking <- DecisionMaking %>%
    left_join(Landtenure[,c('key', 'landtenure_women', 'landtenure_men')], by = 'key') %>%
    mutate(landtenure_women = landtenure_women * 0.01) %>%
    mutate(landtenure_men = landtenure_men * 0.01)
  
  # Access to credit
  DecisionMaking <- DecisionMaking %>%
    mutate(credit_score_wom = case_when(credit_women == 1 ~ 1,
                                        credit_women == 2 & credit_men == 3 ~ 0.8,
                                        credit_women == 2 & credit_men == 2 ~ 0.75,
                                        credit_women == 2 & credit_men == 1 ~ 0.5,
                                        credit_women == 3 & credit_men == 3 ~ 0.25,
                                        credit_women == 3 & credit_men == 2 ~ 0.1,
                                        credit_women == 3 & credit_men == 1 ~ 0,
                                        credit_women == 2 & is.na(credit_men) ~ 0.5,
                                        credit_women == 3 & is.na(credit_men) ~ 0)) %>%
    # For Men-Index
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
    mutate(own_wom_c = case_when(owcrop %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                 owcrop == 2 ~ 0.25, # mostly the man
                                 owcrop == 3 ~ 0.5, # both man and woman
                                 owcrop == 4 ~ 0.75, # mostly the woman
                                 owcrop == 5 ~ 1, # completely the woman
                                 owcrop == 7 ~ NA),
           own_wom_a = case_when(owanim %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                 owanim == 2 ~ 0.25, # mostly the man
                                 owanim == 3 ~ 0.5, # both man and woman
                                 owanim == 4 ~ 0.75, # mostly the woman
                                 owanim == 5 ~ 1, # completely the woman
                                 owanim == 7 ~ NA),
           own_wom_oa = case_when(owotact %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                  owotact == 2 ~ 0.25, # mostly the man
                                  owotact == 3 ~ 0.5, # both man and woman
                                  owotact == 4 ~ 0.75, # mostly the woman
                                  owotact == 5 ~ 1, # completely the woman
                                  owotact == 7 ~ NA)) %>%
    # For Men-Index
    mutate(own_men_c = case_when(owcrop == 1 ~ 1, # completely the man
                                 owcrop == 2 ~ 0.75, # mostly the man
                                 owcrop == 3 ~ 0.5, # both man and woman
                                 owcrop == 4 ~ 0.25, # mostly the woman
                                 owcrop %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                 owcrop == 7 ~ NA),
           own_men_a = case_when(owanim == 1 ~ 1, # completely the man
                                 owanim == 2 ~ 0.75, # mostly the man
                                 owanim == 3 ~ 0.5, # both man and woman
                                 owanim == 4 ~ 0.25, # mostly the woman
                                 owanim %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                 owanim == 7 ~ NA),
           own_men_oa = case_when(owotact == 1 ~ 1, # completely the man
                                  owotact == 2 ~ 0.75, # mostly the man
                                  owotact == 3 ~ 0.5, # both man and woman
                                  owotact == 4 ~ 0.25, # mostly the woman
                                  owotact %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                  owotact == 7 ~ NA)) %>%
    rowwise() %>%
    mutate(own_wom_caoa = mean(c(own_wom_c, own_wom_a, own_wom_oa), na.rm = T)) %>%
    rowwise() %>%
    mutate(own_men_caoa = mean(c(own_men_c, own_men_a, own_men_oa), na.rm = T))
  
  # Ownership of major & minor hh assets
  DecisionMaking <- DecisionMaking %>%
    mutate(own_wom_maj = case_when(owhouse %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                   owhouse == 2 ~ 0.25, # mostly the man
                                   owhouse == 3 ~ 0.5, # both man and woman
                                   owhouse == 4 ~ 0.75, # mostly the woman
                                   owhouse == 5 ~ 1, # completely the woman
                                   owhouse == 7 ~ NA)) %>%
    # For Men-Index
    mutate(own_men_maj = case_when(owhouse == 1 ~ 1, # completely the man
                                   owhouse == 2 ~ 0.75, # mostly the man
                                   owhouse == 3 ~ 0.5, # both man and woman
                                   owhouse == 4 ~ 0.25, # mostly the woman
                                   owhouse %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                   owhouse == 7 ~ NA))
  
  # Total Access To and Decision-Making score
  DecisionMaking <- DecisionMaking %>%
    rowwise() %>%
    mutate(cred_decmak_wom = mean(c(landtenure_women, credit_score_wom, own_wom_caoa, own_wom_maj), na.rm = T)) %>%
    rowwise() %>%
    mutate(cred_decmak_men = mean(c(landtenure_men, credit_score_men, own_men_caoa, own_men_maj), na.rm = T))
  DecisionMaking <- rapply(DecisionMaking, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  ## 2.4.3 Use of Income
  IncomeUse <- data[, c('key', 'dec_rev_crop', 'dec_rev_anim', 'dec_rev_oth')] 
  
  IncomeUse <- IncomeUse %>%
    mutate(decinc_wom_c = case_when(dec_rev_crop %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                    dec_rev_crop == 2 ~ 0.25, # mostly the man
                                    dec_rev_crop == 3 ~ 0.5, # both man and woman
                                    dec_rev_crop == 4 ~ 0.75, # mostly the woman
                                    dec_rev_crop == 5 ~ 1, # completely the woman
                                    dec_rev_crop == 7 ~ NA), 
           decinc_wom_a = case_when(dec_rev_anim %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                    dec_rev_anim == 2 ~ 0.25, # mostly the man
                                    dec_rev_anim == 3 ~ 0.5, # both man and woman
                                    dec_rev_anim == 4 ~ 0.75, # mostly the woman
                                    dec_rev_anim == 5 ~ 1, # completely the woman
                                    dec_rev_anim == 7 ~ NA), 
           decinc_wom_oa = case_when(dec_rev_oth %in% c(1,6) ~ 0, # completely the man & someone else outside the family
                                     dec_rev_oth == 2 ~ 0.25, # mostly the man
                                     dec_rev_oth == 3 ~ 0.5, # both man and woman
                                     dec_rev_oth == 4 ~ 0.75, # mostly the woman
                                     dec_rev_oth == 5 ~ 1, # completely the woman
                                     dec_rev_oth == 7 ~ NA)) %>%
    # For Men-Index
    mutate(decinc_men_c = case_when(dec_rev_crop == 1 ~ 1, # completely the man
                                    dec_rev_crop == 2 ~ 0.75, # mostly the man
                                    dec_rev_crop == 3 ~ 0.5, # both man and woman
                                    dec_rev_crop == 4 ~ 0.25, # mostly the woman
                                    dec_rev_crop %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                    dec_rev_crop == 7 ~ NA), 
           decinc_men_a = case_when(dec_rev_anim == 1 ~ 1, # completely the man
                                    dec_rev_anim == 2 ~ 0.75, # mostly the man
                                    dec_rev_anim == 3 ~ 0.5, # both man and woman
                                    dec_rev_anim == 4 ~ 0.25, # mostly the woman
                                    dec_rev_anim %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                    dec_rev_anim == 7 ~ NA),
           decinc_men_oa = case_when(dec_rev_oth == 1 ~ 1, # completely the man
                                     dec_rev_oth == 2 ~ 0.75, # mostly the man
                                     dec_rev_oth == 3 ~ 0.5, # both man and woman
                                     dec_rev_oth == 4 ~ 0.25, # mostly the woman
                                     dec_rev_oth %in% c(5,6) ~ 0, # completely the woman & someone else outside the family
                                     dec_rev_oth == 7 ~ NA)) %>%
    rowwise() %>%
    mutate(decinc_wom = mean(c(decinc_wom_c, decinc_wom_a, decinc_wom_oa), na.rm = T)) %>%
    rowwise() %>%
    mutate(decinc_men = mean(c(decinc_men_c, decinc_men_a, decinc_men_oa), na.rm = T))
  IncomeUse <- rapply(IncomeUse, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  ## 2.4.4 Leadership in the community
  Leadership <- data[,c('key', 'involv_agri_wom', 'involv_othe_wom', 'involv_agri_men', 'involv_othe_men')]
  
  Leadership <- Leadership %>%
    mutate(lead_wom_agri = (involv_agri_wom - 1)*0.25,
           lead_wom_othe = (involv_othe_wom - 1)*0.25) %>%
    mutate(lead_men_agri = (involv_agri_men - 1)*0.25,
           lead_men_othe = (involv_othe_men - 1)*0.25) %>%
    rowwise() %>%
    mutate(leadership_score_wom = max(lead_wom_agri, lead_wom_othe, na.rm = T)) %>%
    rowwise() %>%
    mutate(leadership_score_men = max(lead_men_agri, lead_men_othe, na.rm = T))
  Leadership <- rapply(Leadership, f = function(x) ifelse(is.infinite(x), NA, x), how = "replace" )
  
  ## 2.4.5 Time use (average between the two scores)
  Timeuse <- data[,c('key', 'wtime_ag_women', 'wtime_dom_women', 'wtime_otgain_women', 'wtime_ag_men', 'wtime_dom_men', 'wtime_otgain_men')]
  
  # set 18hrs/day of work as maximun and if surpassed reduce all activities accordingly
  Timeuse <- Timeuse %>%
    rowwise() %>%
    mutate(hrs_wom = sum(c(wtime_ag_women + wtime_dom_women + wtime_otgain_women), na.rm = T),
           hrs_men = sum(c(wtime_ag_men + wtime_dom_men + wtime_otgain_men), na.rm = T)) %>%
    # calculate shares (which are used if total hours > 18)
    mutate(wtime_ag_women_share = wtime_ag_women/hrs_wom,
           wtime_dom_women_share = wtime_dom_women/hrs_wom,
           wtime_otgain_women_share = wtime_otgain_women/hrs_wom,
           wtime_ag_men_share = wtime_ag_men/hrs_men,
           wtime_dom_men_share = wtime_ag_men/hrs_men,
           wtime_otgain_men_share = wtime_otgain_men/hrs_men) %>%
    # replace if hours > 18
    mutate(wtime_ag_women = ifelse(hrs_wom > 18, wtime_ag_women_share * 18, wtime_ag_women),
           wtime_dom_women = ifelse(hrs_wom > 18, wtime_dom_women_share * 18, wtime_dom_women),
           wtime_otgain_women = ifelse(hrs_wom > 18, wtime_otgain_women_share * 18, wtime_otgain_women),
           wtime_ag_men = ifelse(hrs_men > 18, wtime_ag_men_share * 18, wtime_ag_men),
           wtime_dom_men = ifelse(hrs_men > 18, wtime_dom_men_share * 18, wtime_dom_men),
           wtime_otgain_men = ifelse(hrs_men > 18, wtime_otgain_men_share * 18, wtime_otgain_men),
           hrs_wom = ifelse(hrs_wom > 18, 18, hrs_wom),
           hrs_men = ifelse(hrs_men > 18, 18, hrs_men))
  
  # Total Time Use score:
  Timeuse <- Timeuse %>%
    mutate(timeuse_wom = 0.133*(18-hrs_wom),
           timeuse_men = 0.133*(18-hrs_men)) %>%
    mutate(timeuse_wom = ifelse(timeuse_wom > 1, 1, timeuse_wom),
           timeuse_men = ifelse(timeuse_men > 1, 1, timeuse_men)) 
  
  ## 2.4.6 Total Women Empowerment Score (5DE = Five-dimension empowerment index)
  WomenEmpowerment <- ProductiveDecision %>%
    select(c('key', 'prodec_wom', 'prodec_men')) %>%
    left_join(DecisionMaking[,c('key', 'cred_decmak_wom', 'cred_decmak_men')], by = 'key') %>%
    left_join(IncomeUse[,c('key', 'decinc_wom', 'decinc_men')], by = 'key') %>%
    left_join(Leadership[,c('key', 'leadership_score_wom', 'leadership_score_men')], by = 'key') %>%
    left_join(Timeuse[,c('key', 'timeuse_wom', 'timeuse_men')], by = 'key') %>%
    rowwise() %>%
    mutate(wemp_score = mean(c(prodec_wom, cred_decmak_wom, decinc_wom, leadership_score_wom, timeuse_wom), na.rm = T)*100) %>%
    rowwise() %>%
    mutate(memp_score = mean(c(prodec_men, cred_decmak_men, decinc_men, leadership_score_men, timeuse_men), na.rm = T)*100)
  WomenEmpowerment <- rapply(WomenEmpowerment, f = function(x) ifelse(is.nan(x), NA, x), how = "replace" )
  
  rm(ProductiveDecision, DecisionMaking, IncomeUse, Leadership, Timeuse)
  
  # Replace the result with missing value if there are no women in the household:
  WomenEmpowerment <- WomenEmpowerment %>% 
    left_join(data[,c('key', 'hh_fem' )], by = 'key') %>%
    mutate(wemp_score = ifelse(hh_fem == 0, NA, wemp_score))
  
  # implement GPI score (Gender Parity Index) & AWEAI score (Abbreviated Women's Empowerment in Agriculture Index)
  
  WomenEmpowerment <- WomenEmpowerment %>%
    mutate(GPI = wemp_score/memp_score * 100) %>%
    mutate(AWEAI = 0.9 * wemp_score + 0.1 * GPI)
  return(WomenEmpowerment)
}

#-------------------------------------------------------------------------------
# 2.5 Youth Employment and Emigration
#-------------------------------------------------------------------------------


step2_YouthEmploymentEmigration <- function(){
  # Male
  youth_emp_m <- data_Youth_Males %>%
    left_join(data[,c('key', 'hh_myoung', 'hh_fyoung')], by = 'key') %>%
    mutate(sex = 1) %>% # 1 for male
    rename(y_occup = y_occup_m,
           y_future_farm = y_future_farm_m, 
           y_keen_emigr = y_keen_emigr_m)
  
  # Female
  youth_emp_f <- data_Youth_Females %>%
    left_join(data[,c('key', 'hh_myoung', 'hh_fyoung')], by = 'key') %>%
    mutate(sex = 2) %>% # 2 for female
    rename(y_occup = y_occup_f,
           y_future_farm = y_future_farm_f, 
           y_keen_emigr = y_keen_emigr_f)
  
  # Combine male and female data
  youth_emp <- rbind(youth_emp_m, youth_emp_f) %>%
    mutate(ymembers = hh_myoung + hh_fyoung) 
  
  YouthScore <- youth_emp %>%
    # Employment
    mutate(y_good = ifelse(y_occup %in% c(1,4,5,6),1,0),
           y_med = ifelse(y_occup %in% c(2),1,0),
           y_bad = ifelse(y_occup %in% c(0,3),1,0)) %>%
    # Emigration
    mutate(g1 = ifelse(y_future_farm == 1 & y_keen_emigr == 0,1,0), 
           g2 = ifelse(y_future_farm == 0 & y_keen_emigr == 1,1,0),
           g3 = ifelse(y_future_farm == 1 & y_keen_emigr == 1,1,0),
           g4 = ifelse(y_future_farm == 0 & y_keen_emigr == 0,1,0)) %>%
    group_by(key) %>% # make sum per farm
    # Employment
    mutate(y_good_sum = sum(y_good, na.rm = T),
           y_med_sum = sum(y_med, na.rm = T),
           y_bad_sum = sum(y_bad, na.rm = T),
           ymembers = sum(c(y_good,y_bad,y_med), na.rm = T)) %>%
    # Emigration
    mutate(g1_sum = sum(g1, na.rm = TRUE),
           g2_sum = sum(g2, na.rm = TRUE),
           g3_sum = sum(g3, na.rm = TRUE),
           g4_sum = sum(g4, na.rm = TRUE)) %>%
    # Employment score
    mutate(y_good_perc = y_good_sum*100/ymembers,
           y_med_perc = y_med_sum*100/ymembers,
           y_bad_perc = y_bad_sum*100/ymembers) %>% 
    mutate(youth_employ = (y_good_perc + 0.5*y_med_perc)/1.5) %>%
    # Emigration score
    mutate(g1_perc = g1_sum/ymembers*100,
           g2_perc = g2_sum/ymembers*100,
           g3_perc = g3_sum/ymembers*100,
           g4_perc = g4_sum/ymembers*100) %>% 
    mutate(youth_emigr = (g1_perc + 0.5*g3_perc + 0.5*g4_perc)/2) %>%
    # Total youth score
    rowwise() %>%
    mutate(youth_score = mean(c(youth_employ , youth_emigr),na.rm = T)) %>%
    mutate(youth_score = ifelse(ymembers == 0,NA,youth_score)) %>%
    select(key, youth_employ, youth_emigr, youth_score) %>%
    distinct()
  
  ## Calculate Youth score by sex
  # Male
  YouthScore_male <- youth_emp_m %>%
    # Employment
    mutate(y_good = ifelse(y_occup %in% c(1,4,5,6),1,0),
           y_med = ifelse(y_occup %in% c(2),1,0),
           y_bad = ifelse(y_occup %in% c(0,3),1,0)) %>%
    # Emigration
    mutate(g1 = ifelse(y_future_farm == 1 & y_keen_emigr == 0,1,0), 
           g2 = ifelse(y_future_farm == 0 & y_keen_emigr == 1,1,0),
           g3 = ifelse(y_future_farm == 1 & y_keen_emigr == 1,1,0),
           g4 = ifelse(y_future_farm == 0 & y_keen_emigr == 0,1,0)) %>%
    group_by(key) %>% # make sum per farm
    # Employment
    mutate(y_good_sum = sum(y_good, na.rm = T),
           y_med_sum = sum(y_med, na.rm = T),
           y_bad_sum = sum(y_bad, na.rm = T),
           ymembers = sum(c(y_good,y_bad,y_med), na.rm = T)) %>%
    # Emigration
    mutate(g1_sum = sum(g1, na.rm = TRUE),
           g2_sum = sum(g2, na.rm = TRUE),
           g3_sum = sum(g3, na.rm = TRUE),
           g4_sum = sum(g4, na.rm = TRUE)) %>%
    # Employment score
    mutate(y_good_perc = y_good_sum*100/ymembers,
           y_med_perc = y_med_sum*100/ymembers,
           y_bad_perc = y_bad_sum*100/ymembers) %>% 
    mutate(youth_employ = (y_good_perc + 0.5*y_med_perc)/1.5) %>%
    # Emigration score
    mutate(g1_perc = g1_sum/ymembers*100,
           g2_perc = g2_sum/ymembers*100,
           g3_perc = g3_sum/ymembers*100,
           g4_perc = g4_sum/ymembers*100) %>% 
    mutate(youth_emigr = (g1_perc + 0.5*g3_perc + 0.5*g4_perc)/2) %>%
    # Total youth score
    rowwise() %>%
    mutate(youth_score_male = mean(c(youth_employ , youth_emigr),na.rm = T)) %>%
    mutate(youth_score_male = ifelse(ymembers == 0,NA,youth_score_male)) %>%
    select(key, youth_score_male) %>%
    distinct()
  
  ## Female
  YouthScore_female <- youth_emp_f %>%
    # Employment
    mutate(y_good = ifelse(y_occup %in% c(1,4,5,6),1,0),
           y_med = ifelse(y_occup %in% c(2),1,0),
           y_bad = ifelse(y_occup %in% c(0,3),1,0)) %>%
    # Emigration
    mutate(g1 = ifelse(y_future_farm == 1 & y_keen_emigr == 0,1,0), 
           g2 = ifelse(y_future_farm == 0 & y_keen_emigr == 1,1,0),
           g3 = ifelse(y_future_farm == 1 & y_keen_emigr == 1,1,0),
           g4 = ifelse(y_future_farm == 0 & y_keen_emigr == 0,1,0)) %>%
    group_by(key) %>% # make sum per farm
    # Employment
    mutate(y_good_sum = sum(y_good, na.rm = T),
           y_med_sum = sum(y_med, na.rm = T),
           y_bad_sum = sum(y_bad, na.rm = T),
           ymembers = sum(c(y_good,y_bad,y_med), na.rm = T)) %>%
    # Emigration
    mutate(g1_sum = sum(g1, na.rm = TRUE),
           g2_sum = sum(g2, na.rm = TRUE),
           g3_sum = sum(g3, na.rm = TRUE),
           g4_sum = sum(g4, na.rm = TRUE)) %>%
    # Employment score
    mutate(y_good_perc = y_good_sum*100/ymembers,
           y_med_perc = y_med_sum*100/ymembers,
           y_bad_perc = y_bad_sum*100/ymembers) %>% 
    mutate(youth_employ = (y_good_perc + 0.5*y_med_perc)/1.5) %>%
    # Emigration score
    mutate(g1_perc = g1_sum/ymembers*100,
           g2_perc = g2_sum/ymembers*100,
           g3_perc = g3_sum/ymembers*100,
           g4_perc = g4_sum/ymembers*100) %>% 
    mutate(youth_emigr = (g1_perc + 0.5*g3_perc + 0.5*g4_perc)/2) %>%
    # Total youth score
    rowwise() %>%
    mutate(youth_score_female = mean(c(youth_employ , youth_emigr),na.rm = T)) %>%
    mutate(youth_score_female = ifelse(ymembers == 0,NA,youth_score_female)) %>%
    select(key, youth_score_female) %>%
    distinct()
  
  YouthScore <- YouthScore %>%
    left_join(YouthScore_male, by = 'key') %>%
    left_join(YouthScore_female, by = 'key')

  return(YouthScore)
  
}

#-------------------------------------------------------------------------------
# 2.6 Agricultural Biodiversity - standard TAPE index
#-------------------------------------------------------------------------------

step2_Agricultural_Biodiversity <- function(){
  
  ## Crops
  c1 <- data_Crops %>% 
    group_by(cname_id, key) %>%
    mutate(cprod = sum(cprod, na.rm = TRUE),
           cqsold = sum(cqsold, na.rm = TRUE),
           cpg = mean(cpg, na.rm = TRUE),
           cgift = sum(cgift, na.rm = TRUE),
           cland_ha = sum(cland_ha, na.rm = TRUE),
           cvar = sum(cvar, na.rm = TRUE)) %>%  
    ungroup() %>%
    select(key, cname_id, cland_ha, cvar) %>%
    arrange(cname_id) %>% 
    distinct() %>% as.data.frame()
  
  c1 <- c1 %>%
    mutate(cvar = ifelse(cvar > 50, 1, cvar)) %>%
    group_by(key) %>%
    mutate(cland_ha_sum = sum(cland_ha, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(key) %>%
    mutate(p2_ = (cland_ha/cland_ha_sum)^2/cvar)
  
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
  # import selected livestock unit conversion units from config file
  lsucv_cattle <- cfg$lsucv_cattle
  lsucv_buffalo <- cfg$lsucv_buffalo
  lsucv_sheep <- cfg$lsucv_sheep
  lsucv_pig <- cfg$lsucv_pig
  lsucv_horses <- cfg$lsucv_horses
  lsucv_camels <- cfg$lsucv_camels
  
  a1 <- data_Animals %>% 
    group_by(an_id,key) %>%
    mutate(arais = sum(arais, na.rm = TRUE),
           aborn = sum(aborn, na.rm = TRUE),
           adied = sum(adied, na.rm = TRUE),
           abreed = sum(abreed, na.rm = TRUE),
           aslaughter = sum(aslaughter, na.rm = T),
           abuy = sum(abuy, na.rm = T),
           aqsold = sum(aqsold, na.rm = TRUE),
           apg = sum(apg, na.rm = TRUE),
           aqgift = sum(aqgift, na.rm = TRUE)) %>% 
    ungroup() %>%
    select(key, an_id, arais, aborn, adied, abreed, aslaughter, abuy, aqsold, apg, aqgift) %>%
    arrange(an_id) %>% distinct() %>% as.data.frame()
  
  # We convert in livestok standard units:
  a1 <- a1 %>%
    mutate(livunit = case_when(an_id == 0 ~ lsucv_cattle, # an_id == 0 --> "Cow / Bull"
                               an_id %in% c(1,2) ~ lsucv_buffalo, # Bison & Water buffalo
                               an_id == 3 ~ lsucv_horses, # Horse
                               an_id == 4 ~ 0.8, # Reindeer 
                               an_id %in% c(5,6) ~ 0.3, # Donkey & Mule
                               an_id == 7 ~ lsucv_sheep, # Sheep
                               an_id %in% c(8, 23) ~ 0.1, # Goat (8) & Crocodiles and other reptiles (23)
                               an_id == 9 ~ lsucv_pig, # Pig
                               an_id == 10 ~ lsucv_camels, # Camel
                               an_id == 11 ~ 0.17, # Llama
                               an_id == 12 ~ 0.02, # Rabbit
                               an_id %in% c(13, 19) ~ 0.01, # Chicken (13) & Fish (19)
                               an_id %in% c(14, 15, 16, 22) ~ 0.03, # Duck (14) & Goose (15) & Turkey (16) & Peacock (22)
                               an_id == 17 ~ 0.005, # Pigeon
                               an_id == 18 ~ 0.35, # Ostrich
                               an_id == 20 ~ 0.003, # Crustaceans
                               an_id == 21 ~ 0.002)) %>% # Molluscs
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

#-------------------------------------------------------------------------------
# 2.6 Agricultural Biodiversity - improved Biodiversity index (https://doi.org/10.34776/as172e)
#-------------------------------------------------------------------------------
# needs extended questionnaire!
step2_new_Biodiversity <- function(){
  
  # Total Agricultural area is used for different indices 
  # Total agricultural Area (total area under agricultural production + total grazing area + area semi natural habitats)
  snh_data <- data_snh %>% # Data on Semi-natural habitats
    mutate(SNH_area_ha = ifelse(is.na(sizeha_snh), length_snh*width_snh*0.0001,sizeha_snh)) %>%
    group_by(key) %>%
    summarise_at(vars(SNH_area_ha), sum, na.rm = T)
  
  Area_total <- data[, c('key', 'area_ha', 'area_natural_veg_ha', 'area_permanent_pasture_ha', 'area_common_pasture_ha')] %>%
    left_join(snh_data, by = 'key') %>%
    rowwise() %>%
    mutate(area_total = sum(area_ha, area_natural_veg_ha, area_permanent_pasture_ha, area_common_pasture_ha, SNH_area_ha, na.rm = T))
  
  ## 2.6.1  Agricultural diversity - standard TAPE index, however only GSI from Animals and Crops
  Ag_biodiversity <- step2_Agricultural_Biodiversity()
  Ag_biodiversity <- Ag_biodiversity %>%
    rowwise() %>%
    mutate(Agricultural_diversity = mean(c(GSIndex_crops, GSIndex_animals), na.rm = T))
  
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
  
  Ag_biodiversity <- Ag_biodiversity %>%
    left_join(Patch_size, by = 'key')
  rm(Patch_size)
  
  ## 2.6.3 Tree habitat
  Tree <- data_tree %>%
    group_by(key) %>%
    summarise_at(vars(sizeha), sum, na.rm = T)
  
  Tree_habitat <- data[,c('key', 'width', 'length')] # width and length of tree lines in m
  Tree_habitat <- Tree_habitat %>%
    left_join(Tree, by = 'key') %>%
    mutate(tree_lines_ha = length * width * 0.0001) %>% # length and width are in m
    rowwise() %>%
    mutate(tree_area_total = sum(tree_lines_ha, sizeha, na.rm = T)) %>%
    left_join(Area_total, by = 'key') %>%
    mutate(Tree_habitat = (tree_area_total/area_total)*100)
  
  Ag_biodiversity <- Ag_biodiversity %>%
    left_join(Tree_habitat[,c('key', 'sizeha', 'tree_lines_ha', 'Tree_habitat')], by = 'key')
  rm(Tree_habitat, Tree)
  
  ## 2.6.4 Semi-natural Habitats
  SNH <- data_snh
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
  
  Ag_biodiversity <- Ag_biodiversity %>%
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
  
  Ag_biodiversity <- Ag_biodiversity %>%
    left_join(N_Fert[,c('key', 'FertArea', 'N_input', 'N_application')], by = 'key')
  rm(N_Fert)
  
  ## 2.6.6 Pesticide application
  ## Pesticide application
  
  cp <- data_Chemical_Pesticide %>%
    group_by(key) %>%
    summarise_at(vars(cpspray), sum, na.rm = T) 
  
  ## Toxiocology
  cp_tox <- data_Chemical_Pesticide %>%
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
  
  Ag_biodiversity <- Ag_biodiversity %>%
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
  
  Ag_biodiversity <- Ag_biodiversity %>%
    left_join(Fieldoperations[,c('key', 'FieldOP')], by = 'key')
  rm(Fieldoperations)
  
  ### 2.6.8 Stocking rate
  # Number of animals converted to livestock units (lU/ha)
  Stockingrate <- data_Animals 
  
  Stockingrate <- Stockingrate %>% 
    group_by(an_id, key, onfarm) %>%
    mutate(arais = sum(arais, na.rm = TRUE)) %>% 
    ungroup() %>%
    select(an_id, key, arais, onfarm) %>%
    distinct() %>% as.data.frame()
  
  # We convert in livestok standard units:
  # import selected livestock unit conversion units from config file
  lsucv_cattle <- cfg$lsucv_cattle
  lsucv_buffalo <- cfg$lsucv_buffalo
  lsucv_sheep <- cfg$lsucv_sheep
  lsucv_pig <- cfg$lsucv_pig
  lsucv_horses <- cfg$lsucv_horses
  lsucv_camels <- cfg$lsucv_camels
  
  Stockingrate <- Stockingrate %>%
    mutate(livunit = case_when(an_id == 0 ~ lsucv_cattle, # an_id == 0 --> "Cow / Bull"
                               an_id %in% c(1,2) ~ lsucv_buffalo, # Bison & Water buffalo
                               an_id == 3 ~ lsucv_horses, # Horse
                               an_id == 4 ~ 0.8, # Reindeer 
                               an_id %in% c(5,6) ~ 0.3, # Donkey & Mule
                               an_id == 7 ~ lsucv_sheep, # Sheep
                               an_id %in% c(8, 23) ~ 0.1, # Goat (8) & Crocodiles and other reptiles (23)
                               an_id == 9 ~ lsucv_pig, # Pig
                               an_id == 10 ~ lsucv_camels, # Camel
                               an_id == 11 ~ 0.17, # Llama
                               an_id == 12 ~ 0.02, # Rabbit
                               an_id %in% c(13, 19) ~ 0.01, # Chicken (13) & Fish (19)
                               an_id %in% c(14, 15, 16, 22) ~ 0.03, # Duck (14) & Goose (15) & Turkey (16) & Peacock (22)
                               an_id == 17 ~ 0.005, # Pigeon
                               an_id == 18 ~ 0.35, # Ostrich
                               an_id == 20 ~ 0.003, # Crustaceans
                               an_id == 21 ~ 0.002)) %>% # Molluscs
    mutate(nliv = livunit * arais) %>% # Multiplying livestock units per number of individuals
    group_by(key) %>%
    summarise_at(vars(nliv), sum, na.rm = T)
  
  Stockingrate <- Stockingrate %>%
    left_join(Area_total, by = 'key') %>%
    mutate(stocking_rate = nliv/area_total) %>%
    mutate(AvStock = case_when(stocking_rate > 4 ~ 0,
                               stocking_rate <= 4 ~ (100-(25 * stocking_rate))))
  
  Ag_biodiversity <- Ag_biodiversity %>%
    left_join(Stockingrate[,c('key', 'AvStock')], by = 'key')
  rm(Stockingrate)
  
  ### 2.6.9 Grazing intensity
  Grazing_intensity <- data_Animals 
  
  Grazing_intensity <- Grazing_intensity %>% 
    group_by(an_id, key, onfarm) %>%
    mutate(arais = sum(arais, na.rm = TRUE)) %>% 
    ungroup() %>%
    select(an_id, key, arais, onfarm) %>%
    distinct() %>% as.data.frame()
  
  # We convert in livestok standard units:
  Grazing_intensity <- Grazing_intensity %>%
    mutate(livunit = case_when(an_id == 0 ~ lsucv_cattle, # an_id == 0 --> "Cow / Bull"
                               an_id %in% c(1,2) ~ lsucv_buffalo, # Bison & Water buffalo
                               an_id == 3 ~ lsucv_horses, # Horse
                               an_id == 4 ~ 0.8, # Reindeer 
                               an_id %in% c(5,6) ~ 0.3, # Donkey & Mule
                               an_id == 7 ~ lsucv_sheep, # Sheep
                               an_id %in% c(8, 23) ~ 0.1, # Goat (8) & Crocodiles and other reptiles (23)
                               an_id == 9 ~ lsucv_pig, # Pig
                               an_id == 10 ~ lsucv_camels, # Camel
                               an_id == 11 ~ 0.17, # Llama
                               an_id == 12 ~ 0.02, # Rabbit
                               an_id %in% c(13, 19) ~ 0.01, # Chicken (13) & Fish (19)
                               an_id %in% c(14, 15, 16, 22) ~ 0.03, # Duck (14) & Goose (15) & Turkey (16) & Peacock (22)
                               an_id == 17 ~ 0.005, # Pigeon
                               an_id == 18 ~ 0.35, # Ostrich
                               an_id == 20 ~ 0.003, # Crustaceans
                               an_id == 21 ~ 0.002)) %>% # Molluscs
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
  
  Ag_biodiversity <- Ag_biodiversity %>%
    left_join(Grazing_intensity[,c('key', 'Grazing_intensity')], by = 'key')
  
  ### 2.6.10 Land use change
  LUC <- data[,c('key', 'landuse_change')] %>%
    left_join(Area_total, by = 'key') %>%
    mutate(LUC_SNH = 1 - (landuse_change/area_total)) %>%
    mutate(LUC_SNH = LUC_SNH * 100) %>%
    mutate(LUC_SNH = ifelse(is.na(LUC_SNH), 100, LUC_SNH))
  
  Ag_biodiversity <- Ag_biodiversity %>%
    left_join(LUC[,c('key', 'LUC_SNH')], by = 'key') 
  rm(LUC)
  
  ### Calculation of total Biodiversity Indicator
  Ag_biodiversity <- Ag_biodiversity %>%
    rowwise() %>%
    mutate(Biodiversity_indicator = mean(c(Agricultural_diversity, LUC_SNH, Grazing_intensity, AvStock, FieldOP, cp_ind, N_application, Semi_natural_Habitats, Tree_habitat, Patch_size), na.rm = T)) %>%
    rename( "cp_appl" = "cp") 
  
  return(Ag_biodiversity)
  
}


#-------------------------------------------------------------------------------
# 2.7 Exposure to Pesticides
#-------------------------------------------------------------------------------

step2_pesticides <- function(){
  
  ## Organic Pesticides
  co <- data_Organic_Pesticides %>%
    mutate(coused1 = ifelse(is.na(coused1),NA,
                            ifelse(is.na(comeas1),coused1,
                                   ifelse(comeas1 %in% c('Grams', 'g'), coused1*0.001, coused1)))) %>%
    mutate(otox1 = ifelse(cotox == 1,1,0),
           otox2 = ifelse(cotox == 2,1,0),
           otox3 = ifelse(cotox == 3,1,0))
  
  co <- co %>%
    group_by(key) %>%
    mutate(coused1 = sum(coused1, na.rm = T),
           otox1 = ifelse(!is.na(otox1), max(otox1, na.rm = T), NA),
           otox2 = ifelse(!is.na(otox2), max(otox2, na.rm = T), NA),
           otox3 = ifelse(!is.na(otox3), max(otox3, na.rm = T), NA)) %>% 
    select(key, coused1, otox1, otox2, otox3) %>% distinct()
  
  ## Chemical Pesticides
  cp <- data_Chemical_Pesticide %>%
    mutate(cpused = ifelse(cpmeas %in% c('Grams', 'g'), cpused*0.001, cpused)) %>% 
    mutate(ctox1 = ifelse(cptox == 1,1,0),
           ctox2 = ifelse(cptox == 2,1,0),
           ctox3 = ifelse(cptox == 3,1,0))
  cp <- cp %>%
    group_by(key) %>%
    mutate(cpused = sum(cpused, na.rm = T),
           ctox1 = ifelse(!is.na(ctox1), max(ctox1, na.rm = T), NA),
           ctox2 = ifelse(!is.na(ctox2), max(ctox2, na.rm = T), NA),
           ctox3 = ifelse(!is.na(ctox3), max(ctox3, na.rm = T), NA)) %>% 
    select(key, cpused, ctox1, ctox2, ctox3) %>% distinct()
  
  ## Number of mitigation strategies
  mitigation <- data %>%
    select(mitig) %>%
    mutate(nr_mitigation = str_count(str_replace_all(mitig, " ", ""))) # counts how many different mitigation strategies are in mitig column
  
  ecoman_num <- data %>%
    select(ecoman) %>%
    mutate(nr_ecoman = str_count(str_replace_all(ecoman, " ", ""))) # counts how many different ecoman strategies are in ecoman column
    
  ## Combining
  Pesticides <- data %>%
    select('key', 'opestnum') %>%
    left_join(co, by = 'key') %>%
    left_join(cp, by = 'key') %>%
    # combine toxicity from organic and chemical pesticides
    rowwise() %>%
    mutate(tox1 = ifelse(!is.na(ctox1) | !is.na(!otox1), max(ctox1, otox1, na.rm = T), NA),
           tox2 = ifelse(!is.na(ctox2) | !is.na(!otox2), max(ctox2, otox2, na.rm = T), NA),
           tox3 = ifelse(!is.na(ctox3) | !is.na(!otox3), max(ctox3, otox3, na.rm = T), NA))
  
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
    mutate(pest_mitig = case_when(nr_mitigation == 0 ~ 0,
                                  nr_mitigation == 1 ~ 25,
                                  nr_mitigation == 2 ~ 50,
                                  nr_mitigation == 3 ~ 75,
                                  nr_mitigation >= 4 ~ 100)) %>%
    # Score 4: Number of ecological management measures
    mutate(pest_ecoman = case_when(nr_ecoman == 0 ~ 0,
                                   nr_ecoman == 1 ~ 25,
                                   nr_ecoman == 2 ~ 50,
                                   nr_ecoman == 3 ~ 75,
                                   nr_ecoman >= 4 ~ 100)) %>%
    # Overall pest-score
    mutate(pest_score = case_when(cpused == 0 & coused1 == 0 ~ 100, # no pesticides used --> 100 % even if no ecoman and mitigation!
                                  TRUE ~ (pest_use + pest_tox + pest_mitig + pest_ecoman)/4))
  return(Pesticides)
}

#-------------------------------------------------------------------------------
# 2.8 Productivity - Income - Added Value
#-------------------------------------------------------------------------------

step2_economy <- function(){
  
  Area_total <- data[, c('key', 'area_ha', 'area_natural_veg_ha', 'area_permanent_pasture_ha', 'area_common_pasture_ha')] %>%
    rowwise() %>%
    mutate(area_total = sum(area_ha, area_natural_veg_ha, area_permanent_pasture_ha, area_common_pasture_ha, na.rm = T))
  
  # Householdmembers
  hh <- data[,c('key', 'hh_men', 'hh_women', 'hh_myoung', 'hh_fyoung', 'hh_children')] %>%
    mutate(hh_child_adultval = hh_children/2) %>% # no. of children in adult value
    rowwise() %>%
    mutate(hh_adults = sum(c(hh_men, hh_women, hh_myoung, hh_fyoung, hh_child_adultval), na.rm = T))
  
  ## Family of workers
  data_workers <- data[,c('key', 'ag_men', 'ag_women', 'ag_myoung', 'ag_fyoung', 'ag_children')]
  data_workers <- data_workers %>%
    left_join(Area_total, by = 'key') %>%
    mutate(nag_children = ag_children/2) %>%
    rowwise() %>%
    mutate(fam_workers = sum(c(ag_men, ag_women, ag_myoung, ag_fyoung, nag_children), na.rm = T))
  
  # External workers
  ext_workers <- data[,c('key', 'num_extworkers_year', 'wage_year', 'num_extworkers_season', 'days_ext', 'wage_season')]
  
  ext_workers <- ext_workers %>%
    mutate(num_worker_fseason = num_extworkers_season*days_ext/300) %>% # Assuming 300 workingdays is a full year
    rowwise() %>%
    mutate(ext_workers = sum(c(num_worker_fseason, num_extworkers_year), na.rm = T)) %>%
    mutate(wage = sum(c(wage_year, wage_season), na.rm = T))
  
  data_workers <- data_workers %>%
    left_join(ext_workers, by = 'key') %>%
    rowwise() %>%
    mutate(num_workers = sum(c(fam_workers, ext_workers), na.rm = T))
  
  
  ## Crop productivity
  c1 <- data_Crops
  
  # Make a check for price at the gate (can't be 0 or too low)
  cpg <- c1 %>%
    select(c('cname_label', 'other_crop', 'cpg_per_kg')) %>%
    mutate(cname_label = ifelse(cname_label == 'Other', other_crop, cname_label)) %>%
    group_by(cname_label) %>%
    summarise_at(vars(cpg_per_kg), median, na.rm = T) %>%
    rename(cpg_kg_median = cpg_per_kg)
  
  c1 <- c1 %>%
    left_join(cpg, by = 'cname_label') %>%
    # corrections for cpg if = 0 or NA or too low
    mutate(cpg_kg_corr = ifelse(cpg_per_kg == 0, cpg_kg_median, cpg_per_kg)) %>% 
    mutate(cpg_kg_corr = ifelse(is.na(cpg_per_kg), cpg_kg_median, cpg_per_kg)) %>%
    mutate(cpg_kg_corr = ifelse(cpg_per_kg/cpg_kg_median < 0.1, cpg_kg_median, cpg_per_kg)) %>%
    mutate(cpg_kg_corr = ifelse(cpg_per_kg/cpg_kg_median > 10, cpg_kg_median, cpg_per_kg)) %>%
    mutate(cpg_kg_corr = ifelse(is.na(cpg_kg_corr), cpg_per_kg, cpg_kg_corr)) 
  rm(cpg)
  
  c1 <- c1 %>%
    mutate(crop_prodval = cprod_kg * cpg_kg_corr,
           crop_sales = cqsold_kg * cpg_kg_corr,
           crop_giftcost = cgift_kg * cpg_kg_corr,
           crop_consval = (cprod_kg - cqsold_kg - cgift_kg) * cpg_kg_corr) %>%
    mutate(crop_consval = ifelse(crop_consval < 0, 0, crop_consval)) %>%
    group_by(key) %>%
    mutate(crop_prodval = sum(crop_prodval, na.rm = TRUE),
           crop_sales = sum(crop_sales, na.rm = TRUE),
           crop_giftcost = sum(crop_giftcost, na.rm = TRUE),
           crop_consval = sum(crop_consval, na.rm = TRUE)) %>%
    select(key,crop_prodval, crop_sales, crop_giftcost, crop_consval) %>% 
    distinct()
  
  ## Crops and forest products productivity
  cfp <- data_Crop_Products 
  
  # Make a check for price at the gate (can't be 0 or too low)
  cfppg <- cfp %>%
    select(c('cfp_label', 'cfpname_other', 'cfppg')) %>%
    mutate(cfp_label = ifelse(cfp_label %in% c('Other', 'Other 1', 'Other 2', 'Other 3'), cfpname_other, cfp_label)) %>%
    group_by(cfp_label) %>%
    summarise_at(vars(cfppg), median, na.rm = T) %>%
    rename(cfppg_median = cfppg)
  
  cfp <- cfp %>%
    left_join(cfppg, by = 'cfp_label') %>%
    # corrections for cfppg if == 0 or NA or too low/high
    mutate(cfppg_corr = ifelse(cfppg == 0, cfppg_median, cfppg),
           cfppg_corr = ifelse(is.na(cfppg), cfppg_median, cfppg),
           cfppg_corr = ifelse(cfppg/cfppg_median < 0.1, cfppg_median, cfppg),
           cfppg_corr = ifelse(cfppg/cfppg_median > 10, cfppg_median, cfppg),
           cfppg_corr = ifelse(is.na(cfppg_corr), cfppg, cfppg_corr))
  rm(cfppg)
    
  cfp <- cfp %>%
    mutate(cfp_prodval = cfpprod * cfppg_corr,
           cfp_sales = cfpqsold * cfppg_corr,
           cfp_giftcost = cfpgift * cfppg_corr,
           cfp_consval = (cfpprod - cfpqsold -cfpgift) * cfppg_corr) %>%
    mutate(cfp_consval = ifelse(cfp_consval < 0, 0, cfp_consval)) %>%
    group_by(key) %>%
    mutate(cfp_prodval = sum(cfp_prodval, na.rm = T),
           cfp_sales = sum(cfp_sales, na.rm = T),
           cfp_giftcost = sum(cfp_giftcost, na.rm = T),
           cfp_consval = sum(cfp_consval, na.rm = T)) %>%
    select(key, cfp_prodval, cfp_sales, cfp_giftcost, cfp_consval) %>%
    distinct()
  
  
  ## Animal productivity
  a1 <- data_Animals
  
  # Make a check for price at the gate (can't be 0 or too low)
  apg <- a1 %>%
    select(c('an_label', 'aname_other', 'apg')) %>%
    mutate(an_label = ifelse(an_label %in% c('Other', 'Other 1', 'Other 2', 'Other 3'), aname_other, an_label)) %>%
    group_by(an_label) %>%
    summarise_at(vars(apg), median, na.rm = T) %>%
    rename(apg_median = apg)
  
  a1 <- a1 %>%
    left_join(apg, by = 'an_label') %>%
    # corrections for cpg if = 0 or NA or too low/high
    mutate(apg_corr = ifelse(apg == 0, apg_median, apg)) %>% 
    mutate(apg_corr = ifelse(is.na(apg), apg_median, apg)) %>%
    mutate(apg_corr = ifelse(apg/apg_median < 0.1, apg_median, apg)) %>%
    mutate(apg_corr = ifelse(apg/apg_median > 10, apg_median, apg)) %>%
    mutate(apg_corr = ifelse(is.na(apg_corr), apg, apg_corr)) 
  rm(apg)
  
  a1 <- a1 %>%
    mutate(anim_prodval = arais * apg_corr,
           anim_sales = aqsold * apg_corr,
           anim_giftcost = aqgift * apg_corr,
           anim_consval = (arais - aqsold - aqgift) * apg_corr) %>%
    mutate(anim_consval = ifelse(anim_consval < 0, 0, anim_consval)) %>%
    group_by(key) %>%
    mutate(anim_prodval = sum(anim_prodval, na.rm = T),
           anim_sales = sum(anim_sales, na.rm = T),
           anim_giftcost = sum(anim_giftcost, na.rm = T),
           anim_consval = sum(anim_consval, na.rm = T),
           feedexp = sum(feedexp, na.rm = T),
           vetexp = sum(vetexp, na.rm = T)) %>%
    select(key, anim_prodval, anim_sales, anim_giftcost, anim_consval, feedexp, vetexp) %>%
    distinct()
  
  ## Animal Products Productivity
  ap <- data_Animal_Products
  
  # Make a check for price at the gate (can't be 0 or too low)
  appg <- ap %>%
    select(c('ap_label', 'apname_other', 'appg')) %>%
    mutate(ap_label = ifelse(ap_label %in% c('Ohter', 'Other 1', 'Other 2', 'Other 3'), apname_other, ap_label)) %>%
    group_by(ap_label) %>%
    summarise_at(vars(appg), median, na.rm = T) %>%
    rename(appg_median = appg)
  
  ap <- ap %>%
    left_join(appg, by = 'ap_label') %>%
    # corrections for cpg if = 0 or NA or too low
    mutate(appg_corr = ifelse(appg == 0, appg_median, appg)) %>% 
    mutate(appg_corr = ifelse(is.na(appg), appg_median, appg)) %>%
    mutate(appg_corr = ifelse(appg/appg_median < 0.1, appg_median, appg)) %>%
    mutate(appg_corr = ifelse(appg/appg_median > 10, appg_median, appg)) %>%
    mutate(appg_corr = ifelse(is.na(appg_corr), appg, appg_corr)) 
  rm(appg)
  
  ap <- ap %>%
    mutate(anpr_prodval = approd * appg_corr,
           anpr_sales = apqsold * appg_corr,
           anpr_giftcost = apgift*appg_corr,
           anpr_consval = (approd - apqsold - apgift) * appg_corr) %>%
    mutate(anpr_consval = ifelse(anpr_consval < 0, 0, anpr_consval)) %>%
    group_by(key) %>%
    mutate(anpr_prodval = sum(anpr_prodval, na.rm = TRUE),
           anpr_sales = sum(anpr_sales, na.rm = TRUE),
           anpr_giftcost = sum(anpr_giftcost, na.rm = TRUE),
           anpr_consval = sum(anpr_consval, na.rm = TRUE)) %>%
    select(key, anpr_prodval, anpr_sales, anpr_giftcost, anpr_consval) %>% 
    distinct()
  
  ## Productivity
  Economy <- c1 %>%
    left_join(cfp, by = 'key') %>%
    left_join(a1, by = 'key') %>%
    select(-c(feedexp, vetexp)) %>%
    left_join(ap, by = 'key') %>%
    rowwise() %>%
    mutate(total_output = sum(c(crop_prodval, cfp_prodval, anim_prodval, anpr_prodval), na.rm = T))
  rm(c1, ap)
  
  # Number of animals
  LSU <- data_Animals 
  
  # We convert in livestok standard units:
  # import selected livestock unit conversion units from config file
  lsucv_cattle <- cfg$lsucv_cattle
  lsucv_buffalo <- cfg$lsucv_buffalo
  lsucv_sheep <- cfg$lsucv_sheep
  lsucv_pig <- cfg$lsucv_pig
  lsucv_horses <- cfg$lsucv_horses
  lsucv_camels <- cfg$lsucv_camels
  
  LSU <- LSU %>%
    mutate(livunit = case_when(an_id == 0 ~ lsucv_cattle, # an_id == 0 --> "Cow / Bull"
                               an_id %in% c(1,2) ~ lsucv_buffalo, # Bison & Water buffalo
                               an_id == 3 ~ lsucv_horses, # Horse
                               an_id == 4 ~ 0.8, # Reindeer 
                               an_id %in% c(5,6) ~ 0.3, # Donkey & Mule
                               an_id == 7 ~ lsucv_sheep, # Sheep
                               an_id %in% c(8, 23) ~ 0.1, # Goat (8) & Crocodiles and other reptiles (23)
                               an_id == 9 ~ lsucv_pig, # Pig
                               an_id == 10 ~ lsucv_camels, # Camel
                               an_id == 11 ~ 0.17, # Llama
                               an_id == 12 ~ 0.02, # Rabbit
                               an_id %in% c(13, 19) ~ 0.01, # Chicken (13) & Fish (19)
                               an_id %in% c(14, 15, 16, 22) ~ 0.03, # Duck (14) & Goose (15) & Turkey (16) & Peacock (22)
                               an_id == 17 ~ 0.005, # Pigeon
                               an_id == 18 ~ 0.35, # Ostrich
                               an_id == 20 ~ 0.003, # Crustaceans
                               an_id == 21 ~ 0.002)) %>% # Molluscs
    mutate(arais_lsu = livunit * arais) %>% # Multiplying livestock units per number of individuals
    group_by(key) %>%
    summarise_at(vars(arais_lsu), sum, na.rm = T)
  
  Economy <- Economy %>%
    left_join(data_workers, by = 'key') %>%
    left_join(LSU, by = 'key') %>%
    mutate(tot_productivity_ha = total_output/area_total, 
           tot_productivity_pers = total_output/num_workers,
           crop_productivity_ha = crop_prodval/area_ha,
           animal_productivity_lsu = anim_prodval/arais_lsu) %>%
    rowwise() %>%
    mutate(farm_sales = sum(c(crop_sales, cfp_sales, anim_sales, anpr_sales), na.rm = T))
  
  ## Subsidies, Cost of Inputs, Taxes, interest on loans, cost of renting land
  Expenditures <- data %>%
    select(c('key', 'rentcost'), ends_with('exp')) %>%
    left_join(data_workers, by = 'key') %>%
    left_join(hh, by = 'key') %>%
    left_join(a1[,c('key', 'vetexp', 'feedexp')], by = 'key') %>%
    mutate(food_exp_capita = foodexp/hh_adults) %>%
    rowwise() %>%
    mutate(input_exp = sum(c(seedsexp, fertexp, cp_exp, op_exp), na.rm = T),
              fuel_energy_transp_exp = sum(c(fuelexp, enerexp, transpexp, machexp), na.rm = T),
              livestock_expenditures = sum(c(vetexp, livexp, feedexp), na.rm = T),
              finance_exp = sum(c(rentcost), na.rm = T), # subs, taxes & inter not included anymore
              total_expenditures = sum(c(foodexp, input_exp, fuel_energy_transp_exp, wage, livestock_expenditures, finance_exp), na.rm = T)) %>%
    mutate(share_foodexp = (foodexp/total_expenditures)*100)
  
  Economy <- Economy %>%
    left_join(Expenditures)
  
  ## Depreciation
  machine <- data_Machines
  
  machine <- machine %>%
    mutate(init_v = mowned * mprice,
           res_v = (init_v/100)*10,
           depreciation = (init_v - res_v)/(myused/(myused+myplan))) # change of the formula
  
  machine <- machine %>%
    group_by(key) %>%
    mutate(depreciation = sum(depreciation, na.rm = T)) %>%
    select(key, depreciation) %>%
    distinct()
  
  activities <- data_Activities %>%
    group_by(key) %>%
    summarise_at(vars(acrev), sum, na.rm = T)
  
  Economy <- Economy %>%
    left_join(machine, by = 'key') %>%
    left_join(activities, by = 'key') %>%
    mutate(depreciation = ifelse(is.na(depreciation), 0, depreciation)) %>%
    rowwise() %>%
    mutate(farm_revenue = sum(crop_sales, cfp_sales, anim_sales, anpr_sales, na.rm = T)) 
  
  Economy <- Economy %>%
    rowwise() %>%
    mutate(netrev = sum(c(farm_sales, acrev, -seedsexp, -fertexp, -feedexp, -vetexp, -machexp, -fuelexp, -enerexp, -transpexp, -cp_exp, -op_exp, -depreciation, -wage), na.rm = T),
           value_added = sum(c(total_output, acrev, -seedsexp, -fertexp, -feedexp, -vetexp, -machexp, -fuelexp, -enerexp, -transpexp, -cp_exp, -op_exp, -depreciation), na.rm = T)) %>%
    mutate(netrev_pcapita = netrev/hh_adults,
           value_added_pcapita = value_added/hh_adults,
           value_added_ha = value_added/area_total,
           value_added_gvp = value_added/total_output,
           value_added_gvp = ifelse(value_added_gvp < 0 & total_output < 0, -1*value_added_gvp, value_added_gvp), # rule of inequality and division by negative numbers
           # International Poverty line 2.15 USD
           inc_person_day = (farm_sales + acrev)/hh_adults/365)
  
  Economy <- Economy %>%
    select(!c('ag_men', 'ag_women', 'ag_myoung', 'ag_fyoung', 'ag_children', 'area_ha', 'area_natural_veg_ha', 'area_permanent_pasture_ha', 'area_common_pasture_ha',
              'nag_children', 'fam_workers', 'days_ext' , 'seedsexp', 'fertexp', 'feedexp', 'vetexp', 'machexp', 'fuelexp', 'enerexp', 'transpexp', 'cp_exp', 'op_exp',
              'hh_men', 'hh_women', 'hh_myoung', 'hh_fyoung', 'hh_children', 'hh_child_adultval', 'hh_adults'))
  
  return(Economy)
}


#-------------------------------------------------------------------------------
# 2.9 FIES (Food Insecurity Experience Survey)
#-------------------------------------------------------------------------------
step2_fies <- function(){
  
  fies <- data[,c('key')]
  
  for(i in c('worried', 'healthy', 'fewfoods', 'skipped', 'ateless', 'ranout', 'hungry', 'wholeday')){
    
    score_i <- paste0(i, "_score")
    
    fies_i <- data[,c('key', i)] %>%
      mutate(!!score_i := case_when(!!sym(i) == 0 ~ 100,
                                    !!sym(i) == 1 ~ 66,
                                    !!sym(i) == 2 ~ 33,
                                    !!sym(i) == 3 ~ 0))
    
    fies <- fies %>%
      left_join(fies_i, by = 'key')
  }
  
  # Total FIES score
  fies <- fies %>%
    rowwise() %>%
    mutate(fies_score = mean(c(worried_score, healthy_score, fewfoods_score, skipped_score, ateless_score, ranout_score, hungry_score, wholeday_score), na.rm = T))

  return(fies)
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
  FIES <- step2_fies()
  
  ########################################
  # format
  #######################################
  
  ## Location
  TAPE_final <- data %>%
    select(c('key','team_name', 'location_level_one_name', 'location_level_two_name', 'location_level_three_name'))
  
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
    left_join(Economy[, c('key','total_output', 'farm_revenue', 'netrev', 'netrev_pcapita', 'value_added', 'value_added_pcapita', 'value_added_ha', 'value_added_gvp', 'inc_person_day',
                          'tot_productivity_ha','tot_productivity_pers', 'crop_productivity_ha', 'animal_productivity_lsu')], by = 'key') 
  
  ## FIES
  TAPE_final <- TAPE_final %>%
    left_join(FIES[,c('key', 'fies_score')], by = 'key')
  
  ## Youth
  TAPE_final <- TAPE_final %>%
    left_join(YouthScore, by = 'key')
  
  ## Women's Empowerment
  TAPE_final <- TAPE_final %>%
    left_join(WomenEmpowerment[,c('key', 'prodec_wom', 'cred_decmak_wom', 'decinc_wom', 'leadership_score_wom', 'timeuse_wom', 'wemp_score', 'GPI')], by = 'key')
  
  
  return(TAPE_final)
}




