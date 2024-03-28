
#===============================================================================
# Configuration and sourcing
#===============================================================================

#load the config file and choose the option best suited for the data
Sys.setenv(R_CONFIG_ACTIVE = "OECD")


#load packages
source('Sourced code/Source.R')



#===============================================================================
# STEP 1: Characterisation of Agroecological Transition (CAET)
#===============================================================================

## Extract data and calculate values for single elements of agroecology 

# 1.1 Diversity

Diversity <- caet_diversity()

# 1.2 Synergies

Synergies <- caet_synergies()

# 1.3 Efficiency

Efficiency <- caet_efficiency()

# 1.4 Recycling

Recycling <- caet_recycling()

# 1.5 Resilience

Resilience <- caet_resilience()

# 1.6 Culture and food traditions

Culture <- caet_culture()

# 1.7 Co-creation

Cocreation <- caet_cocreation()

# 1.8 Human & Social values

Humanvalues <- caet_humanvalues()

# 1.9 Circular and solidarity economy

Circular <- caet_circular()

# 1.10 Responsible Governance

Governance <- caet_governance()

## Calculate CAET

CAET <- caet_CAET()

#===============================================================================
# STEP 2: Sustainability. Core Criteria
#===============================================================================

#-------------------------------------------------------------------------------
# 2.1 Dietary Diversity
#-------------------------------------------------------------------------------

DietaryDiv <- step2_Dietary_Diversity()

#-------------------------------------------------------------------------------
# 2.2 Soil Health
#-------------------------------------------------------------------------------

SoilHealth <- step2_SoilHealth( )

#-------------------------------------------------------------------------------
# 2.3 Secure Land Tenure
#-------------------------------------------------------------------------------

Landtenure <- step2_LandTenure()

#-------------------------------------------------------------------------------
# 2.4 Women Empowerment
#-------------------------------------------------------------------------------

WomenEmpowerment <- step2_WomensEmpowerment()

#-------------------------------------------------------------------------------
# 2.5 Youth Employment and Emigration
#-------------------------------------------------------------------------------

YouthScore <- step2_YouthEmploymentEmigration()

#-------------------------------------------------------------------------------
# 2.6 Agricultural Biodiversity
#-------------------------------------------------------------------------------
# There are two Index for Agricultural Biodiversity:
# (1) The standard TAPE Agrobiodiversity Index
# (2) the new biodiversity index developed by Agroscope (Gilgen et al. 2023. DOI: https://doi.org/10.34776/as172e)
# The new biodiversity index needs extra questions during the survey and thus can only be calculated if the extended questionnaire was used!
# Select in the 'config.yml' file if you want to calculate (1) "Agrobiodiversity" or (2) "Biodiversity"

if(cfg$Indicator == "Agrobiodiversity"){
  
  Agricultural_biodiversity <- step2_Agricultural_Biodiversity()
  
} else if (cfg$Indicator == 'Biodiversity'){
  
  Agricultural_biodiversity <- step2_new_Biodiversity()
  
}


#-------------------------------------------------------------------------------
# 2.7 Exposure to Pesticides
#-------------------------------------------------------------------------------

Pesticides <- step2_pesticides()

#-------------------------------------------------------------------------------
# 2.8 Productivity - Income - Added Value
#-------------------------------------------------------------------------------

Economy <- step2_economy()

#-------------------------------------------------------------------------------
# 2.9 FIES (Food Insecurity Experience Survey)
#-------------------------------------------------------------------------------

FIES <- step2_fies()

#===============================================================================
# Generate Output Excel-Table
#===============================================================================

#calculate all the data
TAPE <- all_TAPE()

write.xlsx(TAPE, 'TAPE_final.xlsx')







