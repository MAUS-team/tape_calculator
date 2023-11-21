
#===============================================================================
# Configuration and sourcing
#===============================================================================

#load the config file and choose the option best suited for the data
Sys.setenv(R_CONFIG_ACTIVE = "altered")


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

Agricultural_biodiversity <- step2_Agricultural_Biodiversity()

#-------------------------------------------------------------------------------
# 2.7 Exposure to Pesticides
#-------------------------------------------------------------------------------

Pesticides <- step2_pesticides()

#-------------------------------------------------------------------------------
# 2.8 Productivity - Income - Added Value
#-------------------------------------------------------------------------------

Economy <- step2_economy()

#===============================================================================
# Generate Output Excel-Table
#===============================================================================

#calculate all the data
TAPE <- all_TAPE()

#create an excel table
TAPE_excel <- TAPE_excel()
write.xlsx(TAPE_excel, 'Stevan_functions_tryout/try_if_header.xlsx')







