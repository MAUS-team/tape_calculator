#clear the environement
rm(list = ls())

# Declare necessary packages
pkgs_cran <- c('readxl', 'openxlsx', 'tidyverse', 'fmsb', 'svDialogs', 'reshape2', 'cowplot', 'ggpubr', 'rstatix', 'RobustLinearReg','car','GGally','corrplot','scatterplot3d', 'rlang')


# Load Packages

cat("Loading packages...\n")

# Get the names of all installed packages
inst_pkgs <- installed.packages()[, "Package"]

# Install required packages from CRAN
req_pkgs_cran <- setdiff(pkgs_cran, inst_pkgs)
if (length(req_pkgs_cran) > 0) {
  install.packages(req_pkgs_cran, clean = TRUE)
}

# Load necessary packages
shh <- lapply(setdiff(pkgs_cran, "config"), library,
              character.only = TRUE)

# Purge obsolete variables
rm(inst_pkgs, req_pkgs_cran, shh)

cat("Packages loaded!\n")

# Get config

cat("Reading config...\n")


cfg <- config::get(file = 'config.yml')

cat("Configuration read!\n")

#source functions

source("Sourced code/Functions.R")

# import data

cat("Importing data...\n")

########################################################
# configuration

data <- readxl::read_excel(cfg$source, sheet = "Main Survey")

data <- data %>%
  rename(key = farm_id)

# Data cleaning
source('Sourced code/Tape_cleaning_main_data.R')

#-----------------------------------------------------------------
# read extra excel sheets
#-----------------------------------------------------------------

# List of all the sheet names in the Excelfile to check before data import if the according sheet exists
sheet_names <- excel_sheets(cfg$source)

# new sheets
if("Performances_Youth_Males" %in% sheet_names){
  data_Youth_Males <- readxl::read_excel(cfg$source, sheet = "Performances_Youth_Males") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Youth_Males' does not exist in your data source!")
}
 
if("Performances_Youth_Females" %in% sheet_names){
  data_Youth_Females <- readxl::read_excel(cfg$source, sheet = "Performances_Youth_Females") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Youth_Females' does not exist in your data source!")
}

if("Performances_Crops" %in% sheet_names){
  data_Crops <- readxl::read_excel(cfg$source, sheet = "Performances_Crops") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Crops' does not exist in your data source!")
}

if("Performances_Animals" %in% sheet_names){
  data_Animals <- readxl::read_excel(cfg$source, sheet = "Performances_Animals") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Animals' does not exist in your data source!")
}

if("Performances_Chemical_Pesticide" %in% sheet_names){
  data_Chemical_Pesticide <- readxl::read_excel(cfg$source, sheet = "Performances_Chemical_Pesticide") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Chemical_Pesticide' does not exist in your data source!")
}

if("Performances_Organic_Pesticides" %in% sheet_names){
  data_Organic_Pesticides <- readxl::read_excel(cfg$source, sheet = "Performances_Organic_Pesticides") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Organic_Pesticides' does not exist in your data source!")
}

if("Performances_Crop_Products" %in% sheet_names){
  data_Crop_Products <- readxl::read_excel(cfg$source, sheet = "Performances_Crop_Products") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Crop_Products' does not exist in your data source!")
}

if("Performances_Activities" %in% sheet_names){
  data_Activities <- readxl::read_excel(cfg$source, sheet = "Performances_Activities") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Activities' does not exist in your data source!")
}

if("Performances_Machines" %in% sheet_names){
  data_Machines <- readxl::read_excel(cfg$source, sheet = "Performances_Machines") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Machines' does not exist in your data source!")
}

if("Performances_Animal_Products" %in% sheet_names){
  data_Animal_Products <- readxl::read_excel(cfg$source, sheet = "Performances_Animal_Products") %>%
    rename(key = farm_id)
} else {
  print("Sheet 'Performances_Animal_Products' does not exist in your data source!")
}

## Extra Sheets if questionnaire for Improved Biodiversity Index was filled out
# Only import this if "Biodiversity" is selected in config file
if (cfg$Indicator == "Biodiversity") {
  
  # Semi-natural habitats
  if("snh_tab" %in% sheet_names){
    data_snh <- readxl::read_excel(cfg$source, sheet = "snh_tab") %>%
      rename(key = farm_id)
  } else {
    cat("Sheet 'snh_tab' does not exist in your data source! \nCheck writing of the sheet or if you really want to calculate the improved Biodiversity index!\n")
  }
  
  # Trees
  if("tree_tab" %in% sheet_names){
    data_tree <- readxl::read_excel(cfg$source, sheet = "tree_tab") %>%
      rename(key = farm_id)
  } else {
    cat("Sheet 'tree_tab' does not exist in your data source! \nCheck writing of the sheet or if you really want to calculate the improved Biodiversity index!\n")
  }
}

# Data cleaning
source('Sourced code/Tape_cleaning_extra_sheets.R')
cat("Data imported!\n")
