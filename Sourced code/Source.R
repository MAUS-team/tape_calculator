#clear the environement
rm(list = ls())

# Declare necessary packages
pkgs_cran <- c('readxl', 'openxlsx', 'tidyverse', 'fmsb', 'svDialogs', 'reshape2', 'cowplot', 'ggpubr', 'rstatix', 'RobustLinearReg','car','GGally','corrplot','scatterplot3d')


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






data <- readxl::read_excel(cfg$source, sheet = 1)
data <- data %>%
  mutate(key = 1:n())

#-----------------------------------------------------------------
# read extra excel sheets
#-----------------------------------------------------------------

data_youngsters <- readxl::read_excel(cfg$source, sheet = "youngsters") 
data_c1 <- readxl::read_excel(cfg$source, sheet = "c1") 
data_a1 <- readxl::read_excel(cfg$source, sheet = "a1") 
data_snh <- readxl::read_excel(cfg$source, sheet = 'snh_tab') 
data_tree <- readxl::read_excel(cfg$source, sheet = 'tree_tab') 
data_cp <- readxl::read_excel(cfg$source, sheet = 'cp') 
data_co <- readxl::read_excel(cfg$source, sheet = 'co') 
data_cfp <- readxl::read_excel(cfg$source, sheet = "cfp") 
data_ext_workers <- readxl::read_excel(cfg$source, sheet = 'ext_workers') 
data_m <- readxl::read_excel(cfg$source, sheet = "m")
data_ap <- readxl::read_excel(cfg$source, sheet = "ap")

cat("Data imported!\n")
