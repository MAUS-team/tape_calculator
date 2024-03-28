# TAPE Tool for Agroecology Performance Evaluation

Welcome to the GitHub repository for the TAPE (Tool for Agroecology Performance Evaluation) tool. This tool, developed by the Food and Agriculture Organization (FAO), is designed to assess transitions towards sustainable agricultural and food systems. We therefore expect that you have downloaded the data from the KoboToolbox as an Excel-file with XLM values and headers.

The TAPE tool is a powerful instrument that allows users to calculate various parameters related to agroecology performance. It consists of three main components:

1.  **Main Script**: This is where you can select the parameters you wish to calculate. It serves as the primary interface for interacting with the tool.

2.  **Functions File**: This file contains the calculations for each parameter. It's the engine that powers the tool, performing all the necessary computations based on your selections in the Main script. This file is found in the folder "Sourced code"

3.  **config.yml**: This is where you can enter the location of the file(s) your draw your data from, as well as set diffferent configurations for the main script.

4.  **Visualise.R**: Here you find some plot options commonly used with the calculated TAPE data.

This repository contains all the code necessary to run these scripts and use the TAPE tool. Whether you're a researcher studying sustainable agriculture, a policy maker looking for data to inform decisions, or simply someone interested in agroecology, this tool can provide valuable insights.

TAPE is constantly evolving. The version of the tool in this script is of the year 2023 and has slight changes compared to older versions (different variable names, new data source file). The older version of the script (2022 version) can be found under "Releases". The methodology for the new biodiversity index is described in the paper: How to assess the agroecological status of Swiss farming systems?, Gilgen et al., 2023 (<https://doi.org/10.34776/as172e>)

Stay tuned for more updates and improvements to the TAPE tool as we continue our mission of promoting sustainable agriculture and food systems.

### How to use this repository

#### config.yml

First you open the **config.yml** file. There you enter the path to where you have saved the excel file. Other configurations can be met here too, like changes to the Livestock-Unit and the choice of a different versions of the Biodiversity Indicator ("Agrobiodiversity" for the standard TAPE agrobiodiversity index or "Biodiversity" for the improved biodiversity index described by Gilgen et al. (2023)). To apply the changes made you have to save the **config.yml** file. Right now there are different pre-defined international Regions with their respective Livestock-units (taken from: Guidelines for the preparation of livestock sector reviews - FAO, Annex 1, <https://www.fao.org/3/i2294e/i2294e00.pdf>). The livestock units can be changed manually as well.

```         

default:
  source: "DATA/TAPE_Prosoils sample data.xlsx"
  lsucv_cattle: 0.5
  lsucv_buffalo: 0.6
  lsucv_sheep: 0.1
  lsucv_pig: 0.2
  lsucv_horses: 0.5
  lsucv_camels: 0.75
  Indicator: "Agrobiodiversity" # or "Biodiversity" if you want to calculate the improved Biodiversity index
  
 Near_East_North_Africa:
  source: "DATA/TAPE_Prosoils sample data.xlsx"
  lsucv_cattle: 0.7
  lsucv_buffalo: 0.7
  lsucv_sheep: 0.1
  lsucv_pig: 0.2
  lsucv_horses: 0.4
  lsucv_camels: 0.75
  Indicator: "Biodiversity"
  
```

#### Main script

Here you first you would choose the configuration and run this code. I will chose the default yml configuration.

```         

#load the config file and choose the option best suited for the data
Sys.setenv(R_CONFIG_ACTIVE = "OECD")
```

Here we chose the configuration "OECD" we made in the **config.yml** file. The second step is to source the Source.R file. This will install and load all the libraries needed for the functions. Additionally it loads the excel file with all its sheets to the R Environment.

After this the functions can be used as pleased to calculate the different scores.

As an example here the **caet_CAET()** function gets executed and saves a data frame called CAET with the corresponding score and its sub scores.

```         

## Calculate CAET

CAET <- caet_CAET()
```
