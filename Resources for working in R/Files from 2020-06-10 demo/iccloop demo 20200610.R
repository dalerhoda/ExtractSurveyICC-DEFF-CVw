## Modification of the Harmonia demonstration script
## Used in R demonstration meeting, 10 June 2020

# This program demonstrates running the iccloop program on faux data 
#
# Dale Rhoda & Caitlin Clary
# Biostat Global Consulting
# May 21, 2020
#
# Dale.Rhoda@biostatglobal.com
# Caitlin.Clary@biostatglobal.com

#########################################

# Install required packages if they are not already installed
if("haven" %in% rownames(installed.packages()) == FALSE){
  install.packages("haven")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE){
  install.packages("tidyverse")}
if("doBy" %in% rownames(installed.packages()) == FALSE){
  install.packages("doBy")}
if("survey" %in% rownames(installed.packages()) == FALSE){
  install.packages("survey")}

# Load packages
library(tidyverse)
library(haven)
library(survey)
library(doBy)

# Set paths

# NOTE: If you opened the R Project called "Extract ICCs" in the demo folder, you should not need to change any paths.
# If you are not working inside the R Project, uncomment and edit the setwd() line below to set your working directory to be the location of the Demo folder on your computer. 

# setwd("C:/Downloads/Demo")

# Load calculation functions
source("Programs/iccloop.R")
source("Programs/calc_icc.R")
source("Programs/opplot.R")

# Set file paths 
basepath <- getwd()

SIApath <- paste0(basepath, "/Oz_SIA_MR.rds")

# Read data into R

my_data <- readRDS(SIApath)

# Run iccloop 

my_data$level1name <- "Oz"

# For our first run, we specify replace = TRUE to start with a clean output dataset
iccloop(
  # Required arguments (no default values)
  id = 0,
  data = my_data,
  datapath = SIApath,
  outcome = "got_sia_dose",
  stratvar = "level1id",
  output = "sia_iccs",
  # Arguments with default values
  stratlevel = 1,                          # default value = NA
  stratnamevar = "level1name",             # default value = NA
  row = 0,                                 # default value = NA
  stratumidvar = "strat",                  # default value = "stratumid"
  clustvar = "clust",                      # default value = "clusterid"
  weightvar = "weight",                    # default value = "psweight"
  selfweighted = FALSE,                    # default value = FALSE
  agegroup = "6-59m",                      # default value = ""
  replace = TRUE,                          # default value = FALSE
  comment = "MR"                           # default value = ""
)
# For the arguments with default values, you can omit any arguments where the value in your dataset is the same as the default value. In the above example, we could have left out the stratumidvar, clustvar, selfweighted, and comment arguments. In the function below, we leave out optional arguments where we want to use the default value. 
# Note that replace will be FALSE in the next call to iccloop (below), because we want to start appending results in the sia_iccs output dataset.

iccloop(
  # Required arguments (no default values)
  id = 0,
  data = my_data,
  datapath = SIApath,
  outcome = "got_sia_dose",
  stratvar = "urban_rural",
  output = "sia_iccs",
  # Arguments with default values
  stratlevel = 2,                          # default value = NA
  row = 0,                                 # default value = NA
  stratumidvar = "strat",                  # default value = "stratumid"
  clustvar = "clust",                      # default value = "clusterid"
  weightvar = "weight",                    # default value = "psweight"
  selfweighted = FALSE,                    # default value = FALSE
  agegroup = "6-59m",                      # default value = ""
  replace = FALSE,                          # default value = FALSE
  comment = "MR"                           # default value = ""
)

