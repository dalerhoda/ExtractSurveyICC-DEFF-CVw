# This program demonstrates running the iccloop program four times
# for TT, four times for SIA, and four times for RI.
#
# Dale Rhoda & Caitlin Clary
# Biostat Global Consulting
# May 21, 2020
#
# This demo file works with .csv input datasets
#
# Dale.Rhoda@biostatglobal.com
# Caitlin.Clary@biostatglobal.com

#########################################

# Install required packages if they are not already installed
if("tidyverse" %in% rownames(installed.packages()) == FALSE){
  install.packages("tidyverse")}
if("doBy" %in% rownames(installed.packages()) == FALSE){
  install.packages("doBy")}
if("survey" %in% rownames(installed.packages()) == FALSE){
  install.packages("survey")
}

# Load packages
library(tidyverse)
library(survey)
library(doBy)

# Set paths

# NOTE: If you opened the R Project called "Harmonia_Demo_CSV" in the CSV Demo folder, you should not need to change any paths.
# If you are not working inside the R Project, uncomment and edit the setwd() line below to set your working directory to be the location of the CSV Demo folder on your computer.

#setwd("C:/Downloads/CSV Demo")

# Load calculation functions
source("Programs/iccloop.R")
source("Programs/calc_icc.R")
source("Programs/opplot.R")

# Set file paths to the TT, SIA (MR), and RI datasets
basepath <- getwd()

TTpath <- paste0(basepath, "/tt_example_dataset_Harmonia.csv")
SIApath <- paste0(basepath, "/sia_example_dataset_Harmonia.csv")
RIpath <- paste0(basepath, "/ri_example_dataset_Harmonia.csv")

## TT ----

# Read data into R

TTdata <- read.csv(TTpath) %>%
  mutate(urban_cluster_name = ifelse(urban_cluster == 0, "Rural", "Urban"))

# Run iccloop four times, with different stratification variables

# For our first run, we specify replace = TRUE to start with a clean output dataset
iccloop(
  # Required arguments (no default values)
  id = 1,
  data = TTdata,
  datapath = TTpath,
  outcome = "protected_at_birth_to_analyze",
  stratvar = "level1id",
  output = "tt_iccs",
  # Arguments with default values
  stratlevel = 1,                          # default value = NA
  stratnamevar = "level1name",             # default value = NA
  row = 3,                                 # default value = NA
  stratumidvar = "stratumid",              # default value = "stratumid"
  clustvar = "clusterid",                  # default value = "clusterid"
  weightvar = "psweight_1year",            # default value = "psweight"
  selfweighted = FALSE,                    # default value = FALSE
  agegroup = "Gave birth in the last 12m", # default value = ""
  replace = TRUE,                          # default value = FALSE
  comment = ""                             # default value = ""
)
# For the arguments with default values, you can omit any arguments where the value in your dataset is the same as the default value. In the above example, we could have left out the stratumidvar, clustvar, selfweighted, and comment arguments. In the function below, we leave out optional arguments where we want to use the default value.
# Note that replace will be FALSE in the next call to iccloop (below), because we want to start appending results in the tt_iccs output dataset.

# Now stratify by level2id
iccloop(
  id = 1,
  data = TTdata,
  datapath = TTpath,
  outcome = "protected_at_birth_to_analyze",
  stratvar = "level2id",
  output = "tt_iccs",
  stratlevel = 2,
  stratnamevar = "level2name",
  row = 3,
  weightvar = "psweight_1year",
  agegroup = "Gave birth in the last 12m"
)

# Stratifying by level3id
iccloop(
  id = 1,
  data = TTdata,
  datapath = TTpath,
  outcome = "protected_at_birth_to_analyze",
  stratvar = "level3id",
  output = "tt_iccs",
  stratlevel = 3,
  stratnamevar = "level3name",
  row = 3,
  weightvar = "psweight_1year",
  agegroup = "Gave birth in the last 12m"
)

# Stratifying by urban_cluster
iccloop(
  id = 1,
  data = TTdata,
  datapath = TTpath,
  outcome = "protected_at_birth_to_analyze",
  stratvar = "urban_cluster",
  stratnamevar = "urban_cluster_name",
  output = "tt_iccs",
  stratlevel = 2,
  row = 3,
  weightvar = "psweight_1year",
  agegroup = "Gave birth in the last 12m"
)

## SIA ----
SIAdata <- read.csv(SIApath) %>%
  mutate(urban_cluster_name = ifelse(urban_cluster == 0, "Rural", "Urban"))

iccloop(
  id = 1,
  data = SIAdata,
  datapath = SIApath,
  outcome = "got_sia_dose",
  stratvar = "level1id",
  output = "sia_iccs",
  stratlevel = 1,
  stratnamevar = "level1name",
  row = 2,
  agegroup = "6-59m",
  comment = "MR",
  replace = TRUE
)

iccloop(
  id = 1,
  data = SIAdata,
  datapath = SIApath,
  outcome = "got_sia_dose",
  stratvar = "level2id",
  output = "sia_iccs",
  stratlevel = 2,
  stratnamevar = "level2name",
  row = 2,
  agegroup = "6-59m",
  comment = "MR"
)

iccloop(
  id = 1,
  data = SIAdata,
  datapath = SIApath,
  outcome = "got_sia_dose",
  stratvar = "level3id",
  output = "sia_iccs",
  stratlevel = 3,
  stratnamevar = "level3name",
  row = 2,
  agegroup = "6-59m",
  comment = "MR"
)

iccloop(
  id = 1,
  data = SIAdata,
  datapath = SIApath,
  outcome = "got_sia_dose",
  stratvar = "urban_cluster",
  output = "sia_iccs",
  stratlevel = 2,
  stratnamevar = "urban_cluster_name",
  row = 2,
  agegroup = "6-59m",
  comment = "MR"
)

## RI ----

RIdata <- read.csv(RIpath) %>%
  mutate(urban_cluster_name = ifelse(urban_cluster == 0, "Rural", "Urban"))

iccloop(
  id = 1,
  data = RIdata,
  datapath = RIpath,
  outcome = c("had_card", "got_crude_bcg_to_analyze", "got_crude_penta1_to_analyze", "got_crude_penta3_to_analyze", "got_crude_mcv1_to_analyze"),
  stratvar = "level1id",
  output = "ri_iccs",
  stratlevel = 1,
  stratnamevar = "level1name",
  row = 1,
  agegroup = "12-23m",
  replace = TRUE
  )

iccloop(
  id = 1,
  data = RIdata,
  datapath = RIpath,
  outcome = c("had_card", "got_crude_bcg_to_analyze", "got_crude_penta1_to_analyze", "got_crude_penta3_to_analyze", "got_crude_mcv1_to_analyze"),
  stratvar = "level2id",
  output = "ri_iccs",
  stratlevel = 2,
  stratnamevar = "level2name",
  row = 1,
  agegroup = "12-23m"
  )

iccloop(
  id = 1,
  data = RIdata,
  datapath = RIpath,
  outcome = c("had_card", "got_crude_bcg_to_analyze", "got_crude_penta1_to_analyze", "got_crude_penta3_to_analyze", "got_crude_mcv1_to_analyze"),
  stratvar = "level3id",
  output = "ri_iccs",
  stratlevel = 3,
  stratnamevar = "level3name",
  row = 1,
  agegroup = "12-23m"
)

iccloop(
  id = 1,
  data = RIdata,
  datapath = RIpath,
  outcome = c("had_card", "got_crude_bcg_to_analyze", "got_crude_penta1_to_analyze", "got_crude_penta3_to_analyze", "got_crude_mcv1_to_analyze"),
  stratvar = "urban_cluster",
  output = "ri_iccs",
  stratlevel = 2,
  stratnamevar = "urban_cluster_name",
  row = 1,
  agegroup = "12-23m"
)

# Finally, we can combine the ri, sia, and tt datasets
ri <- readRDS("ri_iccs.rds")
sia <- readRDS("sia_iccs.rds")
tt <- readRDS("tt_iccs.rds")

harmonia_iccs <- rbind(ri, sia, tt)

saveRDS(harmonia_iccs, "Harmonia_ICCs.rds")

# Save the ICC dataset as a .csv also
write.csv(harmonia_iccs, "Harmonia_ICCs.csv")

## Save a basic plot

# Create grouping variables with helpful labels
harmonia_iccs <- harmonia_iccs %>%
  mutate(whichsvy = ifelse(unique_survey_row_id %in% 1, "RI",
                           ifelse(unique_survey_row_id %in% 2, "SIA", "TT")),
         whichstrat = ifelse(stratvar %in% "level3id", "District",
                             ifelse(stratvar %in% "level1id", "National",
                                    ifelse(stratvar %in% "level2id", "Province",
                                           "Urban/Rural"))))

 harmonia_plot <- ggplot(harmonia_iccs, aes(x = p, y = icc)) +
  labs(title = "Harmonia ICCs",
       caption = "Red lines appear at 1/6 and 1/3") +
  geom_hline(aes(yintercept = 1/6), color = "red") +
  geom_hline(aes(yintercept = 1/3), color = "red") +
   facet_wrap("whichstrat", ncol = 2) +
  geom_segment(aes(x = p, xend = p, y = icc_lb, yend = icc_ub), color = "#365d80") +
  scale_color_manual(values = c("#90353b", "#55752f", "#e37e00")) +
  geom_point(aes(color = whichsvy)) +
  ylim(c(min(harmonia_iccs$icc_lb, na.rm = TRUE), 
         max(harmonia_iccs$icc_ub, na.rm = TRUE))) + 
  xlab("Estimated Coverage (%)") +
  ylab("ICC") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        strip.background = element_rect(fill="#d9e6eb", color = NA),
        strip.text = element_text(size = 10))

ggsave("Harmonia_ICCs.png",
       harmonia_plot, width = 8, height = 5, units = "in")
