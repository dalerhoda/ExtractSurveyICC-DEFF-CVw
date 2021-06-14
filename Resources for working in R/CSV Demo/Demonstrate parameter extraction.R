# This program performs all of the steps accomplished in the iccloop.R program
# for one analysis - of rural clusters in Harmonia - for one outcome 
# (got_crude_penta3_to_analyze) in the RI example dataset.

# The curious user can run this program line-by-line to see what iccloop is
# doing inside as it loops through the dataset

# This demo file works with .csv input datasets

# Dale Rhoda & Caitlin Clary
# Biostat Global Consulting
# May 21, 2020

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
library(doBy)
library(survey)

# Set paths

# NOTE: If you opened the R Project called "Harmonia_Demo_CSV" in the CSV Demo folder, you should not need to change any paths.
# If you are not working inside the R Project, uncomment and edit the setwd() line below to set your working directory to be the location of the CSV Demo folder on your computer. 

#setwd("C:/Downloads/CSV Demo")

# Load calculation functions
source("Programs/iccloop.R")
source("Programs/calc_icc.R")
source("Programs/opplot.R")

# Path to the dataset on your computer
datapath <- paste0(getwd(),"/ri_example_dataset_Harmonia.csv")

# Load data, and name the dataset Harmonia_RI.
# For convenience, create a labelled version of the urban cluster variable.
Harmonia_RI <- read.csv(datapath) %>%
  mutate(urban_cluster_name = ifelse(urban_cluster == 0, "Rural", "Urban"))

## If we were to run iccloop.R instead of manually doing calculations, our syntax would look something like this:
#     iccloop(
#       id = 0,
#       data = Harmonia_RI,
#       datapath = datapath,
#       outcome = c("got_crude_penta3_to_analyze"),
#       stratvar = "urban_cluster",
#       output = "Harmonia_Demo",
#       stratlevel = 2,
#       stratnamevar = "urban_cluster_name",
#       row = 0,
#       stratumidvar = "stratumid",
#       clustvar = "clusterid",
#       weightvar = "psweight",
#       selfweighted = FALSE,
#       agegroup = "12-23m",
#       replace = TRUE,
#       comment = "")

id <- 1
row <- 1
stratvar <- "urban_cluster" # Stratification variable: urban/rural
stratlevel <- 2 # Subnational results 
agegroup <- "12-23m"
comment <- ""
outcome <- "got_crude_penta3_to_analyze" # Outcome variable for calculations 

# The R process for using stratvar and stratnamevar (if both are provided) is to convert stratnamevar into a factor, ordered by stratvar. Here we do that with urban_cluster and urban_cluster_name.

ordered_stratname <- Harmonia_RI %>%
  arrange(urban_cluster) %>%
  select(urban_cluster_name) %>%
  unique

Harmonia_RI <- Harmonia_RI %>%
  mutate(
    urban_cluster_name = factor(urban_cluster_name, levels = ordered_stratname$urban_cluster_name)
  )

# Filter the dataset to keep only respondents from rural clusters
strat_data <- Harmonia_RI %>%
  filter(urban_cluster %in% 0)

# Calculate some summary statistics for the rural stratum

# Number of clusters in this stratum
nclust <- length(unique(strat_data$clusterid))

# Number of strata represented in this subset
nstrat <- length(unique(strat_data$stratumid))

# Average respondents per cluster 
respondents_cluster_mean <- nrow(strat_data[!is.na(strat_data$got_crude_penta3_to_analyze),])/nclust

# Standard deviation of respondents per cluster
respondents_cluster_sd <- strat_data %>% group_by(clusterid) %>% summarize(m_avg = n()) %>% summarize(sd(m_avg)) %>% as.numeric

# Minimum respondents in a cluster
respondents_cluster_min <- strat_data %>% group_by(clusterid) %>% summarize(m_avg = n()) %>% summarize(min(m_avg)) %>% as.numeric 

# Maximum respondents in a cluster
respondents_cluster_max <- strat_data %>% group_by(clusterid) %>% summarize(m_avg = n()) %>% summarize(max(m_avg)) %>% as.numeric

# Group by cluster and calculate cluster summary quantities: number of clusters with 100% coverage and number of clusters with 0% coverage
grouped_dat <- strat_data %>%
  group_by(clusterid) %>%
  summarize(cluster_n = n(),
            clustprop = sum(got_crude_penta3_to_analyze, na.rm = TRUE)/n())

# Number of clusters with 100% coverage and 0% coverage
ncp100 <- nrow(grouped_dat[grouped_dat$clustprop == 1,])
ncp0 <- nrow(grouped_dat[grouped_dat$clustprop == 0,])

# Specify the survey design (with weights and stratification)
survey_design <- svydesign(ids = ~clusterid,
                           weights = ~psweight,
                           strata = ~stratumid,
                           data = strat_data)

# Given design, calculate proportion, standard error, and design effect
p_calc <- survey::svymean(~got_crude_penta3_to_analyze,
                            design = survey_design,
                            deff = "replace",
                            df = degf(survey_design))

# Proportion estimate
p_j <- as.numeric(coef(p_calc))

# Standard error estimate
se_j <- as.numeric(SE(p_calc))

# Design effect estimate
deff_j <- as.numeric(deff(p_calc))

# If the calculated design effect is less than 1, we would truncate at 1
if(deff_j < 1){
  deff_j <- 1
}

# The effective sample size is n/DEFF
neff_j <- nrow(strat_data)/deff_j

# Confidence intervals ----

# Set alpha to obtain 95% confidence intervals 
alpha <- 0.05

# Adjusted effective sample size using Dean & Pagano (2015) formulas
neff_dp <- p_j*(1-p_j)/(se_j^2)
adj_neff_j <- neff_dp * ((qnorm(1-alpha/2)/qt(1-alpha/2, df = degf(survey_design)))^2)

# Quantile of the standard normal distribution z(1-a/2)
zquant <- qnorm(p = (1 - alpha/2))

# Wilson confidence interval ----

numerator_lh <- p_j + zquant^2/(2*adj_neff_j)
numerator_rh <- zquant * sqrt((p_j * (1-p_j))/adj_neff_j + zquant^2/((2*adj_neff_j)^2))
denominator <- 1 + zquant^2/adj_neff_j

p_lb_wilson <- ((numerator_lh - numerator_rh)/denominator)
p_ub_wilson <- ((numerator_lh + numerator_rh)/denominator)

# Logit confidence interval ----

logit_l <- (log(p_j/(1-p_j)) - zquant*(adj_neff_j * p_j * (1-p_j))^(-1/2))
logit_u <- (log(p_j/(1-p_j)) + zquant*(adj_neff_j * p_j * (1-p_j))^(-1/2))

p_lb_logit <- exp(logit_l)/(1 + exp(logit_l))
p_ub_logit <- exp(logit_u)/(1 + exp(logit_u))

# Agresti-Coull confidence interval ----

c <- (qnorm(1 - alpha/2)^2)/2
x_tilde <- p_j * adj_neff_j + c
n_tilde <- adj_neff_j + 2*c
p_tilde <- x_tilde/n_tilde

p_lb_agresti <- p_tilde - qnorm(1 - alpha/2) * sqrt((p_tilde * (1-p_tilde))/n_tilde)
p_ub_agresti <- p_tilde + qnorm(1 - alpha/2) * sqrt((p_tilde * (1-p_tilde))/n_tilde)

# Clopper-Pearson confidence interval ----

if(deff_j == 1){
  neff_cp <- neff_j
} else {
  neff_cp <- adj_neff_j
}

cp_x <- p_j * neff_cp  # successes

v1 <- 2 * cp_x
v2 <- 2 * (neff_cp - cp_x + 1)
v3 <- 2 * (cp_x + 1)
v4 <- 2 * (neff_cp - cp_x)

if(cp_x == 0){
  p_lb_clopper <- 0
  p_ub_clopper <- (v3 * qf(1-alpha/2, v3, v4))/(v4 + (v3 * qf(1-alpha/2, v3, v4)))
} else if(cp_x == nrow(strat_data)) {
  p_lb_clopper <- (v1 * qf(alpha/2, v1, v2))/(v2 + (v1 * qf(alpha/2, v1, v2)))
  p_ub_clopper <- 1
} else {
  p_lb_clopper <- (v1 * qf(alpha/2, v1, v2))/(v2 + (v1 * qf(alpha/2, v1, v2)))
  p_ub_clopper <- (v3 * qf(1-alpha/2, v3, v4))/(v4 + (v3 * qf(1-alpha/2, v3, v4)))
}

# Fleiss 

term1l <- 2 * adj_neff_j * p_j + zquant^2 - 1
term2l <- zquant * sqrt(zquant^2 - (2 + (1/adj_neff_j)) + 4* p_j * (adj_neff_j * (1-p_j) + 1))
term3 <- 2 * (adj_neff_j + zquant^2)

term1u <- term1l + 2
term2u <- zquant * sqrt(zquant^2 + (2 - (1/adj_neff_j)) + 4* p_j * (adj_neff_j * (1-p_j) - 1))

p_lb_fleiss <- (term1l - term2l)/term3
p_ub_fleiss <- (term1u + term2u)/term3

# Jeffreys 

jeffreys_successes <- p_j * adj_neff_j
alpha_shape <- jeffreys_successes + 0.5
beta_shape <- adj_neff_j - jeffreys_successes + 0.5

p_lb_jeffreys <- qbeta(alpha/2, alpha_shape, beta_shape)
p_ub_jeffreys <- qbeta((1-alpha/2), alpha_shape, beta_shape)

# Wald 

tquant <- qt(p = (1 - alpha/2), df = (nclust-nstrat))
p_lb_wald <- p_j - tquant*se_j
p_ub_wald <- p_j + tquant*se_j

# We use the Wilson interval as our default CI

lb_j <- p_lb_wilson
ub_j <- p_ub_wilson
ci_meth <- "Wilson"

# Define a survey design object *without* weights and stratification
design_nostrat_nowt <- survey::svydesign(ids = ~clusterid,
                                         data = strat_data)

# Given no-weights, no-stratification design, calculate design effect
nostrat_nowt_calc <- svymean(~got_crude_penta3_to_analyze,
                               design = design_nostrat_nowt,
                               deff = "replace")

deff_ns_nw <- as.numeric(deff(nostrat_nowt_calc))

# Coefficient of variation on weights (sd(weightvar)/mean(weightvar))
cvw_j <- sd(strat_data$psweight, na.rm = TRUE)/mean(strat_data$psweight, na.rm = TRUE)

# Use calc_icc.R function to get ICC and confidence interval 
# We bootstrap the ICC confidence interval with 1000 replications; expect this calculation to take a few seconds to run. 

icc_calc_j <- calc_icc(
  cid = "clusterid",
  y = "got_crude_penta3_to_analyze",
  data = strat_data,
  method = "aov",
  median = FALSE,
  ci.type = "bootstrap",
  ci_bootrep = 1000,
  alpha = 0.05
)

## More details on the ICC calculation (done inside calc_icc)

cid <- strat_data$clusterid                 # clusters
y <- strat_data$got_crude_penta3_to_analyze # response variable 
k <- length(unique(cid))                    # number of clusters
ni <- as.vector(table(cid))                 # n observations in each cluster 
N <- sum(ni)                                # total observations 

n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
yi <- aggregate(y, by = list(cid), sum)[ , 2]
yisq <- yi^2
msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))

icc_anova <- (msb - msw)/(msb + (n0 - 1)*msw)

# Bootstrapped confidence intervals are generated by sampling clusters (with replacement) proportionally from each stratum, saving the estimate from each of the 1000 bootstrapped datasets, and then fetching the 2.5% and 97.5% quantiles. 

##

icc_j <- icc_calc_j$estimates$ICC
icc_j_lb <- icc_calc_j$ci$LowerCI
icc_j_ub <- icc_calc_j$ci$UpperCI

icc_ci_type <- icc_calc_j$ci$Type

out_msb <- icc_calc_j$ms$MSB
out_msw <- icc_calc_j$ms$MSW


# Make opplot folder if it doesn't exist
if(dir.exists(paste0(dirname(datapath), "/Plots_OP_R"))==FALSE){
  dir.create(paste0(dirname(datapath), "/Plots_OP_R"))
}

opplot(
  dat = as.data.frame(strat_data),
  stratvar = "urban_cluster_name",
  clustvar = "clusterid",
  yvar = "got_crude_penta3_to_analyze",
  stratum = levels(strat_data$urban_cluster_name)[1],
  title = paste0(levels(strat_data$urban_cluster_name)[1], " - got_crude_penta3_to_analyze"),
  plotn = TRUE,
  barcolor1 = alpha("blue", 0.5),
  barcolor2 = "white",
  linecolor2 = "white",
  nlinecolor = "red",
  nlinepattern = 2,
  output_to_screen = FALSE,
  platform = "png", 
  filename = paste0(dirname(datapath), "/Plots_OP_R/", "urban_cluster", "_", "1", "_", "got_crude_penta3_to_analyze")
)

# Calculated weighted average cluster size 
n_complete <- nrow(strat_data[!is.na(strat_data$got_crude_penta3_to_analyze),])

grouped_dat <- grouped_dat %>%
  mutate(summit = cumsum(cluster_n * (cluster_n/n_complete)))

m_avg_wtd <- (n_complete - last(grouped_dat$summit))/(nrow(grouped_dat)-1)

# If proportion is exactly 0 or 1, wipe out the CI bounds for Wald and Logit intervals (degenerate)
if(p_j==0 | p_j==1){
  p_lb_wald <- NA
  p_ub_wald <- NA
  p_lb_logit <- NA
  p_ub_logit <- NA
}

icc_dataset_row <- data.frame(
  survey_id = id,
  unique_survey_row_id = row,
  path_to_dataset = dirname(datapath),
  dataset_name = basename(datapath),
  agegroup = agegroup,
  outcome_name = outcome,
  stratum_level = stratlevel,
  stratvar = stratvar,
  stratum_id = 1, # Rural is the first level of stratvar 
  stratum_name = as.character(levels(strat_data$urban_cluster_name)[1]),
  path_to_opplot = "/Plots_OP_R",
  opplot_name = "urban_cluster_1_got_crude_penta3_to_analyze.png",
  n_clusters = nclust,
  n_strata = nstrat,
  n = nrow(strat_data),
  m_avg = respondents_cluster_mean,
  m_avg_wtd = m_avg_wtd,
  m_sd = respondents_cluster_sd,
  m_min = respondents_cluster_min,
  m_max = respondents_cluster_max,
  ncp100 = ncp100,
  ncp0 = ncp0,
  p = p_j * 100,
  p_se = se_j,
  p_lb = lb_j * 100,
  p_ub = ub_j * 100,
  ci_method = ci_meth,
  deff = deff_j,
  neff = nrow(strat_data)/deff_j,
  deff_nostrat_nowt = deff_ns_nw,
  cvw = cvw_j,
  icc = icc_j,
  icc_lb = icc_j_lb,
  icc_ub = icc_j_ub,
  icc_ci_method = icc_ci_type,
  ms_between = out_msb,
  ms_within = out_msw,
  comment = comment,
  p_lb_logit = p_lb_logit * 100,
  p_ub_logit = p_ub_logit * 100,
  p_lb_agresti = p_lb_agresti * 100,
  p_ub_agresti = p_ub_agresti * 100,
  p_lb_clopper = p_lb_clopper * 100,
  p_ub_clopper = p_ub_clopper * 100,
  p_lb_fleiss = p_lb_fleiss * 100,
  p_ub_fleiss = p_ub_fleiss * 100,
  p_lb_jeffreys = p_lb_jeffreys * 100,
  p_ub_jeffreys = p_ub_jeffreys * 100,
  p_lb_wald = p_lb_wald * 100,
  p_ub_wald = p_ub_wald * 100,
  p_lb_wilson = p_lb_wilson * 100,
  p_ub_wilson = p_ub_wilson * 100)

