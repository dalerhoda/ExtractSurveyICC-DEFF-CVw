# Program: iccloop.R
# For selected strata and outcomes, calculate ICC and other quantities of interest
# Author: Caitlin B. Clary, Biostat Global Consulting

## Change log
# 2020-05-05    Original
# 2020-05-07    Correct survey design specification
#               Adjustments to organ pipe plots (colors, weights)
# 2020-05-19    Dean and Pagano formulas for other CIs,
#               n_strata, m_avg_wtd, selfweighted argument,
#               Different handling of data import
# 2020-05-20    Change neff calculation for CIs (Dean & Pagano p487)
# 2020-05-22    Add stratnamevar option
# 2020-05-27    Increase bootstrap ICC CI replications to 1000,
#               Wilson confidence interval as default for p-hat
# 2020-05-28    Make robust to missing values in outcome variable
#               Adjust calculations (n, ncp0, ncp100, neff, cvw, m_avg,
#               m_avg_wtd, adj. neff & df for CIs) to match iccloop.ado
# 2020-06-10    Add weights to opplot, edit to selfweighted=TRUE handling
#               Stop truncating DEFF (and nostrat nowt DEFF) at 1
#               Do truncate DEFF when coverage is homogenous; set flag

## Dependencies:
#   Packages: tidyverse, survey, haven, doBy
#   Functions: calc_icc.R, opplot.R

iccloop <- function(

  # Arguments without default values: required

  id,           # survey ID from BGC study spreadsheet
  data,         # name of dataset to use (should be loaded in R)
  datapath,     # file path to dataset (string); Plots_OP_R folder will be created in the same folder as the dataset
  outcome,      # list of outcome variables; provide in quotes, c() if multiple
  stratvar,     # stratification variable e.g. level1id
  output,       # name of dataset where ICCs will be saved (string)

  # Arguments with default values: optional

  stratlevel = NA,    # 1 = national, 2 = subnational, 3 = subsubnational
  stratnamevar = NA,  # variable with names corresponding to levels of stratvar
  row = NA,           # row number from BGC study spreadsheet
  stratumidvar = "stratumid", # stratum id variable for survey design spec
  clustvar = "clusterid",     # cluster id variable for survey design spec
  weightvar = "psweight",     # weights variable for survey design spec
  selfweighted = FALSE,       # indicate if selfweighted (if TRUE, weightvar not used)
  agegroup = "",      # text comment: age group covered by this survey
  replace = FALSE,    # replace previous results (TRUE) or append (FALSE)
  comment = ""        # text comment to pass through

){

  # Stop if the user provided a weightvar other than the default and specified selfweighted = TRUE
  if(selfweighted == TRUE & weightvar != "psweight"){
    stop("The selfweighted=TRUE option was specified, but a weight variable was also provided. Either omit the weightvar argument, or specify selfweighted = FALSE.")
  }

  # Save the user's current setting for the survey.lonely.psu option in the survey package, to be restored at the end of the function
  save_user_survey_option <- options("survey.lonely.psu")

  # inside function, set survey design handling of lone PSUs
  options(survey.lonely.psu="adjust")

  in_data <- data

  # Create copies of stratvar, clustvar, weightvar, and stratumidvar (for easier reference in calculations)
  in_data$temp_stratvar <- get(stratvar, in_data)
  in_data$temp_clustvar <- get(clustvar, in_data)

  if(selfweighted == TRUE){
    in_data$temp_weightvar <- 1
  } else {
    in_data$temp_weightvar <- get(weightvar, in_data)}

  in_data$temp_stratumidvar <- get(stratumidvar, in_data)

  # If stratnamevar is provided, create a copy in the dataset
  if(!is.na(stratnamevar)){in_data$temp_stratnamevar <- get(stratnamevar, in_data)}

  ## Logic for using stratvar vs. stratnamevar
  # Only relevant if stratnamevar is provided
  if(!is.na(stratnamevar)){
    # Ensure stratvar and stratnamevar have the same number of levels
    if(length(unique(in_data$temp_stratvar)) != length(unique(in_data$temp_stratnamevar))){stop("The provided stratvar and stratnamevar have different numbers of levels.")
    }

    # If stratvar is numeric and stratnamevar is character or factor, order levels of stratnamevar by stratvar and then use stratnamevar as temp_stratvar
    if(is.numeric(in_data$temp_stratvar) & (is.character(in_data$temp_stratnamevar) | is.factor(in_data$temp_stratnamevar))){

      ordered_stratname <- in_data %>%
        arrange(temp_stratvar) %>%
        select(temp_stratnamevar) %>%
        unique

      in_data <- in_data %>%
        mutate(
          orig_stratvar = temp_stratvar,
          temp_stratvar = factor(temp_stratnamevar, levels = ordered_stratname$temp_stratnamevar)
        )
    } else {
      # If stratvar isn't numeric, just use stratnamevar as the stratification variable
      in_data <- in_data %>%
        mutate(
          orig_stratvar = temp_stratvar,
          temp_stratvar = temp_stratnamevar
        )
    }

  }

  # Now, ensure temp_stratvar is a factor
  in_data$temp_stratvar <- as.factor(in_data$temp_stratvar)

  ## Are clusters unique across strata or only within strata?
  cs_vector <- paste0(in_data$temp_stratvar, in_data$temp_clustvar)
  ## If not unique across strata, create a new clustvar as a combination of stratum and cluster IDs
  if(length(unique(cs_vector)) > length(unique(in_data$temp_clustvar))){
    in_data <- in_data %>%
      mutate(
        orig_clustvar = temp_clustvar,
        temp_clustvar = paste0(temp_stratvar, temp_clustvar)
      )
  }

  # How many outcomes were specified? (for indexing in loop)
  n_outcomes <- length(outcome)

  # How many levels in stratvar? (for indexing in loop)
  n_strat_levels <- length(unique(in_data$temp_stratvar))

  ## -- Begin stratum-outcome loops

  ## Loop through outcomes
  for(j in 1:n_outcomes){

    ## Loop through levels of stratvar
    for(i in 1:n_strat_levels){

      strat_data <- in_data %>%
        filter(temp_stratvar %in% levels(temp_stratvar)[i])

      # Calculate the number of clusters in this stratum
      nclust <- length(unique(strat_data$temp_clustvar))

      # Number of strata represented in this subset
      nstrat <- length(unique(strat_data$temp_stratumidvar))

      # Define the outcome variable of interest in this loop
      strat_data_j <- strat_data
      strat_data_j$outcome_j <- get(outcome[j], strat_data_j)

      # Average respondents per cluster
      respondents_cluster_mean <- nrow(strat_data_j[!is.na(strat_data_j$outcome_j),])/nclust

      # Standard deviation of respondents per cluster
      strat_data %>% group_by(temp_clustvar) %>% summarize(m_avg = n()) %>% summarize(sd(m_avg)) %>% as.numeric -> respondents_cluster_sd

      # Minimum respondents in a cluster
      strat_data %>% group_by(temp_clustvar) %>% summarize(m_avg = n()) %>% summarize(min(m_avg)) %>% as.numeric -> respondents_cluster_min

      # Maximum respondents in a cluster
      strat_data %>% group_by(temp_clustvar) %>% summarize(m_avg = n()) %>% summarize(max(m_avg)) %>% as.numeric -> respondents_cluster_max

      # Group by cluster and calculate cluster summary quantities
      grouped_j <- strat_data_j %>%
        group_by(temp_clustvar) %>%
        summarize(cluster_n = n(),
                  clustprop = sum(outcome_j, na.rm = TRUE)/n())

      ncp100 <- nrow(grouped_j[grouped_j$clustprop == 1,])
      ncp0 <- nrow(grouped_j[grouped_j$clustprop == 0,])

      # Define a survey design object (with weights and stratification)
      if(selfweighted == TRUE){
        design_j <- survey::svydesign(ids = ~temp_clustvar,
                                      strata = ~temp_stratumidvar,
                                      data = strat_data_j)
      } else {
      design_j <- survey::svydesign(ids = ~temp_clustvar,
                                    weights = ~temp_weightvar,
                                    strata = ~temp_stratumidvar,
                                    data = strat_data_j)
      }

      # Given survey design, calculate proportion, standard error, design effect, CI
      p_calc_j <- survey::svymean(~outcome_j,
                                  design = design_j,
                                  deff = "replace",
                                  df = degf(design_j),
                                  na.rm = TRUE)

      p_j <- as.numeric(coef(p_calc_j))
      se_j <- as.numeric(SE(p_calc_j))
      deff_j <- as.numeric(deff(p_calc_j))

      # If there is no variation in the outcome, then the DEFF will be missing or 0
      # Overwrite in this case so DEFF = 1
      # And set flag for homogenous coverage

      if(se_j < 1e-15){
        deff_j <- 1
        homog_cvg <- 1
      } else {
        homog_cvg <- 0
      }

      # Effective sample size
      neff_j <- nrow(strat_data)/deff_j

      # Alpha for CIs
      alpha <- 0.05

      # Adjusted effective sample size (formulas from Dean & Pagano 2015)

      # This neff = actual n when there's no variation
      if(se_j < 1e-15){
        neff_dp <- nrow(strat_data)
      } else {
        neff_dp <- p_j*(1-p_j)/(se_j^2)
      }

      adj_neff_j <- neff_dp * ((qnorm(1-alpha/2)/qt(1-alpha/2, df = nclust-nstrat))^2)

      ## Wilson confidence interval ----

      zquant <- qnorm(p = (1 - alpha/2))

      numerator_lh <- p_j + zquant^2/(2*adj_neff_j)
      numerator_rh <- zquant * sqrt((p_j * (1-p_j))/adj_neff_j + zquant^2/((2*adj_neff_j)^2))
      denominator <- 1 + zquant^2/adj_neff_j

      p_lb_wilson <- ((numerator_lh - numerator_rh)/denominator)
      p_ub_wilson <- ((numerator_lh + numerator_rh)/denominator)

      ## Logit confidence interval ----

      logit_l <- (log(p_j/(1-p_j)) - zquant*(adj_neff_j * p_j * (1-p_j))^(-1/2))
      logit_u <- (log(p_j/(1-p_j)) + zquant*(adj_neff_j * p_j * (1-p_j))^(-1/2))

      p_lb_logit <- exp(logit_l)/(1 + exp(logit_l))
      p_ub_logit <- exp(logit_u)/(1 + exp(logit_u))

      ## Agresti-Coull confidence interval ----
      c <- (qnorm(1 - alpha/2)^2)/2
      x_tilde <- p_j * adj_neff_j + c
      n_tilde <- adj_neff_j + 2*c
      p_tilde <- x_tilde/n_tilde

      p_lb_agresti <- p_tilde - qnorm(1 - alpha/2) * sqrt((p_tilde * (1-p_tilde))/n_tilde)
      p_ub_agresti <- p_tilde + qnorm(1 - alpha/2) * sqrt((p_tilde * (1-p_tilde))/n_tilde)

      ## Clopper-Pearson confidence interval ----

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
      }
      else if(cp_x == nrow(strat_data)) {
        p_lb_clopper <- (v1 * qf(alpha/2, v1, v2))/(v2 + (v1 * qf(alpha/2, v1, v2)))
        p_ub_clopper <- 1
      }
      else {
        p_lb_clopper <- (v1 * qf(alpha/2, v1, v2))/(v2 + (v1 * qf(alpha/2, v1, v2)))
        p_ub_clopper <- (v3 * qf(1-alpha/2, v3, v4))/(v4 + (v3 * qf(1-alpha/2, v3, v4)))
      }

      ## Fleiss confidence interval ----

      term1l <- 2 * adj_neff_j * p_j + zquant^2 - 1
      term2l <- zquant * sqrt(zquant^2 - (2 + (1/adj_neff_j)) + 4* p_j * (adj_neff_j * (1-p_j) + 1))
      term3 <- 2 * (adj_neff_j + zquant^2)

      term1u <- term1l + 2
      term2u <- zquant * sqrt(zquant^2 + (2 - (1/adj_neff_j)) + 4* p_j * (adj_neff_j * (1-p_j) - 1))

      p_lb_fleiss <- (term1l - term2l)/term3
      p_ub_fleiss <- (term1u + term2u)/term3

      ## Jeffreys confidence interval ----
      jeffreys_successes <- p_j * adj_neff_j
      alpha_shape <- jeffreys_successes + 0.5
      beta_shape <- adj_neff_j - jeffreys_successes + 0.5

      p_lb_jeffreys <- qbeta(alpha/2, alpha_shape, beta_shape)
      p_ub_jeffreys <- qbeta((1-alpha/2), alpha_shape, beta_shape)

      ## Wald confidence interval ----
      tquant <- qt(p = (1 - alpha/2), df = (nclust-nstrat))
      p_lb_wald <- p_j - tquant*se_j
      p_ub_wald <- p_j + tquant*se_j

      ## Default confidence interval: Wilson ----

        lb_j <- p_lb_wilson
        ub_j <- p_ub_wilson
        ci_meth <- "Wilson"

      ## Calculations without weights and stratification----

      # Define a survey design object *without* weights and stratification
      design_j_nostrat_nowt <- survey::svydesign(ids = ~temp_clustvar,
                                                 data = strat_data_j)

      # Given no-weights, no-stratification design, calculate design effect
      nostrat_nowt_calc_j <- svymean(~outcome_j,
                                     design = design_j_nostrat_nowt,
                                     deff = "replace",
                                     na.rm = TRUE)

      # This design effect = 1 when there's no variation in outcome
      if(se_j < 1e-15){
        deff_ns_nw <- 1
        } else {
        deff_ns_nw <- as.numeric(deff(nostrat_nowt_calc_j))
        }

      # Coefficient of variation on weights ----
      # Among observations with non-NA outcome, (sd(weightvar)/mean(weightvar))
      if(selfweighted == TRUE){
        cvw_j <- 1
      } else {
        cvw_j <- sd(strat_data_j[!is.na(strat_data_j$outcome_j),]$temp_weightvar, na.rm = TRUE)/mean(strat_data_j[!is.na(strat_data_j$outcome_j),]$temp_weightvar, na.rm = TRUE)
      }

      ## ICC and CI ----
      # Use calc_icc.R function to get ICC and confidence interval
      icc_calc_j <- calc_icc(
        cid = "temp_clustvar",
        y = "outcome_j",
        strat = "temp_stratumidvar",
        data = strat_data_j,
        method = "aov",
        median = FALSE, # TRUE means use loneway median equivalent
        ci.type = "bootstrap",
        ci_bootrep = 1000,
        alpha = 0.05
      )

      icc_j <- icc_calc_j$estimates$ICC
      icc_j_lb <- icc_calc_j$ci$LowerCI
      icc_j_ub <- icc_calc_j$ci$UpperCI

      icc_ci_type <- icc_calc_j$ci$Type

      out_msb <- icc_calc_j$ms$MSB
      out_msw <- icc_calc_j$ms$MSW


      ## Organ Pipe Plot ----

      # Make opplot folder if it doesn't exist
      if(dir.exists(paste0(dirname(datapath), "/Plots_OP_R"))==FALSE){
        dir.create(paste0(dirname(datapath), "/Plots_OP_R"))
      }

      # Make organ pipe plot
      opplot(
        dat = as.data.frame(strat_data_j),
        stratvar = "temp_stratvar",
        clustvar = "temp_clustvar",
        weightvar = "temp_weightvar",
        yvar = "outcome_j",
        stratum = levels(strat_data$temp_stratvar)[i],
        title = paste0(levels(strat_data$temp_stratvar)[i], " - ", outcome[j]),
        plotn = TRUE,
        barcolor1 = alpha("blue", 0.5),
        barcolor2 = "white",
        linecolor2 = "white",
        nlinecolor = "red",
        nlinepattern = 2,
        output_to_screen = FALSE,
        platform = "png",
        sizew = 14,
        sizeh = 12,
        filename = paste0(dirname(datapath), "/Plots_OP_R/", stratvar, "_", i, "_", outcome[j])
      )

      ## Weighted average cluster size ----
      n_complete <- nrow(strat_data_j[!is.na(strat_data_j$outcome_j),])

      grouped_j <- grouped_j %>%
        mutate(summit = cumsum(cluster_n * (cluster_n/n_complete)))

      m_avg_wtd <- (n_complete - last(grouped_j$summit))/(nrow(grouped_j)-1)

      ## CI post-processing ----
      # If proportion is exactly 0 or 1, wipe out the CI bounds for Wald and Logit intervals (degenerate)
      if(p_j==0 | p_j==1){
        p_lb_wald <- NA
        p_ub_wald <- NA
        p_lb_logit <- NA
        p_ub_logit <- NA
      }

      ## Create row of dataset ----
      temprow <- data.frame(
        survey_id = id,
        unique_survey_row_id = row,
        path_to_dataset = dirname(datapath),
        dataset_name = basename(datapath),
        agegroup = agegroup,
        outcome_name = outcome[j],
        stratum_level = stratlevel,
        stratvar = stratvar,
        stratum_id = i,
        stratum_name = as.character(levels(strat_data$temp_stratvar)[i]),
        path_to_opplot = "/Plots_OP_R",
        opplot_name = paste0(stratvar, "_", i, "_", outcome[j], ".png"),
        n_clusters = nclust,
        n_strata = nstrat,
        n = nrow(strat_data_j[!is.na(strat_data_j$outcome_j),]),
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
        neff = nrow(strat_data_j[!is.na(strat_data_j$outcome_j),])/deff_j,
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
        p_ub_wilson = p_ub_wilson * 100,
        homog_cvg = homog_cvg
      )

      # Bind with other rows created in this iccloop function
      if(i==1 & j==1){
        ICCrows <- temprow
      } else {
        ICCrows <- rbind(ICCrows, temprow)
      }

    } # end i loop (strata)

  } # end j loop (outcomes)

  ## Save results ----
  #If replace == TRUE, overwrite without merging.
  if(replace == TRUE){

    saveRDS(ICCrows, file = paste0(output, ".rds"))

  } else {
    # If replace == FALSE, merge any previous file with this name, then save.

    if(file.exists(paste0(output, ".rds"))){
      previous <- readRDS(paste0(output, ".rds"))
      ICCrows <- rbind(previous, ICCrows)
    }

    saveRDS(ICCrows, file = paste0(output, ".rds"))

  }

  # Restore user's original setting for survey.lonely.psu
  options(survey.lonely.psu = paste(save_user_survey_option))

  } # end of function
