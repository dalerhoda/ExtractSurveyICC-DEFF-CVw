# Program: calc_icc.R
# For selected strata and outcomes, calculate ICC and other quantities of interest
# Based on ICCbin::iccbin
# Author: Caitlin B. Clary, Biostat Global Consulting

##  Drops methods other than ANOVA for ICC/CI calculations, changes error message behavior and some default values.
# 2020-04-17    Original: adapt iccbin. Drop methods other than ANOVA.
#               Change error message behavior, add some defaults.
# 2020-05-08    Add median option (equiv. Stata loneway median)
# 2020-05-21    Implement bootstrap confidence interval
#               Add stratum ID argument to function (for bootstrap)
# 2020-05-26    Stop truncating negative ICCs at 0
#               Use ICC = -1/n0 when coverage is uniform 0% or 100%
# 2020-06-01    Changed how missing CI values are stored (NA rather than "-")

calc_icc <- function(
  cid,                   # cluster ID
  y,                     # outcome
  data,                  # name of dataset
  strat = NA,            # stratum ID (used when bootstrapping CI)
  method = "aov",        # ICC calculation method (aov only)
  median = FALSE,        # use median adjustment to ICC calculation
  ci.type = "bootstrap", # CI calculation method (smith or bootstrap)
  ci_bootrep = 500,      # no. bootstrap reps if ci.type is bootstrap
  alpha = 0.05){         # alpha for confidence intervals

  ## Processing and checks
  CALL <- match.call()

  indat <- data

  # Define y and clusterid variables in dataset
  indat$y <- get(y, indat)
  indat$cid <- get(cid, indat)

  # Define stratum variable if provided, otherwise create as single stratum
  if(!is.na(strat)){
    indat$strat <- get(strat, indat)
  } else {
    indat$strat <- 1
  }

  ic <- indat %>%
    select(y, cid, strat)

  dt <- data.frame(ic)
  dt <- na.omit(dt)

  # Group dt for weighted average cluster size calculation
  dt_grp <- dt %>%
    group_by(cid) %>%
    summarize(cluster_n = n()) %>%
    mutate(cs = cumsum(cluster_n * (cluster_n/nrow(dt))))

  wtd_avg_clust_size <- (nrow(dt) - last(dt_grp$cs))/(nrow(dt_grp)-1)

  # Number of clusters
  k <- length(unique(dt$cid))

  if(!is.null(attributes(dt)$na.action)){
    warning(cat("NAs removed from data rows:\n", unclass(attributes(dt)$na.action), "\n"))
  }

  if(!is.factor(dt$cid)){
    #warning("'cid' has been coerced to a factor")
    dt$cid <- as.factor(dt$cid)
  } else{
    if(length(levels(dt$cid)) > k){
      dt$x <- factor(as.character(dt$cid), levels = unique(dt$cid))
      warning("Missing levels of 'cid' have been removed")
    }
  }

  zalpha <- qnorm(alpha/2, lower.tail = F)
  square <- function(z){z^2}

  ## Define inputs, preliminary calculations ----

  # Cluster ID
  cid <- dt$cid
  # Response variable
  y <- dt$y
  # Number of clusters
  k <- length(unique(cid))
  # Number of observations in each cluster
  ni <- as.vector(table(cid))
  # Total number of observations
  N <- sum(ni)

  n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
  yi <- aggregate(y, by = list(cid), sum)[ , 2]
  yisq <- yi^2
  msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
  msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))

  # ANOVA estimate of ICC, used in CI calculation even if median=TRUE
  rho.aov <- (msb - msw)/(msb + (n0 - 1)*msw)

  if(median == FALSE){
    meth <- "ANOVA Estimate"
    est <- rho.aov
  }

  if(median == TRUE){
    meth <- "ANOVA with Median Adj."
    Fm <- qf(0.5, (k-1), (N-k))
    rho.aov.m <- ((msb/msw)-Fm)/((msb/msw) + (n0-1)*Fm)
    est <- rho.aov.m
  }

  ## Smith CI ----

  # Calculated with standard ICC (rho.aov, not rho.aov.m), regardless of median option selected by the user

  # Do these calculations even if bootstrap is selected, to have Smith CIs as a fallback

  if(is.na(rho.aov)){
    smith_lci <- NA
    smith_uci <- NA
  } else {

    st0 <- 2*square(1 - rho.aov)/square(n0)

    st1 <- square(1 + rho.aov*(n0 - 1))/(N - k)

    st2 <- ((k - 1)*(1 - rho.aov)*(1 + rho.aov*(2*n0 - 1)) +
              square(rho.aov)*(sum(ni^2) - (2/N)*sum(ni^3) + (1/N^2)*square(sum(ni^2))))/square(k - 1)

    var.smith.rho.aov <- st0*(st1 + st2)

    ci.smith.rho.aov <- c(rho.aov - zalpha*sqrt(var.smith.rho.aov), rho.aov + zalpha*sqrt(var.smith.rho.aov))

    smith_lci <- ifelse(ci.smith.rho.aov[1] < 0, 0, ci.smith.rho.aov[1])
    smith_uci <- ifelse(ci.smith.rho.aov[2] > 1, 1, ci.smith.rho.aov[2])
  }

  if(ci.type == "smith"){
    ci.typ <- "Smith"
      lci <- smith_lci
      uci <- smith_uci
        }

  ## Bootstrap CI
  # nclusters should be >1, p != 0, p != 1
  if(ci.type == "bootstrap"){

    # Do not bootstrap if single cluster
    # Do not bootstrap if no variation in outcome
    if(k == 1){
      warning("Bootstrap cannot be performed; only one cluster in data.")
      lci <- smith_lci
      uci <- smith_uci
      ci.typ <- "Smith (bootstrap failed; single cluster)"

    } else if(sum(y)==0 | sum(y)==N){
      ci.typ <- "Cvg is 0 or 100"
      lci <- NA
      uci <- NA
    } else {
      set.seed(8675309)

      # Number of strata
      st <- length(unique(dt$strat))
      # Number of clusters per stratum
      cps <- dt %>%
        group_by(cid) %>% slice(1) %>%
        group_by(strat) %>% summarize(cbys = n())

      mincps <- min(cps$cbys) # should be >1 to proceed with bootstrap

      if(mincps > 1){
     boot_iccs_out <-  replicate(ci_bootrep, {

       clusters_by_stratum <- vector()
       # Sample clusters from each strata, matching original number of clusters in that stratum
       for(q in 1:st){
         dt_stratum <- dt[dt$strat %in% cps$strat[q],]
         dt_stratum$cid <- droplevels(dt_stratum$cid)
         st_clusters <- sample(levels(dt_stratum$cid), cps$cbys[q], replace = TRUE)
         clusters_by_stratum <- c(clusters_by_stratum, st_clusters)
       }

        clusterlist <- data.frame(
          cid = clusters_by_stratum,
          bootcid = seq(1, k, by = 1))

        suppressWarnings(outdt <- dt %>% inner_join(clusterlist, by = "cid"))

        # ICC calculation on bootstrap dataset
        b_cid <- outdt$bootcid
        b_y <- outdt$y
        b_ni <- as.vector(table(b_cid))
        b_N <- sum(b_ni)

        b_n0 <- (1/(k - 1))*(b_N - sum((b_ni^2)/b_N))
        b_yi <- aggregate(b_y, by = list(b_cid), sum)[ , 2]
        b_yisq <- b_yi^2
        b_msb <- (1/(k - 1))*(sum(b_yisq/b_ni) - (1/b_N)*(sum(b_yi))^2)
        b_msw <- (1/(b_N - k))*(sum(b_yi) - sum(b_yisq/b_ni))

        b_Fm <- qf(0.5, (k-1), (b_N-k))

        # Return value: b_rho (gets saved in boot_iccs_out)
        ifelse(median==FALSE,
               (b_msb - b_msw)/(b_msb + (b_n0 - 1)*b_msw),
               ((b_msb/b_msw)-b_Fm)/((b_msb/b_msw) + (b_n0-1)*b_Fm)
        )

      }) # end replicate

      ci.typ <- "Bootstrap percentiles"
      lci <- quantile(boot_iccs_out, alpha/2, na.rm = TRUE)
      uci <- quantile(boot_iccs_out, 1-alpha/2, na.rm = TRUE)

      } else {# end mincps condition

        lci <- smith_lci
        uci <- smith_uci
        ci.typ <- "Smith (bootstrap failed; 1+ stratum has single cluster)"
        }
    } # end else

  } # end if ci.type = bootstrap

  ## Manual overwrite - when p=0 or p=1, ICC=-1/m and bounds missing

    if(sum(y)==0 | sum(y)==N){
      est <- -1/wtd_avg_clust_size
      ci.typ <- "Cvg is 0 or 100"
      lci <- NA
      uci <- NA
    }

    estimates <- data.frame(Methods = meth, ICC = est); row.names(estimates) <- NULL
    ci <- data.frame(Type = ci.typ, LowerCI = lci, UpperCI = uci); row.names(ci) <- NULL
    ms <- data.frame(MSW = msw, MSB = msb); row.names(ms) <- NULL
    list(estimates = estimates, ci = ci, ms = ms)

} # End of function calc_icc