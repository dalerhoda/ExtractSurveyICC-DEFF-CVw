####################################################################################
# Program:     opplot.R
# Purpose:     Make organ pipe plot
# Author:      Mary Prier, Biostat Global Consulting
# Delivered:   2017-01-28
# Updated:     2019-03-14 MLP: plotn & savedata options
#              2020-05-13 CBC: weightvar handling, various checks/warnings
#                              data.frame assertion at top
#              2020-05-18 CBC: fix bug in savedata
#              2020-05-28 CBC: pass dimensions to png, variable name checks
# Required Packages: doBy
####################################################################################

opplot <- function(
  ## Required arguments: dat, clustvar, yvar
  dat,
  clustvar,
  yvar,
  ## Optional arguments:
  weightvar = NA,
  stratvar = NA,
  stratum = "",
  barcolor1 = "hot pink", # color for respondents with yvar = 1
  barcolor2 = "white",    # color for respondents with yvar = 0
  linecolor1 = "gray",    # color for lines separating lower portion of bars
  linecolor2 = "gray",    # color for lines separating upper portion of bars
  ylabel = "Percent of Cluster",
  ymin = 0,
  ymax = 100,
  yby = 50,
  title = "",
  subtitle = "",
  footnote = "",
  output_to_screen = TRUE,
  filename = "",
  platform = "wmf",
  sizew = 7,
  sizeh = 6,
  plotn = FALSE,
  nlinecolor = "black",
  nlinewidth = 1,       # color: line indicating # of respondents
  nlinepattern = 2,     # pattern: line indicating # of respondents
  ytitle2 = "Number of Respondents",
  yround2 = 5,
  savedata = "")
{

  ### Ensure data is a data.frame object
  dat <- as.data.frame(dat)

  ### Check variable name arguments
  if(length(names(dat)[names(dat) == clustvar])==0){
    stop(paste0("Cannot find a variable called ", clustvar, " in the dataset ", quote(dat), "."))
  }

  if(length(names(dat)[names(dat) == yvar])==0){
    stop(paste0("Cannot find a variable called ", yvar, " in the dataset ", quote(dat), "."))
  }

  if(!is.na(stratvar) & stratvar != ""){
    if(length(names(dat)[names(dat) == stratvar])==0){
      stop(paste0("Cannot find a variable called ", stratvar, " in the dataset ", quote(dat), "."))
    }
  }

  if(!is.na(weightvar) & weightvar != ""){
    if(length(names(dat)[names(dat) == weightvar])==0){
      stop(paste0("Cannot find a variable called ", weightvar, " in the dataset ", quote(dat), "."))
    }
  }

  ### Rename variables

  names(dat)[names(dat) == clustvar] <- "clustvar"
  names(dat)[names(dat) == yvar] <- "yvar"

  if(!is.na(stratvar) & stratvar != ""){
    names(dat)[names(dat) == stratvar] <- "stratvar"
  } else {
    print("Warning: stratvar was not specified. Treating the entire dataset as one stratum.")
    dat$stratvar <- 1
  }

  if(!is.na(weightvar) & weightvar != ""){
    names(dat)[names(dat) == weightvar] <- "weightvar"
  } else {
    dat$weightvar <- 1
  }

  # ^ This process will fail and error ("duplicate subscripts for columns") if there are already variables called clustvar, yvar, stratvar, or weightvar in the input dataset.

  ### Check data and provide warnings if necessary
  dat_orig <- dat

  # Check "yvar" is either 0 or 1 -> records with yvar!=0 or 1 will be ignored
  y_ind <- which(!(dat$yvar %in% c(0,1)))
  if(length(y_ind) > 0) {
    print(paste("Warning:", length(y_ind), "records had yvar without value 0 or 1, and therefore will be ignored."))
  }

  # Check "weightvar" -> records with weightvar=missing or weightvar=0 will be ignored
  wt_ind <- which(dat$weightvar %in% c(0,NA))
  if(length(wt_ind) > 0) {
    print(paste("Warning:", length(wt_ind), "records had weightvar with value 0 or missing.  These records will be ignored."))
  }

  # Check "clustvar" -> records with clustvar=missing will be ignored
  cl_ind <- which(is.na(dat$clustvar))
  if(length(cl_ind)>0) {
    print(paste("Warning:", length(cl_ind), "records had a missing clustvar value.  These records will be ignored."))
  }

  # Subset data to ignore rows flagged above
  if(length(c(y_ind,wt_ind,cl_ind))>0) {
    dat <- dat[-c(y_ind,wt_ind,cl_ind),]
    print(paste("Original dataset had",nrow(dat_orig),"rows.  The dataset used to make OP plot has",nrow(dat),"rows."))
  }

  # Check if plot is saved to disk, that the platform specified is either wmf or pdf
  if(!output_to_screen) {
    check_platform <- platform=="wmf" | platform=="pdf" | platform=="png"
    if(!check_platform) {
      print(paste("Invalid platform specified: ",platform))
      print("Either set it to wmf or pdf or png. For now, it will be set to wmf")
      platform <- "wmf"
    }
  }

  ### Set up whether plot goes to screen or saved to file (either .wmf, .pdf, or .png)

  if(output_to_screen){windows(width = sizew, height = sizeh)}

  if(!output_to_screen & platform=="wmf"){
    win.metafile(file = paste0(filename, ".wmf"), width = sizew, height = sizeh)}

  if(!output_to_screen & platform=="pdf"){
    pdf(file = paste0(filename, ".pdf"), width = sizew, height = sizeh)}

  if(!output_to_screen & platform=="png"){
    png(file = paste0(filename, ".png"), width = sizew, height = sizeh, units = "in", res = 576)}

  ### Generate barwidth variable
  # Generate variable with unique stratum/cluster combinations
  dat <- within(dat, {stratum_cluster_factor <- factor(paste(dat$stratvar, dat$clustvar))})

  # Calculate the sum of the the weights by unique stratum/cluster combination
  wclust_temp <- summaryBy(weightvar~stratum_cluster_factor, data=dat, FUN=sum)
  colnames(wclust_temp)[2] <- "wclust"
  # Merge sum of weights with dat
  dat2 <- merge(dat, wclust_temp, sort=FALSE)

  # Calculate the sum of the the weights by unique stratum
  wstrat_temp <- summaryBy(weightvar~stratvar, data=dat, FUN=sum)
  colnames(wstrat_temp)[2] <- "wstrat"
  # Merge sum of weights with dat2
  dat3 <- merge(dat2, wstrat_temp, sort=FALSE)

  # Calculate barwidth variable
  dat3$barwidth <- 100 * dat3$wclust/dat3$wstrat

  ### Generate barheight variable
  dat3$yweight <- dat3$yvar * dat3$weightvar
  yweight_sum_df <- summaryBy(yweight~stratum_cluster_factor, data=dat3, FUN=sum)
  colnames(yweight_sum_df)[2] <- "wsum1"
  # Merge sum of weights with dat3
  dat4 <- merge(dat3, yweight_sum_df, sort=FALSE)
  dat4$barheight <- round(100*dat4$wsum1/dat4$wclust)

  # Calculate the sum of the the respondents by unique stratum/cluster combination
  # Note: This variable will only be used if plotn==T
  dat4$one <- 1 # make a col of 1's for summing in the next line...
  nresp_temp <- summaryBy(one~stratum_cluster_factor, data=dat4, FUN=sum)
  colnames(nresp_temp)[2] <- "nresp"
  # Merge sum of respondents per stratum/cluster with dat
  dat5 <- merge(dat4,nresp_temp,sort=F)

  ### Organize data for plotting
  # Keep 1 obs for each stratum/cluster
  dat5_sorted <- dat5[order(dat5$stratvar,dat5$clustvar),]
  one_obs <- dat5_sorted[!duplicated(dat5_sorted[names(dat5_sorted)=="stratum_cluster_factor"]),]

  # Keep stratum of interest
  if(stratum!="") {
    if(!is.na(stratvar) & stratvar != ""){
    one_obs_subset <- one_obs[which(one_obs$stratvar==stratum),]
    } else {
      print(paste0("Warning: cannot show results for stratum = ", stratum, " because stratvar is not defined. All observations are used in plot."))
      one_obs_subset <- one_obs
    }
  } else {
    print("Warning: stratum not specified so all observations are used in plot")
    one_obs_subset <- one_obs
  }

  # Sort data (descending) by barheight then by barwidth
  to_plot <- one_obs_subset[order(-one_obs_subset$barheight,-one_obs_subset$barwidth,-one_obs_subset$nresp,one_obs_subset$clustvar),]
  to_plot$barheight2 <- 100-to_plot$barheight

  ### Make OPP
  if(plotn==T) {
    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for second y axis
  }

  counts <- as.matrix(rbind(to_plot$barheight,to_plot$barheight2))
  barplot(height=counts,width=to_plot$barwidth,space=0,beside=FALSE,col=c(barcolor1,barcolor2),
          cex.main=1.5, main=title,ylab=ylabel,las=2,border=c(linecolor1,linecolor2),
          font.main=1,axes=F)
  axis(2,labels=seq(from=ymin,to=ymax,by=yby),at=seq(from=ymin,to=ymax,by=yby),las=2)
  mtext(side=3, subtitle, cex=1, line=.35)
  mtext(side=1, footnote, cex=.8, line=.4, adj=0)
  box()

  # Check if user wants to plot the number of respondents (N) (plotn option),
  #  Add second axis with n's to the plot if plotn==T
  if(plotn==T) {
    # Calculate ymax2 for second y axis
    ymax2_temp <- max(one_obs_subset$nresp)
    ymax2 <- yround2 * ceiling((ymax2_temp+1)/yround2)

    # Calculate cumulative sum of sorted barwidth variable...for plotn stairstep look
    to_plot$cumsum_barwidth <- cumsum(to_plot$barwidth)

    # Add an extra row onto the dataset to make the x values work out correctly when plotted
    to_plot[nrow(to_plot)+1,] <- NA

    # Shift the cumulative bar width up by one observation to make the x values work out correctly
    to_plot$cumsum_barwidth <- c(0,to_plot$cumsum_barwidth[1:(nrow(to_plot)-1)])

    # Repeat the last nresp obs to the "new" last row so nline extends through last cluster
    to_plot$nresp[nrow(to_plot)] <- to_plot$nresp[nrow(to_plot)-1]

    # Add second y axis as new plot on top of barchart plot
    par(new = T)
    plot(to_plot$cumsum_barwidth, to_plot$nresp, type="s", axes=FALSE, bty="n",
         lty=nlinepattern, col=nlinecolor, lwd=nlinewidth, xlab="", ylab="",ylim=c(0,ymax2))
    axis(4,labels=seq(from=0,to=ymax2,by=yround2),at=seq(from=0,to=ymax2,by=yround2),las=2)
    mtext(side = 4, line=2, ytitle2)
  }

  # If the user has asked for underlying data to be saved, then trim down to a small
  # dataset that summarizes what is shown in the bars; this is to help users
  # identify the clusterid of a particular bar in the figure; the order in which
  # clusterids appear in the saved dataset is the same order they appear in the plot
  if(savedata!="") {
    # First, save 10 variables as vectors
    yvar_rep <- rep(yvar,length(to_plot$yvar))
    stratvar_rep <- rep(stratvar,length(to_plot$yvar))
    stratum_rep <- rep(stratum,length(to_plot$yvar))
    cluster_rep <- rep(clustvar,length(to_plot$yvar))
    clusterid <- to_plot$clustvar
    n_respondents <- to_plot$nresp
    barorder <- c(1:length(to_plot$yvar))
    barwidth <- to_plot$barwidth
    cumulative_barwidth <- cumsum(to_plot$barwidth)
    barheight <- to_plot$barheight

    # Now, put those 10 variables in a data.frame
    to_save <- data.frame(yvar_rep, stratvar_rep, stratum_rep, cluster_rep,
                          clusterid, n_respondents, barorder,
                          barwidth, cumulative_barwidth,barheight)

    # Drop the final row, which is not a cluster but only present for plotting purposes
    to_save <- to_save[!is.na(to_save$clusterid),]

    # Re-name the *_rep variables
    colnames(to_save)[colnames(to_save)=="yvar_rep"] <- "yvar"
    colnames(to_save)[colnames(to_save)=="stratvar_rep"] <- "stratvar"
    colnames(to_save)[colnames(to_save)=="stratum_rep"] <- "stratum"
    colnames(to_save)[colnames(to_save)=="cluster_rep"] <- "cluster"

    # Save data.frame to disk (as a .csv)
    write.csv(to_save, file = file.path(paste0(savedata, ".csv")), row.names = FALSE)
  }

  # Close plot window if plot is saved to disk
  if(!output_to_screen) dev.off()

}