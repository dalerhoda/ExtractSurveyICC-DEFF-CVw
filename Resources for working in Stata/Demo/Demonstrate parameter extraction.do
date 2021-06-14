********************************************************************************
* This program performs all of the steps accomplished in the iccplot.ado program
* for one analysis - of rural clusters in Harmonia - for one outcome 
* (got_crude_penta3_to_analyze).
*
* The curious user can run this program line-by-line to see what iccloop is 
* doing inside, over and over again.
*
* Dale Rhoda
* Biostat Global Consulting
* May 20, 2020
*
* Dale.Rhoda@biostatglobal.com
*
********************************************************************************

* You will need to change this line to point to the right folder
* on your computer
cd "Q:\Papers - 2020 - ICCs for vaccination\Survey datasets\00 - Harmonia\Demo"

* Open the postfile dataset for input
capture postclose iout 
postfile iout ///
			survey_id               ///   1 - Dataset ID Number
			unique_survey_row_id    ///   2 - Unique Row ID Within Survey
			str255 path_to_dataset  ///   3 - Pathname to Dataset
			str255 dataset_name     ///   4 - Dataset File Name
			str255 agegroup         ///   5 - Age Group for this Outcome
			str255 outcome_name     ///   6 - Outcome Name
			stratum_level           ///   7 - Geographic Level
			str255 stratvar         ///   8 - Stratification Variable
			stratum_id              ///   9 - Stratum ID Number
			str255 stratum_name     ///  10 - Stratum Name
			str255 path_to_opplot   ///  11 - Pathname to Organ Pipe Plot
			str255 opplot_name      ///  12 - Organ Pipe Plot Name
			n_clusters              ///  13 - Number of Clusters
			n                       ///  14 - Number of Respondents
			m_avg                   ///  15 - Average No. Respondents Per Cluster
			m_sd                    ///  16 - Standard Deviation of No. Respondents per Cluster
			m_min                   ///  17 - Min No. Respondents per Cluster
			m_max                   ///  18 - Max No. Respondents per Cluster
			ncp100                  ///  19 - Number of Clusters with 100% Coverage
			ncp0                    ///  20 - Number of Clusters with 0% Coverage
			p                       ///  21 - Coverage Estimate
			p_se                    ///  22 - Coverage Estimate Standard Error
			p_lb                    ///  23 - Coverage Estimate CI Lower Bound (Wilson)
			p_ub                    ///  24 - Coverage Estimate CI Upper Bound (Wilson)
			str255 ci_method	    ///  25 - Confidence Interval Method
			deff                    ///  26 - Design Effect
			neff                    ///  27 - Effective Sample Size
			deff_nostrat_nowt       ///  28 - Design Effect w/o Stratification or Weights
			cvw                     ///  29 - Coefficient of Variation on Weights
			icc                     ///  30 - Intracluster Correlation Coefficient
			icc_lb                  ///  31 - Intracluster Correlation Coefficient CI Lower Bound
			icc_ub                  ///  32 - Intracluster Correlation Coefficient CI Upper Bound
			str255 icc_ci_method    ///  33 - ICC CI method
			str255 comment          ///  34 - Comments
			n_strata                ///  35 - Number of distinct survey sample strata in this calculation
			m_avg_wtd               ///  36 - Weighted average cluster size
			p_lb_logit              ///  37 - Coverage Estimate CI Lower Bound (Logit)
			p_ub_logit              ///  38 - Coverage Estimate CI Upper Bound (Logit)
			p_lb_agresti            ///  39 - Coverage Estimate CI Lower Bound (Agresti)
			p_ub_agresti            ///  40 - Coverage Estimate CI Upper Bound (Agresti)
			p_lb_clopper            ///  41 - Coverage Estimate CI Lower Bound (Clopper)
			p_ub_clopper            ///  42 - Coverage Estimate CI Upper Bound (Clopper)
			p_lb_fleiss             ///  43 - Coverage Estimate CI Lower Bound (Fleiss)
			p_ub_fleiss             ///  44 - Coverage Estimate CI Upper Bound (Fleiss)
			p_lb_jeffreys           ///  45 - Coverage Estimate CI Lower Bound (Jeffreys)
			p_ub_jeffreys           ///  46 - Coverage Estimate CI Upper Bound (Jeffreys)
			p_lb_wald               ///  47 - Coverage Estimate CI Lower Bound (Wald)
			p_ub_wald               ///  48 - Coverage Estimate CI Upper Bound (Wald)			
		 using icc_example, replace

* In this example we want to 
use ri_example_dataset_Harmonia, clear

tab urban_cluster

levelsof urban_cluster


* Urban / rural has two levels; look at parameters for rural clusters here

scalar go_1 = 0 // demo  id is 0
scalar go_2 = 0 // demo row is 0
scalar go_3 = "Q:\Papers - 2020 - ICCs for vaccination\Survey datasets\00 - Harmonia\RI" // path to dataset
scalar go_4 = "ri_example_dataset_Harmonia" // name of dataset
scalar go_5 = "12-23m" // age group
scalar go_6 = "got_crude_penta3_to_analyze" // outcome variable
scalar go_7 = 2 // Sub-national results
scalar go_8 = "urban_cluster" // stratification results


* Here's where we only keep respondents from rural clusters
keep if urban_cluster == 0

scalar go_9 = 0 // rural
scalar go_10 = "Rural"
scalar go_11 = "Q:\Papers - 2020 - ICCs for vaccination\Survey datasets\00 - Harmonia\RI\Plots_OP" // path to op plots
scalar go_12 = "Harmonia_rural_penta3.png" // op plot file name

svyset clusterid, strata(stratumid) weight(psweight) singleunit(scaled)

* Run svy: proportion to calculate number of PSUs and strata
svy, subpop(if inlist(got_crude_penta3_to_analyze,0,1)) : proportion got_crude_penta3_to_analyze
scalar go_13 = e(N_psu) // number of clusters
scalar go_35 = e(N_strata) // number of strata

count if inlist(got_crude_penta3_to_analyze,0,1)
scalar go_14 = r(N) // number of respondents
scalar go_15 = go_14/go_13 // avg respondents per cluster

capture mkdir Plots_OP

preserve
	* Make the organ pipe plot and make its accompanying dataset, opplot
	opplot got_crude_penta3_to_analyze, clustvar(clusterid) ///
	       stratvar(urban_cluster) stratum(0) ///
		   title(Harmonia - Rural - Penta3) barcolor1(blue*.5) ///
		   barcolor2(white) linecolor2(white) plotn nlinecolor(red) ///
		   nlinepattern(dash) export(Plots_OP/Harmonia_rural_penta3.png) ///
		   saving(Plots_OP/Harmonia_rural_penta3, replace) ///
		   exportwidth(2000) savedata(oppdata,replace) name(opplot, replace)

	use oppdata, clear
	summarize n_respondents
	scalar go_16 = r(sd) // std dev of respondents per cluster
	scalar go_17 = r(min) // min respondents per cluster
	scalar go_18 = r(max) // max respondents per cluster
	count if barheight == 100 
	scalar go_19 = r(N) // number of clusters with 100% coverage
	count if barheight == 0
	scalar go_20 = r(N) // number of clusters with 0% coverage
	* Calculate weighted mean of m
	scalar summit = 0
	forvalues i = 1/`=_N' {
		scalar summit = summit + (n_respondents[`i'] * n_respondents[`i'] / go_14)
	}
	scalar go_36 = (go_14 - summit) / `=_N -1' // weighted average of respondents per cluster
restore

* Estimate coverage and its 2-sided 95% CI
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(wilson)
scalar go_21 = 100*r(svyp) // coverage estimate
scalar go_22 = r(stderr)   // std err of coverage estimate
scalar go_23 = 100*r(lb_alpha) // 2-sided confidence interval lower-bound
scalar go_24 = 100*r(ub_alpha) // 2-sided confidence interval upper-bound
scalar go_25 = r(method) // CI method
scalar go_26 = r(deff) // design effect
scalar go_27 = go_14 / go_26 // effective sample size

* Save the confidence interval bounds using other types of intervals, too
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(logit)
scalar go_37 = 100*r(lb_alpha)
scalar go_38 = 100*r(ub_alpha)
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(agresti)
scalar go_39 = 100*r(lb_alpha)
scalar go_40 = 100*r(ub_alpha)
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(clopper)
scalar go_41 = 100*r(lb_alpha)
scalar go_42 = 100*r(ub_alpha)
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(fleiss)
scalar go_43 = 100*r(lb_alpha)
scalar go_44 = 100*r(ub_alpha)
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(jeffreys)
scalar go_45 = 100*r(lb_alpha)
scalar go_46 = 100*r(ub_alpha)
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(wald)
scalar go_47 = 100*r(lb_alpha)
scalar go_48 = 100*r(ub_alpha)

* Now calculate design effect without stratification or weights 
* (we will use this later in the project to calculate roh)
svyset clusterid
svypd got_crude_penta3_to_analyze if inlist(got_crude_penta3_to_analyze,0,1), adjust truncate method(wilson)
scalar go_28 = r(deff) // design effect ignoring weights and stratification

* Calculate coefficient of variation on weights
sum psweight if inlist(got_crude_penta3_to_analyze,0,1)
scalar go_29 = r(sd) / r(mean) // coefficient of variation on weights


calcicc got_crude_penta3_to_analyze clusterid
scalar go_30 = r(anova_icc) // icc

loneway got_crude_penta3_to_analyze clusterid 
scalar go_31 = r(lb) // icc confidence interval lower bound from loneway
scalar go_32 = r(ub) // icc confidence interval upper bound from loneway
scalar go_33 = "loneway" // icc CI calculation method

* Try to replace the icc CI bounds with bootstrap bounds
capture bootstrap r(rho), reps(100) seed(8675309) strata(stratumid) cluster(clusterid) : loneway got_crude_penta3_to_analyze clusterid 

if _rc == 0 {
	matrix out = e(ci_percentile)
	scalar go_31 = out[1,1] // icc CI lower bound from bootstrap
	scalar go_32 = out[2,1] // icc CI upper bound from bootstrap
	matrix drop out
	scalar go_33 = "loneway (bootstrap percentiles)"
}

scalar go_34 = "Save a comment here if we wish..."  // comment

post iout (go_1) (go_2) ("`=scalar(go_3)'") ("`=scalar(go_4)'") ///
		  ("`=scalar(go_5)'") ("`=scalar(go_6)'") (go_7) ///
		  ("`=scalar(go_8)'") (go_9) ("`=scalar(go_10)'") ///
		  ("`=scalar(go_11)'") ///
		  ("`=scalar(go_12)'") (go_13) (go_14) (go_15) (go_16) ///
		  (go_17) (go_18) (go_19) ///
		  (go_20) (go_21) (go_22) (go_23) (go_24) ///
		  ("`=scalar(go_25)'") (go_26) (go_27) (go_28) (go_29) ///
		  (go_30) (go_31) (go_32) ("`=scalar(go_33)'") ///
		  ("`=scalar(go_34)'") (go_35) (go_36) (go_37) ///
		  (go_38) (go_39) (go_40) (go_41) (go_42) (go_43) ///
		  (go_44) (go_45) (go_46) (go_47) (go_48)
		  
capture erase oppdata.dta

capture postclose iout

use icc_example, clear

label variable survey_id            "Dataset ID Number"
label variable unique_survey_row_id "Unique Row ID Within Survey"
label variable path_to_dataset      "Pathname to Dataset"
label variable dataset_name         "Dataset File Name"
label variable agegroup             "Age Group for this Outcome"
label variable outcome_name         "Outcome Name"
label variable stratum_level        "Geographic Level"
label variable stratvar             "Stratification Variable"
label variable stratum_id           "Stratum ID Number"
label variable stratum_name         "Stratum Name"
label variable path_to_opplot       "Pathname to Organ Pipe Plot"
label variable opplot_name          "Organ Pipe Plot Name"
label variable n_clusters           "Number of Clusters"
label variable n                    "Number of Respondents"
label variable m_avg                "Average No. Respondents Per Cluster"
label variable m_sd                 "Standard Deviation of No. Respondents per Cluster"
label variable m_min                "Min No. Respondents per Cluster"
label variable m_max                "Max No. Respondents per Cluster"
label variable ncp100               "Number of Clusters with 100% Coverage"
label variable ncp0                 "Number of Clusters with 0% Coverage"
label variable p                    "Coverage Estimate"
label variable p_se                 "Coverage Estimate Standard Error"
label variable p_lb                 "Coverage Estimate CI Lower Bound (Wilson)"
label variable p_ub                 "Coverage Estimate CI Upper Bound (Wilson)"
label variable ci_method	        "Confidence Interval Method"
label variable deff                 "Design Effect"
label variable neff                 "Effective Sample Size"
label variable deff_nostrat_nowt    "Design Effect w/o Stratification or Weights"
label variable cvw                  "Coefficient of Variation on Weights"
label variable icc                  "Intracluster Correlation Coefficient"
label variable icc_lb               "Intracluster Correlation Coefficient CI Lower Bound"
label variable icc_ub               "Intracluster Correlation Coefficient CI Upper Bound"
label variable icc_ci_method        "ICC CI method"
label variable comment              "Comments"
label variable n_strata             "Number of distinct survey sample strata in this calculation"
label variable m_avg_wtd            "Weighted average cluster size"
label variable p_lb_logit           "Coverage Estimate CI Lower Bound (Logit)"
label variable p_ub_logit           "Coverage Estimate CI Upper Bound (Logit)"
label variable p_lb_agresti         "Coverage Estimate CI Lower Bound (Agresti)"
label variable p_ub_agresti         "Coverage Estimate CI Upper Bound (Agresti)"
label variable p_lb_clopper         "Coverage Estimate CI Lower Bound (Clopper)"
label variable p_ub_clopper         "Coverage Estimate CI Upper Bound (Clopper)"
label variable p_lb_fleiss          "Coverage Estimate CI Lower Bound (Fleiss)"
label variable p_ub_fleiss          "Coverage Estimate CI Upper Bound (Fleiss)"
label variable p_lb_jeffreys        "Coverage Estimate CI Lower Bound (Jeffreys)"
label variable p_ub_jeffreys        "Coverage Estimate CI Upper Bound (Jeffreys)"
label variable p_lb_wald            "Coverage Estimate CI Lower Bound (Wald)"
label variable p_ub_wald            "Coverage Estimate CI Upper Bound (Wald)"

compress
save, replace
