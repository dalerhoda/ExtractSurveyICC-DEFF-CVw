*! iccloop version 1.13 - Biostat Global Consulting - 2020-07-30
********************************************************************************
* Change log
* 				Updated
*				version
* Date 			number 	Name			What Changed
*-------------------------------------------------------------------------------
* 2020-04-30	1.00	Dale Rhoda	Original version based on Becca Robinson's work
* 2020-05-01	1.01	Becca Robinson	Add detailed variable descriptions 
* 2020-05-07	1.02	Dale Rhoda      Only call loneway if coverage is BETWEEN
*                                       0% and 100%
*                                       Also...calculate 95% CI for ICC using
*                                       500 bootstrap samples rather than the 
*                                       naive Wald estimator
* 2020-05-07	1.03	Becca Robinson	Change CI method from default to Wilson
* 2020-05-08	1.04	Dale Rhoda		Trap bootstrap errors and if found,
*                                       simply run loneway with the median option
* 2020-05-08							Also, save icc_ci_method string
* 2020-05-14	1.05	Dale Rhoda		Added number of survey strata (n_strata)
*                                       Changed num_clusters to n_clusters
*                                       Added 6 additional CI type bounds
*                                       Added weighted cluster size (that 
*                                       Ridout 1999 recommends and Hade 2010 uses)                                     
* 2020-05-19	1.06	Dale Rhoda		Added if inlist(`outcome',0,1) so
*                                       svypd will use the subpop syntax to
*                                       account for clusters that have N=0
*                                       Also wipe out Logit, Wilson and Wald
*                                       CI bounds if ci_method == "Clopper-Pearson"
* 2020-05-19	1.07	Dale Rhoda		Strip the median option off the call
*                                       to loneway; it's not needed to give
*                                       the Ridout formula
* 2020-05-21	1.08	Dale Rhoda		Added the replace option when saving
*                                       gph files, and removed hardcoded psweight
* 2020-05-26	1.09	Dale Rhoda		Added ms_between and ms_within from
*                                       modified loneway_plus program
*                                       Amend bootstrap to use idcluster option
*                                       and thereby get the icc calculation right
*                                       rather than have repeats of the same
*                                       clusterid value when the same cluster is
*                                       selected over and over again
* 2020-06-08	1.10	Dale Rhoda		Label the variables
* 2020-06-09	1.11	Dale Rhoda		The call to opplot now includes the
*                                       weightvar option and the program 
*                                       pulls the name of the survey sampling 
*                                       stratum variable from the svyset output
*                                       Also, do not truncate ICC at 0 or NEFF
*                                       at N for this work.
* 2020-06-10	1.12	Dale Rhoda		Add homog_cvg flag to output
* 2020-07-30	1.13	Wenfeng Gong	Syntax edit to work with Stata v14, too
*
********************************************************************************
*
*
*  Required Inputs
*
*  ID - survey ID from Biostat Global ICC study spreadsheet
*  DATASET - name of Stata dataset (usually TT_COVG_01_1 or RI/SIA_augmented_dataset)
*  OUTCOME - list of outcome variables
*  STRATVAR - stratification variable; use level1id if you don't want to stratify
*  (Note: At this time stratvar must be numeric...and it works best if
*         it has a value label; although the label is not required.)
*  OUTPUT - name of dataset where ICCs will be saved
*
*  Optional Inputs
*
*  STRATLEVEL - Let's say 1 for national results and 2 for sub-national and 3 for sub-sub-national
*  ROW - row number from that same Biostat Global spreadsheet
*  STRATVARNAME - name of variable holding stratum name
*  AGEGROUP - description of agegroup of eligible population
*  REPLACE - If you want Stata to wipe out any old versions of the OUTPUT file
*  COMMENT - pass thru a text comment into the dataset
*  SVYSET - svyset syntax if you do NOT want to use:
*           svyset clusterid, strata(stratum() singleunit(scaled)
*  WEIGHtVAR - name of weightvar if yours is not named psweight
*  SELFWEIGHTED - tells this program to not look for a weightvar and that CVw is 1
*
* Note: User should cd to the folder with the data file before running.
*
* Note: This program creates a sub-folder named Plots_OP in the current
*       working directory and stores the .png organ pipe plots in there.

program define iccloop
syntax , ///
       ID(integer)              /// survey id from BGC spreadsheet
	   DATAset(string)          /// name of dataset
	   OUTCOMElist(string asis) /// varlist
	   STRATvar(string)         /// name of variable
       OUTPUT(string asis)      /// name of dataset
	                            ///
	   [ STRATLEVEL(integer 1)  /// optional: stratification level
	     ROW(integer 1)         /// survey row from BGC spreadsheet
		 REPLACE                /// do not append...replace the output dataset
		 STRATNAMEvar(string)   /// optional string variable name (i.e., level1name)
		 AGEGROUP(string asis)  /// pass thru age group string
		 COMMENT(string asis)   /// pass thru a text comment
		 SVYSET(string asis)    /// svyset syntax
		 WEIGHTvar(string)      /// weight variable (if not psweight and not self-weighted)
		 SELFweighted]          //  indicates this survey was self-weighted
		 
* Place we'll hold temporary output
tempfile tempiout

* If the survey is not self-weighted and the user doesn't specify the weightvar, set it to psweight (VCQI's default)
if "`selfweighted'" == "" & "`weightvar'" == "" local weightvar psweight

* Erase the output dataset if the user specifies the 'replace' option
if "`replace'" == "replace" capture erase `=subinstr("`output'.dta",".dta.dta",".dta",1)'

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
			ms_between				///  49 - ANOVA mean square between clusters
			ms_within               ///  50 - ANOVA mean square within clusters
			homog_cvg				///  51 - Coverage is homogeneous across clusters (0/1)
		 using `tempiout', replace
		 
		 
use `"`dataset'"', clear

* What are the levels of stratvar?
levelsof `stratvar', local(slist)

* Loop over outcomes and strata
foreach outcome in `outcomelist' {
	foreach stratum in `slist' {
		use `"`dataset'"', clear
		keep if `stratvar' == `stratum'  // current code assumes stratvar is an integer, or at least a categorical number
		
		scalar go_1 = `id'
		scalar go_2 = `row'
		scalar go_3 = "`c(pwd)'"  // writes out the current Stata folder
		scalar go_4 = "`dataset'"
		scalar go_5 = "`agegroup'"
		scalar go_6 = "`outcome'"
		
		scalar go_7 = `stratlevel'
		scalar go_8 = "`stratvar'"
		scalar go_9 = `stratum'
		
		* If user specifies a stratnamevar, use that for go_10
		if "`stratnamevar'" != ""  scalar go_10 = `stratnamevar'[1] 
		* Otherwise, use the value label to set go_10
		if "`stratnamevar'" == ""  scalar go_10 ="`: label (`stratvar') `stratum''"
		
		scalar go_11 = "/Plots_OP"
		scalar go_12 = "`stratvar'_`stratum'_`outcome'.png"
		
		if "`svyset'" != "" {
			`svyset'
		}
		else {
			svyset clusterid, strata(stratumid) weight(`weightvar') singleunit(scaled)
		}

		* Store the svyset strata variable, if applicable
		local svystratvar = r(strata1)
		if inlist("`svystratvar'", "", ".") local svystratvar `stratvar'
		
		* Run svy: proportion to calculate number of PSUs and strata
		svy, subpop(if inlist(`outcome',0,1)) : proportion `outcome'
		scalar go_13 = e(N_psu)
		scalar go_35 = e(N_strata)
		local clusterid = e(su1) // store the name of the clusterid variable to use below
		
		
		* Fix from Wenfeng Gong to be compatible with Stata v14.2
		*scalar go_51 = e(V)[1,1] < 1E-15 // coverage is homogenous across clusters if this variance term is essentially zero

		matrix tempm = e(V)
		scalar go_51 = tempm[1,1] < 1E-15
		
		* End of 14.2 fix		
		
		count if inlist(`outcome',0,1)
		scalar go_14 = r(N)
		scalar go_15 = go_14/go_13
		
		* Set the local wv if there is a weightvar, and make it empty otherwise
		if "`weightvar'" != "" local wv weightvar(`weightvar')
		else local wv 
	
		preserve
			capture mkdir Plots_OP
			opplot `outcome', clustvar(`clusterid') stratvar(`stratvar') ///
				stratum(`stratum') `wv' ///
				title("`=scalar(go_10)' - `outcome'") ///
				barcolor1(blue*.5) barcolor2(white) linecolor2(white) ///
				plotn nlinecolor(red) nlinepattern(dash) ///
				ytitle2(Number of Respondents (N)) ///
				export(Plots_op/`=scalar(go_12)') ///
				exportwidth(2000) ///
				saving("Plots_op/`stratvar'_`stratum'_`outcome'", replace) ///
				savedata(oppdata, replace) name(opplot, replace)
				
			graph drop opplot  // drop the graph once it has been saved
			use oppdata, clear
			summarize n_respondents
			scalar go_16 = r(sd)
			scalar go_17 = r(min)
			scalar go_18 = r(max)
			count if barheight == 100
			scalar go_19 = r(N)
			count if barheight == 0
			scalar go_20 = r(N)
			
			* Calculate weighted m (Ridout equation for n0)
			gen summand = n_respondents * n_respondents / go_14
			egen summit = total(summand)
			scalar go_36 = (go_14 - summit) / `=_N -1'
		restore
		
		svypd `outcome' if inlist(`outcome', 0,1), adjust method(Wilson)
		scalar go_21 = 100*r(svyp)
		scalar go_22 = r(stderr)
		scalar go_23 = 100*r(lb_alpha)
		scalar go_24 = 100*r(ub_alpha)
		scalar go_25 = r(method)
		scalar go_26 = r(deff)
		scalar go_27 = go_14 / go_26
		
		* Also store the upper and lower bounds of the logit, agresti, clopper, fleiss, jeffreys and wald intervals
		local gg 37
		foreach m in logit agresti clopper fleiss jeffreys wald {
			svypd `outcome' if inlist(`outcome', 0,1), adjust method(`m')
			scalar go_`gg' = 100*r(lb_alpha)
			local ++gg
			scalar go_`gg' = 100*r(ub_alpha)
			local ++gg
		}
			
		svyset `clusterid' // Change svyset to calculate DEFF without stratification or weights
		svypd `outcome' if inlist(`outcome', 0,1), adjust method(Wilson)
		scalar go_28 = r(deff)
		
		* Calculate CVw if appropriate
		if "`weightvar'" != "" {
			sum `weightvar' if inlist(`outcome',0,1)
			scalar go_29 = r(sd) / r(mean)
		}
		else {
			scalar go_29 = 1  // self-weighted
		}
		
		* Calculate ICC and its 95% confidence interval
		if go_21 == 0 | go_21 == 100 {
			* If all respondents or none of the respondents had the outcome
			* of interest then set ICC to -1/m_avg_wtd which is go_36.
			*
			* Note: We haven't settled on a method for estimating a 95% CI
			* when this is the case...still looking in the literature.
			scalar go_30 = -1/go_36
			scalar go_31 = 0
			scalar go_32 = 0
			scalar go_33 = "Cvg is 0 or 100"
		}
		else {
			
			* Leave the weights out of this calculation as
			* we are assessing their coefficient of variation elsewhere and want 
			* to isolate their influence to that part of the analysis.
			*
			
			calcicc `outcome' `clusterid' 
			scalar go_30 = r(anova_icc)
			loneway_plus `outcome' `clusterid' // this modified program also returns ms_b and ms_w
			scalar go_31 = r(lb)
			scalar go_32 = r(ub)
			scalar go_33 = "loneway"
			scalar go_49 = r(ms_b)
			scalar go_50 = r(ms_w)
			
			* Use a bootstrap approach where the sampling units are clusters 
			* within strata to estimate the 95% confidence interval for ICC
			
			noi di _n "Please be patient while Stata calculates a bootstrap confidence interval..." _n
					
			capture bootstrap r(anova_icc), reps(1000) seed(8675309) strata(`svystratvar') cluster(`clusterid') idcluster(newid) : calcicc `outcome' newid 
			
			* Usually there will be 2+ clusters per stratum and the bootstrap will run; if so, update lb and ub
			if _rc == 0 {
				matrix out = e(ci_percentile)
				scalar go_31 = out[1,1]
				scalar go_32 = out[2,1]
				matrix drop out
				scalar go_33 = "ANOVA estimator (bootstrap percentiles)"
			}
		}
		
		scalar go_34 = "`comment'"
					
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
				  (go_44) (go_45) (go_46) (go_47) (go_48) (go_49) (go_50) ///
				  (go_51)

		capture erase oppdata.dta
	}
}

capture postclose iout

use `tempiout', clear
order m_avg_wtd, after(m_avg) // move m_avg_wtd to be after m_avg
order n_strata, after(n_clusters) // move n_strata to be after n_clusters
order ms_between ms_within, after(icc_ci_method)

clonevar p_lb_wilson = p_lb // put a copy of lb and ub into variables with the word wilson in the name
clonevar p_ub_wilson = p_ub
order p_lb_wilson p_ub_wilson, after(p_ub_wald)

* If svypd could not calculate Wald or logit CIs, wipe them out
foreach cib in wald logit  {
    replace p_lb_`cib' = . if inlist(p,0,100) | p_se < 1E-15
    replace p_ub_`cib' = . if inlist(p,0,100) | p_se < 1E-15
}

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
label variable p_lb_wilson          "Coverage Estimate CI Lower Bound (Wilson)"
label variable p_ub_wilson          "Coverage Estimate CI Upper Bound (Wilson)"
label variable ms_between			"ANOVA mean square between clusters"
label variable ms_within            "ANOVA mean square within clusters"
label variable homog_cvg            "Coverage is homogeneous across clusters"

compress
save, replace

clear

capture use `output', clear
append using `tempiout', force
save `output', replace


end

