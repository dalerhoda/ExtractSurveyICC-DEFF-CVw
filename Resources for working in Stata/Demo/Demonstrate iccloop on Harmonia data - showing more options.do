********************************************************************************
* This program demonstrates running the iccloop program four times
* for TT, four times for SIA, and four times for RI.
*
* 
* When finished, it appends the TT, SIA and RI ICCs all toegether and 
* makes a simple plot.
*
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

************************
*
* TT
*

* The first time thru we invoke the 'replace' option to start with a clean output dataset
*
* We will save the output in a new dataset named tt_iccs

iccloop, id(1) ///
         row(3) ///
         data(tt_example_dataset_Harmonia) ///
         outcomelist(protected_at_birth_to_analyze) ///
         output(tt_iccs) ///
         stratlevel(1) ///
         stratvar(level1id) ///
         stratnamevar(level1name) ///
         agegroup(Gave birth in the last 12m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable) ///
         replace
		 
* The next three calls do not use the 'replace' option because we want to append the output

iccloop, id(1) ///
         row(3) ///
         data(tt_example_dataset_Harmonia) ///
         outcomelist(protected_at_birth_to_analyze) ///
         output(tt_iccs) ///
         stratlevel(2) ///
         stratvar(level2id) ///
         stratnamevar(level2name) ///
         agegroup(Gave birth in the last 12m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable) 

iccloop, id(1) ///
         row(3) ///
         data(tt_example_dataset_Harmonia) ///
         outcomelist(protected_at_birth_to_analyze) ///
         output(tt_iccs) ///
         stratlevel(3) ///
         stratvar(level3id) ///
         stratnamevar(level3name) ///
         agegroup(Gave birth in the last 12m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable)

iccloop, id(1) ///
         row(3) ///
         data(tt_example_dataset_Harmonia) ///
         outcomelist(protected_at_birth_to_analyze) ///
         output(tt_iccs) ///
         stratlevel(2) ///
         stratvar(urban_cluster) ///
         agegroup(Gave birth in the last 12m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable)

************************
*
* SIA - MR
* 

* We will save the output in a new dataset named sia_iccs.  

iccloop, id(1) ///
         row(2) ///
         data(sia_example_dataset_Harmonia) ///
         outcomelist(got_sia_dose) ///
         output(sia_iccs) ///
         stratlevel(1) ///
         stratvar(level1id) ///
         stratnamevar(level1name) ///
         agegroup(6-59m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(MR) ///
         replace
         
iccloop, id(1) ///
         row(2) ///
         data(sia_example_dataset_Harmonia) ///
         outcomelist(got_sia_dose) ///
         output(sia_iccs) ///
         stratlevel(2) ///
         stratvar(level2id) ///
         stratnamevar(level2name) ///
         agegroup(6-59m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(MR)
         
iccloop, id(1) ///
         row(2) ///
         data(sia_example_dataset_Harmonia) ///
         outcomelist(got_sia_dose) ///
         output(sia_iccs) ///
         stratlevel(3) ///
         stratvar(level3id) ///
         stratnamevar(level3name) ///
         agegroup(6-59m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(MR)
         
iccloop, id(1) ///
         row(2) ///
         data(sia_example_dataset_Harmonia) ///
         outcomelist(got_sia_dose) ///
         output(sia_iccs) ///
         stratlevel(2) ///
         stratvar(urban_cluster) ///
         agegroup(6-59m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(MR)

************************
*
* RI
* 

* Output goes to a new dataset named ri_iccs

iccloop, id(1) ///
         row(1) ///
         data(ri_example_dataset_Harmonia) ///
         outcomelist(had_card got_crude_bcg_to_analyze got_crude_penta1_to_analyze got_crude_penta3_to_analyze got_crude_mcv1_to_analyze) ///
         output(ri_iccs) ///
         stratlevel(1) ///
         stratvar(level1id) ///
         stratnamevar(level1name) ///
         agegroup(12-23m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable) ///
         replace
         
iccloop, id(1) ///
         row(1) ///
         data(ri_example_dataset_Harmonia) ///
         outcomelist(had_card got_crude_bcg_to_analyze got_crude_penta1_to_analyze got_crude_penta3_to_analyze got_crude_mcv1_to_analyze) ///
         output(ri_iccs) ///
         stratlevel(2) ///
         stratvar(level2id) ///
         stratnamevar(level2name) ///
         agegroup(12-23m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable) 
                  
iccloop, id(1) ///
         row(1) ///
         data(ri_example_dataset_Harmonia) ///
         outcomelist(had_card got_crude_bcg_to_analyze got_crude_penta1_to_analyze got_crude_penta3_to_analyze got_crude_mcv1_to_analyze) ///
         output(ri_iccs) ///
         stratlevel(3) ///
         stratvar(level3id) ///
         stratnamevar(level3name) ///
         agegroup(12-23m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable) 
                  
iccloop, id(1) ///
         row(1) ///
         data(ri_example_dataset_Harmonia) ///
         outcomelist(had_card got_crude_bcg_to_analyze got_crude_penta1_to_analyze got_crude_penta3_to_analyze got_crude_mcv1_to_analyze) ///
         output(ri_iccs) ///
         stratlevel(2) ///
         stratvar(urban_cluster) ///
         agegroup(12-23m) ///
         weightvar(psweight) ///
         svyset(svyset clusterid, strata(stratumid) weight(psweight)) ///
         comment(Place comment here, if applicable) 
         
*************************
*
* Append three datasets together & make a figure
*

use ri_iccs, clear
append using sia_iccs, force
append using tt_iccs, force
save Harmonia_ICCs, replace

gen label = ""
replace label = "National" if stratvar == "level1id"
replace label = "Province" if stratvar == "level2id"
replace label = "District" if stratvar == "level3id"
replace label = "Urban/Rural" if stratvar == "urban_cluster"

twoway (rspike icc_lb icc_ub p) ///
       (scatter icc p if dataset_name == "ri_example_dataset_Harmonia") ///
       (scatter icc p if dataset_name == "sia_example_dataset_Harmonia") ///
       (scatter icc p if dataset_name == "tt_example_dataset_Harmonia") ///	   
	   , by(label, title(Harmonia ICCs) note("Red lines appear at 1/6 and 1/3") graphregion(color(white)) ) ylabel(, angle(0))  ytitle(ICC) xtitle(Estimated Coverage (%)) yline(`=1/6' `=1/3', lcolor(red)) legend(rows(1) order( 2 "RI" 3 "SIA" 4 "TT")) 

graph export Harmonia_ICCs.png, width(2000) replace
