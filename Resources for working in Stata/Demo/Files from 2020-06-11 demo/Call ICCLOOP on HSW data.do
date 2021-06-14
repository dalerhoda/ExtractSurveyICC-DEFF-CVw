* 2020-06-11 Demo

* The first time thru we invoke the 'replace' option to start with a clean output dataset
*

iccloop, id(0) ///
         row(0) ///
         data(sia_for_icc_project) ///
         outcomelist(got_sia_dose) ///
         output(demo_iccs) ///
         stratlevel(1) ///
         stratvar(strat) ///
         agegroup(Under 5) ///
         weightvar(weight) ///
         svyset(svyset clust, strata(strat) weight(weight)) ///
         comment(Measles-Rubella Campaign) ///
         replace
		 
* Make 3 changes: 
* 1. Remove replace option
* 2. Change stratlevel to 2 (because these results will be from a subset of the data)
* 3. Change stratvar to urban_cluster so we will obtain one row of output for
*    urban clusters and one row for rural clusters

iccloop, id(0) ///
         row(0) ///
         data(sia_for_icc_project) ///
         outcomelist(got_sia_dose) ///
         output(demo_iccs) ///
         stratlevel(2) ///
         stratvar(urban_cluster) ///
         agegroup(Under 5) ///
         weightvar(weight) ///
         svyset(svyset clust, strata(strat) weight(weight)) ///
         comment(Measles-Rubella Campaign) 




