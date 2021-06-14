use sia_example_single_stratum, clear

* Explore the dataset
describe

* Explore clust
sum clust
tabulate clust

* Explore urban/rural
tab urban_cluster , m
tab urban_cluster , m nolabel

* Explore the outcome variable
tab got_sia_dose , m
tab got_sia_dose , m nolabel
* Note that it needs to be recoded !

* Explore the weight variable and its distribution
sum weight
hist weight

* Save with a new name before making changes
save sia_for_icc_project, replace

* Recode outcome
replace got_sia_dose = 0 if got_sia_dose == 2
label list gotit
label define gotit 0 "No" 1 "Yes", replace
tab got_sia_dose
tab got_sia_dose, m nolabel
save, replace

* Make an organ pipe plot to see how the outcome is distributed across clusters
opplot got_sia_dose , clustvar(clust ) weightvar(weight ) plotn nlinecolor(black)

* Calculate weighted coverage
svyset clust , weight(weight )
svy: proportion got_sia_dose
svy: proportion got_sia_dose, citype(wilson)

* Calculate CVw
sum weight
di r(sd)/r(mean)
di (1 + (r(sd)/r(mean))^2)

* Calculate ICC
calcicc got_sia_dose clust

* Generate the stratvar for iccloop
gen strat = 1

* In this case we give it a value label
label define stname 1 "Home Sweet Home"
label values strat stname
tab strat
label variable strat "Stratum ID"
save, replace

* Call iccloop once for the entire stratum; so stratvar == strat
* 
* Use the replace option so the output dataset will not contain any earlier results

iccloop, id(0) ///
         row(0) ///
         data(sia_for_icc_project) ///
         outcomelist(got_sia_dose) ///
         output(demo_iccs) ///
         stratlevel(1) ///        // use 1 because we are calculating parameters for the entire dataset
         stratvar(strat) ///
         agegroup(Under 5) ///
         weightvar(weight) ///
         svyset(svyset clust, strata(strat) weight(weight)) ///
         comment(Measles-Rubella Campaign) ///
         replace

		 
* Call iccloop again to extract urban and rural parameters
*
* Do NOT use the replace option here because we want to append to the output we made in the earlier call		 
		 
iccloop, id(0) ///
         row(0) ///
         data(sia_for_icc_project) ///
         outcomelist(got_sia_dose) ///
         output(demo_iccs) ///
         stratlevel(2) ///          // use 2 because each stratum is a subset of the entire dataset
         stratvar(urban_cluster) ///
         agegroup(Under 5) ///
         weightvar(weight) ///
         svyset(svyset clust, strata(strat) weight(weight)) ///
         comment(Measles-Rubella Campaign) 
	
