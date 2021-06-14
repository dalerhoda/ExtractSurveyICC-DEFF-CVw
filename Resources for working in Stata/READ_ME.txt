We are still making some tweaks to the iccloop.ado program.  We will document the changes here.

The program is stable enough for you to practice with it.  To write a .do file that calls it several times to extract parameters from your dataset.

Please go ahead and try that and let us know if you need help.

Once we have settled on some final parameters for the program we will let everyone know to download the version we will all use to produce our parameter estimates, and you can come back here to download one more time and re-run your .do file.

Thank you,
-Dale

Date          Person            What changed
-------------------------------------------------------------------------------------------
2020-05-26		Dale Rhoda				Change iccloop to ALSO export the mean squares components
                                of the ICC calculation.
2020-05-27		Dale Rhoda				Yesterday's edit doesn't work with datasets with a large
                                number of clusters, so I edited iccloop again.  I also
                                added the loneway_plus.ado program to provide mean squares
                                for datasets with large numbers of clusters and the 
                                calcicc.ado program to calculate ICC based on the ANOVA
                                equation that is widely used, and described, for instance,
                                in Ridout, 1999.
                                
                                I see some odd ICC confidence interval results for a dataset
                                that has a lot of clusters with only one respondent, but 
                                of course that's not the sort of dataset we want to analyze
                                here, so we'll remove it from our loops.
2020-06-08	  Dale Rhoda		    Label the variables
2020-06-09	  Dale Rhoda		    The call to opplot now includes the
                                      weightvar option and the program 
                                      pulls the name of the survey sampling 
                                      stratum variable from the svyset output
                                      Also, do not truncate ICC at 0 or NEFF
                                      at N for this work.
2020-06-10	  Dale Rhoda		    Add homog_cvg flag to output
2020-07-30	  Wenfeng Gong	    Syntax edit to work with Stata v14, too                                