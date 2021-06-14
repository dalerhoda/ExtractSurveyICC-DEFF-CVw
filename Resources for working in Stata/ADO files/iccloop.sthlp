{smcl}
{* *! version 1.0 18 May 2020}{...}
{vieweralsosee "" "--"}{...}
{viewerjumpto "Description" "iccloop##description"}{...}
{viewerjumpto "Required Input" "iccloop##inputdata"}{...}
{viewerjumpto "Optional Input" "iccloop##optional"}{...}
{viewerjumpto "Examples" "iccloop##examples"}{...}
{viewerjumpto "Author" "iccloop##author"}{...}
{title:Title}

{phang}
{bf:iccloop} {hline 2} Extract ICC, DEFF, CVw and other parameters from vaccination coverage cluster survey dataset for 2020 manuscript 

{marker syntax}{...}
{title:Syntax}

{cmdab:iccloop} , {help iccloop##required:ID}(passthru integer)
            {help iccloop##required:DATAset}(dataset name) 
            {help iccloop##required:OUTCOMElist}(varlist)
            {help iccloop##required:STRATvar}(varname)
            {help iccloop##required:OUTPUT}(dataset name)
          [ {help iccloop##optional1:STRATLEVEL}(passthru integer) 
            {help iccloop##optional1:ROW}(passthru integer)
            {help iccloop##optional2:REPLACE} 
            {help iccloop##optional2:STRATNAMEvar}(varname)
            {help iccloop##optional2:AGEGROUP}(passthru string)
            {help iccloop##optional2:COMMENT}(passthru string)
            {help iccloop##optional2:SVYSET}(string)   
            {help iccloop##optional2:WEIGHTvar}(varname)
            {help iccloop##optional2:SELFweighted}]

{p 8 17 2}

{synoptline}
{p2colreset}{...}
{p 4 6 2}

{title:Helpfile Sections}
{pmore}{help iccloop##description:Description} {p_end}
{pmore}{help iccloop##inputdata:Required Input}{p_end}
{pmore}{help iccloop##optional:Optional Input}{p_end}
{pmore}{help iccloop##author:Author}{p_end}

{marker description}{...}
{title:Description}

{pstd} {cmd:iccloop} is a program used to extract parameters from a vaccination coverage 
   cluster survey dataset to be included in a 2020 manuscript being coordinated by 
   Dale Rhoda at Biostat Global Consulting. {p_end}

{pstd} There are several PDF documents that accompany the program to give context and 
   describe how to prepare a dataset before you run the iccloop commmand.  
   Contact Dale if you do not have those PDFs available to you. {p_end}

{hline}
{marker required}
{title:Required Input} 

{pstd} {bf:ID} {it:(integer)} - A study ID integer that is passed thru to the output 
   dataset; Dale will tell you what value to use for your survey.  You may use the value 
   1 if you haven't received other direction from Dale. {p_end}

{pstd} {bf:DATAset} {it:(dataset name)} - This is the name of the Stata dataset that holds the survey data. {p_end}

{pstd} {bf:OUTCOMEMlist} {it:(varlist)} - A list of one or more binary survey outcome variables. {p_end}

{pstd} {bf:STRATvar} {it:(varname)} - The variable in the survey dataset that defines the strata over which the program should loop.  
  If you are documenting ICCs for urban and rural clusters, this might be urban_cluster.  
  The program will determine all the levels of this variable and will extract parameters 
  for each level.  In the urban rural example, iccloop would extract parameters for all 
  the rural clusters and then again for all the urban clusters. {p_end}

{pstd} {bf:OUTPUT} {it:(dataset name)} - Name of the output dataset.  This dataset will be saved 
   in the same folder as the survey dataset.  If the user specifies the REPLACE option, then any 
   previous dataset with this name will be over-written.  Otherwise the output will be appended 
   to whatever is already in that dataset. {p_end}

{marker optional1}
{title:Optional Input} 

{pstd} {bf:STRATLEVEL} {it:(integer)} - Passed thru to output dataset.  Use 1 for national results, 
   2 for sub-national results, and 3 for sub-sub-national.  {p_end}

{pstd} {bf:ROW} {it:(integer)} - Anther passthru integer.  Use the value Dale gives you, or use the value 1.  {p_end}

{pstd} {bf:REPLACE} - Replace the output dataset, if it already exists.  {p_end}

{pstd} {bf:STRATNAMEvar} {it:(varname)} - If {it:stratvar} does not have a value label 
   use this option to specify which string variable holds the stratum name.  If {it:stratvar} 
   DOES have a value label then this option is not needed.  {p_end}

{pstd} {bf:AGEGROUP} {it:(string)} - This is simply passed thru to the output dataset. {p_end}

{pstd} {bf:COMMENT} {it:(string)} - This is also passed thru to the output dataset.  We sometimes
   use it to document what the SIA dose was: e.g., MR or JE.  {p_end}

{pstd} {bf:SVYSET} {it:(string)} - By default iccloop uses this command: {break}{break}{bf:svyset clusterid, strata(stratumid) weight(psweight) singleunit(scaled)}{p_end}
{pstd} If your dataset uses a differnt svyset syntax, specify the entire svyset line of syntax here.  e.g., {break}{break}{bf:svyset(svyset psuid, strata(sample_stratum) weight(survey_weight) singleunit(scaled))}{break}  {p_end}
{pstd} Note: I recommend always including {bf:singleunit(scaled)} at the end of the svyset line as it sometimes fixes irritating problems.{p_end}

{pstd} {bf:WEIGHTvar} {it:(varname)} - Variable that holds the survey weight.  If you name your 
   variables as we do, and use the name psweight, this option is not needed.  
   If your weight variable has another name, specify it with this option.  {p_end}

{pstd} {bf:SELFweighted} - If your survey was self-weighted or un-weighted, you 
   may simply specify the SELF option.  Otherwise you'll need to generate a weight 
   varible and give every respondent the same value.  {p_end}

{hline}

{marker examples}{...}
{title:Examples}

Note: We always cd (change the working directory) to the folder that holds the survey dataset before running iccloop.

See the PDFs from Dale and the .do file in the .zip file from Dale for examples of 
   how to run iccloop.  Do not hesitate to contact us with questions.

{marker author}{...}
{title:Authors}
{p}

Dale Rhoda & Becca Robinson, Biostat Global Consulting

Email {browse "mailto:Dale.Rhoda@biostatglobal.com":Dale.Rhoda@biostatglobal.com}

{hline}
		
