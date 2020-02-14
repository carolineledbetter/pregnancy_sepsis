# Code
Code contains all R scripts. 


Details about the files:

File | Description
---|---------------------------------------------------------------------
Data Clean Source | contains the source code for creating/cleaning all analysis variables.  
Data Clean All | loads the raw data for each year, runs it through the source code and saves it for analysis.  
Descriptives | contains the code for all the descriptive tables.  
Data Labels | creates a data frame to match short variable names to pretty formatted ones.
Assisted Vaginal Delivery | retrieves all dx codes classified as assisted vaginal delivery present in the data.  
Infection and Parasitic Delivery | retrieves all dx codes classified as infection and parasitic diseases present in the data.   
Table1 | is a function to created weighted and unweighted descriptive tables from categorical and continuous variables.  
TableTorF | is a function to create weighted and unweighted descriptives for logical variables.  
Tables | contains the source code for weighted and unweighted tables 1-6.  
Q-values | contains the code to rank p_values, determine which potential covariates exist in less than 5% of observations, and determine a q-value cutoff.  
