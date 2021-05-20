# rgStandardized
Standardized Rogan-Gladen method for prevalence estimation, with applications to SARS-CoV-2 seroprevalence. Manuscript in preparation. 

This code in this repository is currently in draft form and there may be errors included. With that disclaimer, estimators are implemented in estimation_fns.R, and a nonparametric standardization estimator is also implemented in rgStandardized_worksheet.xlsx. The worksheet can be used for entering data from a seroprevalence study to obtain standardized Rogan-Gladen estimates with confidence intervals. 

Code for a simulation study is in the sims directory. Bias sims are in the bias subdirectory and variance sims are in the var and cluster subdirectories. Four data-generating processes (DGPs) are considered, and DGPs 3 and 4 are more computing-intensive so they are best implemented on a SLURM-managed cluster. Running the scripts puts results in .csv files in the results_draft directory, and final results should be moved into the results_final directory, from which they can be analyzed using the plot_bias.R and plot_var.R scripts. Parameter values like sensitivity, specificity, sample sizes, etc. can be tweaked within the inputs subdirectory. 

Please send any questions, comments, etc. to Sam Rosin at srosin@live.unc.edu. 
