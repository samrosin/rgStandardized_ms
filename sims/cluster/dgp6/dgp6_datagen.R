##########
# Generates data for simulation DGP 6.
# Relies on a SLURM-managed cluster and uses array method for speed of computation

# these two directories must be specified by the user 
libs <- "/nas/longleaf/home/srosin/RLibs/" # directory containing R packages
user_home_dir <- "/nas/longleaf/home/srosin/rgStandardized/" # top-level directory for the project

# sim parameter values
sim <- Sys.getenv("SLURM_ARRAY_TASK_ID") # get the array ID to use as the random seed
set.seed(sim)
n_strata <- 80 # number of strata for this scenario
vars_std <- c("z1", "z2", "z3", "z4")
prevs <- seq(.005, .2, by = .005)

setwd(user_home_dir)
library(tidyverse)

# load helpful functions and parameter values from source files, 
# all of which are in the top-level user_home_dir
source("estimation_fns.R")
source("sim_fns.R")
source("param_values_var.R")

# The known stratum proportions (the gamma_{zj}s) must be prespecified,
# and they are loaded here
gammas <- read_csv("dgp4_stratum_props.csv",
                   col_types = cols(
                     z1 = col_character(), 
                     z2 = col_character(), 
                     z3 = col_character(), 
                     z4 = col_character(), 
                     stratum_prop = col_double(),
                     sampling_prob = col_double()
                   ))

# Create copies of the gamma (stratum_prop) dataframe,
# with stratum-specific prevalence created from a true logistic model. 
# The intercept of the logistic model varies to vary the marginal prevalence
stratum_props <- vector(mode = "list", length = length(prevs)) # create list of stratum proportion dataframes
for(p in 1:length(prevs)){
  s <- gammas %>% dplyr::mutate(
    prev = inv.logit(nu_0[p]+nu_1*(gammas$z1=="z11")+
                       nu_2*(gammas$z2=="z20")+nu_3*(gammas$z2=="z21") +
                       nu_4*(gammas$z3=="z30")+nu_5*(gammas$z3=="z31") + 
                       nu_6*(gammas$z4=="z41"))
  )
  stratum_props[[p]] <- s
}

# Uncomment to print prevalences, checking that they are, e.g., {.01, .05, .2}
for(s in 1:length(stratum_props)){
  print(sum(stratum_props[[s]]$stratum_prop * stratum_props[[s]]$prev))
}

# fully factorial combination of sample sizes and parameters, 
# where each row is a sub-scenario
sim_conditions <- tidyr::crossing(
  n_1, n_2, n_3, sigma_e, sigma_p, stratum_props) %>% 
  rowwise() %>% 
  mutate(prev = sum(stratum_props$stratum_prop * stratum_props$prev),
         sigma_e_hat = NA_real_,
         sigma_p_hat = NA_real_,
         rho_hat = NA_real_,
         hat_pi_RG = NA_real_,
         hat_pi_SRG = NA_real_, 
         hat_pi_SRGM = NA_real_  
  )

sample_list <- vector(mode = "list", length = nrow(sim_conditions)) # empty list to store each sample
  
  # simulate the data, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  row <- sim_conditions[i,]
  dat <- gen_data_dgp5(row$n_1, row$sigma_e, row$n_2, row$sigma_p, 
                            row$n_3, as.data.frame(row$stratum_props), vars_std)
  sim_conditions[i,"sigma_e_hat"] <- dat$sigma_e_hat
  sim_conditions[i,"sigma_p_hat"] <- dat$sigma_p_hat
  sim_conditions[i,"rho_hat"] <- dat$rho_hat
  sample_list[[i]] <- dat$sample # store the entire sample 
}
  # store data in an .RData file. This data format is used because 
  # sim_conditions is a dataframe, itself containing a dataframe as a column (stratum_props)
  output_filename <- paste("dgp6_datasets/sim_",sim,".RData",sep="")
  save(sim_conditions,sample_list, file = output_filename)

