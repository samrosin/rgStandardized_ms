##########
# Generates data for simulation DGP 3.
# Relies on a SLURM-managed cluster.

# these two directories must be specified by the user 
libs <- "/nas/longleaf/home/srosin/RLibs/" # directory containing R packages
user_home_dir <- "/nas/longleaf/home/srosin/rgStandardized/" # top-level directory for the project

setwd(user_home_dir)
library(tidyverse)
library(fastDummies)

# load helpful functions and parameter values from source files, 
# all of which are in the top-level user_home_dir
source("estimation_fns.R")
source("sim_fns.R")
source("sim_param_values_variance.R")

# Read in simulation data
sim <- Sys.getenv("SLURM_ARRAY_TASK_ID") # get the number of the simulation
load((paste("scenario3_var_datasets/sim_", sim, ".RData", sep=""))) # load .RData file

# sim parameter values
n_strata <- 40 # number of strata for this scenario
vars_std <- c("z1", "z2", "z3")

# add some NA_real_ results columns to sim_conditions dataframe
sim_conditions <- sim_conditions %>% mutate(
  hat_pi_RG = NA_real_,
  hat_var_pi_RG = NA_real_,
  ci_lower_pi_RG = NA_real_,
  ci_upper_pi_RG = NA_real_,
  covers_pi_RG = NA_real_, # coverage proporation for hat_var_pi
  
  hat_pi_SRG = NA_real_, 
  hat_var_pi_SRG = NA_real_,
  ci_lower_pi_SRG = NA_real_,
  ci_upper_pi_SRG = NA_real_,
  covers_pi_SRG = NA_real_, # coverage proportion for hat_var_pi_SRG
  
  hat_pi_SRG_restriction = NA_real_, 
  hat_var_pi_SRG_restriction = NA_real_,
  ci_lower_pi_SRG_restriction = NA_real_,
  ci_upper_pi_SRG_restriction = NA_real_,
  covers_pi_SRG_restriction = NA_real_, # coverage proportion for hat_var_pi_SRG
  
  hat_pi_SRGM = NA_real_,
  hat_var_pi_SRGM = NA_real_,
  ci_lower_pi_SRGM = NA_real_,
  ci_upper_pi_SRGM = NA_real_,
  covers_pi_SRGM = NA_real_, # coverage proportion for hat_var_pi_SRGM
  
  strata_obs = NA_real_, # how many strata were observed in this simulation?
  positivity = NA
)

for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi_RG_vec <- ests_rg(row$rho_hat, row$sigma_e_hat, row$sigma_p_hat, 
                           row$n_1, row$n_2, row$n_3, variance = TRUE)
  sim_conditions[i, "hat_pi_RG"] <- hat_pi_RG_vec[1]
  sim_conditions[i, "hat_var_pi_RG"] <- hat_pi_RG_vec[2]
  sim_conditions[i, "ci_lower_pi_RG"] <- hat_pi_RG_vec[1] - 
    qnorm(1 - alpha_level / 2) * sqrt(hat_pi_RG_vec[2])
  sim_conditions[i, "ci_upper_pi_RG"] <- hat_pi_RG_vec[1] + 
    qnorm(1 - alpha_level / 2) * sqrt(hat_pi_RG_vec[2])
  sim_conditions[i, "covers_pi_RG"] <-ifelse(
    (sim_conditions[i,"ci_lower_pi_RG"] < row$prev) && 
      (sim_conditions[i,"ci_upper_pi_RG"] > row$prev), 1, 0)
  
  # standardized estimate
  hat_pi_SRG_vec <- ests_std(sample_list[[i]], row$sigma_e_hat, row$sigma_p_hat, 
                             row$n_1, row$n_2, row$n_3, vars_std, variance = TRUE)
  
  sim_conditions[i, "hat_pi_SRG"] <- hat_pi_SRG_vec[1]
  sim_conditions[i, "hat_var_pi_SRG"] <- hat_pi_SRG_vec[2]
  sim_conditions[i, "ci_lower_pi_SRG"] <- hat_pi_SRG_vec[1] - 
    qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRG_vec[2])
  sim_conditions[i, "ci_upper_pi_SRG"] <- hat_pi_SRG_vec[1] + 
    qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRG_vec[2])
  sim_conditions[i, "covers_pi_SRG"] <- ifelse(
    (sim_conditions[i, "ci_lower_pi_SRG"] < row$prev) && 
      (sim_conditions[i, "ci_upper_pi_SRG"] > row$prev), 1, 0)
  
  sim_conditions[i, "strata_obs"] <- hat_pi_SRG_vec[3] # Note we can get this info from either standardization estimator,
  # hat_pi_SRG or hat_pi_SRGM; here I take it from hat_pi_SRG
  sim_conditions[i, "positivity"] <- ifelse(hat_pi_SRG_vec[3] < n_strata, FALSE, TRUE)
  
  # get model standardized estimates
  hat_pi_SRGM_vec <- ests_std_model(
    sample_list[[i]], as.data.frame(row$stratum_props), row$sigma_e_hat,
    row$sigma_p_hat, row$n_1, row$n_2, row$n_3, 
    vars_std = c("z1_z11", "z2_z20", "z2_z21", "z3_z30", "z3_z31"),
    mod_formula = formula("x ~ z1_z11 + z2_z20 + z2_z21 + z3_z30 + z3_z31"), 
    variance = TRUE
  )
  
  sim_conditions[i, "hat_pi_SRGM"] <- hat_pi_SRGM_vec[1]
  sim_conditions[i, "hat_var_pi_SRGM"] <- hat_pi_SRGM_vec[2]
  sim_conditions[i, "ci_lower_pi_SRGM"] <- hat_pi_SRGM_vec[1] - 
    qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRGM_vec[2])
  sim_conditions[i, "ci_upper_pi_SRGM"] <- hat_pi_SRGM_vec[1] + 
    qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRGM_vec[2])
  sim_conditions[i, "covers_pi_SRGM"] <- ifelse(
    (sim_conditions[i, "ci_lower_pi_SRGM"] < row$prev) && 
      (sim_conditions[i, "ci_upper_pi_SRGM"] > row$prev), 1, 0)
}

sim_results <- sim_conditions %>% select(-c(stratum_props))
output_filename <- paste("scenario3_var_results/results_", sim, ".csv", sep="")
write_csv(sim_results, output_filename)

