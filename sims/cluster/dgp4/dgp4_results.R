##########
# Generates data for simulation dgp 4.
# Relies on a SLURM-managed cluster.
t1 <- Sys.time()

# *** MUST READ ***
# these two directories must be specified by the user 
# the user_home_dir *must* initially contain two empty sub-directories titled dgp4_datasets/ and dgp4_results/
libs <- "/nas/longleaf/home/srosin/RLibs/" # directory containing R packages
user_home_dir <- "/nas/longleaf/home/srosin/rgStandardized/" # top-level directory for the project

setwd(user_home_dir)
library(tidyverse)

# load helpful functions and parameter values from source files, 
# all of which are in the top-level user_home_dir
source("estimation_fns.R")
source("sim_fns.R")
source("param_values_dgp4.R")

# Read in simulation data
sim <- Sys.getenv("SLURM_ARRAY_TASK_ID") # get the number of the simulation
load((paste("dgp4_datasets/sim_", sim, ".RData", sep=""))) # load .RData file

# sim parameter values
n_strata <- 80 # number of strata for this scenario
vars_std <- c("z1", "z2", "z3", "z4")

# declare several empty columns in sim_conditions
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
  sim_conditions[i, "hat_pi_RG"] <- truncate_01(hat_pi_RG_vec[1])
  sim_conditions[i, "hat_var_pi_RG"] <- hat_pi_RG_vec[2]
  sim_conditions[i, "ci_lower_pi_RG"] <- truncate_01(hat_pi_RG_vec[1] - 
                                                       qnorm(1 - alpha_level / 2) * sqrt(hat_pi_RG_vec[2]))
  sim_conditions[i, "ci_upper_pi_RG"] <- truncate_01(hat_pi_RG_vec[1] + 
                                                       qnorm(1 - alpha_level / 2) * sqrt(hat_pi_RG_vec[2]))
  sim_conditions[i, "covers_pi_RG"] <-ifelse(
    (sim_conditions[i,"ci_lower_pi_RG"] < row$prev) && 
      (sim_conditions[i,"ci_upper_pi_RG"] > row$prev), 1, 0)
  
  # standardized estimate
  hat_pi_SRG_vec <- ests_std(sample_list[[i]], row$sigma_e_hat, row$sigma_p_hat, 
                             row$n_1, row$n_2, row$n_3, vars_std, variance = TRUE)
  
  sim_conditions[i, "hat_pi_SRG"] <- truncate_01(hat_pi_SRG_vec[1])
  sim_conditions[i, "hat_var_pi_SRG"] <- hat_pi_SRG_vec[2]
  sim_conditions[i, "ci_lower_pi_SRG"] <- truncate_01(hat_pi_SRG_vec[1] - 
                                                        qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRG_vec[2]))
  sim_conditions[i, "ci_upper_pi_SRG"] <- truncate_01(hat_pi_SRG_vec[1] + 
                                                        qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRG_vec[2]))
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
    vars_std = c("z1", "z2", "z3", "z4"),
    mod_formula = formula("x ~ z1 + z2 + z3 + z4"),
    variance = TRUE
  )
  
  sim_conditions[i, "hat_pi_SRGM"] <- truncate_01(hat_pi_SRGM_vec[1])
  sim_conditions[i, "hat_var_pi_SRGM"] <- hat_pi_SRGM_vec[2]
  sim_conditions[i, "ci_lower_pi_SRGM"] <- truncate_01(hat_pi_SRGM_vec[1] - 
                                                         qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRGM_vec[2]))
  sim_conditions[i, "ci_upper_pi_SRGM"] <- truncate_01(hat_pi_SRGM_vec[1] + 
                                                         qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRGM_vec[2]))
  sim_conditions[i, "covers_pi_SRGM"] <- ifelse(
    (sim_conditions[i, "ci_lower_pi_SRGM"] < row$prev) && 
      (sim_conditions[i, "ci_upper_pi_SRGM"] > row$prev), 1, 0)
}

t2 <- Sys.time()
print(t2 - t1)
sim_results <- sim_conditions %>% select(-c(stratum_props))
output_filename <- paste("dgp4_results/results_", sim, ".csv", sep="")
write_csv(sim_results, output_filename)

