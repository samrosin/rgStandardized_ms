##########
# Conducts simulation scenario 1 
time1 <- Sys.time() 

library(tidyverse)
library(looplot) #install with devtools::install_github("matherealize/looplot")
library(here)

source(here("estimation_fns.R"))
source(here("sims/input_files/sim_param_values_variance.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# 'results_final' subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/scenario1_var_results.csv")

# sim parameter values
set.seed(2021)
n_sims <- 500 #number of simulations
prev <- c(.005,.05,.3) #true prevalence 

# fully factorial combination of sample sizes and parameters
# each row is a sub-scenario
sim_conditions <- crossing(n_1, n_2, n_3, sigma_e, sigma_p, prev) %>% 
  mutate(hat_pi = NA_real_,
         ESE_hat_pi = NA_real_, # empirical SE of hat_var_pi
         ASE_hat_pi = NA_real_, # mean asymptotic SE; mean of sqrt(hat_var_pi)
         covers_pi = NA_real_, # coverage proporation using hat_var_pi
  )

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  row <- sim_conditions[i,]
  hat_pi <- rep(NA, n_sims)
  hat_var_pi <- rep(NA, n_sims)
  ci_lower_pi <- rep(NA, n_sims)
  ci_upper_pi <- rep(NA, n_sims)
  covers_pi <- rep(NA, n_sims)
  
  #iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_scenario1(row$n_1, row$sigma_e, row$n_2, 
                              row$sigma_p, row$n_3, row$prev)
    hat_pi_vec <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                         row$n_1, row$n_2, row$n_3, variance = TRUE)
    hat_pi[j] <- hat_pi_vec[1]
    hat_var_pi[j] <- hat_pi_vec[2]
    ci_lower_pi[j] <- hat_pi_vec[1] - 
                      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_vec[2])
    ci_upper_pi[j] <- hat_pi_vec[1] + 
                      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_vec[2])
    covers_pi[j] <- ifelse(
      (ci_lower_pi[j] < row$prev) && (ci_upper_pi[j] > row$prev), 1, 0)
    
  }
  
  #compute mean relative bias of the finite estimates for the sub-scenario
  sim_conditions[i,"hat_pi"] <- 100 * ( 
    mean(hat_pi[is.finite(hat_pi)]) - row$prev ) / row$prev 
  sim_conditions[i, "ESE_hat_pi"] <- sd(hat_pi[is.finite(hat_pi)])
  sim_conditions[i, "ASE_hat_pi"] <- mean(sqrt(hat_var_pi[is.finite(hat_var_pi)]))
  sim_conditions[i, "covers_pi"] <- mean(covers_pi)
  
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
write_csv(sim_conditions, output_file)
time2 <- Sys.time()
print(time2 - time1)

