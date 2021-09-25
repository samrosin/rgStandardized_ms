##########
# Conducts simulation for dgp 1 
time1 <- Sys.time() 

# sim parameter values
set.seed(2021)
n_sims <- 1000 #number of simulations
prev <- seq(.01, .20, by = .01) #true prevalence 

library(tidyverse)
library(here)

source(here("estimation_fns.R"))
source(here("sims/inputs/param_values_var.R")) #load sim parameter values common across dgps
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# 'results_final' subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/dgp1_var_results.csv")

# fully factorial combination of sample sizes and parameters
# each row is a sub-scenario
sim_conditions <- crossing(n_1, n_2, n_3, sigma_e, sigma_p, prev) %>% 
  mutate(hat_pi_RG = NA_real_,
         ESE_hat_pi_RG = NA_real_, # empirical SE of hat_var_pi_RG
         ASE_hat_pi_RG = NA_real_, # mean asymptotic SE; mean of sqrt(hat_var_pi_RG)
         covers_pi_RG = NA_real_, # coverage proportion using hat_var_pi_RG
  )

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi_RG <- rep(NA, n_sims)
  hat_var_pi_RG <- rep(NA, n_sims)
  ci_lower_pi <- rep(NA, n_sims)
  ci_upper_pi <- rep(NA, n_sims)
  covers_pi_RG <- rep(NA, n_sims)
  
  #iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_dgp1(row$n_1, row$sigma_e, row$n_2, 
                              row$sigma_p, row$n_3, row$prev)
    hat_pi_RG_vec <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                         row$n_1, row$n_2, row$n_3, variance = TRUE)
    hat_pi_RG[j] <- truncate_01(hat_pi_RG_vec[1])
    hat_var_pi_RG[j] <- hat_pi_RG_vec[2]
    ci_lower_pi[j] <- truncate_01(hat_pi_RG_vec[1] - 
              qnorm(1 - alpha_level / 2) * sqrt(hat_pi_RG_vec[2]))
    ci_upper_pi[j] <- truncate_01(hat_pi_RG_vec[1] + 
              qnorm(1 - alpha_level / 2) * sqrt(hat_pi_RG_vec[2]))
    covers_pi_RG[j] <- ifelse(
      (ci_lower_pi[j] < row$prev) && (ci_upper_pi[j] > row$prev), 1, 0)
    
  }
  
  #compute mean relative bias of the finite estimates for the sub-scenario
  sim_conditions[i,"hat_pi_RG"] <- 100 * ( 
    mean(hat_pi_RG[is.finite(hat_pi_RG)]) - row$prev ) / row$prev 
  sim_conditions[i, "ESE_hat_pi_RG"] <- sd(hat_pi_RG[is.finite(hat_pi_RG)])
  sim_conditions[i, "ASE_hat_pi_RG"] <- mean(sqrt(hat_var_pi_RG[is.finite(hat_var_pi_RG)]))
  sim_conditions[i, "covers_pi_RG"] <- mean(covers_pi_RG)
  
}


# write results to an output file 
sim_conditions <- sim_conditions %>% dplyr::rename(pi = prev)
sim_conditions$n_sims <- NA_real_
sim_conditions[1,"n_sims"] <- n_sims # store number of simulations (in a non-tidy format, but it is useful to store this value somewhere)
write_csv(sim_conditions, output_file)

time2 <- Sys.time()
print(time2 - time1)

