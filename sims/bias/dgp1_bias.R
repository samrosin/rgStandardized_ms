##########
# Conducts simulation for DGP 1 
time1 <- Sys.time() 

# sim parameter values
set.seed(2021)
n_sims <- 1000 #number of simulations
prev <- seq(.01, .20, by = .01) #true prevalence 

library(tidyverse)
library(looplot) #install with devtools::install_github("matherealize/looplot")
library(here)

source(here("estimation_fns.R"))
source(here("sims/inputs/param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# 'results_final' subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/dgp1_results.csv")
  
# fully factorial combination of sample sizes and parameters
# each row is a sub-scenario
sim_conditions <- crossing(n_1, n_2, n_3, sigma_e, sigma_p, prev) %>% 
  mutate(hat_pi_RG = NA_real_)

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi_RG <- rep(NA, n_sims)
  
  #iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_dgp1(row$n_1, row$sigma_e, row$n_2, 
             row$sigma_p, row$n_3, row$prev)
    hat_pi_RG[j] <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                         row$n_1, row$n_2, row$n_3, variance = FALSE)
  }
  
  #compute mean relative bias of the finite estimates for the sub-scenario
  sim_conditions[i,"hat_pi_RG"] <- 100 * ( 
    mean(hat_pi_RG[is.finite(hat_pi_RG)]) - row$prev ) / row$prev 
}

# write results to an output file 
sim_conditions <- sim_conditions %>% dplyr::rename(pi = prev)
sim_conditions$n_sims <- NA_real_
sim_conditions[1,"n_sims"] <- n_sims # store number of simulations (in a non-tidy format, but it is useful to store this value somewhere)
write_csv(sim_conditions, output_file)

print(sim_conditions)
time2 <- Sys.time()
print(time2 - time1)

