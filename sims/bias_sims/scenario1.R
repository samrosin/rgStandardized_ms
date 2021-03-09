##########
# Conducts simulation scenario 1 
time1 <- Sys.time() 

library(tidyverse)
library(looplot) #install with devtools::install_github("matherealize/looplot")
library(here)

source(here("estimation_fns.R"))
source(here("sims/input_files/sim_param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# 'results_final' subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/scenario1_results.csv")

# sim parameter values
set.seed(2021)
n_sims <- 5 #number of simulations
prev <- c(.005,.05,.3) #true prevalence 

# fully factorial combination of sample sizes and parameters
# each row is a sub-scenario
sim_conditions <- crossing(n_1, n_2, n_3, sigma_e, sigma_p, prev) %>% 
  mutate(hat_pi = NA_real_)

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  row <- sim_conditions[i,]
  hat_pi <- rep(NA, n_sims)
  
  #iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_scenario1(row$n_1, row$sigma_e, row$n_2, 
             row$sigma_p, row$n_3, row$prev)
    hat_pi[j] <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                         row$n_1, row$n_2, row$n_3, variance = FALSE)
  }
  
  #compute mean relative bias of the finite estimates for the sub-scenario
  sim_conditions[i,"hat_pi"] <- 100 * ( 
    mean(hat_pi[is.finite(hat_pi)]) - row$prev ) / row$prev 
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
write_csv(sim_conditions, output_file)
#time2 <- Sys.time()
#print(time2 - time1)

