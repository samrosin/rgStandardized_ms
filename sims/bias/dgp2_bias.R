##########
# Conducts simulation for DGP 2
time1 <- Sys.time() 

# sim parameter values
set.seed(2021)
n_sims <- 50 # number of simulations
n_strata <- 2 # number of strata for this scenario
prevs <- seq(.01, .20, by = .01) 

library(tidyverse)
library(looplot) #install with devtools::install_github("matherealize/looplot")
library(here)

source(here("estimation_fns.R"))
source(here("sims/inputs/param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# results_final subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/dgp2_results.csv")

# The known stratum proportions (the gamma_js) must be prespecified. 
# Here the two strata have the same proportion, gamma_1 = gamma_2 = .5, 
# but the sampling probabilities are different, s_1 = .2 and s_2 = .8. 
# 
# The stratum-specific prevalences are defined to match the 
# true prevalences of .01, .04, .07, ..., .34, 
# where the first stratum has 1.5*pi and the second stratum 0.5*pi as their prevalences
prev_mat <- matrix(NA, nrow = length(prevs), ncol = 2) # create matrix of prevalences using common ratio
i <- 1
for(p in prevs){
  prev_mat[i,] <- c(1.5*p, 0.5*p) # common ratio for each stratum
  i <- i + 1
}

stratum_props <- vector(mode = "list", length = length(prevs)) # create list of stratum proportion dataframes
for(p in 1:length(prevs)){
  prev <- prev_mat[p,]
  dat <- data.frame(z = c("z1", "z2"),
                    stratum_prop = c(.5, .5),
                    prev = prev,
                    sampling_prob = c(.2, .8))
  stratum_props[[p]] <- dat
}

vars_std <- c("z")

# fully factorial combination of sample sizes and parameters, 
# where each row is a sub-scenario
sim_conditions <- tidyr::crossing(
  n_1, n_2, n_3, sigma_e, sigma_p, stratum_props) %>% 
  rowwise() %>% 
  mutate(prev = sum(stratum_props$stratum_prop*stratum_props$prev),
         hat_pi_RG = NA_real_,
         hat_pi_SRG = NA_real_, 
         num_infinite_pi_RG = NA_real_, # number of infinite estimates \hat \pi_RG
         num_infinite_pi_SRG = NA_real_, # number of infinite estimates \hat \pi_SRG
         n_strata_obs_full = NA_real_ # number of simulations with positivity (all strata observed)
  )

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi_RG <- rep(NA, n_sims)
  hat_pi_SRG <- rep(NA, n_sims)
  strata_obs <- rep(NA, n_sims) # number of observed strata in a sim
  
  # iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_dgp2(row$n_1, row$sigma_e, row$n_2, 
                       row$sigma_p, row$n_3, as.data.frame(row$stratum_props))
    hat_pi_RG[j] <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                         row$n_1, row$n_2, row$n_3, variance = FALSE)
    hat_pi_SRG_vec <- ests_std(dat$sample, dat$sigma_e_hat, dat$sigma_p_hat, 
                  row$n_1, row$n_2, row$n_3, vars_std, variance = FALSE)
    hat_pi_SRG[j] <- hat_pi_SRG_vec[1]
    strata_obs[j] <- hat_pi_SRG_vec[2]
  }
  
  # compute mean relative bias of the finite estimates for the sub-scenario,
  # and compute the other data of interest
  sim_conditions[i,"hat_pi_RG"] <- 100 * ( 
    mean(hat_pi_RG[is.finite(hat_pi_RG)]) - row$prev ) / row$prev 
  sim_conditions[i,"num_infinite_pi_RG"] <- sum(!is.finite(hat_pi_RG))
  sim_conditions[i,"hat_pi_SRG"] <- 100 * (
    mean(hat_pi_SRG[is.finite(hat_pi_SRG)]) - row$prev) / row$prev 
  sim_conditions[i,"num_infinite_pi_SRG"] <- sum(!is.finite(hat_pi_SRG))
  sim_conditions[i,"n_strata_obs_full"] <- sum(strata_obs == n_strata)
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
sim_results <- sim_conditions %>% dplyr::select(-c(stratum_props)) %>%
                                  dplyr::rename(pi = prev)
sim_results$n_sims <- NA_real_
sim_results[1,"n_sims"] <- n_sims # store number of simulations (in a non-tidy format, but it is useful to store this value somewhere)
#write_csv(sim_results, output_file)
sim_results
time2 <- Sys.time()
print(time2 - time1)

