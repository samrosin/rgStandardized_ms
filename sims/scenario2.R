##########
# Conducts simulation scenario 2

library(tidyverse)
library(looplot) #install with devtools::install_github("matherealize/looplot")
library(here)

source(here("estimation_fns.R"))
source(here("sims/input_files/sim_param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# results_final subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/scenario2_results.csv")

# sim parameter values
set.seed(2021)
n_sims <- 2 # number of simulations
n_strata <- 2 # number of strata for this scenario

# The known stratum proportions (the gamma_js) must be prespecified. 
# Here the two strata have the same proportion, gamma_1 = gamma_2 = .5, 
# but the sampling probabilities are different, s_1 = .2 and s_2 = .8. 
# 
# The stratum-specific prevalences are defined to match the 
# true prevalences of .005, .05, and .30. 
stratum_props <- list(data.frame(z = c("z1","z2"), 
                                 stratum_prop = c(.5,.5),
                                 prev = c(.075, .025),
                                 sampling_prob = c(.2,.8)),
                      data.frame(z = c("z1","z2"),
                                 stratum_prop = c(.5,.5),
                                 prev = c(.5, .1),
                                 sampling_prob = c(.2,.8)),
                      data.frame(z = c("z1","z2"),
                                 stratum_prop = c(.5,.5),
                                 prev = c(.0075, .0025),
                                 sampling_prob = c(.2,.8))
                      )
vars_std <- c("z")


# fully factorial combination of sample sizes and parameters, 
# where each row is a sub-scenario
sim_conditions <- tidyr::crossing(
  n_1, n_2, n_3, sigma_e, sigma_p, stratum_props) %>% 
  rowwise() %>% 
  mutate(prev = sum(stratum_props$stratum_prop*stratum_props$prev),
         hat_pi = NA_real_,
         hat_pi_st = NA_real_, 
         num_infinite_pi = NA_real_, # number of infinite estimates \hat \pi
         num_infinite_pi_st = NA_real_, # number of infinite estimates \hat \pi_st
         n_strata_obs_full = NA_real_ # number of simulations with positivity (all strata observed)
  )

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  #print(i)
  row <- sim_conditions[i,]
  hat_pi <- rep(NA, n_sims)
  hat_pi_st <- rep(NA, n_sims)
  strata_obs <- rep(NA, n_sims) # number of observed strata in a sim
  
  # iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_scenario2(row$n_1, row$sigma_e, row$n_2, 
                       row$sigma_p, row$n_3, as.data.frame(row$stratum_props))
    hat_pi[j] <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                         row$n_1, row$n_2, row$n_3, variance = FALSE)
    hat_pi_st_vec <- ests_std(dat$sample, dat$sigma_e_hat, dat$sigma_p_hat, 
                  row$n_1, row$n_2, row$n_3, vars_std, variance = FALSE)
    hat_pi_st[j] <- hat_pi_st_vec[1]
    strata_obs[j] <- hat_pi_st_vec[2]
  }
  
  # compute mean relative bias of the finite estimates for the sub-scenario,
  # and compute the other data of interest
  sim_conditions[i,"hat_pi"] <- 100 * ( 
    mean(hat_pi[is.finite(hat_pi)]) - row$prev ) / row$prev 
  sim_conditions[i,"num_infinite_pi"] <- sum(!is.finite(hat_pi))
  sim_conditions[i,"hat_pi_st"] <- 100 * (
    mean(hat_pi_st[is.finite(hat_pi_st)]) - row$prev) / row$prev 
  sim_conditions[i,"num_infinite_pi_st"] <- sum(!is.finite(hat_pi_st))
  sim_conditions[i,"n_strata_obs_full"] <- sum(strata_obs == n_strata)
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
sim_results <- sim_conditions %>% select(-c(stratum_props))
write_csv(sim_results, output_file)

