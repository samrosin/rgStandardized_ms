##########
# Conducts simulation for DGP 5
time1 <- Sys.time() 

# sim parameter values
set.seed(2021)
n_sims <- 10 # number of simulations
n_strata <- 40 # number of strata for this scenario
vars_std <- c("z1", "z2", "z3")
prevs <- seq(.01, .2, by = .01)

library(tidyverse)
library(here)

source(here("estimation_fns.R"))
source(here("sims/inputs/param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# results_final subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/dgp5_results.csv")

#### The known stratum proportions (the gamma_{zj}s) must be prespecified,
#### and they are loaded here
#### note DGP5 uses same stratum_props as DGP3
gammas <- read_csv(here("sims/inputs/dgp3_stratum_props.csv"),
                   col_types = cols(
                     z1 = col_character(), 
                     z2 = col_character(), 
                     z3 = col_character(), 
                     stratum_prop = col_double(),
                     sampling_prob = col_double()
                   ))

# Create copies of the gamma (stratum_prop) dataframe,
# with stratum-specific prevalence created from a true logistic model. 
# The intercept of the logistic model varies to vary the marginal prevalence
stratum_props <- vector(mode = "list", length = length(prevs)) # create list of stratum proportion dataframes
for(p in 1:length(prevs)){
  s <- gammas %>% dplyr::mutate(
    prev = inv.logit(alpha_0[p]+alpha_1*(gammas$z1=="z11")+
                       alpha_2*(gammas$z2=="z20")+alpha_3*(gammas$z2=="z21")+
                       alpha_4*(gammas$z3=="z30")+alpha_5*(gammas$z3=="z31"))
  )
  stratum_props[[p]] <- s
}

for(s in 1:length(stratum_props)){
  print(sum(stratum_props[[s]]$stratum_prop * stratum_props[[s]]$prev))
}

# fully factorial combination of sample sizes and parameters, 
# where each row is a sub-scenario
sim_conditions <- tidyr::crossing(
  n_1, n_2, n_3, sigma_e, sigma_p, stratum_props) %>% 
  rowwise() %>% 
  mutate(prev = sum(stratum_props$stratum_prop * stratum_props$prev),
         hat_pi_RG = NA_real_,
         hat_pi_SRG = NA_real_,
         hat_pi_SRG_restriction = NA_real_,
         hat_pi_SRGM = NA_real_,
         num_infinite_pi_RG = NA_real_, # number of infinite estimates \hat \pi_RG
         num_infinite_pi_SRG = NA_real_, # number of infinite estimates \hat \pi_SRG
         num_infinite_pi_SRG_restriction = NA_real_,
         num_infinite_pi_SRGM = NA_real_, # number of infinite estimates \hat \pi_SRGM
         sims_w_positivity = NA_real_ # number of simulations with positivity (all strata observed)
  )

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi_RG <- rep(NA, n_sims)
  hat_pi_SRG <- rep(NA, n_sims)
  hat_pi_SRG_restriction <- rep(NA, n_sims)
  hat_pi_SRGM <- rep(NA, n_sims)
  strata_obs <- rep(NA, n_sims) # number of observed strata in a sim
  positivity <- rep(NA, n_sims) # is there positivity? True/False
  
  # iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_dgp5_6(row$n_1, row$sigma_e, row$n_2, row$sigma_p, 
                              row$n_3, as.data.frame(row$stratum_props), vars_std)
    hat_pi_RG[j] <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                            row$n_1, row$n_2, row$n_3, variance = FALSE)
    hat_pi_SRG_vec <- ests_std(dat$sample, dat$sigma_e_hat, dat$sigma_p_hat, 
                               row$n_1, row$n_2, row$n_3, vars_std, variance = FALSE)
    hat_pi_SRG[j] <- hat_pi_SRG_vec[1]
    
    strata_obs[j] <- hat_pi_SRG_vec[2] # Note we can get this info from either standardization estimator,
    # hat_pi_SRG or hat_pi_SRGM; here I take it from hat_pi_SRG
    positivity[j] <- ifelse(hat_pi_SRG_vec[2] < n_strata, FALSE, TRUE)
    
    hat_pi_SRGM[j] <- ests_std_model(
      dat$sample, as.data.frame(row$stratum_props), dat$sigma_e_hat,
      dat$sigma_p_hat, row$n_1, row$n_2, row$n_3, 
      vars_std = c("z1", "z2", "z3"),
      mod_formula = formula("x ~ z1 + z2 + z3"), 
      variance = FALSE
    )
  }
  
  # compute mean relative bias of the finite estimates for the sub-scenario,
  # and compute the other results of interest
  sim_conditions[i,"hat_pi_RG"] <- 100 * ( 
    mean(hat_pi_RG[is.finite(hat_pi_RG)]) - row$prev ) / row$prev 
  sim_conditions[i,"num_infinite_pi_RG"] <- sum(!is.finite(hat_pi_RG))
  
  sim_conditions[i,"hat_pi_SRG"] <- 100 * (
    mean(hat_pi_SRG[positivity], na.rm = TRUE) - row$prev) / row$prev 
  sim_conditions[i,"num_infinite_pi_SRG"] <- sum(!is.finite(hat_pi_SRG[positivity]))
  
  sim_conditions[i,"hat_pi_SRG_restriction"] <- 100 * (
    mean(hat_pi_SRG[!positivity], na.rm = TRUE) - row$prev) / row$prev 
  sim_conditions[i,"num_infinite_pi_SRG_restriction"] <- sum(!is.finite(hat_pi_SRG[!positivity]))
  
  sim_conditions[i,"hat_pi_SRGM"] <- 100 * (
    mean(hat_pi_SRGM[is.finite(hat_pi_SRGM)]) - row$prev) / row$prev 
  sim_conditions[i,"num_infinite_pi_SRGM"] <- sum(!is.finite(hat_pi_SRGM))
  
  sim_conditions[i,"sims_w_positivity"] <- sum(positivity)
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
sim_results <- sim_conditions %>% dplyr::select(-c(stratum_props)) %>% 
  dplyr::rename(pi = prev)
sim_results$n_sims <- NA_real_
sim_results[1,"n_sims"] <- n_sims # store number of simulations (in a non-tidy format, but it is useful to store this value somewhere)
#write_csv(sim_results, output_file)

time2 <- Sys.time()
print(time2 - time1)
