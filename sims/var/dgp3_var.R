##########
# Conducts simulation scenario 3
time1 <- Sys.time() 

library(tidyverse)
library(here)
library(fastDummies)

source(here("estimation_fns.R"))
source(here("sims/inputs/sim_param_values_variance.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# results_final subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/scenario3_var_results.csv")

#### The known stratum proportions (the gamma_{zj}s) must be prespecified,
#### and they are loaded here
gammas <- read_csv(here("sims/input_files/scenario3_stratum_props.csv"),
                   col_types = cols(
                     z1 = col_character(), 
                     z2 = col_character(), 
                     z3 = col_character(), 
                     stratum_prop = col_double(),
                     sampling_prob = col_double()
                   ))

# sim parameter values
set.seed(2021)
n_sims <- 50 # number of simulations
n_strata <- 40 # number of strata for this scenario
vars_std <- c("z1", "z2", "z3")

# Create copies of the gamma (stratum_prop) dataframe,
# with stratum-specific prevalence created from a true logistic model. 
# The intercept of the logistic model varies to vary the marginal prevalence
prevs <- seq(.01, .01, by = .01)
stratum_props <- vector(mode = "list", length = length(prevs)) # create list of stratum proportion dataframes
for(p in 1:length(prevs)){
  s <- gammas %>% dplyr::mutate(
    prev = inv.logit(alpha_0[p]+alpha_1*(gammas$z1=="z11")+
                       alpha_2*(gammas$z2=="z20")+alpha_3*(gammas$z2=="z21")+
                       alpha_4*(gammas$z3=="z30")+alpha_5*(gammas$z3=="z31"))
  )
  # make indicator variables for z1, z2, z3
  s <- fastDummies::dummy_cols(s, select_columns = c("z1", "z2", "z3")) %>% 
    dplyr::relocate(c(stratum_prop, sampling_prob, prev), .after = z3_z34) # rearrange columns
  stratum_props[[p]] <- s
}

# Uncomment to print prevalences, checking that they are, e.g., {.005, .05, .3}
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
         ESE_hat_pi_RG = NA_real_, # empirical SE of hat_pi
         ASE_hat_pi_RG = NA_real_, # mean asymptotic SE of hat_pi_RG(mean of hat_var_pi_RG)
         covers_pi_RG = NA_real_, # coverage proporation for hat_var_pi
         
         hat_pi_SRG = NA_real_, 
         ESE_hat_pi_SRG = NA_real_, # empirical SE of hat_pi_SRG
         ASE_hat_pi_SRG = NA_real_, # mean asymptotic SE of hat_pi_SRG
         covers_pi_SRG = NA_real_, # coverage proportion for hat_var_pi_SRG
         
         # repeat, but for sims where there was non-positivity
         hat_pi_SRG_nonpos = NA_real_, 
         ESE_hat_pi_SRG_nonpos = NA_real_, # empirical SE of hat_pi_SRG
         ASE_hat_pi_SRG_nonpos = NA_real_, # mean asymptotic SE of hat_pi_SRG
         covers_pi_SRG_nonpos = NA_real_, # coverage proportion for hat_var_pi_SRG
         
         hat_pi_SRGM = NA_real_,
         ESE_hat_pi_SRGM = NA_real_, # empirical SE of hat_pi_SRGM
         ASE_hat_pi_SRGM = NA_real_, # mean asymptotic SE of hat_pi_SRGM
         covers_pi_SRGM = NA_real_, # coverage proportion for hat_var_pi_SRGM
         
         num_infinite_pi_RG= NA_real_, # number of infinite estimates \hat \pi
         num_infinite_pi_SRG = NA_real_, # number of infinite estimates \hat \pi_SRG
         num_infinite_pi_SRGM = NA_real_, # number of infinite estimates \hat \pi_SRGM
         n_strata_obs_full = NA_real_ # number of simulations with positivity (all strata observed)
  )

# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi_RG<- rep(NA, n_sims)
  hat_var_pi_RG<- rep(NA, n_sims)
  ci_lower_pi_RG<- rep(NA, n_sims)
  ci_upper_pi_RG<- rep(NA, n_sims)
  covers_pi_RG<- rep(NA, n_sims)
  
  hat_pi_SRG <- rep(NA, n_sims)
  hat_var_pi_SRG <- rep(NA, n_sims)
  ci_lower_pi_SRG <- rep(NA, n_sims)
  ci_upper_pi_SRG <- rep(NA, n_sims)
  covers_pi_SRG <- rep(NA, n_sims)
  
  hat_pi_SRGM <- rep(NA, n_sims)
  hat_var_pi_SRGM <- rep(NA, n_sims)
  ci_lower_pi_SRGM <- rep(NA, n_sims)
  ci_upper_pi_SRGM <- rep(NA, n_sims)
  covers_pi_SRGM <- rep(NA, n_sims)
  
  strata_obs <- rep(NA, n_sims) # number of observed strata in a sim
  positivity <- rep(NA, n_sims) # is there positivity? 
  
  # iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    print(paste("sim number:", j))
    dat <- gen_data_scenario3(row$n_1, row$sigma_e, row$n_2, row$sigma_p, 
                              row$n_3, as.data.frame(row$stratum_props), vars_std)
    hat_pi_vec <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                          row$n_1, row$n_2, row$n_3, variance = TRUE)
    hat_pi_RG[j] <- hat_pi_vec[1]
    hat_var_pi_RG[j] <- hat_pi_vec[2]
    ci_lower_pi_RG[j] <- hat_pi_vec[1] - 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_vec[2])
    ci_upper_pi_RG[j] <- hat_pi_vec[1] + 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_vec[2])
    covers_pi_RG[j] <- ifelse(
      (ci_lower_pi_RG[j] < row$prev) && (ci_upper_pi_RG[j] > row$prev), 1, 0)
    
    #standardized estimate
    hat_pi_SRG_vec <- ests_std(dat$sample, dat$sigma_e_hat, dat$sigma_p_hat, 
                               row$n_1, row$n_2, row$n_3, vars_std, variance = TRUE)
    hat_pi_SRG[j] <- hat_pi_SRG_vec[1]
    hat_var_pi_SRG[j] <- hat_pi_SRG_vec[2]
    # set standardized estimators to NA if there is nonpositivity
    #hat_pi_SRG[j] <- ifelse(hat_pi_SRG_vec[3] < n_strata, NA, hat_pi_SRG_vec[1]) 
    #hat_var_pi_SRG[j] <- ifelse(hat_pi_SRG_vec[3] < n_strata, NA, hat_pi_SRG_vec[2]) 
    ci_lower_pi_SRG[j] <- hat_pi_SRG_vec[1] - 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRG_vec[2])
    ci_upper_pi_SRG[j] <- hat_pi_SRG_vec[1] + 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRG_vec[2])
    covers_pi_SRG[j] <- ifelse(
      (ci_lower_pi_SRG[j] < row$prev) && (ci_upper_pi_SRG[j] > row$prev), 1, 0)
    
    strata_obs[j] <- hat_pi_SRG_vec[3] # Note we can get this info from either standardization estimator,
    # hat_pi_SRG or hat_pi_SRGM; here I take it from hat_pi_SRG
    positivity[j] <- ifelse(hat_pi_SRG_vec[3] < n_strata, FALSE, TRUE)
    
    # get model standardized estimates
    hat_pi_SRGM_vec <- ests_std_model(
      dat$sample, as.data.frame(row$stratum_props), dat$sigma_e_hat,
      dat$sigma_p_hat, row$n_1, row$n_2, row$n_3, 
      vars_std = c("z1_z11", "z2_z20", "z2_z21", "z3_z30", "z3_z31"),
      mod_formula = formula("x ~ z1_z11 + z2_z20 + z2_z21 + z3_z30 + z3_z31"), 
      variance = TRUE
    )
    
    hat_pi_SRGM[j] <- hat_pi_SRGM_vec[1]
    hat_var_pi_SRGM[j] <- hat_pi_SRGM_vec[2]
    ci_lower_pi_SRGM[j] <- hat_pi_SRGM_vec[1] - 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRGM_vec[2])
    ci_upper_pi_SRGM[j] <- hat_pi_SRGM_vec[1] + 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_SRGM_vec[2])
    covers_pi_SRGM[j] <- ifelse(
      (ci_lower_pi_SRGM[j] < row$prev) && (ci_upper_pi_SRGM[j] > row$prev), 1, 0)
  }
  
  # compute mean relative bias of the finite estimates for the sub-scenario,
  # and compute the other results of interest
  sim_conditions[i,"hat_pi_RG"] <- 100 * ( 
    mean(hat_pi_RG[is.finite(hat_pi_RG)]) - row$prev ) / row$prev 
  sim_conditions[i, "ESE_hat_pi_RG"] <- sd(hat_pi_RG[is.finite(hat_pi_RG)])
  sim_conditions[i, "ASE_hat_pi_RG"] <- mean(sqrt(hat_var_pi_RG[is.finite(hat_var_pi_RG)]))
  sim_conditions[i, "covers_pi_RG"] <- mean(covers_pi_RG)
  sim_conditions[i, "num_infinite_pi_RG"] <- sum(!is.finite(hat_pi_RG))
  
  # separate results for when there is and isn't positivity
  sim_conditions[i,"hat_pi_SRG"] <- 100 * (
    mean(hat_pi_SRG[positivity], na.rm = TRUE) - row$prev) / row$prev 
  sim_conditions[i, "ESE_hat_pi_SRG"] <- sd(hat_pi_SRG[positivity])
  sim_conditions[i, "ASE_hat_pi_SRG"] <- mean(sqrt(hat_var_pi_SRG[positivity]))
  sim_conditions[i, "covers_pi_SRG"] <- mean(covers_pi_SRG[positivity])
  # sim_conditions[i,"num_infinite_pi_SRG"] <- sum(!is.finite(hat_pi_SRG))
  
  sim_conditions[i,"hat_pi_SRG_nonpos"] <- 100 * (
    mean(hat_pi_SRG[!positivity], na.rm = TRUE) - row$prev) / row$prev 
  sim_conditions[i, "ESE_hat_pi_SRG_nonpos"] <- sd(hat_pi_SRG[!positivity])
  sim_conditions[i, "ASE_hat_pi_SRG_nonpos"] <- mean(sqrt(hat_var_pi_SRG[!positivity]))
  sim_conditions[i, "covers_pi_SRG_nonpos"] <- mean(covers_pi_SRG[!positivity])
  
  sim_conditions[i,"hat_pi_SRGM"] <- 100 * (
    mean(hat_pi_SRGM[is.finite(hat_pi_SRGM)]) - row$prev) / row$prev 
  sim_conditions[i, "ESE_hat_pi_SRGM"] <- sd(hat_pi_SRGM[is.finite(hat_pi_SRGM)])
  sim_conditions[i, "ASE_hat_pi_SRGM"] <- mean(sqrt(hat_var_pi_SRGM[is.finite(hat_var_pi_SRGM)]))
  sim_conditions[i, "covers_pi_SRGM"] <- mean(covers_pi_SRGM)
  sim_conditions[i,"num_infinite_pi_SRGM"] <- sum(!is.finite(hat_pi_SRGM))
  
  sim_conditions[i,"n_strata_obs_full"] <- sum(strata_obs == n_strata)
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
sim_results <- sim_conditions %>% select(-c(stratum_props))
#write_csv(sim_results, output_file)

time2 <- Sys.time()
print(time2 - time1)
