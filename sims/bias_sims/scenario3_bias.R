##########
# Conducts simulation scenario 3
time1 <- Sys.time() 

library(tidyverse)
library(looplot) #install with devtools::install_github("matherealize/looplot")
library(here)

source(here("estimation_fns.R"))
source(here("sims/input_files/sim_param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Note that the final simulation results are placed in the 
# results_final subdirectory, but as the simulations are conducted
# results are placed in the results_draft subdirectory
output_file <- here("sims/results_draft/scenario3_results.csv")

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
n_sims <- 6 # number of simulations
n_strata <- 40 # number of strata for this scenario
vars_std <- c("z1", "z2", "z3")

# Create three copies of the gamma (stratum_prop) dataframe,
# with stratum-specific prevalence created from a true logistic model. 
# The intercept of the logistic model varies, so that the marginal prevalence is
# .005, .05, and .30 in the three dataframes
s1 <- gammas %>% dplyr::mutate(
  prev = inv.logit(alpha_0[1]+alpha_1*(gammas$z1=="z11")+
                     alpha_2*(gammas$z2=="z20")+alpha_3*(gammas$z2=="z21")+
                     alpha_4*(gammas$z3=="z30")+alpha_5*(gammas$z3=="z31"))
  )
s2 <- gammas %>% dplyr::mutate(
  prev = inv.logit(alpha_0[2]+alpha_1*(gammas$z1=="z11")+
                     alpha_2*(gammas$z2=="z20")+alpha_3*(gammas$z2=="z21")+
                     alpha_4*(gammas$z3=="z30")+alpha_5*(gammas$z3=="z31"))
  ) 
s3 <- gammas %>% dplyr::mutate(
  prev = inv.logit(alpha_0[3]+alpha_1*(gammas$z1=="z11")+
                     alpha_2*(gammas$z2=="z20")+alpha_3*(gammas$z2=="z21")+
                     alpha_4*(gammas$z3=="z30")+alpha_5*(gammas$z3=="z31"))
)

stratum_props <- list(s1, s2, s3)

# Uncomment to print prevalences, checking that they are, e.g., {.005, .05, .3}
# for(s in 1:length(stratum_props)){
#   print(sum(stratum_props[[s]]$stratum_prop * stratum_props[[s]]$prev))
# }

# fully factorial combination of sample sizes and parameters, 
# where each row is a sub-scenario
sim_conditions <- tidyr::crossing(
  n_1, n_2, n_3, sigma_e, sigma_p, stratum_props) %>% 
  rowwise() %>% 
  mutate(prev = sum(stratum_props$stratum_prop * stratum_props$prev),
         hat_pi_RG = NA_real_,
         hat_pi_SRG = NA_real_, 
         hat_pi_SRGM = NA_real_,
         num_infinite_pi_RG = NA_real_, # number of infinite estimates \hat \pi_RG
         num_infinite_pi_SRG = NA_real_, # number of infinite estimates \hat \pi_SRG
         num_infinite_pi_SRGM = NA_real_, # number of infinite estimates \hat \pi_SRGM
         n_strata_obs_full = NA_real_ # number of simulations with positivity (all strata observed)
  )


# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi_RG <- rep(NA, n_sims)
  hat_pi_SRG <- rep(NA, n_sims)
  hat_pi_SRGM <- rep(NA, n_sims)
  strata_obs <- rep(NA, n_sims) # number of observed strata in a sim
  
  # iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    dat <- gen_data_scenario3(row$n_1, row$sigma_e, row$n_2, row$sigma_p, 
                              row$n_3, as.data.frame(row$stratum_props), vars_std)
    hat_pi_RG[j] <- ests_rg(dat$rho_hat, dat$sigma_e_hat, dat$sigma_p_hat, 
                         row$n_1, row$n_2, row$n_3, variance = FALSE)
    hat_pi_SRG_vec <- ests_std(dat$sample, dat$sigma_e_hat, dat$sigma_p_hat, 
                              row$n_1, row$n_2, row$n_3, vars_std, variance = FALSE)
    # set standardized estimator to NA if there is nonpositivity
    hat_pi_SRG[j] <- ifelse(hat_pi_SRG_vec[2] < n_strata, NA, hat_pi_SRG_vec[1]) 
    strata_obs[j] <- hat_pi_SRG_vec[2] # Note we can get this info from either standardization estimator,
                                      # hat_pi_SRG or hat_pi_SRGM; here I take it from hat_pi_SRG
    hat_pi_SRGM[j] <- ests_std_model(
              dat$sample, as.data.frame(row$stratum_props), dat$sigma_e_hat, 
              dat$sigma_p_hat, row$n_1, row$n_2, row$n_3, vars_std, 
              mod_formula = formula("x ~ z1 + z2 + z3"), variance = FALSE
    )
    
  }
  
  # compute mean relative bias of the finite estimates for the sub-scenario,
  # and compute the other results of interest
  sim_conditions[i,"hat_pi_RG"] <- 100 * ( 
    mean(hat_pi_RG[is.finite(hat_pi_RG)]) - row$prev ) / row$prev 
  sim_conditions[i,"num_infinite_pi_RG"] <- sum(!is.finite(hat_pi_RG))
  
  sim_conditions[i,"hat_pi_SRG"] <- 100 * (
    mean(hat_pi_SRG[is.finite(hat_pi_SRG)], na.rm = TRUE) - row$prev) / row$prev 
  sim_conditions[i,"num_infinite_pi_SRG"] <- sum(!is.finite(hat_pi_SRG))
  
  sim_conditions[i,"hat_pi_SRGM"] <- 100 * (
    mean(hat_pi_SRGM[is.finite(hat_pi_SRGM)]) - row$prev) / row$prev 
  sim_conditions[i,"num_infinite_pi_SRGM"] <- sum(!is.finite(hat_pi_SRGM))
  
  sim_conditions[i,"n_strata_obs_full"] <- sum(strata_obs == n_strata)
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
sim_results <- sim_conditions %>% dplyr::select(-c(stratum_props)) %>% 
                                  dplyr::rename(pi = prev)
#write_csv(sim_results, output_file)

time2 <- Sys.time()
print(time2 - time1)
