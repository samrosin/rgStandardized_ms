##########
# Conducts simulation scenario 3
time1 <- Sys.time() 

library(tidyverse)
library(looplot) #install with devtools::install_github("matherealize/looplot")
library(here)

source(here("estimation_fns.R"))
source(here("sims/input_files/sim_param_values_variance.R")) #load sim parameter values common across scenarios
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
n_sims <- 3 # number of simulations
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
         hat_pi = NA_real_,
         ESE_hat_pi = NA_real_, # empirical SE of hat_pi
         ASE_hat_pi = NA_real_, # mean asymptotic SE of hat_pi (mean of hat_var_pi)
         covers_pi = NA_real_, # coverage proporation for hat_var_pi
         
         hat_pi_st = NA_real_, 
         ESE_hat_pi_st = NA_real_, # empirical SE of hat_pi_st
         ASE_hat_pi_st = NA_real_, # mean asymptotic SE of hat_pi_st
         covers_pi_st = NA_real_, # coverage proportion for hat_var_pi_st
         
         hat_pi_mst = NA_real_,
         ESE_hat_pi_mst = NA_real_, # empirical SE of hat_pi_mst
         ASE_hat_pi_mst = NA_real_, # mean asymptotic SE of hat_pi_mst
         covers_pi_mst = NA_real_, # coverage proportion for hat_var_pi_mst
         
         num_infinite_pi = NA_real_, # number of infinite estimates \hat \pi
         num_infinite_pi_st = NA_real_, # number of infinite estimates \hat \pi_st
         num_infinite_pi_mst = NA_real_, # number of infinite estimates \hat \pi_mst
         n_strata_obs_full = NA_real_ # number of simulations with positivity (all strata observed)
  )


# conduct the simulation, iterating through the subscenarios
for(i in 1:nrow(sim_conditions)){
  print(i)
  row <- sim_conditions[i,]
  hat_pi <- rep(NA, n_sims)
  hat_var_pi <- rep(NA, n_sims)
  ci_lower_pi <- rep(NA, n_sims)
  ci_upper_pi <- rep(NA, n_sims)
  covers_pi <- rep(NA, n_sims)
  
  hat_pi_st <- rep(NA, n_sims)
  hat_var_pi_st <- rep(NA, n_sims)
  ci_lower_pi_st <- rep(NA, n_sims)
  ci_upper_pi_st <- rep(NA, n_sims)
  covers_pi_st <- rep(NA, n_sims)
  
  hat_pi_mst <- rep(NA, n_sims)
  hat_var_pi_mst <- rep(NA, n_sims)
  ci_lower_pi_mst <- rep(NA, n_sims)
  ci_upper_pi_mst <- rep(NA, n_sims)
  covers_pi_mst <- rep(NA, n_sims)
  
  strata_obs <- rep(NA, n_sims) # number of observed strata in a sim
  
  # iterate through each of the n_sims simulations per sub-scenario
  for(j in 1:n_sims){
    print(j)
    dat <- gen_data_scenario3(row$n_1, row$sigma_e, row$n_2, row$sigma_p, 
                              row$n_3, as.data.frame(row$stratum_props), vars_std)
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
    
    #standardized estimate
    hat_pi_st_vec <- ests_std(dat$sample, dat$sigma_e_hat, dat$sigma_p_hat, 
                              row$n_1, row$n_2, row$n_3, vars_std, variance = TRUE)
    # set standardized estimators to NA if there is nonpositivity
    hat_pi_st[j] <- ifelse(hat_pi_st_vec[3] < n_strata, NA, hat_pi_st_vec[1]) 
    hat_var_pi_st[j] <- ifelse(hat_pi_st_vec[3] < n_strata, NA, hat_pi_st_vec[2]) 
    ci_lower_pi_st[j] <- hat_pi_st_vec[1] - 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_st_vec[2])
    ci_upper_pi_st[j] <- hat_pi_st_vec[1] + 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_st_vec[2])
    covers_pi_st[j] <- ifelse(
      (ci_lower_pi_st[j] < row$prev) && (ci_upper_pi_st[j] > row$prev), 1, 0)
    
    strata_obs[j] <- hat_pi_st_vec[3] # Note we can get this info from either standardization estimator,
    # hat_pi_st or hat_pi_mst; here I take it from hat_pi_st
    
    # finally get model standardized estimates
    hat_pi_mst_vec <- ests_std_model(
      dat$sample, as.data.frame(row$stratum_props), dat$sigma_e_hat, 
      dat$sigma_p_hat, row$n_1, row$n_2, row$n_3, vars_std, 
      mod_formula = formula("x ~ z1 + z2 + z3"), variance = TRUE
    )
    hat_pi_mst[j] <- hat_pi_mst_vec[1]
    hat_var_pi_mst[j] <- hat_pi_mst_vec[2]
    ci_lower_pi_mst[j] <- hat_pi_mst_vec[1] - 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_mst_vec[2])
    ci_upper_pi_mst[j] <- hat_pi_st_vec[1] + 
      qnorm(1 - alpha_level / 2) * sqrt(hat_pi_mst_vec[2])
    covers_pi_mst[j] <- ifelse(
      (ci_lower_pi_mst[j] < row$prev) && (ci_upper_pi_mst[j] > row$prev), 1, 0)
  }
  
  # compute mean relative bias of the finite estimates for the sub-scenario,
  # and compute the other results of interest
  sim_conditions[i,"hat_pi"] <- 100 * ( 
    mean(hat_pi[is.finite(hat_pi)]) - row$prev ) / row$prev 
  sim_conditions[i, "ESE_hat_pi"] <- sd(hat_pi[is.finite(hat_pi)])
  sim_conditions[i, "ASE_hat_pi"] <- mean(sqrt(hat_var_pi[is.finite(hat_var_pi)]))
  sim_conditions[i, "covers_pi"] <- mean(covers_pi)
  sim_conditions[i, "num_infinite_pi"] <- sum(!is.finite(hat_pi))
  
  sim_conditions[i,"hat_pi_st"] <- 100 * (
    mean(hat_pi_st[is.finite(hat_pi_st)], na.rm = TRUE) - row$prev) / row$prev 
  sim_conditions[i, "ESE_hat_pi_st"] <- sd(hat_pi_st[is.finite(hat_pi_st)])
  sim_conditions[i, "ASE_hat_pi_st"] <- mean(sqrt(hat_var_pi_st[is.finite(hat_var_pi_st)]))
  sim_conditions[i, "covers_pi_st"] <- mean(covers_pi_st)
  sim_conditions[i,"num_infinite_pi_st"] <- sum(!is.finite(hat_pi_st))
  
  sim_conditions[i,"hat_pi_mst"] <- 100 * (
    mean(hat_pi_mst[is.finite(hat_pi_mst)]) - row$prev) / row$prev 
  sim_conditions[i, "ESE_hat_pi_mst"] <- sd(hat_pi_mst[is.finite(hat_pi_mst)])
  sim_conditions[i, "ASE_hat_pi_mst"] <- mean(sqrt(hat_var_pi_mst[is.finite(hat_var_pi_mst)]))
  sim_conditions[i, "covers_pi_mst"] <- mean(covers_pi_mst)
  sim_conditions[i,"num_infinite_pi_mst"] <- sum(!is.finite(hat_pi_mst))
  
  sim_conditions[i,"n_strata_obs_full"] <- sum(strata_obs == n_strata)
}

# since the above simulation can take some time, 
# write results to an output file to analyse in a separate script
sim_results <- sim_conditions %>% select(-c(stratum_props))
#write_csv(sim_results, output_file)

time2 <- Sys.time()
print(time2 - time1)
