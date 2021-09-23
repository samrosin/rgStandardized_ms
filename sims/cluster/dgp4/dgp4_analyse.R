# Generate a single results dataset for dgp4.
# Relies on a SLURM-managed cluster.

# *** MUST READ ***
# these two directories must be specified by the user 
# the user_home_dir *must* initially contain two empty sub-directories titled dgp4_datasets/ and dgp4_results/
libs <- "/nas/longleaf/home/srosin/RLibs/" # directory containing R packages
user_home_dir <- "/nas/longleaf/home/srosin/rgStandardized/" # top-level directory for the project

# specify number of simulations and number of strata
n_sims <- 1000
n_strata <- 80

setwd(paste(user_home_dir, "dgp4_results/", sep = ""))
outfile <- paste(user_home_dir, "dgp4_var_results.csv", sep = "")
library(tidyverse)

list_of_all_results <- vector(mode = "list", length = n_sims)

# read in all results from results directory
for(i in 1:n_sims){
  list_of_all_results[[i]] <- read_csv(file = paste("results_", i, ".csv", sep = ""),
                                       col_types = cols(positivity = col_logical(),
                                                        .default = col_double()))
}

# stack all results into one dataframe
all_results <- bind_rows(list_of_all_results) %>% 
  mutate(relbias_RG = 100 * (hat_pi_RG - prev ) / prev,
         relbias_SRG = 100 * (hat_pi_SRG - prev) / prev,
         relbias_SRGM = 100 * (hat_pi_SRGM - prev) / prev,
         inf_RG = ifelse(is.na(hat_pi_RG), 1, 0),
         inf_SRG = ifelse(is.na(hat_pi_SRG), 1, 0),
         inf_SRGM = ifelse(is.na(hat_pi_SRGM), 1, 0))

# initialize results dataframe with the sim_conditions
sim_results <- list_of_all_results[[1]] %>% select(n_1, n_2, n_3, sigma_e, sigma_p, prev)

sim_results <- all_results %>% 
  group_by(n_1, n_2, n_3, sigma_e, sigma_p, prev) %>% 
  summarise(sigma_e_hat = mean(sigma_e_hat),
            sigma_p_hat = mean(sigma_p_hat),
            rho_hat = mean(rho_hat),
            ESE_hat_pi_RG = sd(hat_pi_RG, na.rm = TRUE),
            ASE_hat_pi_RG = mean(sqrt(hat_var_pi_RG), na.rm = TRUE),
            coverage_pi_RG = mean(covers_pi_RG, na.rm = TRUE),
            ESE_hat_pi_SRG = sd(hat_pi_SRG, na.rm = T),
            ASE_hat_pi_SRG = mean(sqrt(hat_var_pi_SRG), na.rm = T),
            coverage_pi_SRG = mean(covers_pi_SRG, na.rm = T),
            ESE_hat_pi_SRGM = sd(hat_pi_SRGM, na.rm = T),
            ASE_hat_pi_SRGM = mean(sqrt(hat_var_pi_SRGM), na.rm = T),
            coverage_pi_SRGM = mean(covers_pi_SRGM, na.rm = T),
            num_infinite_pi_RG = sum(inf_RG),
            num_infinite_pi_SRG = sum(inf_SRG),
            num_infinite_pi_SRGM = sum(inf_SRGM),
            num_negative_var_pi_SRGM = sum(hat_var_pi_SRGM < 0), # number of negative variance estimates for SRGM
            n_strata_obs_full = sum(strata_obs == 80),
            hat_pi_RG = mean(relbias_RG, na.rm = TRUE),
            hat_pi_SRG = mean(relbias_SRG, na.rm = TRUE),
            hat_pi_SRGM = mean(relbias_SRGM, na.rm = TRUE),
            .groups = "drop")
writeLines("It is possible in rare cases for variance estimates to be negative.
           The number of negative variance estimates for SRGM is stored.")

write_csv(sim_results, outfile)
