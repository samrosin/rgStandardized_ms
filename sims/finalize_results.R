# Moves results .csv files from the drafts to the final folder,
# while renaming some of the columns
library(here)
library(tidyverse)

results_1 <- read_csv(here("sims/results_draft/scenario1_results.csv"),
                      col_types = cols(.default = col_double())) %>% 
             rename(hat_pi_RG = hat_pi,
                    pi = prev)
write_csv(results_1, here("sims/results_final/scenario1_results.csv"))

results_2 <- read_csv(here("sims/results_draft/scenario2_results.csv"),
                      col_types = cols(.default = col_double())) %>% 
             rename(hat_pi_RG = hat_pi,
                     hat_pi_SRG = hat_pi_st,
                     pi = prev)
write_csv(results_2, here("sims/results_final/scenario2_results.csv"))

results_3 <- read_csv(here("sims/results_draft/scenario3_results.csv"),
                      col_types = cols(.default = col_double())) %>% 
  rename(hat_pi_RG = hat_pi,
         hat_pi_SRG = hat_pi_st,
         hat_pi_SRGM = hat_pi_mst,
         pi = prev)
write_csv(results_3, here("sims/results_final/scenario3_results.csv"))

results_4 <- read_csv(here("sims/results_draft/scenario4_results.csv"),
                      col_types = cols(.default = col_double())) %>% 
  rename(hat_pi_RG = hat_pi,
         hat_pi_SRG = hat_pi_st,
         hat_pi_SRGM = hat_pi_mst,
         pi = prev)
write_csv(results_4, here("sims/results_final/scenario4_results.csv"))

