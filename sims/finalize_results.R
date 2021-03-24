# Moves results .csv files from the drafts to the final folder,
# while renaming some of the columns
library(here)
library(tidyverse)

results_1 <- read_csv(here("sims/results_draft/scenario1_results.csv"),
                      col_types = cols(.default = col_double())) %>% 
             rename(hat_pi_RG = hat_pi,
                    pi = prev)
write_csv(results_1, here("sims/results_final/scenario1_results.csv"))

