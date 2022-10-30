# compute statistics on model misspecification for the manuscript

library(tidyverse)
library(here)

gammas_3 <- read_csv(here("sims/inputs/dgp3_stratum_props.csv"),
                     col_types = cols(
                       z1 = col_character(), 
                       z2 = col_character(), 
                       z3 = col_character(), 
                       stratum_prop = col_double(),
                       sampling_prob = col_double()
                     ))

# model misspecification computations
sum(gammas_3[gammas_3$z1 == "z11",]$stratum_prop)
sum(gammas_3[gammas_3$z1 == "z11",]$sampling_prob)

sum(gammas_3[gammas_3$z2 %in% c("z20", "z21"),]$stratum_prop)
sum(gammas_3[gammas_3$z2 %in% c("z20", "z21"),]$sampling_prob)

sum(gammas_3[gammas_3$z3 %in% c("z30", "z31"),]$stratum_prop)
sum(gammas_3[gammas_3$z3 %in% c("z30", "z31"),]$sampling_prob)
