# Plot variance/coverage results from each scenario in a nested loop plot.
#
# For each scenario and set of simulation parameters,
# the plotting parameters must be tweaked significantly,
# so that the plot is legible and conveys information well. 
# Note that there are hundreds of sub-scenarios per scenario.
#
# For information on nested loop plots, see their introduction
# at https://doi.org/10.1186/1471-2288-14-129 (Rucker and Schwarzer 2014)
# and the plotting package https://github.com/matherealize/looplot


# Setup -------------------------------------------------------------------

library(tidyverse)
library(looplot) # install with devtools::install_github("matherealize/looplot")
library(here)
source(here("sims/input_files/sim_param_values_variance.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Read in results files and stratum_proportion files.
# Note that the results are read in from the results_final subdirectory,
# not the results_draft subdirectory. 
# Results are manually moved into results_final 
# (after they are finalized, of course).
results_1 <- read_csv(here("sims/results_draft/scenario1_var_results.csv"),
                      col_types = cols(.default = col_double())
)
# results_2 <- read_csv(here("sims/results_final/scenario2_results.csv"),
#                       col_types = cols(.default = col_double())
# )
# 
# results_3 <- read_csv(here("sims/results_final/scenario3_results.csv"),
#                       col_types = cols(.default = col_double())
# )
# gammas_3 <- read_csv(here("sims/input_files/scenario3_stratum_props.csv"),
#                      col_types = cols(
#                        z1 = col_character(), 
#                        z2 = col_character(), 
#                        z3 = col_character(), 
#                        stratum_prop = col_double(),
#                        sampling_prob = col_double()
#                      ))
# 
# 
# results_4 <- read_csv(here("sims/results_final/scenario4_results.csv"),
#                       col_types = cols(.default = col_double())
# )
# 
# gammas_4 <- read_csv(here("sims/input_files/scenario4_stratum_props.csv"),
#                      col_types = cols(
#                        z1 = col_character(), 
#                        z2 = col_character(), 
#                        z3 = col_character(), 
#                        z4 = col_character(),
#                        stratum_prop = col_double(),
#                        sampling_prob = col_double()
#                      ))
# 

# Scenario 1 Plots --------------------------------------------------------------

# for ease of using the nested loop plot, start with a 2x2 plot
#res1_2x2 <- results_1 %>% filter(sigma_e > .75 & sigma_p > .75)

p1_var <- nested_loop_plot(resdf = results_1,
                           x = "prev", steps = c("n_1","n_2","n_3"),
                           grid_rows = "sigma_e", grid_cols = "sigma_p",
                           steps_y_base = 0.5, 
                           steps_y_height = .25,
                           steps_y_shift = .4,
                           steps_values_annotate = TRUE, 
                           steps_annotation_size = 2.5,
                           x_name = "Prevalence in Loops of {.005, .05, .3}", 
                           y_name = "95% CI Coverage",
                           methods = c("covers_pi"),
                           spu_x_shift = .2,
                           parameter_decreasing = FALSE,
                           #steps_annotation_nudge = 1,
                           ylim = c(0, 1),
                           hline_intercept = 0.95,
                           x_labels = NULL,
                           y_expand_add = c(1.5, 0),
                           post_processing = list(
                             add_custom_theme = list(
                               axis.text.x = element_text(angle = -90,
                                                          vjust = 0.5,
                                                          size = 8))
                             
                             
                           )
) + ggtitle("Scenario 1 Results") + 
  theme( plot.title = element_text(hjust = 0.5))

print(p1_var)

#send plot to pdf
# pdf(here("sims/plots/scenario1_var_2x2.pdf"),
#     paper = "USr",width=11,height=9)
# print(p1_2x2)
# dev.off()

