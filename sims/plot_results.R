######
# Plot results from each scenario in a nested loop plot.

# For each scenario and set of simulation parameters,
# the plotting parameters must be tweaked significantly,
# so that the plot is legible and conveys information well. 
# Note that there are hundreds of sub-scenarios per scenario.
#
# For information on nested loop plots, see their introduction
# at https://doi.org/10.1186/1471-2288-14-129 (Rucker and Schwarzer 2014)
# and the plotting package https://github.com/matherealize/looplot

library(tidyverse)
library(looplot) # install with devtools::install_github("matherealize/looplot")
library(here)
source(here("sims/input_files/sim_param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

# Read in results files and stratum_proportion files.
# Note that the results are read in from the results_final subdirectory,
# not the results_draft subdirectory. 
# Results are manually moved into results_final 
# (after they are finalized, of course).
results_1 <- read_csv(here("sims/results_final/scenario1_results.csv"),
                      col_types = cols(.default = col_double())
                      )
# results_2 <- read_csv(here("sims/results_final/scenario2_results.csv"),
#                       col_types = cols(.default = col_double())
#                       )
results_3 <- read_csv(here("sims/results_final/scenario3_results.csv"),
                      col_types = cols(.default = col_double())
)
gammas_3 <- read_csv(here("sims/input_files/scenario3_stratum_props.csv"),
                   col_types = cols(
                     z1 = col_character(), 
                     z2 = col_character(), 
                     z3 = col_character(), 
                     stratum_prop = col_double(),
                     sampling_prob = col_double()
                   ))


########
# plots for scenario 1 
# for ease of using the NLP, start with a 2x2 plot
res1_2x2 <- results_1 %>% filter(sigma_e > .75 & sigma_p > .75)

p1_2x2 <- nested_loop_plot(resdf = res1_2x2,
            x = "prev", steps = c("n_3","n_2","n_1"),
            grid_rows = "sigma_e", grid_cols = "sigma_p",
            steps_y_base = -150, steps_y_height = 5,
            steps_y_shift = 40,
            steps_values_annotate = TRUE, steps_annotation_size = 3,
            x_name = "Prevalence", y_name = "Relative Bias (%)",
            methods = c("hat_pi"),
            spu_x_shift = .6,
            parameter_decreasing = TRUE,
            #steps_values_annotate = TRUE,
            hline_intercept = 0,
            y_expand_add = c(20, NULL),
            post_processing = list(
              add_custom_theme = list(
                axis.text.x = element_text(angle = 45,
                                           vjust = 0.5,
                                           hjust = 1,
                                           size = 7)
              )
            )
        ) 
#send plot to pdf 
pdf(here("sims/plots/scenario1_2x2.pdf"), 
    paper = "USr",width=9,height=7)
print(p1_2x2)
dev.off()

########
# plots for scenario 2
# 
# 
# p_2 <- nested_loop_plot(resdf = results_2,
#                            x = "prev", steps = c("n_3","n_2","n_1"),
#                            grid_rows = "sigma_e", grid_cols = "sigma_p",
#                            steps_y_base = -2000, steps_y_height = 5,
#                            steps_y_shift = 15,
#                            steps_values_annotate = TRUE, steps_annotation_size = 4,
#                            x_name = "Prevalence", y_name = "Relative Bias (%)",
#                            methods = c("hat_pi","hat_pi_st"),
#                            spu_x_shift = .2,
#                            parameter_decreasing = TRUE,
#                            #steps_values_annotate = TRUE,
#                            hline_intercept = 0,
#                            y_expand_add = c(10, NULL),
#                            post_processing = list(
#                              add_custom_theme = list(
#                                axis.text.x = element_text(angle = -90,
#                                                           vjust = 0.5,
#                                                           size = 8)
#                              )
#                            )
# )
# 
# #send plot to pdf 
# pdf(here("sims/plots/p2.pdf"))
# print(p2)
# dev.off()


########
# plots for scenario 3
# 

# First, plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp <- gammas_3
  
# Under prev \approx .005, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp_005 <- sp %>% dplyr::mutate(
  prev = inv.logit(alpha_0[1]+alpha_1*(gammas_3$z1=="z11")+
                     alpha_2*(gammas_3$z2=="z20")+alpha_3*(gammas_3$z2=="z21")+
                     alpha_4*(gammas_3$z3=="z30")+alpha_5*(gammas_3$z3=="z31"))
)

# basic plot that needs to be refined 
p <- ggplot(data = sp_005, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_point(aes(size=prev), alpha = .8) + 
  geom_abline(colour = "grey50", size = 2)


# plot scenario 3 results
p_3 <- nested_loop_plot(resdf = results_3,
                        x = "prev", steps = c("n_3","n_2","n_1"),
                        grid_rows = "sigma_e", grid_cols = "sigma_p",
                        steps_y_base = -500, steps_y_height = 20,
                        steps_y_shift = 20,
                        steps_values_annotate = TRUE, steps_annotation_size = 4,
                        x_name = "Prevalence", y_name = "Relative Bias (%)",
                        methods = c("hat_pi","hat_pi_st","hat_pi_mst"),
                        spu_x_shift = .2,
                        parameter_decreasing = TRUE,
                        #steps_values_annotate = TRUE,
                        hline_intercept = 0,
                        y_expand_add = c(20, NULL),
                        post_processing = list(
                          add_custom_theme = list(
                            axis.text.x = element_text(angle = -90,
                                                       vjust = 0.5,
                                                       size = 8)
                          )
                        )
)

p_3
