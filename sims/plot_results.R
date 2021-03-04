# Plot results from each scenario in a nested loop plot.
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
results_2 <- read_csv(here("sims/results_final/scenario2_results.csv"),
                      col_types = cols(.default = col_double())
                      )

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


results_4 <- read_csv(here("sims/results_final/scenario4_results.csv"),
                      col_types = cols(.default = col_double())
)
gammas_4 <- read_csv(here("sims/input_files/scenario4_stratum_props.csv"),
                     col_types = cols(
                       z1 = col_character(), 
                       z2 = col_character(), 
                       z3 = col_character(), 
                       z4 = col_character(),
                       stratum_prop = col_double(),
                       sampling_prob = col_double()
                     ))


# Scenario 1 Plots --------------------------------------------------------------

# for ease of using the nested loop plot, start with a 2x2 plot
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
print(p1_2x2)
# #send plot to pdf 
# pdf(here("sims/plots/scenario1_2x2.pdf"), 
#     paper = "USr",width=9,height=7)
# print(p1_2x2)
# dev.off()


# Scenario 2 Plots --------------------------------------------------------

res2_2x2 <- results_2 %>% filter(sigma_e > .75 & sigma_p > .75)

p2_2x2 <- nested_loop_plot(resdf = res2_2x2,
                           x = "prev", steps = c("n_1","n_2","n_3"),
                           grid_rows = "sigma_e", grid_cols = "sigma_p",
                           steps_y_base = -75, 
                           steps_y_height = 10,
                           steps_y_shift = 60,
                           steps_values_annotate = TRUE, 
                           steps_annotation_size = 2.6,
                           x_name = "Prevalence in Loops of {.005, .05, .3}", 
                           y_name = "Relative Bias (%)",
                           methods = c("hat_pi","hat_pi_st"),
                           spu_x_shift = .2,
                           parameter_decreasing = FALSE,
                           steps_annotation_nudge = 1,
                           #steps_values_annotate = TRUE,
                           hline_intercept = 0,
                           x_labels = NULL,
                           y_expand_add = c(50, NULL),
                           post_processing = list(
                             add_custom_theme = list(
                               axis.text.x = element_text(angle = -90,
                                                          vjust = 0.5,
                                                          size = 8))
                            
                             
                           )
) + ggtitle("Scenario 2 Results") + 
  theme( plot.title = element_text(hjust = 0.5))

print(p2_2x2)

# #send plot to pdf 
pdf(here("sims/plots/scenario2_2x2.pdf"),
    paper = "USr", width = 11, height = 9)
print(p2_2x2)
dev.off()



# Scenario 3 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
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

# plot gamma_j vs s_j
scenario3_selectionbias_plot <- ggplot(data = sp_005, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_point(aes(size=prev), alpha = .8) + 
  labs(size = "Prevalence") + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(paste(gamma[j]," (Stratum proportion)",sep=""))) + 
  ylab(expression(paste(s[j]," (Sampling probability)", sep=""))) + 
  ggtitle("Selection bias in Scenario 3") + 
  theme( plot.title = element_text(hjust = 0.5),
         text = element_text(size=14))

pdf(here("sims/plots/scenario3_selectionbias.pdf"))
print(scenario3_selectionbias_plot)
dev.off()

# plot scenario 3 results

res3_2x2 <- results_3 %>% filter(sigma_e > .75 & sigma_p > .75)

p3_2x2 <- nested_loop_plot(resdf = res3_2x2,
                           x = "prev", steps = c("n_1","n_2","n_3"),
                           grid_rows = "sigma_e", grid_cols = "sigma_p",
                           steps_y_base = -75, 
                           steps_y_height = 10,
                           steps_y_shift = 60,
                           steps_values_annotate = TRUE, 
                           steps_annotation_size = 3,
                           x_name = "Prevalence in Loops of {.005, .05, .3}", 
                           y_name = "Relative Bias (%)",
                           methods = c("hat_pi","hat_pi_mst"),
                           spu_x_shift = .2,
                           parameter_decreasing = FALSE,
                           steps_annotation_nudge = 1,
                           #steps_values_annotate = TRUE,
                           hline_intercept = 0,
                           x_labels = NULL,
                           y_expand_add = c(50, NULL),
                           post_processing = list(
                             add_custom_theme = list(
                               axis.text.x = element_text(angle = -90,
                                                          vjust = 0.5,
                                                          size = 8))
                             
                             
                           )
) + ggtitle("Scenario 3 Results") + 
  theme( plot.title = element_text(hjust = 0.5))

print(p3_2x2)

# send plot to pdf 
pdf(here("sims/plots/scenario3_2x2.pdf"),
    paper = "USr", width = 11, height = 9)
print(p3_2x2)
dev.off()


# Scenario 4 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp4 <- gammas_4

# Under prev \approx .005, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp_005 <- sp4 %>% dplyr::mutate(
  prev = inv.logit(nu_0[1] + nu_1*(gammas_4$z1 == "z11") +
                     nu_2 * (gammas_4$z2 == "z20") + nu_3 * (gammas_4$z2 == "z21") +
                     nu_4 * (gammas_4$z3 == "z30") + nu_5 * (gammas_4$z3 == "z31") +
                     nu_6 * (gammas_4$z4 == "z41"))
)

# plot gamma_j vs s_j
scenario4_selectionbias_plot <- ggplot(data = sp_005, 
  aes(x=stratum_prop, y = sampling_prob)) + 
  geom_point(aes(size=prev), alpha = .8) + 
  labs(size = "Prevalence") + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(paste(gamma[j]," (Stratum proportion)",sep=""))) + 
  ylab(expression(paste(s[j]," (Sampling probability)", sep=""))) + 
  ggtitle("Selection bias in Scenario 4") + 
  theme( plot.title = element_text(hjust = 0.5),
         text = element_text(size=14))

pdf(here("sims/plots/scenario4_selectionbias.pdf"))
print(scenario4_selectionbias_plot)
dev.off()

res4_2x2 <- results_4 %>% filter(sigma_e > .75 & sigma_p > .75)

p4_2x2 <- nested_loop_plot(resdf = res4_2x2,
                           x = "prev", steps = c("n_1","n_2","n_3"),
                           grid_rows = "sigma_e", grid_cols = "sigma_p",
                           steps_y_base = -75, 
                           steps_y_height = 10,
                           steps_y_shift = 60,
                           steps_values_annotate = TRUE, 
                           steps_annotation_size = 3,
                           x_name = "Prevalence in Loops of {.005, .05, .3}", 
                           y_name = "Relative Bias (%)",
                           methods = c("hat_pi","hat_pi_mst"),
                           spu_x_shift = .2,
                           parameter_decreasing = FALSE,
                           steps_annotation_nudge = 1,
                           #steps_values_annotate = TRUE,
                           hline_intercept = 0,
                           x_labels = NULL,
                           y_expand_add = c(50, NULL),
                           post_processing = list(
                             add_custom_theme = list(
                               axis.text.x = element_text(angle = -90,
                                                          vjust = 0.5,
                                                          size = 8))
                             
                             
                           )
) + ggtitle("Scenario 4 Results") + 
  theme( plot.title = element_text(hjust = 0.5))

print(p4_2x2)

# send plot to pdf 
pdf(here("sims/plots/scenario4_2x2.pdf"),
    paper = "USr", width = 11, height = 9)
print(p4_2x2)
dev.off()
