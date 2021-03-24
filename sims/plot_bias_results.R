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

# plot for results with sigma_p = 1
res1_sp100 <- results_1 %>% filter(sigma_p == 1) 

res1_sp100_nlp <- nested_loop_plot(resdf = res1_sp100,
                                  x = "pi", steps = c("n_1","n_2","n_3"),
                                  grid_rows = "sigma_e", grid_cols = "sigma_p",
                                  steps_y_base = -8, 
                                  steps_y_height = 1,
                                  steps_y_shift = 5,
                                  steps_values_annotate = TRUE, 
                                  steps_annotation_size = 2.5,
                                  x_name = "Prevalence in Loops of {.005, .05, .3}", 
                                  y_name = "Relative Bias (%)",
                                  methods = c("hat_pi_RG"),
                                  spu_x_shift = .2,
                                  steps_annotation_nudge = 1,
                                  hline_intercept = 0,
                                  x_labels = NULL,
                                  y_expand_add = c(3, NULL),
                                  post_processing = list(add_custom_theme = list(
                                      axis.text.x = element_text(angle = -90, vjust = 0.5, size = 8)))
  ) + ggtitle(expression(paste(
        "Scenario 1 Results: ", sigma[p], " = 1", sep = ""))) + 
      theme(plot.title = element_text(hjust = 0.5))

res1_sp100_nlp

# send plot to pdf
pdf(here("sims/bias_plots/scenario1_sp100.pdf"),
    paper = "USr",width=11,height=9)
print(res1_sp100_nlp)
dev.off()

# plot for results with sigma_p = .95
res1_sp95 <- results_1 %>% filter(sigma_p == .95)

res1_sp95_nlp <- nested_loop_plot(resdf = res1_sp95,
                            x = "pi", steps = c("n_1","n_2","n_3"),
                            grid_rows = "sigma_e", grid_cols = "sigma_p",
                            steps_y_base = -75, 
                            steps_y_height = 10,
                            steps_y_shift = 70,
                            steps_values_annotate = TRUE, 
                            steps_annotation_size = 2.5,
                            x_name = "Prevalence in Loops of {.005, .05, .3}", 
                            y_name = "Relative Bias (%)",
                            methods = c("hat_pi_RG"),
                            spu_x_shift = .2,
                            steps_annotation_nudge = 1,
                            hline_intercept = 0,
                            x_labels = NULL,
                            y_expand_add = c(100, NULL),
                            post_processing = list(add_custom_theme = list(
                                axis.text.x = element_text(angle = -90, vjust = 0.5, size = 8)))
  ) + ggtitle(expression(paste(
         "Scenario 1 Results: ", sigma[p], " = 0.95", sep = ""))) + 
      theme(plot.title = element_text(hjust = 0.5))

# send plot to pdf
pdf(here("sims/bias_plots/scenario1_sp95.pdf"),
    paper = "USr",width=11,height=9)
print(res1_sp95_nlp)
dev.off()

# plot for results with sigma_p = .70
res1_sp70 <- results_1 %>% filter(sigma_p == .70)

res1_sp70_nlp <- nested_loop_plot(resdf = res1_sp70,
                                  x = "pi", steps = c("n_1","n_2","n_3"),
                                  grid_rows = "sigma_e", grid_cols = "sigma_p",
                                  steps_y_base = -150, 
                                  steps_y_height = 20,
                                  steps_y_shift = 200,
                                  steps_values_annotate = TRUE, 
                                  steps_annotation_size = 2.5,
                                  x_name = "Prevalence in Loops of {.005, .05, .3}", 
                                  y_name = "Relative Bias (%)",
                                  methods = c("hat_pi_RG"),
                                  spu_x_shift = .2,
                                  steps_annotation_nudge = 1,
                                  hline_intercept = 0,
                                  x_labels = NULL,
                                  y_expand_add = c(175, NULL),
                                  post_processing = list(add_custom_theme = list(
                                      axis.text.x = element_text(angle = -90, size = 8)))
  ) + ggtitle(expression(paste(
        "Scenario 1 Results: ", sigma[p], " = 0.70", sep = ""))) + 
      theme(plot.title = element_text(hjust = 0.5))

res1_sp70_nlp

# send plot to pdf
pdf(here("sims/bias_plots/scenario1_sp70.pdf"),
    paper = "USr",width=11,height=9)
print(res1_sp70_nlp)
dev.off()

# Scenario 1, without Prevalence .005, and with sigma_e=.7 -------------------------------------

results1_filtered <- results_1 %>% filter(pi > .01 & sigma_e == .7) %>%
                          mutate(n_2 = as.factor(n_2))

results1_filt_nlp <- nested_loop_plot(resdf = results1_filtered,
                                  x = "n_2", steps = c("n_1","n_3"),
                                  grid_rows = "sigma_p", grid_cols = "pi",
                                  steps_y_base = -25, 
                                  steps_y_height = 5,
                                  steps_y_shift = 30,
                                  steps_values_annotate = TRUE, 
                                  steps_annotation_size = 2.5,
                                  x_name = expression(paste(n[2], " in Loops of {30, 300, 3000}")),
                                  y_name = "Relative Bias (%)",
                                  methods = c("hat_pi_RG"),
                                  spu_x_shift = 1,
                                  steps_annotation_nudge = 1,
                                  hline_intercept = 0,
                                  y_expand_add = c(20, NULL),
                                  post_processing = list(add_custom_theme = list(
                                      axis.text.x = element_text(angle = -90, vjust = 0.5, size = 8)))
  ) + ggtitle(expression(paste(
         "Scenario 1 Results where ", sigma[e], " = 0.70", sep = ""))) + 
      theme(plot.title = element_text(hjust = 0.5))

results1_filt_nlp

# based on those results, filter down to cases where n_2 \in {300, 3000}
results1_filt2 <- results1_filtered %>% filter(n_2 != 30)

results1_filt2_nlp <- nested_loop_plot(resdf = results1_filt2,
                                      x = "n_2", steps = c("n_1","n_3"),
                                      grid_rows = "sigma_p", grid_cols = "pi",
                                      steps_y_base = -10, 
                                      steps_y_height = 2,
                                      steps_y_shift = 15,
                                      steps_values_annotate = TRUE, 
                                      steps_annotation_size = 2.5,
                                      x_name = expression(paste(n[2], " in Loops of {300, 3000}")),
                                      y_name = "Relative Bias (%)",
                                      methods = c("hat_pi_RG"),
                                      spu_x_shift = 1,
                                      parameter_decreasing = FALSE,
                                      steps_annotation_nudge = 1,
                                      #steps_values_annotate = TRUE,
                                      hline_intercept = 0,
                                      # x_labels = NULL,
                                      # y_labels = c(-20,20),
                                      y_expand_add = c(15, NULL),
                                      post_processing = list(
                                        add_custom_theme = list(
                                          axis.text.x = element_text(angle = -90,
                                                                     vjust = 0.5,
                                                                     size = 8))
                                      )
) + ggtitle(expression(paste(
  "Scenario 1 Results where ", sigma[e], " = 0.70", sep = ""))) + 
  theme(plot.title = element_text(hjust = 0.5))

results1_filt2_nlp



# Scenario 2 Plots --------------------------------------------------------


# Scenario 3 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp <- gammas_3
  
# Under pi \approx .005, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp_005 <- sp %>% dplyr::mutate(
  pi = inv.logit(alpha_0[1]+alpha_1*(gammas_3$z1=="z11")+
                     alpha_2*(gammas_3$z2=="z20")+alpha_3*(gammas_3$z2=="z21")+
                     alpha_4*(gammas_3$z3=="z30")+alpha_5*(gammas_3$z3=="z31"))
)

# plot gamma_j vs s_j
scenario3_selectionbias_plot <- ggplot(data = sp_005, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_point(aes(size=pi), alpha = .8) + 
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


# Scenario 4 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp4 <- gammas_4

# Under pi \approx .005, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp_005 <- sp4 %>% dplyr::mutate(
  pi = inv.logit(nu_0[1] + nu_1*(gammas_4$z1 == "z11") +
                     nu_2 * (gammas_4$z2 == "z20") + nu_3 * (gammas_4$z2 == "z21") +
                     nu_4 * (gammas_4$z3 == "z30") + nu_5 * (gammas_4$z3 == "z31") +
                     nu_6 * (gammas_4$z4 == "z41"))
)

# plot gamma_j vs s_j
scenario4_selectionbias_plot <- ggplot(data = sp_005, 
  aes(x=stratum_prop, y = sampling_prob)) + 
  geom_point(aes(size=pi), alpha = .8) + 
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
