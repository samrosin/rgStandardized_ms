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

color_scheme <- "Set2"

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

# plot where n_1 == 40

# subset results 
res1_gg <- results_1 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG)

# construct ggplot
res1_facet <- ggplot(data = res1_gg, 
                     mapping = aes(x = pi, y = rel_bias,
                      linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.94,0.927),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2),
                        labels = c(expression(hat(pi)[RG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(1),
                     labels = c(expression(hat(pi)[RG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res1_facet

pdf(here("sims/bias_plots/DGP1.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_facet)
dev.off()

# repeat, but with n_1 == 250

# subset results
res1_gg_n1_250 <- results_1 %>% filter(n_1 == 250 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG)

# construct ggplot
res1_facet_n1_250 <- ggplot(data = res1_gg_n1_250, 
                     mapping = aes(x = pi, y = rel_bias,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.94,0.927),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2),
                        labels = c(expression(hat(pi)[RG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(1),
                     labels = c(expression(hat(pi)[RG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")

pdf(here("sims/bias_plots/DGP1_n1_250.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_facet_n1_250)
dev.off()

# Scenario 2 Plots --------------------------------------------------------
res2_gg <- results_2 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
            dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
            gather(key = Method, value = rel_bias,
                   hat_pi_RG, hat_pi_SRG)

res2_facet <- ggplot(data = res2_gg, 
                     mapping = aes(x = pi, y = rel_bias,
                                    linetype = Method, color = Method)) +
              geom_line() + 
              facet_grid(Sens ~ Spec, 
                         labeller = labeller(.rows = label_both, .cols = label_both)) + 
              theme(axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
                    axis.title = element_text(size = 16),
                    legend.position = c(.94,0.927),
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 12),
#                    legend.background = element_rect(fill=alpha('white', 0)),
                    strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2, 1),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(2),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res2_facet

pdf(here("sims/bias_plots/DGP2.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_facet)
dev.off()

res2_gg_n1_250 <- results_2 %>% filter(n_1 == 250 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG, hat_pi_SRG)

res2_n1_250 <- ggplot(data = res2_gg_n1_250, 
                     mapping = aes(x = pi, y = rel_bias,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.94,0.927),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2, 1),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(2),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")

pdf(here("sims/bias_plots/DGP2_n1_250.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_n1_250)
dev.off()

# Scenario 3 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp3 <- gammas_3
  
# Under pi \approx .05, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp3_05 <- sp3 %>% dplyr::mutate(
  pi = inv.logit(alpha_0[2]+alpha_1*(gammas_3$z1=="z11")+
                     alpha_2*(gammas_3$z2=="z20")+alpha_3*(gammas_3$z2=="z21")+
                     alpha_4*(gammas_3$z3=="z30")+alpha_5*(gammas_3$z3=="z31"))
)

# plot gamma_j vs s_j
scenario3_selectionbias_plot <- ggplot(data = sp3_05, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_point(aes(size=pi), alpha = .8, 
             color = "black") + 
  labs(size = "Prevalence") + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(paste(gamma[j]," (Stratum proportion)",sep=""))) + 
  ylab(expression(paste(s[j]," (Sampling probability)", sep=""))) + 
  ggtitle("Selection bias in DGP 3") + 
  theme(plot.title = element_text(hjust = 0.5),
         text = element_text(size=20))

scenario3_selectionbias_plot


pdf(here("sims/bias_plots/scenario3.pdf"),
    paper = "USr",width=8.5,height=11)
print(res3_filtered_nlp)
dev.off()

# Scenario 4 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp4 <- gammas_4

# Under pi \approx .05, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp4_05 <- sp4 %>% dplyr::mutate(
  pi = inv.logit(nu_0[2] + nu_1*(gammas_4$z1 == "z11") +
                     nu_2 * (gammas_4$z2 == "z20") + nu_3 * (gammas_4$z2 == "z21") +
                     nu_4 * (gammas_4$z3 == "z30") + nu_5 * (gammas_4$z3 == "z31") +
                     nu_6 * (gammas_4$z4 == "z41"))
)

# plot gamma_j vs s_j
scenario4_selectionbias_plot <- ggplot(data = sp4_05, 
  aes(x=stratum_prop, y = sampling_prob)) + 
  geom_point(aes(size=pi), alpha = .8, color = "black") + 
  labs(size = "Prevalence") + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(paste(gamma[j]," (Stratum proportion)",sep=""))) + 
  ylab(expression(paste(s[j]," (Sampling probability)", sep=""))) + 
  ggtitle("Selection bias in DGP 4") + 
  theme( plot.title = element_text(hjust = 0.5),
         text = element_text(size=20))

pdf(here("sims/bias_plots/scenario4_selectionbias.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(scenario4_selectionbias_plot)
dev.off()

# plot scenario 4 results
res4_filtered <- results_4 %>% filter(sigma_e == .95 & n_2 != 30 & pi > .01) %>% 
  mutate(n_2 = as.factor(n_2),
         pi = round(pi, digits = 2)) 

res4_filtered_nlp <- nested_loop_plot(resdf = res4_filtered,
                                      x = "n_2", steps = c("n_1","n_3"),
                                      grid_rows = "sigma_p", grid_cols = "pi",
                                      line_alpha = .7, 
                                      point_alpha = .7,
                                      steps_y_base = -2, 
                                      steps_y_height = 0.5,
                                      steps_y_shift = 1.5,
                                      steps_values_annotate = TRUE, 
                                      steps_annotation_size = 2.5,
                                      x_name = expression(paste(n[2], " in Loops of {300, 3000}")),
                                      y_name = "Relative Bias (%)",
                                     # methods = c("hat_pi_RG","hat_pi_SRG","hat_pi_SRGM"),
                                     methods = c("hat_pi_SRG","hat_pi_SRGM"),
                                      colors = scales::brewer_pal(palette = color_scheme),                                       spu_x_shift = 1,
                                      parameter_decreasing = FALSE,
                                      steps_annotation_nudge = 1,
                                      hline_intercept = 0,
                                      y_expand_add = c(2, NULL),
                                      post_processing = list(
                                        add_custom_theme = list(
                                          axis.text.x = element_text(angle = -90, vjust = 0.5, size = 8)))
) + ggtitle("Scenario 4 Results") + 
  theme(plot.title = element_text(hjust = 0.5))

res4_filtered_nlp

pdf(here("sims/bias_plots/scenario4.pdf"),
    paper = "USr",width=11,height=9)
print(res4_filtered_nlp)
dev.off()

