# Plot results from each scenario 

# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(cowplot)
source(here("sims/inputs/param_values.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

color_scheme <- "Set2"

# Read in results files and stratum_proportion files.
# Note that the results are read in from the results_final subdirectory,
# not the results_draft subdirectory. 
# Results are manually moved into results_final 
# (after they are finalized, of course).
results_1 <- read_csv(here("sims/results_final/dgp1_results.csv"),
                      col_types = cols(.default = col_double())
)
results_2 <- read_csv(here("sims/results_final/dgp2_results.csv"),
                      col_types = cols(.default = col_double())
)

results_3 <- read_csv(here("sims/results_final/dgp3_var_results.csv"),
                      col_types = cols(.default = col_double())
)
gammas_3 <- read_csv(here("sims/inputs/dgp3_stratum_props.csv"),
                     col_types = cols(
                       z1 = col_character(), 
                       z2 = col_character(), 
                       z3 = col_character(), 
                       stratum_prop = col_double(),
                       sampling_prob = col_double()
                     ))

results_4 <- read_csv(here("sims/results_final/dgp4_var_results.csv"),
                      col_types = cols(.default = col_double())
)

gammas_4 <- read_csv(here("sims/inputs/dgp4_stratum_props.csv"),
                     col_types = cols(
                       z1 = col_character(), 
                       z2 = col_character(), 
                       z3 = col_character(), 
                       z4 = col_character(),
                       stratum_prop = col_double(),
                       sampling_prob = col_double()
                     ))

# DGP 1 Plots --------------------------------------------------------------

# plot where n_1 == 40

# subset results 
res1_gg <- results_1 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG)

# construct ggplot
res1_bias <- ggplot(data = res1_gg, 
       mapping = aes(x = pi, y = rel_bias,
                     linetype = Method, color = Method)) +
  geom_line(size = 2) + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.93,0.94),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c(2),
                        labels = c(expression(hat(pi)[RG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(2)[1],
                     labels = c(expression(hat(pi)[RG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")

res1_bias

pdf(here("sims/figs/bias/DGP1.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_bias)
dev.off()


# DGP 2 Plots --------------------------------------------------------
res2_gg <- results_2 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG, hat_pi_SRG)

res2_facet <- ggplot(data = res2_gg, 
                     mapping = aes(x = pi, y = rel_bias,
                                   linetype = Method, color = Method)) +
  geom_line(size = 2) + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.92,0.92),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c(2, 1),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3)[1:2],
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res2_facet

pdf(here("sims/figs/bias/DGP2.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_facet)
dev.off()

# DGP 3 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp3 <- gammas_3

# Under pi \approx .05, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp3_05 <- sp3 %>% dplyr::mutate(
  pi = inv.logit(alpha_0[5]+alpha_1*(gammas_3$z1=="z11")+
                   alpha_2*(gammas_3$z2=="z20")+alpha_3*(gammas_3$z2=="z21")+
                   alpha_4*(gammas_3$z3=="z30")+alpha_5*(gammas_3$z3=="z31"))
)

# plot gamma_j vs s_j
scenario3_selectionbias_plot <- ggplot(data = sp3_05, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_jitter(aes(size=pi), alpha = .6, 
              color = "black", width = .001,
              show.legend = FALSE)  + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(gamma[j])) + 
  ylab(expression(s[j])) + 
  theme_bw() + 
  theme(text = element_text(size=20),
        legend.position = c(.17,0.79),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))# + 
scenario3_selectionbias_plot

# plot bias from DGP 4 from variance results 
# note this is **truncated** inside [-100%, 100%]
res3_bias <- results_3 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = relbias,
         hat_pi_RG, 
         hat_pi_SRG, 
         hat_pi_SRGM) %>% 
  mutate(
    Method = factor(Method, levels = unique(Method))) %>% 
  mutate(relbias = ifelse(relbias > 99.9, 
                          100, relbias)) %>%
  mutate(relbias = ifelse(relbias < -99.9, 
                          -100, relbias))

res3_bias_plot <- ggplot(data = res3_bias, 
                         mapping = aes(x = prev, y = relbias,
                                       linetype = Method, color = Method)) +
  geom_line(size = 2) + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.91,0.89),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res3_bias_plot

#send plot to pdf
pdf(here("sims/figs/bias/DGP3.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res3_bias_plot)
dev.off()

# DGP 4 Plots --------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp4 <- gammas_4

# Under pi \approx .05, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp4_05 <- sp4 %>% dplyr::mutate(
  pi = inv.logit(nu_0[5] + nu_1*(gammas_4$z1 == "z11") +
                   nu_2 * (gammas_4$z2 == "z20") + nu_3 * (gammas_4$z2 == "z21") +
                   nu_4 * (gammas_4$z3 == "z30") + nu_5 * (gammas_4$z3 == "z31") +
                   nu_6 * (gammas_4$z4 == "z41"))
)

# plot gamma_j vs s_j
scenario4_selectionbias_plot <- ggplot(data = sp4_05, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_jitter(aes(size=pi), alpha = .6, 
              color = "black", width = .001, 
              show.legend = FALSE) + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(gamma[j])) + 
  ylab(expression(s[j])) + 
  theme_bw() + 
  theme(text = element_text(size=20),
        legend.position = c(.8375,0.78),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) #+ 

scenario4_selectionbias_plot

pdf(here("sims/figs/bias/selbias.pdf"),
    paper = "USr", width = 8.5, height = 11)
plot_grid(scenario3_selectionbias_plot, 
          scenario4_selectionbias_plot,
          ncol = 1,
          labels = c("A.", "B."),
          label_size = 18,
          hjust = 0
          )
# grid.arrange(scenario3_selectionbias_plot,
#              scenario4_selectionbias_plot,
#              ncol = 1)
dev.off()


# plot bias from DGP 4 from variance results 
res4_bias <- results_4 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = relbias,
         hat_pi_RG, 
         hat_pi_SRG, 
         hat_pi_SRGM) %>% 
  mutate(
    Method = factor(Method, levels = unique(Method))) %>% 
  mutate(relbias = ifelse(relbias > 99.9, 
                          100, relbias)) %>%
  mutate(relbias = ifelse(relbias < -99.9, 
                          -100, relbias))

res4_bias_plot <- ggplot(data = res4_bias, 
                     mapping = aes(x = prev, y = relbias,
                                   linetype = Method, color = Method)) +
  geom_line(size = 2) + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.91,0.89),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-100, 100)) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res4_bias_plot

pdf(here("sims/figs/bias/DGP4.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res4_bias_plot)
dev.off()


