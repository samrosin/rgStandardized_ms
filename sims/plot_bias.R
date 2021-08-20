# Plot results from each scenario 

# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
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

results_3 <- read_csv(here("sims/results_final/dgp3_results.csv"),
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

results_4 <- read_csv(here("sims/results_draft/dgp4_results.csv"),
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

pdf(here("sims/figs/bias/n1_250/DGP1.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_facet_n1_250)
dev.off()

# absolute bias plot 
res1_bias <- res1_gg %>% mutate(bias = rel_bias * pi)

res1_bias_facet <- ggplot(data = res1_bias, 
                          mapping = aes(x = pi, y = bias,
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
  labs(x = "Prevalence", y = expression(paste("Bias  ", (hat(pi) - pi)))) + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2),
                        labels = c(expression(hat(pi)[RG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(1),
                     labels = c(expression(hat(pi)[RG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res1_bias_facet

pdf(here("sims/figs/bias/DGP1_bias.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_bias_facet)
dev.off()


# DGP 2 Plots --------------------------------------------------------
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
  scale_color_manual(name = "Method", values = scales::hue_pal()(3)[1:2],
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res2_facet

pdf(here("sims/figs/bias/DGP2.pdf"),
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

pdf(here("sims/figs/bias/n1_250/DGP2.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_n1_250)
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
  geom_point(aes(size=pi), alpha = .8, 
             color = "black") + 
  labs(size = "Prevalence") + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(paste(gamma[j]," (Stratum proportion)",sep=""))) + 
  ylab(expression(paste(s[j]," (Sampling probability)", sep=""))) + 
  theme(text = element_text(size=20),
        legend.position = c(.125,0.92),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_size(name = "Prevalence",
             breaks = c(.025, .05, .1),
             labels = c(expression(0.5 %*% "Marginal Prev"),
                        "Marginal Prev",
                        expression(2 %*% "Marginal Prev"))) 

scenario3_selectionbias_plot

pdf(here("sims/figs/bias/DGP3_selectionbias.pdf"),
    paper = "USr",width=8.5,height=11)
print(scenario3_selectionbias_plot)
dev.off()

# plot results for n_1 = 40
res3_gg <- results_3 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG, hat_pi_SRG, hat_pi_SRGM,
         hat_pi_SRG_restriction)

res3_facet <- ggplot(data = res3_gg, 
                     mapping = aes(x = pi, y = rel_bias,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.90,0.90),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res3_facet

pdf(here("sims/figs/bias/DGP3.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res3_facet)
dev.off()

# plot results for DGP3, n_1 = 250
res3_gg_n1_250 <- results_3 %>% filter(n_1 == 250 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG, hat_pi_SRG, hat_pi_SRGM)

res3_facet_n1_250 <- ggplot(data = res3_gg_n1_250, 
                            mapping = aes(x = pi, y = rel_bias,
                                          linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.935,0.915),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res3_facet_n1_250

pdf(here("sims/figs/bias/n1_250/DGP3.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res3_facet_n1_250)
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
              color = "black", width = .001) + 
  labs(size = "Prevalence") + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab(expression(paste(gamma[j]," (Stratum proportion)",sep=""))) + 
  ylab(expression(paste(s[j]," (Sampling probability)", sep=""))) + 
  theme(text = element_text(size=20),
        legend.position = c(.85,0.92),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_size(name = "Prevalence",
             breaks = c(.025, .05, .1),
             labels = c(expression(0.5 %*% "Marginal Prev"),
                        "Marginal Prev",
                        expression(2 %*% "Marginal Prev"))) 

#scenario4_selectionbias_plot

pdf(here("sims/figs/bias/DGP4_selectionbias.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(scenario4_selectionbias_plot)
dev.off()

# plot results for DGP4, n_1 = 40
res4_gg <- results_4 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG, hat_pi_SRG_restriction, hat_pi_SRGM)

res4_facet <- ggplot(data = res4_gg, 
                     mapping = aes(x = pi, y = rel_bias,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.9,0.915),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res4_facet

pdf(here("sims/figs/bias/DGP4.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res4_facet)
dev.off()

# plot results for DGP4, n_1 = 250
res4_gg_n1_250 <- results_4 %>% filter(n_1 == 250 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG, hat_pi_SRG, hat_pi_SRGM)

res4_facet_n1_250 <- ggplot(data = res4_gg_n1_250, 
                            mapping = aes(x = pi, y = rel_bias,
                                          linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.935,0.915),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
res4_facet_n1_250

pdf(here("sims/figs/bias/DGP4_n1_250.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res4_facet)
dev.off()


