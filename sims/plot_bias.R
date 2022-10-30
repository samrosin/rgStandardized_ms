# Plot results from each scenario 

# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(cowplot)
source(here("sims/inputs/param_values.R")) #load sim parameter values common across scenarios
source(here("sims/inputs/param_values_dgp3.R")) #load sim parameter values common across scenarios
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

gammas_3 <- read_csv(here("sims/inputs/dgp3_stratum_props.csv"),
                     col_types = cols(
                       z1 = col_character(), 
                       z2 = col_character(), 
                       z3 = col_character(), 
                       stratum_prop = col_double(),
                       sampling_prob = col_double()
                     ))

results_3 <- read_csv(here("sims/results_final/dgp3_var_results.csv"),
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

results_4 <- read_csv(here("sims/results_final/dgp4_var_results.csv"),
                      col_types = cols(.default = col_double())
)

results_5 <- read_csv(here("sims/results_final/dgp5_var_results.csv"),
                      col_types = cols(.default = col_double())
)

gammas_5 <- gammas_3 

results_6 <- read_csv(here("sims/results_final/dgp6_var_results.csv"),
                      col_types = cols(.default = col_double())
)

gammas_6 <- gammas_4

# dgp 1 plots --------------------------------------------------------------

# plot where n_1 == 40

# subset results 
res1_gg <- results_1 %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>%
  rename(tilde_pi_RG = hat_pi_RG_notrunc) %>%
  gather(key = Method, value = rel_bias,
         hat_pi_RG, tilde_pi_RG) 

# construct ggplot
# res1_relbias <- ggplot(data = res1_gg, 
#        mapping = aes(x = pi, y = rel_bias,
#                      linetype = Method, color = Method, size = Method)) +
#   geom_line() + 
#   facet_grid(Sens ~ Spec, 
#              labeller = labeller(.rows = label_both, .cols = label_both)) + 
#   theme_classic() + 
#   theme(axis.text.y = element_text(size = 16),
#         axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
#         axis.title = element_text(size = 20),
#        legend.position = "none",
#         # legend.position = c(.93,0.94),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 18),
#         #                    legend.background = element_rect(fill=alpha('white', 0)),
#         panel.spacing.y = unit(1.25, "lines"),
#         strip.text = element_text(size = 18)) + 
#   labs(x = "Prevalence", y = "Relative Bias") + 
#   scale_y_continuous(labels = function(x) paste0(x, "%")) + 
#   #                   limits = c(-50, 200)) + 
#   scale_linetype_manual(name = "Method", values = c("dashed"),
#                         labels = c(expression(hat(pi)[RG]))) + 
#   scale_color_manual(name = "Method", values = c("black"),
#                      labels = c(expression(hat(pi)[RG]))) + 
#   scale_size_manual(name = "Method", values = c(0.5),
#                     labels = c(expression(hat(pi)[RG]))) + 
#   geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
# 
# #res1_relbias
# 
# pdf(here("sims/figs/relbias/dgp1.pdf"),
#     paper = "USr",width = 8.5, height = 11)
# print(res1_relbias)
# dev.off()

# non-relative bias plot 
res1_gg <- res1_gg %>% mutate(bias = rel_bias * pi / 100)

res1_bias <- ggplot(data = res1_gg, 
                       mapping = aes(x = pi, y = bias,
                                     linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
       # legend.position = "none",
         legend.position = c(.74, 0.45),
        #legend.position = c(.93,0.94),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Bias") + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid"),
                        labels = c(expression(hat(pi)[RG]),
                        expression(tilde(pi)[RG]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray"),
                     labels = c(expression(hat(pi)[RG]),
                     expression(tilde(pi)[RG]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 0.5),
                    labels = c(expression(hat(pi)[RG]),
                    expression(tilde(pi)[RG]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 

pdf(here("sims/figs/bias/dgp1.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_bias)
dev.off()

# dgp 2 plots --------------------------------------------------------
res2_gg <- results_2 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = rel_bias,
         hat_pi_RG, hat_pi_SRG)

res2_facet <- ggplot(data = res2_gg, 
                     mapping = aes(x = pi, y = rel_bias,
                                   linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.93,0.94),
        #legend.position = c(.92,0.92),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 

res2_facet

pdf(here("sims/figs/relbias/dgp2.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_facet)
dev.off()

# non-relative bias plot 
res2_gg <- res2_gg %>% mutate(bias = rel_bias * pi / 100)

res2_bias <- ggplot(data = res2_gg, 
                     mapping = aes(x = pi, y = bias,
                                   linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.74, 0.08),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Bias") + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 1.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 

pdf(here("sims/figs/bias/dgp2.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_bias)
dev.off()

# dgp 3 plots -------------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp3 <- gammas_3

# Under pi \approx .01, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model.  
# we pick beta_0[1] and use the corresponding sigma_e, sigma_p values of .8 and .8, 
# but could use different beta_0[j] and different sigma_e, sigma_p to generate the identical plot
sp3_05 <- sp3 %>% dplyr::mutate(
  prev_x = inv.logit(beta_0[1]+beta_1*(gammas_3$z1=="z11")+
                   beta_2*(gammas_3$z2=="z20")+beta_3*(gammas_3$z2=="z21")+
                   beta_4*(gammas_3$z3=="z30")+beta_5*(gammas_3$z3=="z31"))
  #pi = (prev_x - (1 - .8)) / (.8 - (1 - .8))
)

# plot gamma_j vs s_j
scenario3_selectionbias_plot <- ggplot(data = sp3_05, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_jitter(aes(size=prev_x), alpha = .6,
              color = "black", width = 0.001,
              show.legend = FALSE)  +
  geom_abline(colour = "grey50", size = 2) + 
 xlab("Stratum proportion in target pop.") + ylab("Sampling probability") + 
   #xlab(expression(gamma[j])) + 
  #ylab(expression(s[j])) + 
  theme_bw() + 
  theme(text = element_text(size=20),
        legend.position = c(.17,0.79),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))# + 
scenario3_selectionbias_plot

# plot bias from dgp 3 from variance results 
# note this is **truncated** inside [-100%, 100%]
res3_relbias <- results_3 %>% #filter(n_1 == 40 & sigma_e != .6) %>% 
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

res3_relbias_plot <- ggplot(data = res3_relbias, 
                         mapping = aes(x = prev, y = relbias,
                                       linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.91,0.89),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 2, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
#res3_relbias_plot

#send plot to pdf
pdf(here("sims/figs/relbias/dgp3.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res3_relbias_plot)
dev.off()


# non-relative bias plot 
res3_gg <- res3_relbias %>% mutate(bias = relbias * prev / 100)

res3_bias <- ggplot(data = res3_gg, 
                    mapping = aes(x = prev, y = bias,
                                  linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        #legend.position = c(.75,0.12),
        legend.position = c(.74, 0.1),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Bias") + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 2.5, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
res3_bias

pdf(here("sims/figs/bias/dgp3.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res3_bias)
dev.off()

# dgp 4 plots -------------------------------------------------------------

# plot the gamma_js vs sampling probs s_js to understand 
# the amount of sampling bias in each 
sp4 <- gammas_4

# Under pi \approx .05, assign the stratum-specific prevalences
# and then plot them. 
# Note that this plot will look the same under *any* of the marginal 
# prevalences, because of the way that we are varying marginal prevalence 
# by only varying the intercept of the logistic model .
sp4_05 <- sp4 %>% dplyr::mutate(
  prev_x = inv.logit(nu_0[1] + nu_1*(gammas_4$z1 == "z11") +
                   nu_2 * (gammas_4$z2 == "z20") + nu_3 * (gammas_4$z2 == "z21") +
                   nu_4 * (gammas_4$z3 == "z30") + nu_5 * (gammas_4$z3 == "z31") +
                   nu_6 * (gammas_4$z4 == "z41"))
)

# plot gamma_j vs s_j
scenario4_selectionbias_plot <- ggplot(data = sp4_05, aes(x=stratum_prop, y = sampling_prob)) + 
  geom_jitter(aes(size=prev_x), alpha = .6, 
              color = "black", width = .001, 
              show.legend = FALSE) + 
  geom_abline(colour = "grey50", size = 2) + 
  xlab("Stratum proportion in target pop.") + ylab("Sampling probability") + 
# xlab(expression(gamma[j])) + 
  #ylab(expression(s[j])) + 
  theme_bw() + 
  theme(text = element_text(size=20),
        legend.position = c(.8375,0.78),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) #+ 

#scenario4_selectionrelbias_plot

pdf(here("sims/figs/relbias/selbias_labels.pdf"),
    paper = "USr", width = 8.5, height = 11)
plot_grid(scenario3_selectionbias_plot, 
          scenario4_selectionbias_plot,
          ncol = 1,
          labels = c("A.", "B."),
          label_size = 18,
          hjust = 0
)
# grid.arrange(scenario3_selectionrelbias_plot,
#              scenario4_selectionrelbias_plot,
#              ncol = 1)
dev.off()


# plot bias from dgp 4 from variance results 
res4_relbias <- results_4 %>% 
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

res4_relbias_plot <- ggplot(data = res4_relbias, 
                         mapping = aes(x = prev, y = relbias,
                                       linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.91,0.89),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-100, 100)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 0.75, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
#res4_relbias_plot

pdf(here("sims/figs/relbias/dgp4.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res4_relbias_plot)
dev.off()


# non-relative bias plot 
res4_gg <- res4_relbias %>% mutate(bias = relbias * prev / 100)

res4_bias <- ggplot(data = res4_gg, 
                    mapping = aes(x = prev, y = bias,
                                  linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.74, 0.1),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Bias") + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 2, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 

pdf(here("sims/figs/bias/dgp4.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res4_bias)
dev.off()

# dgp 5 plots --------------------------------------------------------


# plot bias from dgp 5 from variance results 
# note this is **truncated** inside [-100%, 100%]
res5_relbias <- results_5 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
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

res5_relbias_plot <- ggplot(data = res5_relbias, 
                         mapping = aes(x = prev, y = relbias,
                                       linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.91,0.89),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 2, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
#res5_relbias_plot

#send plot to pdf
pdf(here("sims/figs/relbias/dgp5.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res5_relbias_plot)
dev.off()


# non-relative bias plot 
res5_gg <- res5_relbias %>% mutate(bias = relbias * prev / 100)

res5_bias <- ggplot(data = res5_gg, 
                    mapping = aes(x = prev, y = bias,
                                  linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.74, 0.1),
        #legend.position = c(.75,0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = 18), # or size=20
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Bias") + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 2, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 

pdf(here("sims/figs/bias/dgp5.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res5_bias)
dev.off()

# dgp 6 plots --------------------------------------------------------

# plot bias from dgp 6 from variance results 
res6_relbias <- results_6 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
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

res6_relbias_plot <- ggplot(data = res6_relbias, 
                     mapping = aes(x = prev, y = relbias,
                                   linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.91,0.89),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Relative Bias (%)") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-100, 100)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 0.5, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 

#res6_relbias_plot

pdf(here("sims/figs/relbias/dgp6.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res6_relbias_plot)
dev.off()


# non-relative bias plot 
res6_gg <- res6_relbias %>% mutate(bias = relbias * prev / 100)

res6_bias <- ggplot(data = res6_gg, 
                    mapping = aes(x = prev, y = bias,
                                  linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.74, 0.1),
        #legend.position = c(.75,0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Bias") + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray", "black"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 2, 0.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]),
                               expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 

pdf(here("sims/figs/bias/dgp6.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res6_bias)
dev.off()

