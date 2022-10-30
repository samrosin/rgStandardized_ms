# Plot mse results from each scenario 

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

results_3 <- read_csv(here("sims/results_final/dgp3_mse.csv"),
                      col_types = cols(.default = col_double())
)


results_4 <- read_csv(here("sims/results_final/dgp4_mse.csv"),
                      col_types = cols(.default = col_double())
)

results_5 <- read_csv(here("sims/results_final/dgp5_mse.csv"),
                      col_types = cols(.default = col_double())
)


results_6 <- read_csv(here("sims/results_final/dgp6_mse.csv"),
                      col_types = cols(.default = col_double())
)
# 
# results_7 <- read_csv(here("sims/results_final/dgp7_var_results.csv"),
#                       col_types = cols(.default = col_double())
# )


# dgp 1 plots --------------------------------------------------------------




# subset results 
res1_gg <- results_1 %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = mse,
         mse_pi_RG)


res1_mseplot <- ggplot(data = res1_gg, 
                    mapping = aes(x = pi, y = mse,
                                  linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = "none",
        # legend.position = c(.74, 0.45),
        #legend.position = c(.93,0.94),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Mean squared error") + 
  #scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  #                   limits = c(-50, 200)) + 
  scale_linetype_manual(name = "Method", values = c("dashed"),
                        labels = c(expression(hat(pi)[RG]))) + 
  scale_color_manual(name = "Method", values = c("black"),
                     labels = c(expression(hat(pi)[RG]))) + 
  scale_size_manual(name = "Method", values = c(0.5),
                    labels = c(expression(hat(pi)[RG]))) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
res1_mseplot

pdf(here("sims/figs/mse/mse1.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_mseplot)
dev.off()

# dgp 2 plots --------------------------------------------------------
res2_gg <- results_2 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = mse,
         mse_pi_RG, mse_pi_SRG)

res2_mseplot <- ggplot(data = res2_gg, 
                     mapping = aes(x = pi, y = mse,
                                   linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.75,0.92),
        #legend.position = c(.92,0.92),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Mean squared error") + 
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

res2_mseplot

pdf(here("sims/figs/mse/mse2.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_mseplot)
dev.off()

# dgp 3 plots -------------------------------------------------------------

# plot mse from dgp 3 
res3_mse <- results_3 %>% #filter(n_1 == 40 & sigma_e != .6) %>%
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>%
  gather(key = Method, value = mse,
         MSE_hat_pi_RG,
         MSE_hat_pi_SRG,
         MSE_hat_pi_SRGM) %>%
 mutate(Method = factor(Method, levels = unique(Method)))

res3_mseplot <- ggplot(data = res3_mse, 
                    mapping = aes(x = prev, y = mse,
                                  linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        #legend.position = c(.75,0.12),
        legend.position = c(.74, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Mean squared error") + 
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
  scale_y_continuous(limits = c(0, 0.005)) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
res3_mseplot

pdf(here("sims/figs/mse/mse3.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res3_mseplot)
dev.off()

# dgp 4 plots -------------------------------------------------------------

# plot mse from dgp 4 
res4_mse <- results_4 %>% #filter(n_1 == 40 & sigma_e != .6) %>%
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>%
  gather(key = Method, value = mse,
         MSE_hat_pi_RG,
         MSE_hat_pi_SRG,
         MSE_hat_pi_SRGM) %>%
  mutate(Method = factor(Method, levels = unique(Method)))

res4_mseplot <- ggplot(data = res4_mse, 
                       mapping = aes(x = prev, y = mse,
                                     linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        #legend.position = c(.75,0.12),
        legend.position = c(.74, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Mean squared error") + 
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
  scale_y_continuous(limits = c(0, 0.015)) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
res4_mseplot

pdf(here("sims/figs/mse/mse4.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res4_mseplot)
dev.off()


# dgp 5 plots --------------------------------------------------------


# plot mse from dgp 5
res5_mse <- results_5 %>% #filter(n_1 == 40 & sigma_e != .6) %>%
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>%
  gather(key = Method, value = mse,
         MSE_hat_pi_RG,
         MSE_hat_pi_SRG,
         MSE_hat_pi_SRGM) %>%
  mutate(Method = factor(Method, levels = unique(Method)))

res5_mseplot <- ggplot(data = res5_mse, 
                       mapping = aes(x = prev, y = mse,
                                     linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        #legend.position = c(.75,0.12),
        legend.position = c(.75, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Mean squared error") + 
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
  scale_y_continuous(limits = c(0, 0.005)) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
res5_mseplot

pdf(here("sims/figs/mse/mse5.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res5_mseplot)
dev.off()

# dgp 6 plots --------------------------------------------------------

# plot mse from dgp 6 
res6_mse <- results_6 %>% #filter(n_1 == 40 & sigma_e != .6) %>%
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>%
  gather(key = Method, value = mse,
         MSE_hat_pi_RG,
         MSE_hat_pi_SRG,
         MSE_hat_pi_SRGM) %>%
  mutate(Method = factor(Method, levels = unique(Method)))

res6_mseplot <- ggplot(data = res6_mse, 
                       mapping = aes(x = prev, y = mse,
                                     linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        #legend.position = c(.75,0.12),
        legend.position = c(.72, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "Mean squared error") + 
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
  scale_y_continuous(limits = c(0, 0.01)) + 
  geom_hline(aes(yintercept = 0), size = 0.5, linetype = "dotted", color = "gray") 
res6_mseplot

pdf(here("sims/figs/mse/mse6.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res6_mseplot)
dev.off()
