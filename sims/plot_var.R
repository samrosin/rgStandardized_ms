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
source(here("sims/inputs/sim_param_values_variance.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

color_scheme <- "Set2"

# Read in results files and stratum_proportion files.
# Note that the results are read in from the results_final subdirectory,
# not the results_draft subdirectory. 
# Results are manually moved into results_final 
# (after they are finalized, of course).
results_1 <- read_csv(here("sims/results_final/scenario1_var_results.csv"),
                      col_types = cols(.default = col_double())
) 
results_2 <- read_csv(here("sims/results_final/scenario2_var_results.csv"),
                      col_types = cols(.default = col_double())
)

results_3 <- read_csv(here("sims/results_final/scenario3_var_results.csv"),
                      col_types = cols(.default = col_double())
)
gammas_3 <- read_csv(here("sims/inputs/scenario3_stratum_props.csv"),
                     col_types = cols(
                       z1 = col_character(),
                       z2 = col_character(),
                       z3 = col_character(),
                       stratum_prop = col_double(),
                       sampling_prob = col_double()
                     ))

results_4 <- read_csv(here("sims/results_final/scenario4_var_results.csv"),
                      col_types = cols(.default = col_double())
)

gammas_4 <- read_csv(here("sims/inputs/scenario4_stratum_props.csv"),
                     col_types = cols(
                       z1 = col_character(),
                       z2 = col_character(),
                       z3 = col_character(),
                       z4 = col_character(),
                       stratum_prop = col_double(),
                       sampling_prob = col_double()
                     ))


# DGP 1 Plots --------------------------------------------------------------
res1_gg <- results_1 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         covers_pi_RG) %>% 
  mutate(covers = 100 * covers)

res1_facet <- ggplot(data = res1_gg, 
                     mapping = aes(x = pi, y = covers,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.945,0.948),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(80, 100)) + 
  scale_linetype_manual(name = "Method", values = 2,
                        labels = expression(hat(pi)[RG])) +
  scale_color_manual(name = "Method", values = scales::hue_pal()(1),
                     labels = expression(hat(pi)[RG])) + 
  geom_hline(aes(yintercept = 95), linetype = "dashed")

res1_facet

#send plot to pdf
pdf(here("sims/figs/var/DGP1_coverage.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res1_facet)
dev.off()


# DGP 2 Plots -------------------------------------------------------------
res2_gg <- results_2 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         covers_pi_RG, covers_pi_SRG) %>% 
  mutate(covers = 100 * covers)

res2_facet <- ggplot(data = res2_gg, 
                     mapping = aes(x = prev, y = covers,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.935,0.85),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
  scale_linetype_manual(name = "Method", values = c(2, 1),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3)[1:2],
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 95), linetype = "dashed")
res2_facet

#send plot to pdf
pdf(here("sims/figs/var/DGP2_coverage.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res2_facet)
dev.off()


# DGP 3 Plots -------------------------------------------------------------

# plot DGP3 for n_1 = 40

res3_gg <- results_3 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         covers_pi_RG, covers_pi_SRG, covers_pi_SRGM) %>% 
  mutate(covers = 100 * covers)

res3_facet <- ggplot(data = res3_gg, 
                     mapping = aes(x = prev, y = covers,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.935,0.75),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 95), linetype = "dashed")
res3_facet

#send plot to pdf
pdf(here("sims/figs/var/DGP3_coverage.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res3_facet)
dev.off()

# plot ESEs
res3_ese_gg <- results_3 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = ESE,
        # ESE_hat_pi_RG, 
         ESE_hat_pi_SRG, ESE_hat_pi_SRGM) 

res3_eses <- ggplot(data = res3_ese_gg, 
                     mapping = aes(x = prev, y = ESE,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.935,0.9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Empirical SE") + 
 # scale_y_continuous(labels = function(x) paste0(x, "%"),
 #                    limits = c(0, 100)) + 
  scale_linetype_manual(name = "Method", values = c(1, 3),
                        labels = c(
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3)[2:3],
                     labels = c(
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) #+ 
 # geom_hline(aes(yintercept = 95), linetype = "dashed")
res3_eses

#send plot to pdf
pdf(here("sims/figs/var/DGP3_ESEs.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res3_eses)
dev.off()

# DGP 4 Plots -------------------------------------------------------------

# plot for n_1 == 40
res4_gg <- results_4 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         covers_pi_RG, covers_pi_SRG, covers_pi_SRGM) %>% 
  mutate(covers = 100 * covers)

res4_facet <- ggplot(data = res4_gg, 
                     mapping = aes(x = prev, y = covers,
                                   linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.935,0.75),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
  scale_linetype_manual(name = "Method", values = c(2, 1, 3),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  geom_hline(aes(yintercept = 95), linetype = "dashed")
res4_facet

# send to pdf 
pdf(here("sims/figs/var/DGP4_coverage.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res4_facet)
dev.off()

# plot ESEs
res4_ese_gg <- results_4 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = ESE,
         # ESE_hat_pi_RG, 
         ESE_hat_pi_SRG, ESE_hat_pi_SRGM) 

res4_eses <- ggplot(data = res4_ese_gg, 
                    mapping = aes(x = prev, y = ESE,
                                  linetype = Method, color = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 16),
        legend.position = c(.935,0.9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        strip.text = element_text(size = 14)) + 
  labs(x = "Prevalence", y = "Empirical SE") + 
  # scale_y_continuous(labels = function(x) paste0(x, "%"),
  #                    limits = c(0, 100)) + 
  scale_linetype_manual(name = "Method", values = c(1, 3),
                        labels = c(
                          expression(hat(pi)[SRG]),
                          expression(hat(pi)[SRGM]))) + 
  scale_color_manual(name = "Method", values = scales::hue_pal()(3)[2:3],
                     labels = c(
                       expression(hat(pi)[SRG]),
                       expression(hat(pi)[SRGM]))) #+ 
# geom_hline(aes(yintercept = 95), linetype = "dashed")
res4_eses

#send plot to pdf
pdf(here("sims/figs/var/DGP4_ESEs.pdf"),
    paper = "USr", width = 8.5, height = 11)
print(res4_eses)
dev.off()
