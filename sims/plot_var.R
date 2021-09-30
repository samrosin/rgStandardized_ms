# plot variance/coverage results from each scenario 

# setup -------------------------------------------------------------------

library(tidyverse)
library(here)
source(here("sims/inputs/param_values_var.R")) #load sim parameter values common across scenarios
source(here("sims/sim_fns.R"))

color_scheme <- "Set2"

# Read in results files and stratum_proportion files.
# Note that the results are read in from the results_final subdirectory,
# not the results_draft subdirectory. 
# Results are manually moved into results_final 
# (after they are finalized, of course).
results_1 <- read_csv(here("sims/results_final/dgp1_var_results.csv"),
                      col_types = cols(.default = col_double())
) 
results_2 <- read_csv(here("sims/results_final/dgp2_var_results.csv"),
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

results_5 <- read_csv(here("sims/results_final/dgp5_var_results.csv"),
                      col_types = cols(.default = col_double())
)

gammas_5 <- gammas_3

results_6 <- read_csv(here("sims/results_final/dgp6_var_results.csv"),
                      col_types = cols(.default = col_double())
)

gammas_6 <- gammas_4

# dgp 1 --------------------------------------------------------------
res1_gg <- results_1 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         covers_pi_RG) %>% 
  mutate(covers = 100 * covers)

res1_cov <- ggplot(data = res1_gg, 
                     mapping = aes(x = pi, y = covers,
                                   linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = "none",
        #legend.position = c(.07,0.65),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
  scale_linetype_manual(name = "Method", values = c("dashed"),
                        labels = c(expression(hat(pi)[RG]))) + 
  scale_color_manual(name = "Method", values = c("black"),
                     labels = c(expression(hat(pi)[RG]))) + 
  scale_size_manual(name = "Method", values = c(0.5),
                    labels = c(expression(hat(pi)[RG]))) + 
  geom_hline(aes(yintercept = 95), size = 0.5, linetype = "dotted", color = "gray") 

#res1_cov

#send plot to pdf
pdf(here("sims/figs/var/dgp1_coverage.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_cov)
dev.off()

# dgp 2 -------------------------------------------------------------
is_in_subset <- function(x){round((100*x) %% 1.0, 5) == 0} # some extra sims were run but we use this to subset to {.01, .02, ..., .20}

res2_gg <- results_2 %>% 
  filter(is_in_subset(prev)) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         covers_pi_RG, covers_pi_SRG) %>% 
  mutate(covers = 100 * covers)

# note dim(results_2 %>% filter(is_in_subset(prev))) is half of dim(results_2)

res2_cov <- ggplot(data = res2_gg, 
                     mapping = aes(x = prev, y = covers,
                                   linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.09,0.65),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid"),
                        labels = c(expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]))) + 
  scale_color_manual(name = "Method", values = c("black", "gray"),
                     labels = c(expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]))) + 
  scale_size_manual(name = "Method", values = c(0.5, 1.5),
                    labels = c(expression(hat(pi)[RG]), 
                               expression(hat(pi)[SRG]))) + 
  geom_hline(aes(yintercept = 95), size = 0.5, linetype = "dotted", color = "gray") 
#res2_cov

#send plot to pdf
pdf(here("sims/figs/var/dgp2_coverage.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res2_cov)
dev.off()


# dgp 3 -------------------------------------------------------------------

res3_gg <- results_3 %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         coverage_pi_RG, 
         coverage_pi_SRG, 
         coverage_pi_SRGM) %>% 
  mutate(covers = 100 * covers, 
         Method = factor(Method, levels = unique(Method)))

res3_cov <- ggplot(data = res3_gg, 
                   mapping = aes(x = prev, y = covers,
                                 linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.91, 0.2),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
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
  geom_hline(aes(yintercept = 95), size = 0.5, linetype = "dotted", color = "gray") 
#res3_cov

#send plot to pdf
pdf(here("sims/figs/var/dgp3_coverage.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res3_cov)
dev.off()

# dgp 4 -------------------------------------------------------------------

res4_gg <- results_4 %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         coverage_pi_RG, 
         coverage_pi_SRG, 
         coverage_pi_SRGM) %>% 
  mutate(covers = 100 * covers, 
         Method = factor(Method, levels = unique(Method)))

res4_cov <- ggplot(data = res4_gg, 
                   mapping = aes(x = prev, y = covers,
                                 linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.09, 0.65),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                    limits = c(0, 100)) + 
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
  geom_hline(aes(yintercept = 95), size = 0.5, linetype = "dotted", color = "gray") 
#res4_cov

#send plot to pdf
pdf(here("sims/figs/var/dgp4_coverage.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res4_cov)
dev.off()


# dgp 5 -------------------------------------------------------------

# plot dgp5 for n_1 = 40

res5_gg <- results_5 %>% filter(n_1 == 40 & sigma_e != .6) %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
        coverage_pi_RG, 
         coverage_pi_SRG, 
         coverage_pi_SRGM) %>% 
  mutate(covers = 100 * covers, 
         Method = factor(Method, levels = unique(Method)))

res5_cov <- ggplot(data = res5_gg, 
                   mapping = aes(x = prev, y = covers,
                                 linetype = Method, color = Method,
                                 size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.09, 0.65),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
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
  geom_hline(aes(yintercept = 95), size = 0.5, linetype = "dotted", color = "gray")  
#res5_cov

#send plot to pdf
pdf(here("sims/figs/var/dgp5_coverage.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res5_cov)
dev.off()

# dgp 6 -------------------------------------------------------------

# plot for n_1 == 40
res6_gg <- results_6 %>% 
  dplyr::rename(Spec = sigma_p, Sens = sigma_e) %>% 
  gather(key = Method, value = covers,
         coverage_pi_RG,
         coverage_pi_SRG, 
         coverage_pi_SRGM
  ) %>% 
  mutate(covers = 100 * covers, 
         Method = factor(Method, levels = unique(Method)))

res6_cov <- ggplot(data = res6_gg, 
                   mapping = aes(x = prev, y = covers,
                                 linetype = Method, color = Method, size = Method)) +
  geom_line() + 
  facet_grid(Sens ~ Spec, 
             labeller = labeller(.rows = label_both, .cols = label_both)) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        #legend.position = c(.1,0.65),
        legend.position = c(.09, 0.63),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        #                    legend.background = element_rect(fill=alpha('white', 0)),
        panel.spacing.y = unit(1.25, "lines"),
        strip.text = element_text(size = 18)) + 
  labs(x = "Prevalence", y = "95% CI Coverage") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) + 
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
  geom_hline(aes(yintercept = 95), size = 0.5, linetype = "dotted", color = "gray")  
#res6_cov

#send plot to pdf
pdf(here("sims/figs/var/dgp6_coverage.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res6_cov)
dev.off()

