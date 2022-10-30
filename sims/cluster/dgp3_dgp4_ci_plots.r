# plot supplementary figures 8 and 11 showing random samples of 
# point and interval estimates for DGPs 3 and 4 \hat \pi_{SRGM}

library(ggplot2)
# (1) run lines 1-35 of dgp3_analyse.R for DGP *3* then run the following:

all_res <- all_results %>% filter(
  sigma_e == .99 & sigma_p == .99 & prevs == .01
) %>%
  select(c(sigma_e, sigma_p, prev, prevs, hat_pi_SRGM, hat_var_pi_SRGM, ci_lower_pi_SRGM,
           ci_upper_pi_SRGM, covers_pi_SRGM, relbias_SRGM))
all_res <- all_res %>%
  mutate(bias_SRGM = relbias_SRGM * prevs / 100,
         sample = 1:nrow(all_res)) %>%
  mutate(covers_pi_SRGM = factor(covers_pi_SRGM)) %>%
  filter(!is.na(covers_pi_SRGM))

dgp3_res <- all_res

# (2) run lines 1-35 of dgp4_analyse for DGP *4* then run the following:

all_res <- all_results %>% filter(
  sigma_e == .99 & sigma_p == .99 & prevs == .01
) %>%
  select(c(sigma_e, sigma_p, prev, prevs, hat_pi_SRGM, hat_var_pi_SRGM, ci_lower_pi_SRGM,
           ci_upper_pi_SRGM, covers_pi_SRGM, relbias_SRGM))
all_res <- all_res %>%
  mutate(bias_SRGM = relbias_SRGM * prevs / 100,
         sample = 1:nrow(all_res)) %>%
  mutate(covers_pi_SRGM = factor(covers_pi_SRGM)) %>%
  filter(!is.na(covers_pi_SRGM))

 dgp4_res <- all_res
 
 # (3) run the rest of ths file

# dgp3_hist <- ggplot(dgp3_res, aes(x = hat_pi_SRGM)) + 
#   geom_histogram() + 
#   xlab(expression(hat(pi)[SRGM])) +
#   geom_vline(xintercept=.01, linetype="dashed", color = "gray") +
#   theme_minimal() + 
#   theme(axis.text.y = element_text(size = 16),
#         axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
#         axis.title = element_text(size = 20),
#         legend.position = c(.9, 0.8),
#         legend.title = element_text(size=18),
#         legend.text = element_text(size = 16),
#         strip.text = element_text(size = 16))
# dgp3_hist

dgp3_reorder <- dgp3_res[sample(nrow(dgp3_res), 200), ]
dgp3_reorder <- dgp3_reorder[order(dgp3_reorder$hat_pi_SRGM),] %>%
  mutate(sample = 1:200)

dgp3_plot <- dgp3_reorder %>% 
  ggplot(aes(x = sample, y = hat_pi_SRGM)) + 
  geom_point(aes(color = covers_pi_SRGM)) + 
  geom_errorbar(aes(ymin = ci_lower_pi_SRGM, ymax = ci_upper_pi_SRGM, color = covers_pi_SRGM), alpha = .4) + 
  coord_flip() + 
  geom_hline(yintercept = .01, linetype = "dashed", color = "black") + 
  #labs(title = "DGP 4: 95% Confidence Intervals") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0, .4) +  
  scale_color_manual(values = c("black", "gray"), labels = c("No", "Yes")) + 
  labs(colour="CI covers?") +
  xlab("Ordered simulation index") + 
  ylab(expression(hat(pi)[SRGM])) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.9, 0.8),
        legend.title = element_text(size=18),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))
dgp3_plot


  
#   #gghist(dgp4_res$hat_pi_SRGM, xlab=expression(hat(pi)[SRGM]),main=NULL) 
# dgp4_hist <- ggplot(dgp4_res, aes(x = hat_pi_SRGM)) + 
#   geom_histogram() + 
#   xlab(expression(hat(pi)[SRGM])) +
#   geom_vline(xintercept=.01, linetype="dashed", color = "gray") +
#   theme_minimal() + 
#   theme(axis.text.y = element_text(size = 16),
#         axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
#         axis.title = element_text(size = 20),
#         legend.position = c(.9, 0.8),
#         legend.title = element_text(size=18),
#         legend.text = element_text(size = 16),
#         strip.text = element_text(size = 16))
# dgp4_hist
# 
# dgp4_hist_srg <- ggplot(dgp4_res, aes(x = hat_pi_SRG)) + 
#   geom_histogram() + 
#   xlab(expression(hat(pi)[SRG])) +
#   geom_vline(xintercept=.01, linetype="dashed", color = "gray") +
#   theme_minimal() + 
#   theme(axis.text.y = element_text(size = 16),
#         axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
#         axis.title = element_text(size = 20),
#         legend.position = c(.9, 0.8),
#         legend.title = element_text(size=18),
#         legend.text = element_text(size = 16),
#         strip.text = element_text(size = 16))
# dgp4_hist_srg


dgp4_reorder <- dgp4_res[sample(nrow(dgp4_res), 200), ]
dgp4_reorder <- dgp4_reorder[order(dgp4_reorder$hat_pi_SRGM),] %>%
  mutate(sample = 1:200)

dgp4_plot <- dgp4_reorder %>% ggplot(aes(x = sample, y = hat_pi_SRGM)) + 
  geom_point(aes(color = covers_pi_SRGM)) + 
  geom_errorbar(aes(ymin = ci_lower_pi_SRGM, ymax = ci_upper_pi_SRGM, color = covers_pi_SRGM), alpha = .2) + 
   coord_flip() + 
  geom_hline(yintercept = .01, linetype = "dashed", color = "black") + 
  #labs(title = "DGP 4: 95% Confidence Intervals") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0, .4) +  
  scale_color_manual(values = c("black", "lightgray"), labels = c("No", "Yes")) + 
  labs(colour="CI covers?") +
  xlab("Ordered simulation index") + 
  ylab(expression(hat(pi)[SRGM])) + 
  theme_classic() + 
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5),
        axis.title = element_text(size = 20),
        legend.position = c(.9, 0.8),
        legend.title = element_text(size=18),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))

dgp4_plot#send plots to pdf
# pdf("dgp4_var_fix",
#     paper = "USr",width = 8.5, height = 11)
# print(dgp4_plot)
# dev.off()

setwd("/nas/longleaf/home/srosin/rgStandardized")

pdf("dgp3_plot.pdf",
    paper = "USr", width = 8.5, height = 11)
dgp3_plot
dev.off()

pdf("dgp4_plot.pdf",
    paper = "USr", width = 8.5, height = 11)
dgp4_plot
dev.off()

pdf("dgp3_plots.pdf",
    paper = "USr", width = 8.5, height = 11)
plot_grid(dgp3_hist, 
          dgp3_plot,
          ncol = 1,
          labels = c("A.", "B."),
          label_size = 18,
          hjust = 0
)
dev.off()

pdf("dgp4_plots.pdf",
    paper = "USr", width = 8.5, height = 11)
plot_grid(dgp4_hist, 
          dgp4_plot,
          ncol = 1,
          labels = c("A.", "B."),
          label_size = 18,
          hjust = 0
)
dev.off()


# 
# pdf("dgp3_4_varplots.pdf",
#     paper = "USr", width = 8.5, height = 11)
# plot_grid(dgp3_plot, 
#           dgp4_plot,
#           ncol = 1,
#           labels = c("A.", "B."),
#           label_size = 18,
#           hjust = 0
# )
# # grid.arrange(scenario3_selectionrelbias_plot,
# #              scenario4_selectionrelbias_plot,
# #              ncol = 1)
# dev.off()




#hist(all_res$bias_SRGM)
# 
# ggplot(all_res, aes(x = bias_SRGM)) + stat_bin() + 
#   scale_x_continuous(breaks = seq(-.1, .15, by = .01))
# 
# ggplot(all_res, aes(x = hat_pi_SRGM)) + stat_bin() + 
#   scale_x_continuous(breaks = seq(0, .25, by = .01))

