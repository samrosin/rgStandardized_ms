results_1 <- sim_results %>% filter(pi != .005)
res1_nlp <- nested_loop_plot(resdf = results_1,
                                   x = "pi",
                                   grid_rows = "sigma_e", grid_cols = "sigma_p",
                                   x_name = "Marginal Prevalence", 
                                   y_name = "Relative Bias (%)",
                                   methods = c("hat_pi_RG","hat_pi_SRG"),
                                   colors = scales::brewer_pal(palette = color_scheme),
                                   spu_x_shift = .2,
                                   hline_intercept = 0,
                                  # line_linetypes = c(2,1),
                                  # x_labels = NULL,
                                   y_expand_add = c(2.5, NULL),
                                   post_processing = list(add_custom_theme = list(
                                     axis.text.y = element_text(size = 12),
                                     axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5),
                                     axis.title = element_text(size = 16),
                                     legend.position = c(.825,0.08),
                                     legend.title = element_text(size = 14),
                                     legend.text = element_text(size = 12),
                                     legend.background = element_rect(fill=alpha('white', 0)),
                                     strip.text = element_text(size = 14)))
) + scale_linetype_manual("Method", values = c('hat_pi_RG' = 'dashed', 'hat_pi_SRG' = 'solid'))

res1_nlp
pdf(here("sims/bias_plots/DGP2.pdf"),
    paper = "USr",width = 8.5, height = 11)
print(res1_nlp)
dev.off()
