######
# Plot results from each scenario in a nested loop plot.
# For each scenario and set of simulation parameters,
# the plotting parameters must be tweaked significantly,
# so that the plot is legible and conveys information well. 
# Note that there are hundreds of sub-scenarios per scenario.

library(tidyverse)
library(looplot) # install with devtools::install_github("matherealize/looplot")
library(here)

#read in results files
results_1 <- read_csv(here("sims/results/scenario1_results.csv"))
results_2 <- read_csv(here("sims/results/scenario2_results.csv"))

########
# plots for scenario 1 
# for ease of using the NLP, start with a 2x2 plot
res1_2x2 <- results_1 %>% filter(sigma_e > .75 & sigma_p > .75)

p1_2x2 <- nested_loop_plot(resdf = res1_2x2,
            x = "prev", steps = c("n_3","n_2","n_1"),
            grid_rows = "sigma_e", grid_cols = "sigma_p",
            steps_y_base = -150, steps_y_height = 5,
            steps_y_shift = 15,
            steps_values_annotate = TRUE, steps_annotation_size = 4,
            x_name = "Prevalence", y_name = "Relative Bias (%)",
            methods = c("hat_pi"),
            spu_x_shift = .2,
            parameter_decreasing = TRUE,
            #steps_values_annotate = TRUE,
            hline_intercept = 0,
            y_expand_add = c(10, NULL),
            post_processing = list(
              add_custom_theme = list(
                axis.text.x = element_text(angle = -90,
                                           vjust = 0.5,
                                           size = 8)
              )
            )
        )

#send plot to pdf 
pdf(here("sims/plots/p1_2x2.pdf"))
print(p1_2x2)
dev.off()

########
# plots for scenario 2
# 

foo <- results_2[,1:8]
p_2 <- nested_loop_plot(resdf = foo,
                           x = "prev", steps = c("n_3","n_2","n_1"),
                           grid_rows = "sigma_e", grid_cols = "sigma_p",
                           steps_y_base = -2000, steps_y_height = 5,
                           steps_y_shift = 15,
                           steps_values_annotate = TRUE, steps_annotation_size = 4,
                           x_name = "Prevalence", y_name = "Relative Bias (%)",
                           methods = c("hat_pi","hat_pi_st"),
                           spu_x_shift = .2,
                           parameter_decreasing = TRUE,
                           #steps_values_annotate = TRUE,
                           hline_intercept = 0,
                           y_expand_add = c(10, NULL),
                           post_processing = list(
                             add_custom_theme = list(
                               axis.text.x = element_text(angle = -90,
                                                          vjust = 0.5,
                                                          size = 8)
                             )
                           )
)

p_2
