library(tidyverse)
library(ggplot2)
library(here)
library(scales)

# helpful: https://datascienceplus.com/lattice-like-forest-plot-using-ggplot2-in-r/

# belgium forest plot -----------------------------------------------------------------
belg <- read_csv(here("ests.csv")) %>%
  filter(method != "herzog")

# collection round dates
cr <- factor(rep(c("30 Mar - 5 Apr", "20 - 26 Apr", "18 - 25 May", "8 - 13 Jun", 
        "29 Jun - 4 Jul", "9 - 12 Sept", "12 - 17 Aug"), each = 4),
        levels = c("30 Mar - 5 Apr", "20 - 26 Apr", "18 - 25 May", "8 - 13 Jun", 
                   "29 Jun - 4 Jul", "12 - 17 Aug", "9 - 12 Sept"))
belg$cr <- cr

p <-  ggplot(data = belg,
           aes(x = method,y = mean, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col = method, shape = method)) +
  #geom_hline(aes(fill=method),yintercept =1, linetype=2)+
  xlab('Collection period') + ylab("Seroprevalence")+
  geom_errorbar(aes(ymin=lower, ymax=upper, col=method), width=0.2, cex=1)+ 
  scale_color_manual(name = "Method", values = scales::hue_pal()(4),
                     
  #scale_color_manual(name = "Method", values = c("gray25", "gray50", "black", "gray75"),
                     labels = c(expression(hat(rho)),
                                expression(hat(pi)[RG]), 
                                expression(hat(pi)[SRG]),
                                expression(hat(pi)[SRGM]))) + 
  # scale_linetype_manual(name = "Method", values = c("dashed", "dashed", "solid", "solid"),
  #                    labels = c(expression(hat(rho)),
  #                               expression(hat(pi)[RG]),
  #                               expression(hat(pi)[SRG]),
  #                               expression(hat(pi)[SRGM]))) +
  scale_shape_manual(name = "Method", values = 15:18,
                        labels = c(expression(hat(rho)),
                                   expression(hat(pi)[RG]), 
                                   expression(hat(pi)[SRG]),
                                   expression(hat(pi)[SRGM]))) + 
  facet_wrap(~cr, strip.position="top", nrow=1, scales = "free_x") +
  theme_bw() + 
  theme(plot.title = element_text(size=16,face="bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.ticks.x = element_blank(),
        legend.position = c(.935,0.89),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size=20),
        strip.text = element_text(size = 13)) 
p

pdf(here("forest_color.pdf"),
    paper = "USr",width = 11, height = 8.5)
print(p)
dev.off()

