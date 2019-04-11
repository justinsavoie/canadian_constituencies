library(tidyverse)
library(RColorBrewer)

to_plot <- readRDS("data/to_plot.rds")

to_plot$`Mean score 1D` <- 1-to_plot$`Mean score 1D`

ggplot(to_plot, aes(x = `Mean self placement`, y = `Mean score 1D`, color = `Province`)) +
  geom_point(size = 2.5) +
  theme_minimal() +
  geom_smooth(se=FALSE, color = "grey50") +
  scale_color_manual(name = "Party", values = brewer.pal(n = 8, name = "Dark2")) +
  theme(legend.position = c(0.25,0.75),
        text = element_text(size = 18),
        legend.background = element_rect(colour = "black",fill="transparent"),
        panel.grid.minor = element_blank())

ggsave("figures/fig1.pdf",width = 8, height = 6, bg = "transparent")

