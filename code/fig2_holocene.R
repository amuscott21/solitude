
##### Lake Solitude Figure 2 code: sedimentology plot #####


## Load libraries
library(tidyverse)
library(cowplot)
library(ggplot2)


# Read data
fig2 <- read.csv("data/fig2_250907_holocene.csv")

fig2_BD <- ggplot() +
  geom_line(data = fig2, aes(x = Depth_BD_1B, y = BD_1B), color = "#777777") +  #1B
  geom_point(data = fig2, aes(x = Depth_BD_1B, y = BD_1B), color = "darkblue") +
  geom_line(data = fig2, aes(x = Depth_BD_1A, y = BD_1A), color = "#777777") + #1A
  geom_point(data = fig2, aes(x = Depth_BD_1A, y = BD_1A), color = "darkgreen") +
  geom_line(data = fig2, aes(x = Depth_BD_1C_clean, y = BD_1C_clean), color = "#777777") + #1C
  geom_point(data = fig2, aes(x = Depth_BD_1C_clean, y = BD_1C_clean), color = "darkred", size = 2) +
  geom_smooth(data = fig2, method = "loess", span = 0.1, color = "black", fill = NA, size = 2, aes(x = Depth_BD_compiled_clean, y = BD_compiled_clean)) +
  scale_x_continuous(limits = c (0,400)) +
  scale_y_continuous(limits = c(0.2,1.4)) +
  labs(y = bquote('Dry Bulk Density' ~ (g / cm ^ 3)), x = "Depth (cm)") + 
  theme_classic()
fig2_BD    

fig2_LOI <- ggplot() +
  geom_line(data = fig2, aes(x = Depth_BD_1B, y = LOI_1B), color = "#777777") +  #1B
  geom_point(data = fig2, aes(x = Depth_BD_1B, y = LOI_1B), color = "darkblue") +
  geom_line(data = fig2, aes(x = Depth_BD_1A, y = LOI_1A), color = "#777777") + #1A
  geom_point(data = fig2, aes(x = Depth_BD_1A, y = LOI_1A), color = "darkgreen") +
  geom_line(data = fig2, aes(x = Depth_BD_1C_clean, y = LOI_1C_clean), color = "#777777") + #1C
  geom_point(data = fig2, aes(x = Depth_BD_1C_clean, y = LOI_1C_clean), color = "darkred", size =2) +
  geom_smooth(data = fig2, method = "loess", span = 0.1, color = "black", fill = NA, size = 2, aes(x = Depth_BD_compiled_clean, y = LOI_compiled_clean)) +
  scale_x_continuous(limits = c (0,400)) +
  scale_y_continuous(limits = c(0,20)) +
  labs(y = "Organic Content (Loss-on-ignition %)", x = "") + 
  theme_classic()
fig2_LOI  

fig2_SAR <- ggplot() +
  geom_line(data = fig2, aes(x = depth, y = SAR), color = "black", linewidth = 2) +  #1B 
  scale_x_continuous(limits = c (0,400)) +
  scale_y_continuous(limits = c(0,0.075)) +
  labs(y = "Sediment Accumulation Rate (cm/yr)", x = "") + 
  theme_classic()
fig2_SAR

plot_grid(fig2_LOI, fig2_BD, fig2_SAR, ncol = 1, align = "v")
ggsave2("figures/fig2.svg", plot=ggplot2::last_plot(), scale=1, width = 7.76, height = 8.14)
