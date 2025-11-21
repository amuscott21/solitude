##SOL20 Fig 3. Age model & strat column

library(tidyverse)
library(clam)
library(ggplot2)
library(dplyr)
library(cowplot)
library(knitr)
library(kableExtra)
library(tidyr)
library(reshape2)



### UPDATED SAR FROM 250828 CLAM RUN ## (with updated MZ age and new age at 408 cm)

fig3 <- read.csv("/Users/ameliamuscott/Desktop/R/Tetons/Teton data/Solitude_data/resubmit/fig 2/fig2_250907.csv")

fig3


fig3_BD <- ggplot() +
  geom_line(data = fig3, aes(x = Depth_BD_1B, y = BD_1B), color = "#777777") +  #1B
  geom_point(data = fig3, aes(x = Depth_BD_1B, y = BD_1B), color = "darkblue") +
  geom_line(data = fig3, aes(x = Depth_BD_1A, y = BD_1A), color = "#777777") + #1A
  geom_point(data = fig3, aes(x = Depth_BD_1A, y = BD_1A), color = "darkgreen") +
  geom_line(data = fig3, aes(x = Depth_BD_1C_clean, y = BD_1C_clean), color = "#777777") + #1C
  geom_point(data = fig3, aes(x = Depth_BD_1C_clean, y = BD_1C_clean), color = "darkred", size = 2) +
  geom_smooth(data = fig3, method = "loess", span = 0.1, color = "black", fill = NA, size = 2, aes(x = Depth_BD_compiled_clean, y = BD_compiled_clean)) +
  scale_x_continuous(limits = c (0,400)) +
  scale_y_continuous(limits = c(0.2,1.4)) +
  labs(y = bquote('Dry Bulk Density' ~ (g / cm ^ 3)), x = "Depth (cm)") + 
  theme_classic()
fig3_BD    

fig3_LOI <- ggplot() +
  geom_line(data = fig3, aes(x = Depth_BD_1B, y = LOI_1B), color = "#777777") +  #1B
  geom_point(data = fig3, aes(x = Depth_BD_1B, y = LOI_1B), color = "darkblue") +
  geom_line(data = fig3, aes(x = Depth_BD_1A, y = LOI_1A), color = "#777777") + #1A
  geom_point(data = fig3, aes(x = Depth_BD_1A, y = LOI_1A), color = "darkgreen") +
  geom_line(data = fig3, aes(x = Depth_BD_1C_clean, y = LOI_1C_clean), color = "#777777") + #1C
  geom_point(data = fig3, aes(x = Depth_BD_1C_clean, y = LOI_1C_clean), color = "darkred", size =2) +
  geom_smooth(data = fig3, method = "loess", span = 0.1, color = "black", fill = NA, size = 2, aes(x = Depth_BD_compiled_clean, y = LOI_compiled_clean)) +
  scale_x_continuous(limits = c (0,400)) +
  scale_y_continuous(limits = c(0,20)) +
  labs(y = "Organic Content (Loss-on-ignition %)", x = "") + 
  theme_classic()
fig3_LOI  

fig3_SAR <- ggplot() +
  geom_line(data = fig3, aes(x = depth, y = SAR), color = "black", linewidth = 2) +  #1B 
  scale_x_continuous(limits = c (0,400)) +
  scale_y_continuous(limits = c(0,0.075)) +
  labs(y = "Sediment Accumulation Rate (cm/yr)", x = "") + 
  theme_classic()
fig3_SAR


fig1_MS <- ggplot() +
  geom_line(data = fig3, aes(x = Depth_MS_1B, y = MS_1B), color = "#777777") +  #1B
  geom_point(data = fig3, aes(x = Depth_MS_1B, y = MS_1B), color = "darkblue") +
  geom_line(data = fig3, aes(x = Depth_MS_1A, y = MS_1A), color = "#777777") + #1A
  geom_point(data = fig3, aes(x = Depth_MS_1A, y = MS_1A), color = "darkgreen") +
  geom_line(data = fig3, aes(x = Depth_MS_1C_clean, y = MS_1C_clean), color = "#777777") + #1C
  geom_point(data = fig3, aes(x = Depth_MS_1C_clean, y = MS_1C_clean), color = "darkred") +
  geom_smooth(data = fig3, method = "loess", span = 0.1, fill = NA, size = 2, color = "black", aes(x = Depth_MS_compiled_clean, y = MS_compiled_clean)) +
  scale_x_continuous(limits = c (0,400)) +
  scale_y_continuous(limits = c(0,25)) +
  labs(y = "Magnetic Susceptibility", x = "") + 
  theme_classic()
fig1_MS


plot_grid(fig3_LOI, fig3_BD, fig3_SAR, ncol = 1, align = "v")
ggsave2("fig3_250907.svg", plot=ggplot2::last_plot(), scale=1, width = 7.76, height = 8.14)