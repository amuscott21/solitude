





# fig compilation ---------------------------------------------------------

fig3 <- read.csv("/Users/ameliamuscott/Desktop/R/Tetons/Teton data/Solitude_data/resubmit/fig 3/data updates 250828/Fig3data_250925.csv")
fig3

fig3_CF <- ggplot() + 
 # geom_line(data = fig3, aes(x = age_1B/1000, y = CF_1B), color = "#777777") +  #1B
 # geom_point(data = fig3, aes(x = age_1B/1000, y = CF_1B), color = "#0066FF") +
 # geom_line(data = fig3, aes(x = age_1A/1000, y = CF_1A), color = "#777777") + #1A
  #geom_point(data = fig3, aes(x = age_1A/1000, y = CF_1A), color = "#0066FF") +
  #geom_line(data = fig3, aes(x = age_1C/1000, y = CF_1C), color = "#777777") + #1C
  #geom_point(data = fig3, aes(x = age_1C/1000, y = CF_1C), color = "#0066FF") +
  geom_line(data = fig3, aes(x = BD_age_1C_250828/1000, y = CF), color = "#777777") + #1C
  geom_point(data = fig3, aes(x = BD_age_1C_250828/1000, y = CF), shape = 21, fill = "#2e3192", color = "black", size = 2) +
  geom_smooth(data = fig3, method = "loess", span = 0.1, color = "black", aes(x = BD_age_1C_250828/1000, y = CF)) +
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  scale_y_continuous(limits = c(0,0.05)) +
  #scale_color_gradientn(limits = c (0, .50), colors = c("white","#77CCFF", "#3388FF", "#0066FF","#0000FF")) +
  labs(y = "Clastic Flux", x = "") + 
  theme_minimal() +
  guides(color = "none")

fig3_CF

"#0031FD"



fig3_GS <- ggplot(data = fig3, aes(x = grainsize_age_250828/1000, y = D50)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "#2e3192", color = "black", size = 2) + 
  #geom_point(aes( color = D50))+
  geom_smooth(method = "loess", span = .1, color = "black")+
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  scale_y_continuous(limits = c(0,100)) +
  # scale_color_gradientn(limits = c (0, 100), colors = c("white","#77CCFF", "#3388FF", "#0066FF","#0000FF")) +
  labs(y = "Grain Size (d50)", x = "Age (ka)") + 
  theme_minimal() +
  guides(color = "none")
fig3_GS

fig3_Sandflux <- ggplot(data = fig3, aes(x = SF_age_250828/1000, y = Sandflux)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "#2e3192", color = "black", size = 2)+
  geom_smooth(method = "loess", span = .1, color = "black") +
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  labs(y = "Sand flux", x = "Age (ka)") + 
  scale_y_continuous(limits = c(0,.037)) +
  theme_minimal() 
fig3_Sandflux

library(cowplot)

plot_grid(fig3_CF,fig3_Sandflux, fig3_GS,  align = "v", ncol = 1)

plot_grid(fig3_SAR, fig3_CF,fig3_Sandflux, fig3_GS, align = "v", ncol = 1)
ggsave2("Fig3phys_250925.svg", plot=ggplot2::last_plot(), scale=1)

fig3_SiTi <- ggplot(data = fig3, aes(x = XRF_age_250828/1000, y = SiTi)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "darkgreen", color = "black", size = 2) + 
  # geom_point(aes(color = Si_Ti))+
  geom_smooth(method = "loess", span = .1, color = "black")+
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  scale_y_continuous(limits = c(2,11)) +
  # scale_color_gradientn(limits = c (2, 11), colors = c("#4C53AA","#838ED9",    "#ECE8E5", "#F59292","#D9564C")) + 
  labs(y = "Si/Ti", x = "Age (ka)") + 
  theme_minimal() +
  guides(color = "none")
fig3_SiTi

View(fig3)

fig3_CN <- ggplot(data = fig3, aes(x = bulkgeo_age_250828/1000, y = c13)) + 
  geom_line(color = "#777777") +
  geom_point(color = "darkgreen")+
  geom_smooth(method = "loess", span = .09, color = "black")+
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  scale_y_continuous(limits = c(9.3,13.3)) +
  labs(y = "C:N", x = "Age (ka)") + 
  theme_classic() 
fig3_CN

fig3_del13 <- ggplot(data = fig3, aes(x = bulkgeo_age_250828/1000, y = c13)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "darkgreen", color = "black", size = 2) + 
  # geom_point(aes(color = c13))+
  geom_smooth(method = "loess", span = .2, color = "black")+
  scale_x_reverse(limits = c (11.28,-0), breaks = seq (0,10, by = 2)) +
  scale_y_reverse(limits = c(-20,-27)) +
  # scale_color_gradientn(limits = c (-26, -20), colors = c("#5C4033", "lightgreen")) + 
  labs(y = "δ13C", x = "Age (ka)") + 
  guides(color = "none")+
  theme_minimal() 
fig3_del13


fig3_DNAbar <- ggplot(gradient_plot_data_agg, aes(x = age/1000, y = proportion, fill = habitat_detail)) +
  geom_col(position = "stack", color = "black", width = 0.4) +
  labs(
    x = "Age (cal ka)", 
    y = "Relative Abundance",
    fill = "Habitat Type"
  ) +
  scale_fill_manual(
    values = c(
      "Aquatic" = "#08519C",
      "Aquatic/Riparian" = "#3182BD", 
      "Riparian" = "#6BAED6",
      "Riparian/Meadow" = "#9ECAE1",
      "Riparian/Moist Woodland" = "#4A9B7F",
      "Meadow" = "#FFFACD",
      "Terrestrial/Shrub" = "#8B7355",
      "Conifer" = "#654321"
    )
  ) +
  scale_x_reverse(limits = c(11.28, 0), breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "top")
fig3_DNAbar


# Area plot with the refined classifications
fig3_DNA <- ggplot(gradient_plot_data_agg, aes(x = age/1000, y = proportion, fill = habitat_detail)) +
  geom_area(position = "stack", alpha = 0.8, color = "black", linewidth = 0.2) +
  labs(
    x = "Age (cal ka)", 
    y = "Relative Abundance",
    fill = "Habitat Type"
  ) +
  scale_fill_manual(
    values = c(
      "Aquatic" = "#08519C",
      "Aquatic/Riparian" = "#3182BD", 
      "Riparian" = "#6BAED6",
      "Riparian/Meadow" = "#9ECAE1",
      "Riparian/Moist Woodland" = "#4A9B7F",
      "Meadow" = "#FFFACD",
      "Terrestrial/Shrub" = "#8B7355",
      "Conifer" = "#654321"
    )
  ) +
  scale_x_reverse(limits = c(11.28, 0), breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "top")

fig3_DNA





plot_grid(fig3_SiTi,fig3_del13,fig3_DNAbar, align = "v", ncol = 1)

plot_grid(fig3_CF, fig3_Sandflux, fig3_GS, fig3_SiTi, fig3_del13, fig3_DNA, ncol = 1, align = "v")
library(sv)

ggsave2("Fig3_250923_bio.svg", plot=ggplot2::last_plot(), scale=1)
