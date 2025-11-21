

##### Lake Solitude Figure 3 code: data compilation #####

## Load libraries
library(ggplot2)
library(dplyr)
library(cowplot)


data_dir <- "data"
figures_dir <- "figures"

# Read data (relative path)
fig3 <- read.csv(file.path(data_dir,"data/fig3_250925_holocene.csv"))


# fig compilation ---------------------------------------------------------

## Clastic Flux
fig3_CF <- ggplot() + 
  geom_line(data = fig3, aes(x = BD_age_1C_250828/1000, y = CF), color = "#777777") + #1C
  geom_point(data = fig3, aes(x = BD_age_1C_250828/1000, y = CF), shape = 21, fill = "#2e3192", color = "black", size = 2) +
  geom_smooth(data = fig3, method = "loess", span = 0.1, color = "black", aes(x = BD_age_1C_250828/1000, y = CF)) +
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  scale_y_continuous(limits = c(0,0.05)) +
  labs(y = "Clastic Flux", x = "") + 
  theme_minimal() +
  guides(color = "none")

fig3_CF

## D50
fig3_GS <- ggplot(data = fig3, aes(x = grainsize_age_250828/1000, y = D50)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "#2e3192", color = "black", size = 2) + 
  geom_smooth(method = "loess", span = .1, color = "black")+
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(y = "Grain Size (d50)", x = "Age (ka)") + 
  theme_minimal() +
  guides(color = "none")
fig3_GS

## Sand Flux
fig3_Sandflux <- ggplot(data = fig3, aes(x = SF_age_250828/1000, y = Sandflux)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "#2e3192", color = "black", size = 2)+
  geom_smooth(method = "loess", span = .1, color = "black") +
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  labs(y = "Sand flux", x = "Age (ka)") + 
  scale_y_continuous(limits = c(0,.037)) +
  theme_minimal() 
fig3_Sandflux


plot_grid(fig3_CF,fig3_Sandflux, fig3_GS,  align = "v", ncol = 1)

ggsave2("figures/Fig3snowpack_250925.svg", plot=ggplot2::last_plot(), scale=1)

## Biogenic Silica (Si/Ti)
fig3_SiTi <- ggplot(data = fig3, aes(x = XRF_age_250828/1000, y = SiTi)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "darkgreen", color = "black", size = 2) + 
  geom_smooth(method = "loess", span = .1, color = "black")+
  scale_x_reverse(limits = c (11.28,0), breaks = seq (0,10, by = 2)) +
  scale_y_continuous(limits = c(2,11)) +
  labs(y = "Si/Ti", x = "Age (ka)") + 
  theme_minimal() +
  guides(color = "none")
fig3_SiTi

## del13C
fig3_del13 <- ggplot(data = fig3, aes(x = bulkgeo_age_250828/1000, y = c13)) + 
  geom_line(color = "#777777") +
  geom_point(shape = 21, fill = "brown", color = "black", size = 2) + 
  geom_smooth(method = "loess", span = .2, color = "black")+
  scale_x_reverse(limits = c (11.28,), breaks = seq (0,10, by = 2)) +
  scale_y_reverse(limits = c(-20,-27)) +
  labs(y = "δ13C", x = "Age (ka)") + 
  guides(color = "none")+
  theme_minimal() 
fig3_del13

plot_grid(fig3_CF,fig3_Sandflux, fig3_GS, fig3_SiTi, fig3_del13,  align = "v", ncol = 1)
ggsave2("figures/Fig3compiled_250925.svg", plot=ggplot2::last_plot(), scale=1)



### Note: the below DNA bar 

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


