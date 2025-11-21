##### Lake Solitude Figure 4 code: Rioja plot, CONISS #####


## Load libraries
library(tidyverse)
library(cowplot)
library(ggplot2)

## Notes: 
## this code should be run AFTER the sedaDNA_dataprep.R code


# CONISS ------------------------------------------------------------------

## load libraries
library(dplyr)
library(tidyr)
library(rioja)
library(vegan)
library(tibble)



## Prepare OTU matrix

### Ensure samples are rows, taxa are columns
coniss_data <- t(final_reads_matrix_ra_rel)  # transpose: samples as rows


###  Sample ages
ages_sorted <- meta$new_age[match(rownames(coniss_data), meta$extraction_ID)]
age_order <- order(ages_sorted)
coniss_data_sorted <- coniss_data[age_order, ]
ages_sorted <- ages_sorted[age_order]


### Growth habit assignment

tree <- c("Juniperus", "Pinus", "Abies", "Picea")
shrub <- c("Salix", "Vaccinium", "Spiraea")
forb <- c("Bothriospermum", "Caltha", "Erythranthe", "Parnassia", "Potentilla",
          "Pedicularis", "Hypericum", "Penstemon", "Epilobium", "Veronica", "Athyrium")
graminoid <- c("Carex", "Juncus")

taxon_growth <- sapply(colnames(coniss_data_sorted), function(taxon) {
  if(taxon %in% tree) return("Tree")
  if(taxon %in% shrub) return("Shrub")
  if(taxon %in% forb) return("Forb")
  if(taxon %in% graminoid) return("Graminoid")
  return("Unknown")
})

growth_habit_colors <- c(
  Tree = "#1b9e77",
  Shrub = "#d95f02",
  Forb = "#7570b3",
  Graminoid = "#1f78b4",
  Unknown = "#b2b2b2"
)

taxa_colors <- growth_habit_colors[taxon_growth]

### Custom taxa order on for rioja plot

custom_order <- c(
  "Salix", "Vaccinium","Spiraea",
  "Athyrium", "Caltha","Pedicularis",
  "Bothriospermum","Parnassia","Penstemon","Veronica","Erythranthe", 
  "Hypericum","Potentilla", "Epilobium","Juncus", "Carex",
  "Juniperus","Pinus","Abies","Picea"
)

### Keep only taxa present in the dataset
custom_order <- custom_order[custom_order %in% colnames(coniss_data_sorted)]
coniss_data_ordered <- coniss_data_sorted[, custom_order]

### Adjust colors to match the custom order
plot_colors <- ifelse(custom_order %in% tree, "#1b9e77",
                      ifelse(custom_order %in% shrub, "#d95f02",
                             ifelse(custom_order %in% graminoid, "#1f78b4",
                                    "#7570b3")))
names(plot_colors) <- custom_order


###  Run CONISS clustering

coniss_result <- chclust(
  vegdist(coniss_data_sorted, method = "bray"),
  method = "coniss"
)
plot(coniss_result)

### Broken stick test ###
bstick_result <- bstick(coniss_result)

plot(bstick_result)


### PERMANOVA and ANOSIM for Early vs Late Holocene (test for statistical differences betweent the two groups)

ages_sorted

# Define groups
holocene_group <- ifelse(ages_sorted <= 5900, "Early", "Late")

# Convert to factor
holocene_group <- factor(holocene_group, levels = c("Early", "Late"))

# PERMANOVA using Bray-Curtis
set.seed(123)  # for reproducibility
permanova_result <- adonis2(coniss_data_sorted ~ holocene_group, 
                            method = "bray", permutations = 999)
cat("=== PERMANOVA Result ===\n")
print(permanova_result)

# ANOSIM using Bray-Curtis
anosim_result <- anosim(vegdist(coniss_data_sorted, method = "bray"), 
                        grouping = holocene_group, permutations = 999)
cat("=== ANOSIM Result ===\n")
print(anosim_result)



### RIOJA PLOT

svg("figures/fig4_stratplot_CONISS_251002.svg", width = 15.5, height = 12.9)
strat.plot(
  coniss_data_ordered,
  clust = coniss_result,
  yvar = ages_sorted / 1000,  # convert to cal ka
  y.rev = TRUE,
  y.tks = seq(0, max(ages_sorted)/1000, by = 2),
  scale.percent = FALSE,
  col.poly = plot_colors,
  plot.poly = TRUE,
  plot.line = TRUE,
  col.line = "black",
  lwd.bar = 1,
  cex.yaxis = 0.8,
  cex.axis = 0.8,
  ylabel = "Age (cal ka)",
  xlabel = "Relative Abundance",
  srt.xlabel = 45
)
dev.off()




