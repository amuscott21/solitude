
##### Lake Solitude Figure 1 code: PRISM and regional site map #####


## Load libraries
library(ggplot2)
library(sf)
library(raster)
library(tigris)
library(rnaturalearth)
library(maps)
library(prism)
library(dplyr)


## Instructions to run this script: 
## 1. Download PRISM monthly precipitation normals:
#   prism_set_dl_dir("data/prism")
#   get_prism_normals("ppt", "800m", annual = FALSE, mon = 1:12)

## 2. Download river shapefiles from Natural Earth:
#   https://www.naturalearthdata.com/downloads/
#   Place files into: data/shapefiles/


##  Set directory for PRISM data
data_dir <- "data"
prism_dir <- file.path(data_dir, "prism")           # Where PRISM rasters will be stored
shapefile_dir <- file.path(data_dir, "shapefiles") # Where river shapefiles will be stored
figures_dir <- "figures"

prism_set_dl_dir(prism_dir)



# Load PRISM monthly normals precip data ---------------------------------------------------------

# Subset PRISM ppt files for each month
decnorm <- prism_archive_subset("ppt", "monthly normals", mon = 12, resolution = "800m")
jannorm <- prism_archive_subset("ppt", "monthly normals", mon = 1, resolution = "800m")
febnorm <- prism_archive_subset("ppt", "monthly normals", mon = 2, resolution = "800m")

junenorm <- prism_archive_subset("ppt", "monthly normals", mon = 6, resolution = "800m")
julynorm <- prism_archive_subset("ppt", "monthly normals", mon = 7, resolution = "800m")
augnorm <- prism_archive_subset("ppt", "monthly normals", mon = 8, resolution = "800m")

# Convert to raster layer
decnorm_rast <- raster(pd_to_file(decnorm))
jannorm_rast <- raster(pd_to_file(jannorm))
febnorm_rast <- raster(pd_to_file(febnorm))

junenorm_rast <- raster(pd_to_file(junenorm))
julynorm_rast <- raster(pd_to_file(julynorm))
augnorm_rast <- raster(pd_to_file(augnorm))


# Function to sum rasters and calculation summer:winter ratio
addRasterCalc <- function(x, y, z) {
  return(x + y + z)
}

# Sum the rasters for total winter and summer precipitation 
winter_DJF_raster_precip_inset <- raster::overlay(decnorm_rast, jannorm_rast, febnorm_rast, fun = addRasterCalc)
summer_JJA_raster_precip_inset <- raster::overlay(junenorm_rast, julynorm_rast, augnorm_rast, fun = addRasterCalc)

# Calculate the ratio of winter to summer precipitation
precip_ratio_raster <- winter_DJF_raster_precip_inset / summer_JJF_raster_precip_inset

# Define map extent
map_extent <- extent(-118, -105, 38, 49) # Example for Rocky Mountain region
precip_ratio_crop<- crop(precip_ratio_raster, map_extent)

# Convert the raster to a data frame
precip_ratio_df <- raster::rasterToPoints(precip_ratio_crop)
precip_ratio_df <- data.frame(precip_ratio_df)  # Make sure it's a data.frame


# download/load in rnaturalearth data
# state_borders <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
# rivers <- st_read(file.path(shapefile_dir, "rivers.shp"))
# rivers_major <- st_read(file.path(shapefile_dir, "rivers_major.shp"))


p <- ggplot() +
  borders("state") + theme_classic() + geom_sf(data = rivers, color = "blue", size = 0.5) + geom_sf(data = rivers_large, color = "lightblue", size = 0.5)
# Get the state borders as an sf object
state_borders <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Coordinates for Bison Lake
bison_lake <- data.frame(
  lon = -107.3464, 
  lat = 39.7653   
)

rainbow_lake <- data.frame(
  lon = -109.50,  
  lat = 44.94     
)

duncan_lake <- data.frame(
  lon = -107.45,  
  lat = 44.65     
)

LWHP_lake <- data.frame(
  lon = -106.33,  
  lat = 41.43   
)

LOW_lake <- data.frame(
  lon = -109.89,  
  lat = 43.48     
)

beauty <- data.frame(
  lon =  -109.572523,
  lat = 44.970034
)

emerald <- data.frame(
  lon =  -106.41,
  lat = 39.15
)


ggplot() +
  geom_raster(data = precip_ratio_df, aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = state_borders, color = "white", fill = NA, size = 0.25) +
  geom_sf(data = rivers_major, color = "#333333", size = 0.7) + 
  coord_sf(xlim = c(suppfig_extent3[1], suppfig_extent3[2]), 
          ylim = c(suppfig_extent3[3], suppfig_extent3[4]), 
          expand = FALSE) +  # Crop to the extent
  theme_light() +
  theme(
    legend.position = "bottom",  # Move the legend to the bottom
    legend.direction = "horizontal",  # Set the legend to be horizontal
    legend.title = element_text(size = 12),  # Adjust title size
    legend.text = element_text(size = 10),  # Adjust legend text size
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),  # Adjust plot margins (top, right, bottom, left)
    legend.margin = margin(t = -10)  # Move legend up to reduce gap between plot and legend
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) + 
  scale_fill_gradientn(
    colors = c("white","#B3D9FF","#73B1FF", "blue", "darkblue"),
    values = scales::rescale(c(min(precip_ratio_df$layer), 
                               median(precip_ratio_df$layer), 
                               max(precip_ratio_df$layer))),
    na.value = "gray",
    guide = "colorbar", limits = c(0, 10)
  ) +  
  geom_point(data = bison_lake, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 4) + 
  geom_point(data = emerald, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 4) + 
  geom_point(data = rainbow_lake, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 4) + 
  geom_point(data = duncan_lake, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 4) + 
  geom_point(data = LWHP_lake, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 4) + 
  geom_point(data = LOW_lake, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 4) +  
  geom_point(data = beauty, aes(x = lon, y = lat), 
             shape = 2,  # Shape 17 is a filled triangle
             color = "black", size = 5) +
  geom_point(data = upper_kintla, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 5) + 
  geom_point(data = silver_lake, aes(x = lon, y = lat), 
             shape = 17,  # Shape 17 is a filled triangle
             color = "black", size = 5) 


ggsave(file.path(figures_dir,"Fig1_PRISM_ration.svg", plot=ggplot2::last_plot(), scale=1))
