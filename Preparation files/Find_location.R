library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

#  The bathy file changes depending on the area of interest
#  locaiton remains the same for all areas
#  this one uses the area of Sør-Trøndelag as an example

bathy <- raster("/Users/pernillerubinwollan/Documents/master netlogo/data files master/abc/a/extreme_bathya_smooth.asc")
location <- st_read("/Users/pernillerubinwollan/Documents/master netlogo/R koding for netlogo/_Lokasjoner_2018/Fiskeridir_Lokasjoner fra 2018.shp")

# Loading the smoothed bathy file
bathy_smoothed <- raster(bathy)
  
# Fishery statistic locations. Remains the same for all areas
crs(bathy) <- 32633
location <- st_transform(location, crs = 32633)
  
# Landmass polygons. land remains the same for all areas
land <- ne_countries(country = "Norway", scale = "large", returnclass = "sf")
land <- st_transform(land, crs = 32633)
  
# Subtract land area from location polygons
location <- st_difference(location, land)
  
# Calculate area of each location cell
location$area <- st_area(location)
  
# Crop polygons overlapping extent of the simulation area
cropped_location <- st_crop(location, extent(bathy_smoothed))
cropped_location$new_area <- st_area(cropped_location)
  
# Group by "lokref" and select the first occurrence.
# This is because some of the text of lokref was duplicated.
cropped_location <- cropped_location %>%
    group_by(lokref) %>%
    slice(1) %>%
    ungroup()
  
cropped_location$label <- sprintf("%s\n%.02f", cropped_location$lokref, cropped_location$new_area / cropped_location$area)
  
ggplot() +
    geom_sf(data = cropped_location, fill = "cornsilk2") +
    geom_sf_text(data = st_centroid(cropped_location), aes(label = label), size = 3) +
    coord_sf(expand = FALSE) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.position = "none")
  
saveRDS(cropped_location, file = "/Users/pernillerubinwollan/Documents/master netlogo/R koding for netlogo/_Lokasjoner_2018/nytrondelag_a.RData")


# The process is reated for all areas, changing only the bathy path.

