

#map of norway with colonies

#library(ggplot2)
library(rnaturalearth)
library(sf)

# Fetch map data for Norway
norway <- ne_countries(scale = 10, country = "Norway", returnclass = "sf")

# Read the CSV file of all seal colonies along the Norwegian coast (can be provided)
selkolonier <- fread("/Users/pernillerubinwollan/Desktop/steinkobbe_2016_2022.csv")

# Plot the map with all colony points
colony_map <- ggplot() +
  geom_sf(data = norway, fill = "gray70", color = "black", size = 0.8) +
  geom_point(data = selkolonier, aes(x = lon, y = lat), color = "red", size = 0.5) +
  xlim(0, 32) +
  ylim(57, 72) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

colony_map


# the original seal colony file was later been manipulated in Excel to only include the areas of interest

#This next section uses the colonies of Sør-Trøndelag as an example. 
#The same process can be used for all areas, changing the colony directory
colonies <- fread("/Users/pernillerubinwollan/Documents/master netlogo/data files master/abc/a/a_selkoloni.csv")

#set colonies as data.table
colonies <- as.data.table(colonies)


#find maximum count for the area. These will be used in the analyses.
colonies[, max_count := max(count1, count2, na.rm=T)]

#set as sf, and transform to the right crs
colonies <- st_as_sf(colonies, coords = c("lon", "lat"), crs = 4326)
colonies <- st_transform(colonies, crs = 32633) # transform to utm33

#combine geometries of all features
studyarea <- st_convex_hull(st_combine(colonies))

#create a buffer around the geometry
studyarea <- st_buffer(studyarea, dist = 100000) # distance in meters

#read in European coastline, and set it to fit Norwegian waters
norway <- st_read("/Users/pernillerubinwollan/Documents/master netlogo/data files master/Europe_coastline_shapefile/Europe_coastline_poly.shp")
studyarea <- st_transform(studyarea, crs = 32633)

norway <- st_transform(norway, crs = 32633)
# the coastline data cover all of europe, but we don't really need that, so let's
# crop the data to only cover our study area
norway <- st_crop(norway, st_bbox(studyarea))

#plot the study area with colonies
ggplot() +
  geom_sf(data = norway, fill = "gray70") +
  geom_sf(data = colonies, color = "red") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.grid = element_blank(), plot.background = element_rect(fill = "white")) +
  coord_sf()


