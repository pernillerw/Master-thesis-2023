

library(sf)
library(ggplot2)
library(rnaturalearth)

# Read the shapefile from the Direcotate of Fisheries
location <- st_read("/Users/pernillerubinwollan/Documents/master netlogo/R koding for netlogo/_Lokasjoner_2018/Fiskeridir_Lokasjoner fra 2018.shp")

#Set the crs
location <- st_transform(location, crs = 32633)
location[, c("center.X", "center.Y")] <- st_coordinates(st_transform(st_centroid(location), crs = 4326))
st_agr(location) <- rep("constant", times = ncol(location))

# Filter the desired fishery statistical areas (FSA), found as havomr in the location file
filtered_location <- location %>%
  filter(havomr %in% c("00", "03", "04", "05", "06", "07", "08", "09", "28", "41"))


# Keep only specific lokref values for havomr 41 because it streches more than where the
# colonies of Rogaland are observed.
lokref_to_keep_for_41 <- c("41-72", "41-73", "41-74", "41-75", "41-76", "41-93", "41-92", 
                           "41-62","41-63","41-64", "41-65", "41-66", "41-67")

filtered_location <- filtered_location %>%
  filter(!(havomr == "41" & !lokref %in% lokref_to_keep_for_41))


# Create the map of Norway 
europe <- ne_countries(scale = 50, returnclass = "sf", continent = "Europe")
norway <- ne_countries(scale = 10, returnclass = "sf", country = "Norway")

# Make the polygons valid
europe_valid <- st_make_valid(europe)

# Calculate centroid coordinates for each country
europe_centroids <- st_centroid(europe_valid) %>%
  st_coordinates() %>%
  as.data.frame()

# Rename columns
colnames(europe_centroids) <- c("x", "y")


#create the plot of Norway with neighboring countries with the FSA illustrated with different colors
norway_FSL <- ggplot() +
  geom_sf(data = europe) +
  geom_sf(data = filtered_location, aes(fill = havomr), color = "black", size = 0.5, alpha = 0.4) +
  geom_text(data = filtered_location, aes(x = center.X, y = center.Y, label = lokasjon), color = "black", fontface = "bold", size = 1.7) + 
  geom_sf(data = norway, fill = "gray70", color = "black") +
  geom_sf(data = europe %>% filter(name_long %in% c("Sweden", "Denmark", "Finland","Russia"))) +
  geom_text(data = europe_centroids, aes(x = ifelse(europe_valid$name_long == "Aland Islands", 3,
                                                    ifelse(europe_valid$name_long == "Norway", 10,
                                                           ifelse(europe_valid$name_long == "Sweden", 15,
                                                                  ifelse(europe_valid$name_long == "Denmark", 9.5, x)))),
                                         y = ifelse(europe_valid$name_long == "Aland Islands", 60,
                                                    ifelse(europe_valid$name_long == "Norway", 61,
                                                           ifelse(europe_valid$name_long == "Sweden", 60,
                                                                  ifelse(europe_valid$name_long == "Denmark", 56, y))))),
            label = ifelse(europe_valid$name_long == "Aland Islands", "",
                           ifelse(europe_valid$name_long %in% c("Norway", "Sweden", "Denmark"), 
                                  ifelse(europe_valid$name_long == "Norway", "Norway",
                                         ifelse(europe_valid$name_long == "Sweden", "Sweden", "Denmark")), 
                                  europe_valid$name_long)),
            color = "black", fontface = "bold", size = 2) +
  coord_sf(xlim = c(0, 32), ylim = c(55, 72)) +
  scale_fill_manual(values = c("03" = "blue3",
                               "04" = "steelblue4",
                               "05" = "khaki2",
                               "00" = "orange",
                               "06" = "red",
                               "07" = "magenta",
                               "28" = "darkorchid3",
                               "08" = "darkolivegreen",
                               "09" = "brown",
                               "41" = "lightgreen"),
                    breaks = c("03", "04", "05", "00", "06", "07","28","08", "09", "41"),
                    name = "FSA") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = c(0.02, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(0.5, "cm"))

print(norway_FSL)
