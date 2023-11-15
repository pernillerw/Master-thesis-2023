#creating results file based on the three replicates for each area.

library(ggplot2)
library(sf)
library(rnaturalearth)
library(data.table)
library(dplyr)

#read in a results file from all areas. This was done in excel after all simulations was runned.
results <- fread("/Users/pernillerubinwollan/Documents/master netlogo/Results abc/all_areas_season.csv")
lowest_value <- min(results$season_interaction)

# Find the highest value
highest_value <- max(results$season_interaction)

# Create a function to map values to categories
assign_category <- function(value) {
  if (value <= lowest_value) {
    return(1)  # Very Low category
  } else if (value <= 0.001) {
    return(2)  # Low category
  } else if (value <= 0.01) {
    return(3)  # Medium 1 category
  } else if (value <= 0.1) {
    return(4)  # Medium 2 category
  } else if (value <= 0.25) {
    return(5)  # High category
  } else {
    return(6)  # Very High (values above 0.2)
  }
}

# Apply the function to create the category column in the results data frame
results$category <- sapply(results$season_interaction, assign_category)

results$fseason <- factor(results$season, levels = c("A", "Wi", "Sp", "Su"))

results <- results %>%
left_join(location %>% select(lokref, geometry), by = "lokref")
results$geometry <- st_as_text(results$geometry)
results <- st_as_sf(results, wkt = "geometry")

st_crs(results) <- 32633
head(results)

# Read the shapefile
location <- st_read("/Users/pernillerubinwollan/Documents/master netlogo/R koding for netlogo/_Lokasjoner_2018/Fiskeridir_Lokasjoner fra 2018.shp")
location <- st_transform(location, crs = 32633)
location[, c("center.X", "center.Y")] <- st_coordinates(st_transform(st_centroid(location), crs = 4326))
st_agr(location) <- rep("constant", times = ncol(location))

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

# Create a dataset for labels to set the land polygons on the right place
label_data <- data.frame(
  x = c(9, 15, 9, 25, 25.5, 25.5, 24), # x-coordinates for the selected countries
  y = c(60.5, 60, 56, 63, 59, 57, 55.5), # y-coordinates for the selected countries
  label = c("Norway", "Sweden", "Denmark", "Finland", "Estonia", "Latvia", "Lithuania")
)

# Define the color palette
color_palette <- c(
  "1" = "lightblue",
  "2" = "darkseagreen1",
  "3" = "burlywood1",
  "4" = "darkorange",
  "5" = "red",
  "6" = "black"
)

ggplot() +
  facet_wrap(~fseason) +
  geom_sf(data = europe) +
  geom_sf(data = results, aes(fill = as.factor(category)), color = "black", size = 0.5) +
  geom_sf(data = norway, fill = "gray70", color = "black") +
  geom_sf(data = europe %>% filter(name_long %in% c("Sweden", "Denmark", "Finland","Russia"))) +
  geom_text(data = label_data, aes(x = x, y = y, label = label), fontface = "bold", size = 1.9, color = "black", nudge_x = 0.2) + # Adjust nudge_x to fine-tune label placement
  coord_sf(xlim = c(0, 32), ylim = c(55, 72)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_manual(
    values = color_palette,
    breaks = c("1", "2", "3", "4", "5", "6"),
    labels = c("1 Very Low (0)", "2 Low (0-0.001)", "3 Medium (0.001-0.01)", "4 Medium (0-01-0.1)", "5 High (0.1-0.25)", "6 Very High (>0.25)"),
    name = "Interaction risk category"
  ) +
  guides(fill = guide_legend(key_height = 1, key_width = 1)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.key.height = unit(0.4, "cm"),  # Adjust the height of legend keys
        legend.key.width = unit(0.4, "cm")) + # Adjust the width of legend keys
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

#finding relative use for Norway. the categorised are based on lowest and highest value with reasonable choices of categories. 
lowest_value <- min(results$season_rel_use)
# Find the highest value
highest_value <- max(results$season_rel_use)

# Define custom category labels
custom_labels <- c( "1", "2", "3", "4", "5", "6")

# Create a function to assign custom categories based on values
assign_use_category <- function(value) {
  if (value == 0) {
    return(custom_labels[1])
  } else if (value <= 0.00009712) {
    return(custom_labels[2])
  } else if (value <= 0.00927616) {
    return(custom_labels[3])
  } else if (value <= 0.02619928) {
    return(custom_labels[4])
  } else if (value <= 0.099502469) {
    return(custom_labels[5])
  } else {
    return(custom_labels[6])
  }
}

#Create a color palette for the custom categories
custom_color_palette <- c("lightblue", "darkseagreen1", "burlywood1", "darkorange", "red", "black")

# Apply the function to create the category column in the results data frame
results$usecategory <- sapply(results$season_rel_use, assign_use_category)

useplot <- ggplot() +
  facet_wrap(~fseason) +
  geom_sf(data = europe) +
  geom_sf(data = results, aes(fill = usecategory), color = "black", size = 0.5) +
  geom_sf(data = norway, fill = "gray70", color = "black") +
  geom_sf(data = europe %>% filter(name_long %in% c("Sweden", "Denmark", "Finland","Russia"))) +
  geom_text(data = label_data, aes(x = x, y = y, label = label), fontface = "bold", size = 1.9, color = "black", nudge_x = 0.2) +
  coord_sf(xlim = c(0, 32), ylim = c(55, 72)) +
  ylab("Latitude") +  # Set y-axis label
  scale_fill_manual(
    values = custom_color_palette,
    breaks = custom_labels,
    labels = c( "1 (0)", "2 (0.00009712)","3 (0.00927616)", "4 (0.02619928)", "5 (0.099502469)", "6 (>0.099502469)"),
    name = "Preluse(Seals)") +
  guides(fill = guide_legend(key_height = 1, key_width = 2)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.7))

useplot

#catch plot for whole area with categories based on highest and lowest value like the use plot
lowest_value <- min(results$season_rel_catch)
# Find the highest value
highest_value <- max(results$season_rel_catch)

# Define custom category labels
custom_labels <- c( "1", "2", "3", "4", "5", "6")

# Create a function to assign custom categories based on values
assign_custom_category <- function(value) {
  if (value <= 0.000110527) {
    return(custom_labels[1])
  } else if (value <= 0.000494845) {
    return(custom_labels[2])
  } else if (value <= 0.006645336) {
    return(custom_labels[3])
  } else if (value <= 0.018078847) {
    return(custom_labels[4])
  } else if (value <= 0.046268839) {
    return(custom_labels[5])
  } else {
    return(custom_labels[6])
  }
}

# Apply the function to create the category column in the results data frame
results$catchcategory <- sapply(results$season_rel_catch, assign_custom_category)

results$fseason <- factor(results$season, levels = c("A", "Wi", "Sp", "Su"))

# Create a color palette for the custom categories
custom_color_palette <- c("lightblue", "darkseagreen1", "burlywood1", "darkorange", "red", "black")

catchplot<- ggplot() +
  facet_wrap(~fseason) +
  geom_sf(data = europe) +
  geom_sf(data = results, aes(fill = catchcategory), color = "black", size = 0.5) +
  geom_sf(data = norway, fill = "gray70", color = "black") +
  geom_sf(data = europe %>% filter(name_long %in% c("Sweden", "Denmark", "Finland","Russia"))) +
  geom_text(data = label_data, aes(x = x, y = y, label = label), fontface = "bold", size = 1.9, color = "black", nudge_x = 0.2) +
  coord_sf(xlim = c(0, 32), ylim = c(55, 72)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_manual(
    values = custom_color_palette,
    breaks = custom_labels,
    labels = c("1 (0.000110527)", "2 (0.000494845)","3 (0.006645336)", "4 (0.018078847)", "5 (0.046268839)", "6 (>0.046268839)"),
    name = "Prelcatch(Fisheries)")+
  guides(fill = guide_legend(key_height = 1, key_width = 1)) +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))


catchplot


#reading in all seasons csv files from the output-result script to zoom in on specific areas. Change the pathway to your computer 
#Sør-Trøndelag is used as a example here. But the code works for other areas as well. 
sim1season <- fread("/Users/pernillerubinwollan/Documents/master netlogo/Results abc/a/extreme/ST_seasons_sim1.csv", header=TRUE)
sim2season <- fread("/Users/pernillerubinwollan/Documents/master netlogo/Results abc/a/extreme/ST_seasons_sim2.csv", header=TRUE)
sim3season <- fread("/Users/pernillerubinwollan/Documents/master netlogo/Results abc/a/extreme/ST_seasons_sim3.csv", header=TRUE)

#combining the dataset to a whole sheet and find the mean value of relative use, catch and interaction from all replicates
all_data <- rbind(sim1season, sim2season, sim3season)
mean_values <- aggregate(cbind(season_rel_use, season_rel_catch, season_interaction) ~ lokref + season, data = all_data, mean)

#set disired order for the seasons
desired_order <- c("A", "Wi", "Sp", "Su")

# Use the factor function to specify the desired order in the mean values data
mean_values$season <- factor(mean_values$season, levels = desired_order)

# Arrange the data frame based on the 'season' column
mean_values_season <- mean_values %>% arrange(lokref,season)

#change the file path to your computer
file_path <- "/Users/pernillerubinwollan/Documents/master netlogo/Results abc/a/extreme/nyST_extreme_result_season.csv"

#Save the mean_values data frame from the combined dataset
write.table(mean_values, file = file_path, sep = ";", row.names = FALSE)

#getting the dataset (change the pathway to your directory)
seasons <- fread("/Users/pernillerubinwollan/Documents/master netlogo/Results abc/a/extreme/nyST_extreme_result_season.csv")

#need to get the geometry again to plot the results, using the location from the find location script
seasons <- seasons %>%
  left_join(location %>% select(lokref, geometry), by = "lokref")

seasons$geometry <- st_as_text(seasons$geometry)
seasons <- st_as_sf(seasons, wkt = "geometry")

st_crs(seasons) <- 32633

#finding the highest value from the interaction column
highest_value <- max(seasons$season_interaction)
lowest_value <- 0
#make a interaction plot, by using semi-logaritmic scale to set the categories

# Apply the function to create the category column in the results data frame
seasons$category <- sapply(seasons$season_interaction, assign_category)

color_palette <- c(
  "1" = "lightblue",
  "2" = "darkseagreen1",
  "3" = "burlywood1",
  "4" = "darkorange",
  "5" = "red",
  "6" = "black"
)

seasons$fseason <- factor(seasons$season, levels = c("A", "Wi", "Sp", "Su"))

# Create the interaction plot for the zoomed in area of Sør-Trøndelag. The # in the plot are removing the lokref text, so it can eighter be used or removed.
plotseason <- ggplot() +
  geom_sf(data = seasons, aes(fill = factor(category))) + # Fill based on 'category'
  geom_sf(data = st_crop(land, seasons), fill="gray70") + # Use category for color
  #geom_sf_text(data = subset(st_centroid(seasons), lokref == "07-25" & season == "A"), aes(label = lokref), size = 1.8, color = "white") +
  #geom_sf_text(data = subset(st_centroid(seasons), !(lokref == "07-25" & season == "A")), aes(label = lokref), size = 1.8, color = "black") +
  coord_sf(expand = FALSE) +
  scale_fill_manual(
    values = color_palette,
    breaks = c("1", "2", "3", "4", "5", "6"),
    labels = c("1 Very Low (0)", "2 Low (0-0.001)", "3 Medium (0.001-0.01)", "4 Medium (0.01-0.1)", "5 High (0.1-0.25)", "6 Very High(>0.25)"),
    name = "Interaction\nrisk category"
  ) +
  guides(fill = guide_legend(key_height = 1, key_width = 1, title.vjust = 0.9)) + # Adjust title.vjust to move the title above the keys
  theme(legend.position = "bottom",
        legend.box = "vertical") + # Move legend title above the labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.key.height = unit(0.4, "cm"),  # Adjust the height of legend keys
        legend.key.width = unit(0.4, "cm")) + # Adjust the width of legend keys
  xlab("Longitude") +
  ylab("Latitude") +
  facet_wrap(~fseason) +
  guides(fill = guide_legend(override.aes = list(size = 2))) # Adjust the legend title

# Print the plot
print(plotseason)

