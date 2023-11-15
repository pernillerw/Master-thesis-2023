#file from output results from the area of Søt-Trøndelag after running the simulation for mounth in NETLOGO.
#The basic script can be used for other areas as well, by changing the path of moves

library(data.table)
library(ggplot2)
library(dplyr)
library(sf)

#read in the output file of the seals movement and specify which columns to use
moves <- read.csv("/Users/pernillerubinwollan/Documents/master netlogo/AgentSeal -master mine koder/Output/Movement_extremesmooth1_simulation.csv", header=FALSE)
colnames(moves) <- c("expindex", "ticks", "who", "dayNumber", "currenttime", "activity", "fishConsumed_g", "TotalfishConsumed_g", "daily_ee", "daily_ei", "dailyNetEnergy", "DaysWithNegativeDNE",
                      "typeOfHa", "fishConsumed_g_longResting", "distHaulOutDigestion", "Blength", "Tmass", "LBM", "ResMass", "sex", "xcor", "ycor", "[km_25_id] of patch-here", "[km_5_id] of patch-here",
                      "foraging_trip#", "durationOfresting", "previous_ho")
moves <- moves[,c("who", "ticks", "dayNumber", "activity", "sex", "xcor", "ycor")]
# Assuming the `moves_a` data frame is already loaded and contains the data

# Calculate the month column based on dayNumber. Making sure that the start date is the same as when the simulation started which was in october=month 10.
moves$month <- ceiling((moves$dayNumber + 1) / 30) + 9
moves$month[moves$month > 12] <- moves$month[moves$month > 12] - 12

# Adjust month 10 to be month 9 for dayNumber 360
moves$month[moves$dayNumber == 360] <- 9

# This next step display the result from the activity sensitivity anaysis to see if BURN-IN time mattered (how long it took for seal to distribute naturally)
#Remove all data corresponding to dayNumber 0 to 4
moves <- moves[moves$dayNumber > 5, ]

# Remove rows corresponding to months 7 and 8 (July and August) as it corresponds to breeding season
# take out the code to add the months
moves <- moves[!(moves$month %in% c(7, 8)), ]

#reading in the location RDS file find from the "find location" script 
location <- readRDS("/Users/pernillerubinwollan/Documents/master netlogo/R koding for netlogo/_Lokasjoner_2018/trondelag_a.RData")
bathy <- raster("/Users/pernillerubinwollan/Documents/master netlogo/data files master/abc/a/extreme_bathya_smooth.asc")
bathy_extent <- extent(bathy)

location$prop <- as.numeric(gsub("\\[|\\]", "", location$prop))

# convert from map coordinates to utm33 coordinates
moves$x <- bathy_extent[1] + (bathy_extent[2]-bathy_extent[1]) * (moves$xcor-1)/ncol(bathy)
moves$y <- bathy_extent[3] + (bathy_extent[4]-bathy_extent[3]) * (moves$ycor-1)/nrow(bathy)


# convert to spatial object
moves2 <- st_as_sf(moves, coords = c("x", "y"), crs = 32633)

#change only the bathy.sf$your bathy file path>= in this section
bathy.sf <- rasterToPolygons(bathy)
bathy.sf <- st_as_sf(bathy.sf, crs = 32633)
land <- bathy.sf[bathy.sf$extreme_bathya_smooth.asc>=0,]
st_crs(land) <- 32633
land <- st_union(land)
st_crs(location) <- 32633

# take a sample, where size indicate how many seals movement to look at, here is 5 seals visualized
sampled_seals <- moves2[moves2$who %in% sample(unique(moves2$who), size = 5),]

# Create a plot using the subsetted data to plot 5 seals only. Change the size number in the code above to view fewer or more seals
# the seal colony file has to be read in if the geom_sf(data =  colonies, color = "black" are suppose to work. It can also be removed bysetting # before the code
movewho <- ggplot() + 
  geom_sf(data = location, fill = "lightblue") +
  geom_sf(data = land, fill = "cornsilk2") +
  geom_sf_text(data = st_centroid(location), aes(label = lokref)) +
  geom_sf(data = sampled_seals, pch = 19, size = 0.5, aes(col = factor(who))) +
  geom_sf(data =  colonies, color = "black") +
  coord_sf(expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = "none")

# Display the plot
movewho

# use the seal coordinates to find out which location cell they're in
cells <- st_intersects(moves2, location, sparse = FALSE)
cells_lokref_ok <- apply(cells, 1, any)
moves2$lokref <- NA_character_

#for cells that are in the water, just assign the lokref to them directly based on their coordinates
moves2$lokref[cells_lokref_ok] <- apply(cells[cells_lokref_ok,], 1, function(x) location$lokref[which(x)[1]])

#because the map was smoothed a lot, some cells may occasionally swim over areas that are on land and therefore not have an 
#associated location cell. For this location use nearest location cell by geodesic distance
#(since we cant use their coordinates directly)
moves2$lokref[cells_lokref_ok == FALSE] <- location$lokref[st_nearest_feature(moves2[cells_lokref_ok==FALSE,],location)]
#too check that it works
nrow(moves2)
table(is.na(moves2$lokref))

#next line probably unnecessary,but keeo it here to be safe
moves2 <- moves2[complete.cases(moves2$lokref), ]

#next section aims to calculate the relative use of seals in each lokref cell
seal_use <- st_drop_geometry(moves2)
setDT(seal_use) #setting to data table
#want to sum up how many timesteps the seals has spent time in each lokref cell.
sealuse <- seal_use[activity != "HO", .N, keyby = .(lokref, who)]
sealuse[, rel_use := N/sum(N)] 
options(scipen=10000) #how much time they use in each lokrefcell in %.
sealuse

#further was fishery data of how much catch there is in each lokref cell added. Files can be retrieved from the Directorate of Fisheries
#this next step are presenting the fishery data and output of results: rel_use+ineraction of catches.

#read in file. Here is the dataset of 2019 used as it corresponded to the last harbour seal count. This may vary depending on area of interest.
fisheryfile <- fread("/Users/pernillerubinwollan/Documents/fangstdata_2019.csv",  sep=";", header=T) 

#trying to only add Sør-Trøndelag data. 
#This might also vary depending on area of interest, but it just to add in the right county in Landingsfylke ==" ".
fisheryarea <-fisheryfile [fisheryfile $Landingsfylke == "Trøndelag", ] 

#making the dataset smaller by including only relevant information
fishery <- fisheryarea[,c("Dokument salgsdato", "Landingsmåned (kode)", "Fangstår", "Fartøynavn", 
                  "Redskap", "Hovedområde (kode)", 
                  "Lokasjon (kode)", "Art", "Bruttovekt")] 

#setting them to english names and focus on "settegarn" as seals are caught in gillnets
names(fishery) <- c("Docid", "Month", "Year", "Vesselname", "Equipment", "Area", "Location", "Species", "Catch")
fishery<-filter(fishery, Equipment %in%  'Settegarn')

#summing up all catches based on month, year and equipment used for the various lokref cells
fishery[, Catch := as.numeric(Catch)]
fiskerydata <- fishery[, .(Catch = sum(Catch, na.rm=T)), .(Month, Year, Equipment, Area, Location, Species)]
fiskerydata[, lokref := sprintf("%02s-%02s", Area, Location)]
fiskerydata <- fiskerydata[fiskerydata$lokref %in% location$lokref,]
fiskerydata$prop <- location$prop[match(fiskerydata$lokref, location$lokref)]

#sum up catches from the fisherydata file based on the proportion of the location cell from the location file (from the Find location script)
fisherycatch <- fiskerydata[, .(Catch = sum(Catch) * unique(prop)), lokref]

# Create a data table with all unique lokref values from fisherycatch
all_lokref <- unique(fisherycatch$lokref)
all_lokrefcells <- data.table(lokref = all_lokref)

# Left join all_lokrefcells with fisherycatch
all_lokrefcells <- merge(all_lokrefcells, fisherycatch, by = "lokref", all.x = TRUE)

# Left join with sealuse based on lokref 
all_lokrefcells<- merge(all_lokrefcells, sealuse, by = "lokref", all.x = TRUE, allow.cartesian = TRUE)

# Replace NA values with 0 in all columns
for (col in names(all_lokrefcells)) {
  set(all_lokrefcells, which(is.na(all_lokrefcells[[col]])), col, 0)
}

# Round the Catch column to 2 decimal places
all_lokrefcells[, Catch := round(Catch, 2)]

#check that the lokref cells matches the lokref of the location data
location$catch <- 0
location$catch <- all_lokrefcells$Catch[match(location$lokref, all_lokrefcells$lokref)]

# calculate total use and catch in each location cell
tot <- all_lokrefcells[, .(catch = Catch,
                 use = sum(N, na.rm=TRUE)), lokref]
tot <- unique(all_lokrefcells[, .(catch = Catch,
                        use = sum(N, na.rm=TRUE)), by = lokref])

location$use <- tot$use[match(location$lokref, tot$lokref)]
location$catch[is.na(location$catch)] <- 0
location$use[is.na(location$use)] <- 0
location$rel_use <- location$use / sum(location$use)
location$rel_catch <- location$catch / sum(location$catch)
#finding the interaction between seals and fisheries
location$interaction <- (location$rel_use * location$rel_catch) / sum(location$rel_use*location$rel_catch) #equation from Jonas thesis
location[, c("lokref", "rel_use", "rel_catch", "interaction")]

location <- as.data.table(location)

# Remove the "geometry" column
location[, geometry := NULL]
# Now the "location" dataset will only contain the columns you want to keep
# Save the data as a CSV file with the specified path and filename
fwrite(location, file = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /simulation1_Wholeyear.csv")

#DOING IT FOR MONTHS
#read in all above until reaching SetDT(sealuse) then proceed from here:

location1 <- readRDS("/Users/pernillerubinwollan/Documents/master netlogo/R koding for netlogo/_Lokasjoner_2018/trondelag_a.RData")
location1$prop <- as.numeric(gsub("\\[|\\]", "", location1$prop))

st_crs(location1) <- 32633

# exact same method as earlier in the "cells" coding above, but necessary to get the aquired results
cells1 <- st_intersects(moves2, location1, sparse = FALSE)
cells1_lokref_ok <- apply(cells1, 1, any)
moves2$lokref <- NA_character_

moves2$lokref[cells1_lokref_ok] <- apply(cells1[cells1_lokref_ok,], 1, function(x) location1$lokref[which(x)[1]])

moves2$lokref[cells1_lokref_ok == FALSE] <- location1$lokref[st_nearest_feature(moves2[cells1_lokref_ok==FALSE,],location1)]
#too check that it works
nrow(moves2)
table(is.na(moves2$lokref))


seal_use1 <- st_drop_geometry(moves2)
setDT(seal_use1) #setting to data table

sealuse1 <- seal_use1[activity != "HO", .N, keyby = .(lokref, month)]
sealuse1[, rel_use := N/sum(N)] # equation 2.2 from Jonas sin oppgave
options(scipen=10000) #how much time they use in each lokrefcell in %.
sealuse1

colnames(sealuse1) <- c("lokref", "Month", "N", "rel_use")

fisherycatch1 <- fiskerydata[, .(Catch = sum(Catch)*unique(prop)), by = c("lokref", "Month")]

# Create a data table with all unique lokref values from a_datacatch2
all_lokrefs_month <- unique(fisherycatch1$lokref)
month <- data.table(lokref = all_lokrefs_month)


# Left join rel_a_month with fisherycatch1 based on both "lokref" and "Month"
month <- merge(month, fisherycatch1, by = "lokref", all.x = TRUE)

month <- merge(month, sealuse1, by = c("lokref", "Month"), all.x = TRUE, all.y = TRUE, allow.cartesian = TRUE)

# Replace NA values with 0 in all columns
for (col in names(month)) {
  set(month, which(is.na(month[[col]])), col, 0)
}

# Step 2: Merge the month_combinations data frame with location1
location2 <- merge(month, location1, by = "lokref", all.x = TRUE)

location2 <- st_as_sf(location2)
# calculate total use and catch in each location cell
tot2 <- month[, .(use = sum(N, na.rm = TRUE)), by = .(lokref, Month)]

location2 <- merge(location2, tot2, by = c("lokref", "Month"), all.x = TRUE,allow.cartesian = TRUE)
location2$Catch[is.na(location2$Catch)] <- 0
location2$use[is.na(location2$use)] <- 0
location2$rel_use <- location2$use / sum(location2$use)
location2$rel_catch <- location2$Catch / sum(location2$Catch)

location2$interaction <- (location2$rel_use * location2$rel_catch) / sum(location2$rel_use*location2$rel_catch) #equation from Jonas thesis
location2 <- location2[, c("lokref", "Month", "Catch", "use", "rel_use", "rel_catch", "interaction")]

location2 <- as.data.table(location2)
# Remove rows with months 7 and 8
location2 <- location2 %>%
  filter(Month != 7 & Month != 8)

# Remove the "geometry" column
location2[, geometry := NULL]

#write the montly csv file.
fwrite(location2, file = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /Month_sim1.csv")


#TRYING TO ADD IN SEASONS BASED ON MONTH

# Merge the summarized data with the original dataset based on season and lokref
# Define the seasons
season_ranges <- list(
  Winter = c(12, 1, 2),
  Spring = c(3, 4, 5),
  Summer = c(6),
  Autumn = c(9, 10, 11)
)

# Summarize the data by season and lokref
season_summary <- location2 %>%
  mutate(season = case_when(
    Month %in% season_ranges$Winter ~ "Wi",
    Month %in% season_ranges$Spring ~ "Sp",
    Month %in% season_ranges$Summer ~ "Su",
    Month %in% season_ranges$Autumn ~ "A"
  )) %>%
  group_by(season, lokref) %>%
  summarize(
    season_rel_use = sum(rel_use),
    season_rel_catch = sum(rel_catch),
    season_interaction = sum(interaction),
    season_catch = sum(Catch)
  )

# Define the order of seasons
desired_season_order <- c("A", "Wi", "Sp", "Su")

# Reorder the levels of the season column
season_summary$season <- factor(season_summary$season, levels = desired_season_order)

# Arrange the rows first by lokref and then by season
arranged_summary <- season_summary %>%
  arrange(lokref, season)

print(arranged_summary)

# Write the arranged_summary data to a CSV file
fwrite(arranged_summary, file = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /Seasons_sim1.csv")
