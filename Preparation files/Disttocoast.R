
#calculating the distance to coast based on the smoothed map

#read in the smoothed asc bathy file
bathysmoothed <- raster("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /ST_extremesmoothed.asc")

# invert NAs and non-NAs
# reclassify values of a raster object, reclassifies groups of values to other values
# eg. all values between 1 and 10 become 1, and 11 to 15 becomes 2.
# changing max depth in the matrise to -10000 so i dont have to change for every new area when creating the other rasterfiles. 

bathysmoothed_m <- matrix(c(-10000, -0.0000001, NA, 
                     NA, NA, 0), ncol = 3, byrow=TRUE)
bathysmoothed_water <- reclassify(bathysmoothed, bathysmoothed_m, right = FALSE, filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /disttocoast.asc", overwrite=TRUE)

# distance: for a single raster layer (y is missing) this method computes
# the distance for all cells that are NA to the nearest cell that is not a NA, distance is in meters. 
disttocoast <- distance(bathysmoothed_water)
disttocoast <- round(disttocoast,2)
#writing the distancetocoast file
writeRaster(disttocoast, filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /dist2coast_a.asc",overwrite=TRUE)
plot(disttocoast)
#plot all colonies on top of the raster
plot(colonies, add = TRUE, pch = 16, col = "red")
