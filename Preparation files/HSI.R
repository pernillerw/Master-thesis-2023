#Creating HSI for the study areas

library(raster)
library(sf)
library(raster)
#reading in hsi file for the entire Norwegian coast. This can be provided.
#The bathysmoothed raster file can be changed, based on area of interest. Using Sør-Trøndelag in this case

hsifile <- raster("/Users/pernillerubinwollan/Desktop/hsikodeee.asc")
bathysmoothed <- raster("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /ST_extremesmoothed.asc")

# Set CRS for both raster files
crs(hsifile) <- 32633
crs(bathysmoothed) <-32633

# project and crop 
hsi <- projectRaster(from = hsifile, to = bathysmoothed)
plot(hsi)

mean_hsi <- mean(hsi[],na.rm=T)

water <- bathysmoothed[] < 0
land <- bathysmoothed[] >= 0
bad_hsi <- is.na(hsi[])
hsi2 <- hsi
hsi2[water&bad_hsi] <- mean_hsi

# make sure land cells are still land cells in the HSI raster
hsi2[land] <- 2
plot(hsi2)

# Replace all occurrences of -9999 with 2
hsi2[is.na(hsi2)] <- 2

#round values in raster to 2 decimal place
round(hsi2, 2)

#generate hsi file. 
hsi_file<- writeRaster(hsi2, filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /hsi.asc", format= "ascii", overwrite=TRUE)


#maxhsi for 5km and 25km blocks used for memory procedures in NetLogo

# blockify: divides a raster object into square blocks by replacing all values with sequential integers,
# starting in the upper left corner, moving in block_size multiples of rows, and then columns. Note that
# if the nrow and/or ncol are/is not a multiple of block_size, then blocks furthest to the right and/or
# bottom will be truncated
# r = raster object
# block_size = block size in number of cells
blockify <- function(r, block_size) {
  nx <- seq(1, nrow(r), block_size) # first column number in each block
  ny <- seq(1, ncol(r), block_size) # first row number in each block
  block <- 1 # block number, incremented in each block
  
  for (x in nx) {
    for (y in ny) {
      xsec <- seq(x, min(x+block_size-1, nrow(r)))
      ysec <- seq(y, min(y+block_size-1, ncol(r)))
      r[xsec, ysec] <- block
      block <- block +1
    }
  }
  r
}

areas5km <- blockify(bathysmoothed, 5)
areas25km <- blockify(bathysmoothed, 25)

# save before aggregation, so we can use this raster to generate max hsi
writeRaster(areas25km, filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /areas25km_unaggregated.asc", format= "ascii", overwrite=TRUE)  

# aggregate and save
areas5km <- aggregate(areas5km, fact = c(5, 5), filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /areas5km_a.asc",overwrite=TRUE)
areas25km <- aggregate(areas25km, fact = c(25, 25), filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /areas25km_a.asc", overwrite=TRUE)

# getting the max HSI in each 25km block
# 
areas5km <- raster("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /areas5km_a.asc", crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
areas25km <- raster("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /areas25km_a.asc", crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
areas25km_unaggregated <- raster("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /areas25km_unaggregated.asc", crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

block_vals <- values(areas25km_unaggregated)
hsi_vals <- values(hsi2)

max_hsi_per_block <- sapply(unique(block_vals), function(block_id) {
  i <- which(block_vals == block_id)
  hsi <- hsi_vals[i]
  if (all(is.na(hsi))) {
    return(NA)
  } else {
    return(max(hsi, na.rm=T))
  }
})

max_hsi <- subs(areas25km, data.frame(1:ncell(areas25km), max_hsi_per_block), 
                             filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /maxhsi.asc", overwrite=TRUE)

