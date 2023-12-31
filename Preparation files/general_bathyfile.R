#Creating bathymetry raster files

# Load emodnet data (change path to whereve you've saved C5_MSL_2020.asc). 
# You can download this file from https://portal.emodnet-bathymetry.eu/ through
# downloads -> DTM tiles -> C5 -> ESRI ASCII)

bathy_c5 <- raster("/Users/pernillerubinwollan/Documents/master netlogo/data files master/abc/a/C5_MSL_a_2020.asc", crs = "+proj=longlat +datum=WGS84")

# Now we're going to create a raster file from our study area polygon
# Rasters can't have irregular shapes, like our polygon has; they must be rectangular.
# So we'll use the extent/bbox of our study region (bbox = boundary box) #return bounding of a simple feature or set
# use the "studyarea" from the Seal colonies in Norway map (this must be read in prior to this step)
ext <- st_bbox(studyarea) 
ext

cellsize <- 1000 # cell size in meters.

# Calculate the number of rows and columns that go into our raster
ncols <- ceiling((aext[3] - aext[1]) / cellsize)
nrows <- ceiling((aext[4] - aext[2]) / cellsize)

# adjust max X and Y coordinates to allow all rows/columns to fit.
ext[3] <- ext[1] + ncols * cellsize
ext[4] <- ext[2] + nrows * cellsize

raster <- raster(nrows = nrows, ncols = ncols, xmn = ext[1], xmx = ext[3], ymn = ext[2], ymx = ext[4], crs = 32633)

# Save raster for Sør-Trøndelag in this case. The raster depends on the studyarea which is found in the seal colonies in Norway script
bathy <- projectRaster(bathy_c5, to = raster, crs = 32633,
                           filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /bathy_ST_testnew.asc", overwrite=TRUE)

bathy[is.na(bathy[])]<-0

# Have a look.
plot(bathy)

#write the corrected bathy file after removing NAs
bathynew <- bathy
writeRaster(bathynew, filename = "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /bathy_ST_new.asc", overwrite=TRUE)

#this next part are using the original bathyfile to aggregate and smooth the map so that seals can move free
#and avoiding land. 

#function used to transform the coordinates to cells: use click() to find the cells to change in the map
#cellFromXY( bathy file, click(x,y)) 
#some cells had to be generated as land, that were water originally, and land became water. 
#The points were chosen by where the seals have their colonies. 

#changing cells from water cells to land by setting the values to 0
bathynew <- raster("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /bathy_ST_new.asc")
plot(bathynew)

# aggreggate to see which cells to change. 
# #The chaning cells will of course vary depending on the studyarea. This is just a base script for how to generate bathymetry raster
bathy_smoothed<- aggregate(bathynew, fact = c(2, 2), fun = mean)
plot(bathy_smoothed<0)

cells_to_land <- c(3984,4860,4861,4971,5522,5631,5411,5412,5519:5523,6407:6410,6517,6518,6520,6629,7036,7037,
                   7148,7149,7258,7405,7417,7471:7474,7476,7846,8923:8932,9031:9050,9139:9160,9252:9269,9251,9364,9363,9371:9379,9488:9490,9599:9603,
                   9710:9719,9820:9828,9929:9934,10042,10043,10152,10153,10900:10920,10790:10803, 11012,11026,11027,11028,11138,11139,11012:11020,11237:11242,11126,11127,11130, 11131,11349:11353,
                   11249,11670:11692,11795:11804,11783:11788,11460:11464,11572:11575,11348,11789,11571,11455,11459,11559:11565,9630,
                   8908:8914,9019:9023,9131:9139,9024,9025,9354:9363,9242:9250,9707:9720,9683:9695,9465:9472,
                   9574:9583,9903:9914,9792:9806,10019:10026,10238:10243,10125:10139,9915,9916,10027,10014:10019,10237,8040,8150,8175,8368,8375,8509,8620,8644,8755,8756,8819:8822,8977,8978,
                   9161:9164,9270:9273,9365,9475,9476,9482,9530:9533,9571,9587,9588,9640:9643,9699,9751,9808:9812,9862,9917,9918,
                   10249,10290,10291,10350,10351,10353,10357,10359:10361,10462,10464,10465,10467,10471,10472,10573,10576,10583,10684,10806,10688,10694:10704,10804,10805,11549,11550,11582,11782,11803,11804,11907:11915,11662,11663,11668:11670,11773:11775,11777:11779,11885:11889,11893:11897,
                   11900,11901,11995:11999,12003,12004,12012:12014,12018:12026,12113,12123,12124,12128:12132,12234:12239,12240:12245,12345:12347,
                   12351:12357,12456:12458,12462:12470,12544,12567:12569,12572,12574:12583)

bathy_smoothed[cells_to_land] <- 0

# highlight changed cells
plot(bathy_smoothed<0)

cells_to_water <- c(102:104,212:215,328,330,437:441,545:549,551:560,656:666,764:769,770,774:776,882:889,876:880,
                    987:990,994:999,1106,1107,1221,1775,2109,2218:2220,2326:2331,2436,2437,2442,2546:2551,2553,
                    2652,2655,2656,2767:2769,2772,2985,2878,2879,2882:2884,2989,2990,2994,2995, 3100,3101,3103,3104,3105,3107,3108,3218,3219,
                    3328,2326,3426,3427,3428,3535,3537,3550,3643,3644,3648,3660:3663,3758:3760,3771:3773,
                    3882,3883,3884,3869,3870,3977,3980,3993:3994,4086,4089,4104,4197,4198,4527,4637,4638,4967,
                    5299, 5518,5627,5738,5772,5847,5881:5885,5958,5991:5994,6105,6183,6548,6549,6581,6587,6588,6657:6662,
                    6733,6734,6767:6771,6841,6877:6882,6951:6953,6871,6980:6983,6986:6990,6993,7055,7090:7095,7097:7102,7104,7152,7165,
                    7198:7206,7208,7209,7214,7215,7308:7318,7484,7650,7699,7769,7770,7823,7934,7876:7882,7933:7935,7942,8044:8047,
                    8093,8155:8157,8253,8254,8282,8390:8394,8491,8531,8580,8641:8643,8751:8754,8826,8828,8839,8935:8939,
                    9515,9516,7925:7928, 7280:7292,7507:7512,7393:7403,11434:11447,11335,11224,11225,11331,11332,11109,11117:11119,11220,11221,11219,11339,
                    11007,11228:11231,11003,11001,11338,11227,10896,10786,10900,10567,11116,11008,10783,10457,
                    7725:7730,7836,7837,7616:7621,7173:7181,7067:7069,6955:6958,7065,
                    7066,6845:6847,10900,9074:9080,8036:8041,8144,8145,8600,8601,8822,8377,8378,8489
)

bathy_smoothed[cells_to_water] <- -25
plot(bathy_smoothed<0)

# disaggreger before saving raster asc file
bathysmoothed <- disaggregate(bathy_smoothed, fact = c(2,2))
writeRaster(bathysmoothed, filename =  "/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /ST_extremesmoothed.asc", overwrite=TRUE)

#reading in the bathysmoothed file
smoothed_area <- raster("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /ST_extremesmoothed.asc", crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs", overwrite=TRUE)

#using the seal colonies (must be read in prior to this step), and the smoothed bathy file to set the colonies on the map. 
distances <- vector(mode = "list", length = nrow(colonies))

for (i in 1:nrow(colonies)) {
  newacellnum <- cellFromXY(smoothed_area, st_coordinates(colonies[i,]))
  distances[[i]] <- raster(smoothed_area)
  distances[[i]][newacellnum] <- 1
  distances[[i]] <- round(distance(distances[[i]]))
  
  landpatches = which(smoothed_area[] == 0) #getting land patches from bathy raster 
  distances[[i]][landpatches] <- 0 #setter verdier paa land til 0
  writeRaster(distances[[i]], crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs", overwrite= TRUE,
              filename= sprintf("/Users/pernillerubinwollan/Documents/master netlogo/R-scripts /smoothed_HOdistance_id%d.asc", i))
  
}

