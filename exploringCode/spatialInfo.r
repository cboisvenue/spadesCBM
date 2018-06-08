#----------------------------------------------
# Figure out the spatial info
# starting with the info from the init event in 
# the spadesCBMinputs module
#
# CBoisvenue April 9th, 2018
#----------------------------------------------

# the only data that truly needs to be spatial are the ages and the distrubance events. All other
# info can be calculated or linked back to where the pixels are. This only works in Canada

# Notes on where/how each of these needs to be calculated.

#### AGES####
# from a raster 
# sim$ages <- c(0)#,2,3,140) # this will come from a raster
# TASK: READ-IN RASTER
# this should be age in 1984:

library(raster)
age <- raster("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/age1_recliner.tif")
#IE: I will use the recliner files because they are smaller and easier to work with.
casfri <- raster("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/age1CASFRI.tif")
#CB: is this the same? 
#IE: age1 is 3 years younger than age1casfri, almost everywhere. Likely different years from CASFRI?



## TASK1: CALCULATE THESE FROM THE NUMBER OF PIXELS IN THE ABOVE AGE RASTER 
# sim$nStands ####<- length(sim$ages) # this will come from the number of pixels in the raster above that have ages
nStands <- ncell(age)

# standIdx ####<- 1:sim$nStands 
standIDx <- 1:nStands

#THIS NEEDS TO LINK A GROWTH CRUVE TO EACH STAND/PIXEL
# for now...make this work with scott's tables...
# need to a vector of gcids for each stand/pixel
# TASK2: intersect the two rasters that determine growth curve:
#casfri_dom2.tif
#site_productivity.tif
dom <- raster("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/casfri_dom2_recliner.tif")
siteprod <- raster("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/site_productivity_recliner.tif")
adminBoundaries <- shapefile("C:/Ian/Global GIS/Can_pol_boundaries/boundary_p_v2/boundary_l_v2.shp")
#source http://ftp.geogratis.gc.ca/pub/nrcan_rncan/vector/framework_cadre/North_America_Atlas10M/boundaries/
can <- adminBoundaries[adminBoundaries$COUNTRY == "CAN",]
ecozones <- shapefile("C:/Ian/Global GIS/Ecozones/ecozones.shp") #source http://sis.agr.gc.ca/cansis/nsdb/ecostrat/gis_data.html
#get into same coordinate system
canAdmin <- spTransform(can, CRSobj = ecozones@proj4string)

#read in spatial . We will want to make these ourselves eventually by intersecting Admin and Ecozones, matching field to table.  
# spUnits <- shapefile("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/pspu.shp") 
# spUnits <- spTransform(spUnits, CRSobj = age@crs)
# abovedefinitely not spatial unit ids. values = 90026-90020.....
spUnits <- raster("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/pspuRas2.tif")
spUnits <- raster::crop(spUnits, y = age)
#Clip to study area

outSim@.envir$cbmData@disturbanceMatrixAssociation

#get the following from spUnits Raster: gcID, disturbance????



temptable <- outSim@.envir$cbmData@disturbanceMatrixAssociation #links disturbance matrix id with spatial unit id but there are duplicate spatial unit ids. (because there are multiple disturbances within one spatial unit?)
temptable1 <- outSim@.envir$cbmData@disturbanceMatrixValues #uses disturbance matrix id
temptable
outSim@.envir$cbmData@spinupParameters

# sim$historicDMIDs#### <- c(214)#,1,1,1)
# sim$lastPassDMIDS#### <- c(214)#,1,1,1)
# sim$returnIntervals#### <- c(200)#,110,120,130)

# THESE CAN BE SET TO 0 (i.e., no regeneration dealy):
# sim$delays <- c(0)#,0,0,0)


# SET TO THE DEAFULTS OF 10 and 30:
# sim$minRotations <- rep(0, sim$nStands)
# sim$maxRotations <- rep(100, sim$nStands)

# WHICH SPATIAL UNITS WILL BE DETERMINED MY THE INTERSECTION OF admin_boudary and ecozone
# THESE TABLES ARE IN cbm_defaults (cbmTables readInSQLiteData.r). but it might also be in this raster:
test <- raster("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/pspuRas2.tif")

# sim$spatialUnits <- rep(26, sim$nStands)
# sim$ecozones <- rep(5, sim$nStands)

# THE DISTURBANCE EVENTS ARE HERE:
projchange85 <- raster("C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/projected_change_type/dist1985.tif")
plot(projchange85)
raslist <- list.files(path = "C:/Ian/Boisvenue/forIan/SK_data/SK_ReclineRuns30m/layers/projected_change_type/", pattern = "tif$", full.names = TRUE)
raslist1 <- lapply(raslist, FUN = function(x, shr = age) {
  
  cropras <- raster::crop(raster(x), y = extent(shr))
  return(cropras)
} )

rasstack <- raster::stack(raslist1)
# sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))
# colnames(sim$disturbanceEvents)<-c("standIndex", "Year", "DisturbanceMatrixId")


