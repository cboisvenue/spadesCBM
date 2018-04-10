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

## AGES
# from a raster 
# sim$ages <- c(0)#,2,3,140) # this will come from a raster
# TASK: READ-IN RASTER
# this should be age in 1984:
C:\Celine\Syndocs\RES_Work\Work\SpaDES\spadesCBM\rasters\age1.tif
# is this the same?
(\SK_ReclineRuns30m\layers\age1CASFRI.tif)

## TASK: CALCULATE THESE FROM THE NUMBER OF PIXELS IN THE ABOVE AGE RASTER
# sim$nStands <- length(sim$ages) # this will come from the number of pixels in the raster above that have ages
# standIdx <- 1:sim$nStands 


#THIS NEEDS TO LINK A GROWTH CRUVE TO EACH STAND/PIXEL
# for now...make this work with scott's tables...
# need to a vector of gcids for each stand

# sim$gcids <- c(1)#,2,3,101) # this Celine has to figure out
# this should be age in 1984:
C:\Celine\Syndocs\RES_Work\Work\SpaDES\spadesCBM\rasters\age1.tif
casfri_dom2.tif
site_productivity.tif



# THESE THREE CAN BE GOTTEN FROM THE spatial_unit_id:
# sim$historicDMIDs <- c(214)#,1,1,1)
# sim$lastPassDMIDS <- c(214)#,1,1,1)
# sim$returnIntervals <- c(200)#,110,120,130)

# THESE CAN BE SET TO 0 (i.e., no regeneration dealy):
# sim$delays <- c(0)#,0,0,0)


# SET TO THE DEAFULTS OF 10 and 30 years:
# sim$minRotations <- rep(0, sim$nStands)
# sim$maxRotations <- rep(100, sim$nStands)

# WHICH SPATIAL UNITS WILL BE DETERMINED MY THE INTERSECTION OF admin_boudary and ecozone
# THESE TABLES ARE IN cbm_defaults (cbmTables readInSQLiteData.r). but it might also be in this raster:
SK_ReclineRuns30m\layers\pspuRas2.tif
# sim$spatialUnits <- rep(26, sim$nStands)
# sim$ecozones <- rep(5, sim$nStands)

# THE DISTURBANCE EVENTS ARE HERE:
SK_ReclineRuns30m\layers\projected_change_type
# sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))
# colnames(sim$disturbanceEvents)<-c("standIndex", "Year", "DisturbanceMatrixId")


