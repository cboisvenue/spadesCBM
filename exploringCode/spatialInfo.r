#----------------------------------------------
# Figure out the spatial info
# starting with the info from the init event in 
# the spadesCBMinputs module
#
# CBoisvenue April 9th, 2018
#----------------------------------------------

# This is input that needs to be spatial
sim$ages <- c(0)#,2,3,140) # this will come from a raster
sim$nStands <- length(sim$ages) # this will come from the number of pixels in the raster above that have ages
standIdx <- 1:sim$nStands 
sim$gcids <- c(1)#,2,3,101) # this Celine has to figure out
sim$historicDMIDs <- c(214)#,1,1,1)
sim$lastPassDMIDS <- c(214)#,1,1,1)
sim$delays <- c(0)#,0,0,0)
sim$minRotations <- rep(0, sim$nStands)
sim$maxRotations <- rep(100, sim$nStands)
sim$returnIntervals <- c(200)#,110,120,130)
sim$spatialUnits <- rep(26, sim$nStands)
sim$ecozones <- rep(5, sim$nStands)
sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))
colnames(sim$disturbanceEvents)<-c("standIndex", "Year", "DisturbanceMatrixId")


