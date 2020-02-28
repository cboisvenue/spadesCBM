# find and look at SBW matrices
# which SPU are they in?
# where does the carbon leave from and go to?
# are there multiple year ones?

# February 28, 2020
# CBoisvenue

# run a spadesCBM call either a complete one of just spadesCBM default to get the list of disturbance matrices
# sim$cbmData@disturabanceMatrix
# run the distMatrix exploration functions from spadesCBMextraFunctions

# which spu has a SBW disturbances matrix
outSim$cbmData@disturbanceMatrix[grep("worm", outSim$cbmData@disturbanceMatrix, ignore.case = TRUE)]
## there seems to be no SBW disturbances :)

#I will ask Mark Hafer
#misc code
a <- spuDist(mySpu)
# don't need all the cbm_default tables since column 3 of a give you the names
# this searches for "wildfire"
#a[which(grepl("wildfire",a[,3],ignore.case = TRUE)),]
# if there are more then 1 "wildfire" designation, chose the maximum disturbance
# matrix id number since that would be the most recent in the database
b <- aggregate(disturbance_matrix_id ~ spatial_unit_id, data = a[which(grepl("wildfire",a[,3],ignore.case = TRUE)),], max)
c <- merge.data.frame(a,b)
return(c)


