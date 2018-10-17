#------------------------------------------------------
# Functions to look at disturbance matrices
#
# CBoisvenue
# October, 17, 2018
#-----------------------------------------------------

### historical disturbances in CBM-CFS3 there are default historical
#disturbances These are mostly fire (all?) and they are used in the
#disturbe-grow cycles of the spinup spinup is when the above-ground biomass
#curves are used grow stand which are then disturbed and regrown with turnover,
#overmature, decay, functioning until the dead organic matter pools biomass
#values stabilise (+ or - 10% I think but that is in the
#Rcpp-RCMBGrowthIncrements.cpp so can't check)

### figure out which spu you are in
# could be with the rasters
spuRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
spatial_unit_id <- getValues(spuRaster) #28 27
# or with the growth curves
gcComponent <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldComponentRCBM.csv"))
gcIn <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv"))

mySpu <- unique(spatial_unit_id)


### Disturbances in a simulation
# requires a simulation list post simulations (from spades())
# and returns a list of data.frames. Each data had the descriptive name of a
# disturbance used in the simulations, the disturbance matrix identification
# number from cbm_defaults, the pool from which carbon is taken (source pools)
# in this specific disturbance, the pools into which carbon goes, and the
# proportion in which the carbon-transfers are completed.
seeDist <- function(sim){
  # put names to the pools
  poolNames <- as.data.frame(cbind(sim@.envir$pooldef[-1],c(1:24,26)))
  names(poolNames) <- c("pool","dmPoolId")
  
  # Getting the number of DisturbanceMatrixID
  matNum <- unique(sim@.envir$disturbanceEvents[,3])
  # matNum will be the lenght of the list of data.frames
  clearDists <- vector("list", length=length(matNum))
  
  # for each matNum, create a data.frame that explains the pool transfers
  for(i in 1:length(matNum)){
    # get the lines specific to the distMatrix in question
    matD <- as.data.frame(sim@.envir$cbmData@disturbanceMatrixValues[which(sim@.envir$cbmData@disturbanceMatrixValues[,1]==matNum[i]),])
    names(poolNames) <- c("sinkName","sink_pool_id")
    sinkNames <- merge.data.frame(poolNames,matD)
    
    names(poolNames) <- c("sourceName","source_pool_id")
    sourceNames <- merge.data.frame(poolNames,sinkNames)
    clearDists[[i]] <- sourceNames[,c(5,1:4,6)]
  }
  # each data.frame gets a descriptive name
  names(clearDists) <- sim@.envir$cbmData@disturbanceMatrix[matNum,3]
  # description 
  # "Salvage uprooting and burn for Boreal Plains"
  return(clearDists)
}
