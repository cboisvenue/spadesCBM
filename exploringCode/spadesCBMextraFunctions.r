#------------------------------------------------------
# Functions to look at disturbance matrices
#
# CBoisvenue
# October, 17, 2018
#-----------------------------------------------------

### lastDist()-------------------------------------------------------------------
### historical disturbances in CBM-CFS3 there are default historical
#disturbances These are mostly fire (all?) and they are used in the
#disturbe-grow cycles of the spinup spinup is when the above-ground biomass
#curves are used grow stand which are then disturbed and regrown with turnover,
#overmature, decay, functioning until the dead organic matter pools biomass
#values stabilise (+ or - 10% I think but that is in the
#Rcpp-RCMBGrowthIncrements.cpp so can't check)

### figure out which spu you are in
# Note: can we have a canada-wide spu map and they locate themselves on the map?
# this needs to be done before simulations are run so the user can provide this
# info (location info) for the simulations

# could be with the rasters
library(raster)
spuRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
spatial_unit_id <- getValues(spuRaster) #28 27
# or with the growth curves
gcIn <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv"))

mySpu <- unique(spatial_unit_id)
# or 
mySpu <- unique(gcIn[,1])

lastDist <- function(mySpu = c(27,28),dbPath = file.path(getwd(),"data","cbm_defaults","cbm_defaults.db")){

  library(RSQLite)
  
  sqlite.driver <- dbDriver("SQLite")
  
  cbmDefaults <- dbConnect(sqlite.driver,
                           dbname = dbPath)
  
  alltables = dbListTables(cbmDefaults)
  cbmTables <- list()
  
  for(i in 1:length(alltables)){
    cbmTables[[i]] <- dbReadTable(cbmDefaults,alltables[i])
  }
  # match mySpu with the disturbance_matrix_association table
  dmid <- unique(cbmTables[[7]][which(cbmTables[[7]][,1] %in% mySpu),c(1,3)])
  
  # add the descriptive names
  lastDist <- cbind(dmid,cbmTables[[6]][dmid$disturbance_matrix_id,3])
  return(lastDist)
}

###END lastDist------------------------------------------------------------------

### seeDist-------------------------------------------------------
### whatever disturbance matrix id, get the descriptive name and the proportions transferred
# for general purposes, when you want to examine a disturbance matrix

distId <- c(161,230,313,361)


seeDist(distId,dbPath = file.path(getwd(),"data","cbm_defaults","cbm_defaults.db")) <- {
  # get the defaults
  library(RSQLite)
  
  sqlite.driver <- dbDriver("SQLite")
  
  cbmDefaults <- dbConnect(sqlite.driver,
                           dbname = dbPath)
  
  alltables = dbListTables(cbmDefaults)
  cbmTables <- list()
  
  for(i in 1:length(alltables)){
    cbmTables[[i]] <- dbReadTable(cbmDefaults,alltables[i])
  }
  
  # one copy of each distId
  matNum <- unique(distId)  
  lookDists <- vector("list", length=length(matNum))
  poolNames = as.data.frame(cbind(c("SoftwoodMerch",
                  "SoftwoodFoliage",
                  "SoftwoodOther",
                  "SoftwoodCoarseRoots",
                  "SoftwoodFineRoots",
                  "HardwoodMerch",
                  "HardwoodFoliage",
                  "HardwoodOther",
                  "HardwoodCoarseRoots",
                  "HardwoodFineRoots",
                  "AboveGroundVeryFastSoil",
                  "BelowGroundVeryFastSoil",
                  "AboveGroundFastSoil",
                  "BelowGroundFastSoil",
                  "MediumSoil",
                  "AboveGroundSlowSoil",
                  "BelowGroundSlowSoil",
                  "SoftwoodStemSnag",
                  "SoftwoodBranchSnag",
                  "HardwoodStemSnag",
                  "HardwoodBranchSnag",
                  "CO2",
                  "CH4",
                  "CO",
                  "Products"), c(1:24, 26)))
  names(poolNames) <- c("pool","dmPoolId")
  
  # for each matNum, create a data.frame that explains the pool transfers
  for(i in 1:length(matNum)){
    # get the lines specific to the distMatrix in question
    matD <- as.data.frame(cbmTables[[6]][which(cbmTables[[6]][,1]==matNum[i]),])
    names(poolNames) <- c("sinkName","sink_pool_id")
    sinkNames <- merge.data.frame(poolNames,matD)
    
    names(poolNames) <- c("sourceName","source_pool_id")
    sourceNames <- merge.data.frame(poolNames,sinkNames)
    lookDists[[i]] <- sourceNames[,c(5,1:4,6)]
  }
  # each data.frame gets a descriptive name
  names(lookDists) <- sim@.envir$cbmData@disturbanceMatrix[matNum,3]
  # description 
  # "Salvage uprooting and burn for Boreal Plains"
  return(lookDists)
  
}



### simDist()----------------------------------------
### get the descriptive name and proportions transferred for disturbances in a simulation
# requires a simulation list post simulations (from spades())
# and returns a list of data.frames. Each data had the descriptive name of a
# disturbance used in the simulations, the disturbance matrix identification
# number from cbm_defaults, the pool from which carbon is taken (source pools)
# in this specific disturbance, the pools into which carbon goes, and the
# proportion in which the carbon-transfers are completed.
simDist <- function(sim){
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
### END simDist----------------------------------------