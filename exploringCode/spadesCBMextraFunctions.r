#------------------------------------------------------
# Functions to look at disturbance matrices
#
# CBoisvenue, Ian Eddy
# October, 17, 2018
# January 2019
#-----------------------------------------------------

### spuDist()-------------------------------------------------------------------
#This function identifies the ID number (CBM-CFS3 legacy) that are possible in
#the specific spatial unit you are in. You give is spatial units you are
#targetting (mySpu) and it give you the disturbance matrix id that are
#possible/default in that specific spu and a descriptive name of that disturbance matrix
#it creates a data.frame of length number of disturbances, with three columns: spatial_unit_is, 
#disturbance_matrix_id, and a desciption of the disturbance.

## figure out which spu you are in 
#Note: can we have a canada-wide spu map and they locate themselves on the map?
#this needs to be done before simulations are run so the user can provide this
#info (location info) for the simulations - Ian is working on this.

# could be with the rasters
library(raster)
spuRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
spatial_unit_id <- getValues(spuRaster) #28 27
# or with the growth curves
gcIn <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv"))

mySpu <- unique(spatial_unit_id)
# or 
mySpu <- unique(gcIn[,1])

# the function has the defaults from the Sk managed forest example. These can my
# changed by feeing in other spu
spuDist <- function(mySpu = c(27,28),dbPath = file.path(getwd(),"data","cbm_defaults","cbm_defaults.db")){

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
  spuDist <- cbind(dmid,cbmTables[[6]][dmid$disturbance_matrix_id,3])
  return(spuDist)
}

###END spuDist------------------------------------------------------------------

### histDist()---------------------------------------------
#Historical disturbances in CBM-CFS3 are used for "filling-up" the soil-related
#carbon pools. Boudewyn et al. (2007) translate the m3/ha curves into biomass
#per ha in each of four pools: total biomass for stem wood, total biomass for
#bark, total biomass for branches and total biomass for foliage. Biomass in
#coarse and fine roots, in aboveground- and belowground- very-fast, -fast,
#-slow, in medium-soil, and in snags still needs to be estimated. In all spatial
#units in Canada, the historical disturbance is set to fire. A stand-replacing
#fire disturbance is used in a disturbe-grow cycle, where stands are disturbed
#and regrown with turnover, overmature, decay, functioning until the dead
#organic matter pools biomass values stabilise (+ or - 10% I think but that is
#in the Rcpp-RCMBGrowthIncrements.cpp so can't check).
#This function histDist(), identifies the stand-replacing wildfire disturbance
#in each spatial unit. By default the most recent is selected, but the user can
#change that.
# as per spuDist, you need to specify your spu
mySpu <- c(27,28)
histDist<- function(mySpu = c(27,28)){
  # used the spuDist() function to narrow down the choice
  a <- spuDist(mySpu)
  # don't need all the cbm_default tables since column 3 of a give you the names
  # this searches for "wildfire"
  #a[which(grepl("wildfire",a[,3],ignore.case = TRUE)),]
  # if there are more then 1 "wildfire" designation, chose the maximum disturbance
  # matrix id number since that would be the most recent in the database
  b <- aggregate(disturbance_matrix_id ~ spatial_unit_id, data = a[which(grepl("wildfire",a[,3],ignore.case = TRUE)),], max)
  c <- merge.data.frame(a,b)
  return(c)
  }

### END histDist()-----------------------------------------

### seeDist()-------------------------------------------------------
#You give this function one or more disturbance matrix id, and it will return
#the descriptive name of the disturbance, the source pools, the sink pools, and
#the proportions transferred. It returns a list of data frames, one data.frame
#per disturbance matrix id


distId <- c(161,230,313,361)


seeDist <-function(distId = c(161,230,313,361),dbPath = file.path(getwd(),"data","cbm_defaults","cbm_defaults.db")) {
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
    matD <- as.data.frame(cbmTables[[8]][which(cbmTables[[8]][,1]==matNum[i]),])
    names(poolNames) <- c("sinkName","sink_pool_id")
    sinkNames <- merge.data.frame(poolNames,matD)
    
    names(poolNames) <- c("sourceName","source_pool_id")
    sourceNames <- merge.data.frame(poolNames,sinkNames)
    lookDists[[i]] <- sourceNames[,c(5,1:4,6)]
  }
  # each data.frame gets a descriptive name
  names(lookDists) <- cbmTables[[6]][matNum,3]
  # description 
  # "Salvage uprooting and burn for Boreal Plains"
  return(lookDists)
  
}
### END seeDist()---------------------------------------------------



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
  matNum <- unique(sim$mySpuDmids[,2])
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

### plotCarbonRasters() ---------------------------------------------------------------------------
## Plotting pools -----------------------------------------------------------
# masterRaster is now saved in the sim (sim$masterRaster)
# and cbmPools is also saved in the sim$
# this function will eventually be an event in the simulations but for now, this will plot post simulation
# making the preliminary function here:

#When revisiting, change this so it doesnt' use sim, but rather pixelKeep and cbmPools. 
plotCarbonRasters <- function(pixelkeep, cbmPools, poolsToPlot, years, masterRaster) {
  
  if ("totalCarbon" %in% poolsToPlot) {
    totalCarbon <- apply(cbmPools[,5:25], 1, 'sum')
    cbmPools <- cbind(cbmPools, totalCarbon)
  }
  
  if (any(!poolsToPlot %in% colnames( cbmPools))) {
    stop("The carbon pool you specified is not contained in the pool definitions")
  }
  
  cbmPools <- as.data.table( cbmPools) 
  #Build rasters for every year and pool
  carbonStacks <- vector(mode = "list", length = length(poolsToPlot))
  names(carbonStacks) <- poolsToPlot
 
  for (pool in poolsToPlot) {
    carbonStacks[[pool]] <- lapply(years, FUN = function(x, poolsDT = cbmPools, 
                                                       var = pool, 
                                                       pixelKeep =  pixelkeep) {
    poolsDT <- poolsDT[order(pixelGroup)] %>% #order by stand index
    .[simYear == x, .(pixelGroup, "var" =  get(pool))] #subset by year
    #subset  pixelKeep
    colsToKeep <- c("pixelIndex", paste0("pixelGroup", x))
    
    pixelKeep <- pixelKeep[, colsToKeep, with = FALSE] %>%
    setnames(., c("pixelIndex", "pixelGroup")) #with=FALSE tells data.table colsToKeep isn't a column name    
    pixelKeep <- pixelKeep[poolsDT, on = c("pixelGroup")] %>% #join with pixelKeep
    .[order(pixelIndex)] #order by rowOrder for raster prep
    
    masterRaster[masterRaster == 0] <- NA #Species has zeroes instead of NA. Revisit if masterRaster changes
    masterRaster[!is.na(masterRaster)] <- pixelKeep$var
    
    names(masterRaster) <- x #name will begin with x if no character assigned
    return(masterRaster)
    })
  names(carbonStacks[[pool]]) <- paste(pool, years, sep = "_")
  }
  names(carbonStacks) <- NULL
  temp <- unlist(carbonStacks)
  clearPlot()
  Plot(temp)
}

dev()
#include 'totalCarbon' in poolsToPlot to add plot of total carbon
plotCarbonRasters(cbmPools = spadesCBMout$cbmPools, 
                  poolsToPlot = c('totalCarbon', "BelowGroundSlowSoil"), 
                  masterRaster = spadesCBMout$masterRaster,
                  pixelkeep = spadesCBMout$pixelKeep, 
                  years = c(1990, 2000, 2005))
### End plotCarbonRasters -----------------------------------------------------------------------------

###spuLocator() --------------------------------------------------------------------------------------
library(raster)
library(fasterize) #We will need this to speed up the rasterize process
library(sf)#We will need this to speed up the rasterize process

#Make a function that produces a raster with spUnits
retrieveSpuRaster <- function(spatialUnitsFile = shapefile("data/spUnit_Locator.shp"), UserArea, rasterRes = c(250,250)){
 
  if (!(class(UserArea) == "SpatialPolygonsDataFrame" | class(UserArea) == "RasterLayer")) {
    stop("Please supply UserArea as a SpatialPolygonsDataFrame or RasterLayer")
  }
  
  if (!identicalCRS(spatialUnitsFile, UserArea)) {
    spatialUnitsFile <- spTransform(x = spatialUnitsFile, CRSobj = crs(UserArea))
  }
  
  test <- couldBeLonLat(UserArea)
  if(test == TRUE & any(rasterRes>1)) {
    warning("rasterRes is measured in units of the UserArea, which appears to be lat long")
  }
  
  temp <- crop(spatialUnitsFile, UserArea) %>%
    sf::st_as_sf(.)
  template <- raster(extent(temp), res = rasterRes, crs = crs(UserArea))
  spuRaster <- fasterize::fasterize(sf = temp, raster = template, field = "spu_id")
  
  return(spuRaster)
}

##Some tests that should eventually become actual tests
test1 <- shapefile("data/forIan/SK_data/CBM_GIS/SpadesCBM_TestArea.shp")
out1 <- retrieveSpuRaster(UserArea = test1, rasterRes = c(250, 250))
Plot(out1)
test2 <- LandR::randomStudyArea(seed = 100, size = 10000*100*300)
out2 <- retrieveSpuRaster(UserArea = test2, rasterRes = c(250,250))
Plot(out2)
###END spuLocator() ----------------------------------------------------------------------------------

# NPP()--------------------------------------
#calculate NPP post simulation
NPP <- function(pools = spadesCBMout$cbmPools, pixelGroupSpu=spadesCBMout$pixelGroupC[,.(pixelGroup,spatial_unit_id)],
                year1=1990, year2=1991, turnoverRates = spadesCBMout$turnoverRates)
  {
  a <- as.data.table(pools)[simYear==year1,.(
    pixelGroup,
    ages,
    SoftwoodMerch,
    SoftwoodFoliage,
    SoftwoodOther,
    SoftwoodCoarseRoots,
    SoftwoodFineRoots,
    HardwoodMerch,
    HardwoodFoliage,
    HardwoodOther,
    HardwoodCoarseRoots,
    HardwoodFineRoots 
  )]
  b <- pixelGroupSpu[(pixelGroup %in% a$pixelGroup)]
  stockt <- merge(a,b,by="pixelGroup")
  setkey(stockt,pixelGroup)
  
  stockt1 <- as.data.table(pools)[simYear==year2,.(
    pixelGroup,
    ages,
    SoftwoodMerch,
    SoftwoodFoliage,
    SoftwoodOther,
    SoftwoodCoarseRoots,
    SoftwoodFineRoots,
    HardwoodMerch,
    HardwoodFoliage,
    HardwoodOther,
    HardwoodCoarseRoots,
    HardwoodFineRoots 
  )]
  setkey(stockt1,pixelGroup)
  
  stocks2t <- merge(stockt[,-c("ages","spatial_unit_id")],stockt1[,-"ages"],by = "pixelGroup",sort=TRUE)
  
  grossGrowth <- stocks2t[,.(
    pixelGroup,
    grossGrowthAG = (
      (SoftwoodMerch.y-SoftwoodMerch.x)+
        (SoftwoodFoliage.y-SoftwoodFoliage.x)+
        (SoftwoodOther.y-SoftwoodOther.x)+
        (HardwoodMerch.y-HardwoodMerch.x)+
        (HardwoodFoliage.y-HardwoodFoliage.x)+
        (HardwoodOther.y-HardwoodOther.x)),
    grossGrowthBG= (
      (SoftwoodCoarseRoots.y-SoftwoodCoarseRoots.x)+
        (SoftwoodFineRoots.y-SoftwoodFineRoots.x)+
        (HardwoodCoarseRoots.y-HardwoodCoarseRoots.x)+
        (HardwoodFineRoots.y-HardwoodFineRoots.x))
  )]
  
  turnoverRates <- turnoverRates[, spatial_unit_id := SpatialUnitID]
  
  turnoverComponents <- merge(stockt,turnoverRates,by="spatial_unit_id")
  
  turnover <- turnoverComponents[,.(
    pixelGroup,
    AGturnover = (
      (SoftwoodMerch * StemAnnualTurnoverRate)+
        (SoftwoodFoliage * SoftwoodFoliageFallRate)+
        (SoftwoodOther * SoftwoodBranchTurnoverRate)+
        (HardwoodMerch * StemAnnualTurnoverRate)+
        (HardwoodFoliage * HardwoodFoliageFallRate)+
        (HardwoodOther * HardwoodBranchTurnoverRate)),
    BGturnover = (
      (SoftwoodCoarseRoots * CoarseRootTurnProp)+
        (SoftwoodFineRoots * FineRootTurnProp)+
        (HardwoodCoarseRoots * CoarseRootTurnProp)+
        (HardwoodFineRoots * FineRootTurnProp))
  )]
  
  NPP <- merge(turnover,grossGrowth,by="pixelGroup")[,.(
    pixelGroup,
    NPP = (
      AGturnover+
        BGturnover+
        grossGrowthAG+
        grossGrowthBG)
  )]
  
  return(NPP)  
  
}
