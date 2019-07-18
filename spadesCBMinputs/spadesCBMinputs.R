
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "spadesCBMinputs",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9007", spadesCBMinputs = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spadesCBMinputs.Rmd"),
  reqdPkgs = list("RSQLite","data.table","CBMVolumeToBiomass", "raster", "LandR"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    #expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA),
    expectsInput(objectName = "cbmData", objectClass = "dataset", desc = "S4 object created from selective reading in of cbm_default.db in spadesCBMefaults module", sourceURL = NA),
    expectsInput(objectName = "pooldef", objectClass = "character", desc = "Vector of names (characters) for each of the carbon pools, with `Input` being the first one", sourceURL = NA),
    expectsInput(objectName = "PoolCount", objectClass = "numeric", desc = "count of the length of the Vector of names (characters) for each of the carbon pools, with `Input` being the first one", sourceURL = NA),
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "sqlDir", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "gcurveFileName", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "gcurveComponentsFileName", objectClass = "character", desc = NA, sourceURL = NA)
    
    
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #createsOutput(objectName = NA, objectClass = NA, desc = NA)
    createsOutput(objectName = "pools", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "ages", objectClass = "numeric", desc = "Ages of the stands from the inventory in 1990"),
    createsOutput(objectName = "nStands", objectClass = "numeric", desc = "not really the number of stands, but the number of pixel groups"),
    createsOutput(objectName = "gcids", objectClass = "numeric", desc = "The identification of which growth curves to use on the specific stands provided by..."),
    createsOutput(objectName = "historicDMIDs", objectClass = "numeric", desc = "Vector, one for each stand, indicating historical disturbance type, linked to the S4 table called cbmData. Only Spinup."),
    createsOutput(objectName = "lastPassDMIDS", objectClass = "numeric", desc = "Vector, one for each stand, indicating final disturbance type, linked to the S4 table called cbmData. Only Spinup."),
    createsOutput(objectName = "delays", objectClass = "numeric", desc = "Vector, one for each stand, indicating regeneration delay post disturbance. Only Spinup."),
    createsOutput(objectName = "minRotations", objectClass = "numeric", desc = "Vector, one for each stand, indicating minimum number of rotations. Only Spinup."),
    createsOutput(objectName = "maxRotations", objectClass = "numeric", desc = "Vector, one for each stand, indicating maximum number of rotations. Only Spinup."),
    createsOutput(objectName = "returnIntervals", objectClass = "numeric", desc = "Vector, one for each stand, indicating the fixed fire return interval. Only Spinup."),
    createsOutput(objectName = "spatialUnits", objectClass = "numeric", desc = "The id given to the intersection of province and ecozones across Canada, linked to the S4 table called cbmData"),
    createsOutput(objectName = "ecozones", objectClass = "numeric", desc = "Vector, one for each stand, indicating the numeric represenation of the Canadian ecozones, as used in CBM-CFS3"),
    createsOutput(objectName = "growth_increments", objectClass = "matrix", desc = "to this later"),
    createsOutput(objectName = "gcHash", objectClass = "matrix", desc = "to this later"),
    createsOutput(objectName = "level3DT", objectClass = "data.table", desc = "the table linking the spu id, with the disturbance_matrix_id and the events. The events are the possible raster values from the disturbance rasters of Wulder and White"),
    createsOutput(objectName = "spatialDT", objectClass = "data.table", desc = "the table containing one line per pixel"),
    createsOutput(objectName = "mySpuDmids", objectClass = "data.frame", desc = "the table containing one line per pixel"),
    createsOutput(objectName = "disturbanceRasters", objectClass = "raster", desc = "Character vector of the disturbance rasters for SK"),
    createsOutput(objectName = "masterRaster", objectClass = "raster", desc = "Raster has NAs where there are no species and the pixel groupID where the pixels were simulated. It is used to map results")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.spadesCBMinputs = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "spadesCBMinputs", "save")
    },
    
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "spadesCBMinputs", "save")

      # ! ----- STOP EDITING ----- ! #
    },
   
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
#  sim$PoolCount <- length(sim$pooldef)
  
  gcID <- read.csv(sim$gcurveFileName)
  ## HAVE TO maintain this format
  growthCurves <- as.matrix(gcID[,c(3,2,5,4,6)])
  growthCurveComponents <- as.matrix(read.csv(sim$gcurveComponentsFileName))
  
  sim$growth_increments<-NULL
  for(gcid in unique(growthCurves[,"growth_curve_id"])) { 
    curve <- processGrowthCurve(gcid, growthCurves, growthCurveComponents,sim = sim)
    sim$growth_increments <- rbind(sim$growth_increments,
                                   cbind(rep(gcid,(nrow(curve)-1)), cbind(curve[0:(nrow(curve)-1),1], diff(curve[,2:ncol(curve)]))))
    
  }
  
  colnames(sim$growth_increments)<- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
  ## this is where I will be replacing black spruce increments### THIS IS
  # ## TEMPORARY TO CHECK IF GROWTH IS OK AND THEN CHECK OF DISTURBANCES ARE OK
  bSpruceInc <- read.csv(file.path(paths(sim)$inputPath,"blackSpruceInc.csv"))
  BSid <- c(8,9,29,30,50,51,71,72,92,93)
  growth.inc <- sim$growth_increments
  for(i in 1:length(BSid)){
    growth.inc[growth.inc[,1] == BSid[i],3] <- bSpruceInc[,2]
    growth.inc[growth.inc[,1] == BSid[i],4] <- bSpruceInc[,3]
    growth.inc[growth.inc[,1] == BSid[i],5] <- bSpruceInc[,4]
  }
  sim$growth_increments <- growth.inc

  sim$gcHash <- matrixHash(sim$growth_increments)
  #create a nested hash (by gcid/by age)
  ## used in SpinUp function later...
  for(item in ls(sim$gcHash)){
    sim$gcHash[[item]] <- hash(sim$gcHash[[item]])
  }
  
  
  #### Data will have to be provided by user in a separated module...short cut for now...
  #####################################################################################

  age <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/age_TestArea.tif"))
  #This works
  ages <- getValues(age)
  # read-in species
  ldSpsRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/ldSp_TestArea.tif"))
  rasterSps <- getValues(ldSpsRaster) # 5 0 3 4 6 7
  # read-in productivity  levels
  prodRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/prod_TestArea.tif"))
  Productivity <- getValues(prodRaster)#1 2 3 0
  # read-in spatial units
  spuRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
  spatial_unit_id <- getValues(spuRaster) #28 27

  level2DT <- as.data.table(cbind(ages,rasterSps,Productivity,spatial_unit_id))	  
  level2DT <- level2DT[level2DT$rasterSps>0]
  level2DT$pixelIndex <- 1:nrow(level2DT)
  setkey(level2DT,rasterSps,Productivity,spatial_unit_id)
  level2DT[order(pixelIndex),]
  # add the gcID	  # add the gcID
  #gcID <- read.csv(file.path(getwd(),"data/spadesGCurvesSK.csv"))#gcID_ref.csv
  gcID <- as.data.table(gcID[,-1]) 
  gcID <- gcID[,.(rasterSps,Productivity,growth_curve_component_id,spatial_unit_id,growth_curve_id)]
  setkey(gcID,growth_curve_component_id,rasterSps,Productivity,spatial_unit_id)
  
  spatialDT <- level2DT[gcID, on = c("rasterSps","Productivity","spatial_unit_id"),nomatch = 0]
  spatialDT[order(pixelIndex),]
  spatialDT$pixelGroup <- LandR::generatePixelGroups(spatialDT,0,
                                     columns = c("spatial_unit_id", "growth_curve_component_id", "ages"))
  spatialDT[order(pixelIndex),]
  ## NEED TO ASK ELIOT ABOUT THIS: why does it create all these extra vars?
  # why the number starts at the number you are asking for? not logical to me -
  # max is the max value your groups should have.@..?
  spatialDT <- spatialDT[,.(ages, rasterSps, Productivity, spatial_unit_id, pixelIndex,
                          growth_curve_component_id, growth_curve_id, pixelGroup)]
  spatialDT <- spatialDT[order(pixelIndex),]
  # make the data.table that will be used in simulations
  level3DT <- unique(spatialDT[,-("pixelIndex")])%>% .[order(pixelGroup)]
  # might have to keep this when we integrate the disturbances
  sim$level3DT <- level3DT
  
  # spatial data table keeps the pixels number to re-populate for maps
  #setkey(gcID, NULL) #have to unkey before a join
  # spatialDT <- gcID[level2DT, on = c("rasterSps", "Productivity", "spatial_unit_id")]
  # spatialDT <- spatialDT[order(rowOrder)]
  # spatialDT$PixelGroupID <- as.numeric(factor(paste(spatialDT$spatial_unit_id,
                                                       # spatialDT$growth_curve_component_id,
                                                       # spatialDT$ages)))
  sim$spatialDT <- spatialDT
  
  sim$masterRaster <- ldSpsRaster
  # masterRaster[rasterSps == 0] <- NA
  # masterRaster[!rasterSps == 0] <- spatialDT$PixelGroupID
  # sim$masterRaster <- masterRaster
   
  
  ############################################################
  ## can't seem to solve why growth curve id 58 (white birch, good productivity) will not run with ages=1
  ## this is a problem to tackle once we have some insight into the cpp code
  ###########################################################
  # temp fix:
  sim$level3DT[ages==1 & growth_curve_component_id==58,ages:=3]
  sim$ages <- sim$level3DT[,ages]
  sim$nStands <- length(sim$ages)
  
  ## the pooldef needs to be a sim$ because if will be used in the spatial data portion later
  sim$pools <- matrix(ncol = sim$PoolCount, nrow=sim$nStands, data=0)
  colnames(sim$pools)<- sim$pooldef
  sim$pools[,"Input"] = rep(1.0, nrow(sim$pools))
  
  sim$gcids <- sim$level3DT[,growth_curve_component_id]
  sim$delays <-  rep.int(0,sim$nStands)
  sim$minRotations <- rep.int(10,sim$nStands)
  sim$maxRotations <- rep.int(30,sim$nStands)
  retInt <- merge(sim$level3DT[,],sim$cbmData@spinupParameters[,c(1,2)], by="spatial_unit_id", all.x=TRUE) %>% .[order(pixelGroup)]
  sim$returnIntervals <- retInt[,"return_interval"]
  sim$spatialUnits <- sim$level3DT[,spatial_unit_id]
  spu <- as.data.frame(sim$cbmData@spatialUnitIds)
  
  # change this here so it will be easier to access when disturbances change PixelGroupID
#  ecoToSpu <- as.data.frame(sim$cbmData@spatialUnitIds[which(spu$SpatialUnitID %in% unique(gcID$spatial_unit_id)),c(1,3)])
  ecoToSpu <- as.data.frame(sim$cbmData@spatialUnitIds[,c(1,3)])
  names(ecoToSpu) <- c("spatial_unit_id","ecozones")
  sim$spatialDT <- merge(sim$spatialDT,ecoToSpu,by="spatial_unit_id") %>% .[order(pixelIndex),]

  ecozones <- unique(sim$spatialDT[, .(pixelGroup,ecozones)]) %>% .[order(pixelGroup),]
  sim$ecozones <- ecozones[,ecozones]
  
#  ecoz <- merge.data.frame(sim$level3DT[,],ecoToSpu,by="spatial_unit_id", all.x=TRUE)
  #sim$ecozones <- ecoz[,"ecozones"]

  # make the disturbance look-up table to the disturbance_matrix_id(s)
  # making sim$mySpuDmids
  #raster values 1 to 5
  #GitHub\spadesCBM\data\forIan\SK_data\SK_ReclineRuns30m\LookupTables\DisturbanceTypeLookup.csv
  # 1 is Wildfire
  # 2 is Clearcut harvesting with salvage
  # 3 is Deforestation â€” Transportation â€” Salvage, uprooting and burn
  # 4 Generic 20% mortality
  # 5	Generic 20% mortality
  
  spu <- unique(sim$spatialDT$spatial_unit_id)
  # what disturbances in those spu(s)?
  listDist <- spuDist(spu)
  
  #get the right ones
  fire <- listDist[grep("wildfire",listDist[,3], ignore.case=TRUE),1:3]
  
  #had to figure this one out by hand...there were 12 clearcut types...took the
  #one that said 50% salvage got that from looking at the published paper Boivenue
  #et al 2016...and the word salvage is misspelled in the database (sigh). In the
  #publication, we said 85% of the merchantable trees and 50% of the snags...
  #there is no "85%" clearcut in the whole data base (cbmTables[[6]][,2])...85% is
  #only used in precommercial thinning Sylva EPC
  clearCut <- listDist[grep("Clearcut",listDist[,3], ignore.case=TRUE),1:3]
  clearCut <- clearCut[7:8,]
  
  # Again, there are 12 deforestation, but only two are not called "Fixed
  # Deforestation-Hydro", so I picked these two
  defor1 <- listDist[grep("Deforestation",listDist[,3], ignore.case=TRUE),1:3]
  defor <- defor1[1:2,]
  
  generic <- listDist[grep("20% mortality",listDist[,3], ignore.case=TRUE),1:3]
  
  mySpuDmids <- rbind(fire[,1:2],clearCut[,1:2],defor[,1:2],generic[,1:2],generic[,1:2])
  #creating a vector of the pixel values to be able to match the disturbance_matrix_id
  events <- c(1,1,2,2,4,4,3,3,5,5)
  # need to match the historic and last past dist to the spatial unit
  # DECISION: both the last pass and the historic disturbance will be the same for these runs
  setkey(sim$level3DT,spatial_unit_id)
  setkey(as.data.table(fire[,1:2]),spatial_unit_id)
  histLastDMIDs <- merge(sim$level3DT,fire)

  sim$historicDMIDs <- histLastDMIDs$disturbance_matrix_id
  sim$lastPassDMIDS <- histLastDMIDs$disturbance_matrix_id
  # and merge them on the level3DT$spatial_unit_id
  
  
  #sim$historicDMIDs <- rep.int(214,sim$nStands)#c(214)#,1,1,1)
  #sim$lastPassDMIDS <- rep.int(214,sim$nStands)#c(214)#,1,1,1)
   
  
  sim$mySpuDmids <- cbind(mySpuDmids,events)
  
 
  # old bogus disturbance
  #sim$disturbanceEvents <- cbind(sim$level3DT$PixelGroupID,rep(2001,sim$nStands),rep(214,sim$nStands))
  #colnames(sim$disturbanceEvents)<-c("PixelGroupID", "Year", "DisturbanceMatrixId")
  
  # changing them
  sim$disturbanceRasters <- list.files("data/forIan/SK_data/CBM_GIS/disturbance_testArea",
                                   full.names = TRUE) %>%
    grep(., pattern = ".grd$", value = TRUE)
  
  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events


### template for your event1

.inputObjects = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  dataPath <- file.path(modulePath(sim),"data")
  if(!suppliedElsewhere(sim$sqlDir))
    sim$sqlDir <- file.path(dataPath,"cbm_defaults")
  if(!suppliedElsewhere(sim$dbPath))
    sim$dbPath <- file.path(dataPath, "cbm_defaults", "cbm_defaults.db")
  if(!suppliedElsewhere(sim$gcurveFileName))
    sim$gcurveFileName <- file.path(dataPath, "spadesGCurvesSK.csv")#"SK_ReclineRuns30m", "LookupTables", 
  if(!suppliedElsewhere(sim$gcurveComponentsFileName))
    sim$gcurveComponentsFileName <- file.path(dataPath, "yieldComponentSK.csv")#"SK_ReclineRuns30m", "LookupTables", 
  
  
  if(!suppliedElsewhere(sim$cbmData)){
    spatialUnitIds <- as.matrix(getTable("spatialUnitIds.sql", sim$dbPath, sim$sqlDir))
    disturbanceMatrix <- as.matrix(getTable("disturbanceMatrix.sql", sim$dbPath, sim$sqlDir))
    sim$cbmData <- new("dataset",
                       turnoverRates=as.matrix(getTable("turnoverRates.sql", sim$dbPath, sim$sqlDir)),
                       rootParameters=as.matrix(getTable("rootParameters.sql", sim$dbPath, sim$sqlDir)),
                       decayParameters=as.matrix(getTable("decayParameters.sql", sim$dbPath, sim$sqlDir)),
                       spinupParameters=as.matrix(getTable("spinupParameters.sql", sim$dbPath, sim$sqlDir)),
                       climate=as.matrix(getTable("climate.sql", sim$dbPath, sim$sqlDir)),
                       spatialUnitIds=spatialUnitIds,
                       slowAGtoBGTransferRate=as.matrix(0.006),
                       biomassToCarbonRate=as.matrix(0.5),
                       stumpParameters=as.matrix(getTable("stumpParameters.sql", sim$dbPath, sim$sqlDir)),
                       overmatureDeclineParameters=as.matrix(getTable("overmaturedecline.sql", sim$dbPath, sim$sqlDir)),
                       disturbanceMatrix=disturbanceMatrix,
                       disturbanceMatrixAssociation=as.matrix(getTable("disturbanceMatrixAssociation.sql", sim$dbPath, sim$sqlDir)),
                       disturbanceMatrixValues=as.matrix(getTable("disturbanceMatrixValues.sql", sim$dbPath, sim$sqlDir)),
                       landclasses=as.matrix(getTable("landclasses.sql", sim$dbPath, sim$sqlDir)),
                       pools = as.matrix(getTable("pools.sql", sim$dbPath, sim$sqlDir)),
                       domPools = as.matrix(getTable("domPools.sql", sim$dbPath, sim$sqlDir))
    ) 
  }
  if (!suppliedElsewhere(sim$pooldef)) 
    sim$pooldef = c("Input",
                    "SoftwoodMerch",
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
                    "Products")
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
