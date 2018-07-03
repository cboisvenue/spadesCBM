
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
  reqdPkgs = list("RSQLite","data.table","CBMVolumeToBiomass", "raster"),
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
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "sqlDir", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "gcurveFileName", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "gcurveComponentsFileName", objectClass = "character", desc = NA, sourceURL = NA)
    
    
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #createsOutput(objectName = NA, objectClass = NA, desc = NA)
    createsOutput(objectName = "PoolCount", objectClass = "numeric", desc = "count of the length of the Vector of names (characters) for each of the carbon pools, with `Input` being the first one", sourceURL = NA),
    createsOutput(objectName = "pools", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "ages", objectClass = "numeric", desc = "Ages of the stands from the inventory in 1990"),
    createsOutput(objectName = "gcids", objectClass = "numeric", desc = "The identification of which growth curves to use on the specific stands provided by..."),
    createsOutput(objectName = "historicDMIDs", objectClass = "numeric", desc = "Vector, one for each stand, indicating historical disturbance type, linked to the S4 table called cbmData. Only Spinup."),
    createsOutput(objectName = "lastPassDMIDS", objectClass = "numeric", desc = "Vector, one for each stand, indicating final disturbance type, linked to the S4 table called cbmData. Only Spinup."),
    createsOutput(objectName = "delays", objectClass = "numeric", desc = "Vector, one for each stand, indicating regeneration delay post disturbance. Only Spinup."),
    createsOutput(objectName = "minRotations", objectClass = "numeric", desc = "Vector, one for each stand, indicating minimum number of rotations. Only Spinup."),
    createsOutput(objectName = "maxRotations", objectClass = "numeric", desc = "Vector, one for each stand, indicating maximum number of rotations. Only Spinup."),
    createsOutput(objectName = "returnIntervals", objectClass = "numeric", desc = "Vector, one for each stand, indicating the fixed fire return interval. Only Spinup."),
    createsOutput(objectName = "spatialUnits", objectClass = "numeric", desc = "The id given to the intersection of province and ecozones across Canada, linked to the S4 table called cbmData"),
    createsOutput(objectName = "ecozones", objectClass = "numeric", desc = "Vector, one for each stand, indicating the numeric represenation of the Canadian ecozones, as used in CBM-CFS3"),
    createsOutput(objectName = "disturbanceEvents", objectClass = "matrix", desc = "3 column matrix, Stand Index, Year, and DisturbanceMatrixId. Not used in Spinup."),
    createsOutput(objectName = "growth_increments", objectClass = "matrix", desc = "to this later"),
    createsOutput(objectName = "gcHash", objectClass = "matrix", desc = "to this later")
    
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
  sim$PoolCount <- length(sim$pooldef)
  
  growthCurves <- as.matrix(read.csv(sim$gcurveFileName))
  growthCurveComponents <- as.matrix(read.csv(sim$gcurveComponentsFileName))
  
  sim$growth_increments<-NULL
  for(gcid in unique(growthCurves[,"growth_curve_id"])) { 
    curve <- processGrowthCurve(gcid, growthCurves, growthCurveComponents,sim = sim)
    sim$growth_increments <- rbind(sim$growth_increments,
                                   cbind(rep(gcid,(nrow(curve)-1)), cbind(curve[0:(nrow(curve)-1),1], diff(curve[,2:ncol(curve)]))))
    
  }
  
  colnames(sim$growth_increments)<- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
  sim$gcHash <- matrixHash(sim$growth_increments)
  #create a nested hash (by gcid/by age)
  ## used in SpinUp function later...
  for(item in ls(sim$gcHash)){
    sim$gcHash[[item]] <- hash(sim$gcHash[[item]])
  }
  
  ## the pooldef needs to be a sim$ because if will be used in the spatial data portion later
  sim$pools <- matrix(ncol = sim$PoolCount, nrow=1, data=0)
  colnames(sim$pools)<- sim$pooldef
  sim$pools[,Input] = rep(1.0, nrow(sim$pools))
  
  #### Data will have to be provided...short cut for now...
  ##############################################################
  library(data.table)
  library(raster)
  
  age <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/age_TestArea.tif"))
  #This works
  ages <- getValues(age)
  # read-in species
  ldSpsRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/ldSp_TestArea.tif"))
  rasterSps <- getValues(ldSpsRaster) # 5 0 3 4 6 7
  # read-in productivity  levels
  prodRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/prod_TestArea.tif"))
  RasterValue <- getValues(prodRaster)#1 2 3 0
  # read-in spatial units
  spuRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
  spatial_unit_id <- getValues(spuRaster) #28 27
  
  # make it a data.table
  level2DT <- as.data.table(cbind(ages,rasterSps,RasterValue,spatial_unit_id))
  level2DT1 <- unique(level2DT) # 820 4
  level2DT1 <- level2DT1[level2DT1$rasterSps>0,] # 759   4
  setkey(level2DT1,rasterSps,RasterValue,spatial_unit_id)

  
  # add the gcID
  gcID <- read.csv(file.path(getwd(),"data/forIan/SK_data/gcID_ref.csv"))
  gcID <- as.data.table(gcID[,-1])
  setkey(gcID,rasterSps,RasterValue,spatial_unit_id)
  
  level3DT <- merge(level2DT1, gcID, all.x=TRUE) #759   8
  ############################################################
  
  
  sim$ages <- level3DT[,ages]#c(0)#,2,3,140)
  sim$nStands <- length(sim$ages)
  standIdx <- 1:sim$nStands
  sim$gcids <- level3DT[,growth_curve_component_id]#c(1)#,2,3,101)
  sim$historicDMIDs <- rep.int(214,sim$nStands)#c(214)#,1,1,1)
  sim$lastPassDMIDS <- rep.int(214,sim$nStands)#c(214)#,1,1,1)
  sim$delays <-  rep.int(0,sim$nStands)#c(0)#,0,0,0)
  sim$minRotations <- rep(0, sim$nStands)
  sim$maxRotations <- rep(100, sim$nStands)
  sim$returnIntervals <- merge(level3DT,sim$cbmData@spinupParameters[,c(1,2)], by="spatial_unit_id", all.x=TRUE)[,9] #c(200)#,110,120,130)
  sim$spatialUnits <- level3DT[,spatial_unit_id]#rep(26, sim$nStands)
  spu <- as.data.frame(spatialUnitIds)
  ecoToSpu <- as.data.frame(spatialUnitIds[which(spu$SpatialUnitID %in% unique(gcID$spatial_unit_id)),c(1,3)])
  names(ecoToSpu) <- c("spatial_unit_id","ecozones")
  sim$ecozones <- merge.data.frame(level3DT,ecoToSpu,by="spatial_unit_id", all.x=TRUE)[,9]#rep(5, sim$nStands)
  
  # no change in disturbance for now
  sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))
  colnames(sim$disturbanceEvents)<-c("standIndex", "Year", "DisturbanceMatrixId")
  
  
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
  dataPath <- file.path(modulePath(sim),currentModule(sim),"data")
  if(is.null(sim$sqlDir))
    sim$sqlDir <- file.path(dataPath,"cbm_defaults")
  if(is.null(sim$dbPath))
    sim$dbPath <- file.path(dataPath, "cbm_defaults", "cbm_defaults.db")
  if(is.null(sim$gcurveFileName))
    sim$gcurveFileName <- file.path(dataPath, "SK_ReclineRuns30m", "LookupTables", "yieldRCBM.csv")
  if(is.null(sim$gcurveComponentsFileName))
    sim$gcurveComponentsFileName <- file.path(dataPath, "SK_ReclineRuns30m", "LookupTables", "yieldComponentRCBM.csv")
  
  
  if(is.null(sim$cbmData)){
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
  if (is.null(sim$pooldef)) 
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
