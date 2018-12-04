
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "spadesCBMcore",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9007", spadesCBMcore = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spadesCBMcore.Rmd"),
  reqdPkgs = list("Rcpp","raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),    
    defineParameter("spinupDebug", "logical", FALSE, NA, NA, "If TRUE spinupResult will be outputed to a text file (spinup.csv). FALSE means no ouput of the spinupResult"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "cbmData", objectClass = "dataset", desc = NA, sourceURL = NA),
    expectsInput(objectName = "pooldef", objectClass = "character", desc = "Vector of names (characters) for each of the carbon pools, with `Input` being the first one", sourceURL = NA),
    expectsInput(objectName = "PoolCount", objectClass = "numeric", desc = "Length of pooldef", sourceURL = NA),
    expectsInput(objectName = "pools", objectClass = "matrix", desc = NA, sourceURL = NA),
    expectsInput(objectName = "ages", objectClass = "numeric", desc = "Ages of the stands from the inventory in 1990", sourceURL = NA),
    expectsInput(objectName = "gcids", objectClass = "numeric", desc = "The identification of which growth curves to use on the specific stands provided by...", sourceURL = NA),
    expectsInput(objectName = "historicDMIDs", objectClass = "numeric", desc = "Vector, one for each stand, indicating historical disturbance type, linked to the S4 table called cbmData. Only Spinup.", sourceURL = NA),
    expectsInput(objectName = "lastPassDMIDS", objectClass = "numeric", desc = "Vector, one for each stand, indicating final disturbance type, linked to the S4 table called cbmData. Only Spinup.", sourceURL = NA),
    expectsInput(objectName = "delays", objectClass = "numeric", desc = "Vector, one for each stand, indicating regeneration delay post disturbance. Only Spinup.", sourceURL = NA),
    expectsInput(objectName = "minRotations", objectClass = "numeric", desc = "Vector, one for each stand, indicating minimum number of rotations. Only Spinup.", sourceURL = NA),
    expectsInput(objectName = "maxRotations", objectClass = "numeric", desc = "Vector, one for each stand, indicating maximum number of rotations. Only Spinup.", sourceURL = NA),
    expectsInput(objectName = "returnIntervals", objectClass = "numeric", desc = "Vector, one for each stand, indicating the fixed fire return interval. Only Spinup.", sourceURL = NA),
    expectsInput(objectName = "spatialUnits", objectClass = "numeric", desc = "The id given to the intersection of province and ecozones across Canada, linked to the S4 table called cbmData", sourceURL = NA),
    expectsInput(objectName = "ecozones", objectClass = "numeric", desc = "Vector, one for each stand, indicating the numeric represenation of the Canadian ecozones, as used in CBM-CFS3", sourceURL = NA),
    expectsInput(objectName = "disturbanceRasters", objectClass = "raster", desc = "Character vector of the disturbance rasters for SK"),
    expectsInput(objectName = "disturbanceEvents", objectClass = "matrix", desc = "3 column matrix, PixelGroupID, Year, and DisturbanceMatrixId. Not used in Spinup.", sourceURL = NA),
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "level3DT", objectClass = "data.table", desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #createsOutput(objectName = "processes", objectClass = "list", desc = NA),
    #createsOutput(objectName = "decayRates", objectClass = "list", desc = NA),
    createsOutput(objectName = "opMatrixCBM", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "spinupResult", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "allProcesses", objectClass = "list", desc = "A list of the constant processes, anything NULL is just a placeholder for dynamic processes"),
    createsOutput(objectName = "cbmPools", objectClass = "data.frame", desc = "Three parts: PixelGroupID, Age, and Pools "),
    #createsOutput(objectName = "disturbanceEvents", objectClass = "matrix", desc = "3 column matrix, PixelGroupID, Year, and DisturbanceMatrixId. Not used in Spinup."),
    createsOutput(objectName = "yearEvents", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "pools", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "ages", objectClass = "numeric", desc = "Ages of the stands after simulation")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.spadesCBMcore = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      #sim <- Init(sim) #? can I call the function something else then Init?
      sim <- spinup(sim) ## this is the spinup
      if(P(sim)$spinupDebug)
        sim <- scheduleEvent(sim, start(sim), "spadesCBMcore", "saveSpinup")
      #sim <- postSpinup(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "spadesCBMcore", "postSpinup")
      #sim <- scheduleEvent(sim, start(sim), "spadesCBMcore", "annual")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "spadesCBMcore", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "spadesCBMcore", "save")
    },
    saveSpinup = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      colnames(sim$spinupResult) <- c( c("PixelGroupID", "age"), sim$pooldef)
      write.csv(file = file.path(outputPath(sim), "spinup.csv"), sim$spinupResult)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "spadesCBMcore", "templateEvent")
      
      # ! ----- STOP EDITING ----- ! #
    },
    annual = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- annual(sim)
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spadesCBMcore", "plot")
      sim <- scheduleEvent(sim, time(sim) + 1, "spadesCBMcore", "annual")
      if(time(sim)==end(sim))
        sim <- scheduleEvent(sim, end(sim), "spadesCBMcore", "savePools", .last())
      # ! ----- STOP EDITING ----- ! #
    },
    postSpinup = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- postSpinup(sim)
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spadesCBMcore", "plot")
      sim <- scheduleEvent(sim, time(sim), "spadesCBMcore", "annual")
      # ! ----- STOP EDITING ----- ! #
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #Plot(objectFromModule) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spadesCBMcore", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    savePools = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      colnames(sim$cbmPools) <- c( c("PixelGroupID", "age"), sim$pooldef)
      write.csv(file = file.path(outputPath(sim),"output1stand.csv"), sim$cbmPools)
      
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "spadesCBMcore", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "spadesCBMcore", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "spadesCBMcore", "templateEvent")

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
spinup <- function(sim) {
    opMatrix <- cbind(
    1:sim$nStands, #growth 1
    sim$ecozones, #domturnover
    sim$ecozones, #bioturnover
    1:sim$nStands, #overmature
    1:sim$nStands, #growth 2
    sim$spatialUnits, #domDecay
    sim$spatialUnits, #slow decay
    rep(1, sim$nStands) #slow mixing
  )
  #don't need this...all rootParameters are constant across all spatial units
  #ourSpus <- unique(sim$level3DT[,3]) 
  #roots <- sim$cbmData@rootParameters[sim$cbmData@rootParameters[,1] %in% ourSpus$spatial_unit_id,]
  # for these spatial units, turnover parameters are also the same
  #ourEcos <- sim$cbmData@spatialUnitIds[sim$cbmData@spatialUnitIds[,1] %in% ourSpus$spatial_unit_id,3]
  #turn <- sim$cbmData@turnoverRates[sim$cbmData@turnoverRates[,1] %in% ourEcos,]
  
    # try making the rootParameter the same length as the rest of the vectors
  # root1 <- t(sim$cbmData@rootParameters[1,-1])
  # SpatialUnitID <- sim$level3DT[,3]
  # root2 <- cbind(SpatialUnitID,root1)
    

  sim$spinupResult <- Spinup(pools = sim$pools, 
                             opMatrix = opMatrix,
                             constantProcesses = sim$processes,
                             growthIncrements = sim$gcHash, 
                             ages = sim$ages, 
                             gcids = sim$gcids,
                             historicdmids = sim$historicDMIDs, 
                             lastPassdmids = sim$lastPassDMIDS, 
                             delays = sim$delays, 
                             minRotations = sim$minRotations, 
                             maxRotations = sim$maxRotations,
                             returnIntervals = sim$returnIntervals$return_interval, 
                             rootParameters = as.data.frame(t(sim$cbmData@rootParameters[1,])),
                             turnoverParams = as.data.frame(t(sim$cbmData@turnoverRates[1,])),
                             biomassToCarbonRate = as.numeric(sim$cbmData@biomassToCarbonRate),
                             debug=P(sim)$spinupDebug)
  
  
  
  return(invisible(sim))
}


postSpinup <- function(sim) {
  
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  if(P(sim)$spinupDebug){
    opMatrix <- cbind(
      1:sim$nStands, #growth 1
      sim$ecozones, #domturnover
      sim$ecozones, #bioturnover
      1:sim$nStands, #overmature
      1:sim$nStands, #growth 2
      sim$spatialUnits, #domDecay
      sim$spatialUnits, #slow decay
      rep(1, sim$nStands) #slow mixing
    )

    sim$spinupResult <- Spinup(pools = sim$pools, 
                               opMatrix = opMatrix,
                               constantProcesses = sim$processes,
                               growthIncrements = sim$gcHash, 
                               ages = sim$ages, 
                               gcids = sim$gcids, 
                               historicdmids = sim$historicDMIDs, 
                               lastPassdmids = sim$lastPassDMIDS, 
                               delays = sim$delays, 
                               minRotations = sim$minRotations, 
                               maxRotations = sim$maxRotations,
                               returnIntervals = sim$returnIntervals$return_interval, 
                               rootParameters = as.data.frame(t(sim$cbmData@rootParameters[1,])),
                               turnoverParams = as.data.frame(t(sim$cbmData@turnoverRates[1,])),
                               biomassToCarbonRate = as.numeric(sim$cbmData@biomassToCarbonRate),
                               debug=FALSE)
  }
  
  sim$pools <- sim$spinupResult
  
  
  # set up the constant processes, anything NULL is just a 
  # placeholder for dynamic processes
  sim$allProcesses <- list(
    Disturbance=sim$processes$disturbanceMatrices,
    Growth1=NULL, 
    DomTurnover=sim$processes$domTurnover,
    BioTurnover=sim$processes$bioTurnover,
    OvermatureDecline=NULL, 
    Growth2=NULL, 
    DomDecay=sim$processes$domDecayMatrices,
    SlowDecay=sim$processes$slowDecayMatrices,
    SlowMixing=sim$processes$slowMixingMatrix
  )
  
  sim$opMatrixCBM <- cbind(
    rep(0, sim$nStands), #disturbance
    1:sim$nStands, #growth 1
    sim$ecozones, #domturnover
    sim$ecozones, #bioturnover
    1:sim$nStands, #overmature
    1:sim$nStands, #growth 2
    sim$spatialUnits, #domDecay
    sim$spatialUnits, #slow decay
    rep(1, sim$nStands) #slow mixing
  )
  
  colnames(sim$opMatrixCBM) <- c("disturbance", "growth 1", "domturnover", 
                                 "bioturnover", "overmature", "growth 2",
                                 "domDecay", "slow decay", "slow mixing")
  
  
  sim$cbmPools<-NULL
  
  
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
Plot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

annual <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # compute the growth increments

    growthAndDecline <- ComputeGrowthAndDeclineMatrices2(
    growthIncrements = sim$gcHash,
    ages = sim$ages,
    gcids = sim$gcids,
    pools = sim$pools,
    rootParameters = as.data.frame(t(sim$cbmData@rootParameters[1,])),
    turnoverParams = as.data.frame(t(sim$cbmData@turnoverRates[1,])),
    biomassToCarbonRate = as.numeric(sim$cbmData@biomassToCarbonRate),
    swMult = 0.5, hwMult = 0.5)
  
  sim$allProcesses$Growth1=growthAndDecline$Growth
  sim$allProcesses$Growth2=growthAndDecline$Growth
  sim$allProcesses$OvermatureDecline=growthAndDecline$OvermatureDecline
  
  eventDMIDs <- rep(0,sim$nStands)
  #annualDisturbance <- raster(grep(sim$disturbanceRasters, pattern = paste0(time(spadesCBMout)[1],".tif$"), value = TRUE))
  # yearEvents <- getValues(annualDisturbance)
  # length(which(!is.na(yearEvents)))
  #pixels <- getValues(spadesCBMout$masterRaster)
  #yearEvents <- getValues(annualDisturbance) %>% .[pixels != 0] #same length as spatialDT
  # now we need the disturbance_matrix_id
  
  
  #aggregate(disturbance_matrix_id ~ spatial_unit_id, data = a[which(grepl("wildfire",a[,3],ignore.case = TRUE)),], max)
  
  #sim$disturbanceEvents <- cbind(sim$level3DT$PixelGroupID,rep(2001,sim$nStands),rep(214,sim$nStands))
  colnames(disturbanceEvents)<-c("PixelGroupID", "Year", "DisturbanceMatrixId")
  sim$yearEvents <- sim$disturbanceEvents[which(sim$disturbanceEvents[,"Year"]==time(sim)),c("PixelGroupID","DisturbanceMatrixId"),drop=FALSE]
  
  if(nrow(sim$yearEvents)>0){
    for(e in 1:nrow(sim$yearEvents)) {
      eventDMIDs[sim$yearEvents[e,"PixelGroupID"]] <- sim$yearEvents[e,"DisturbanceMatrixId"]
      sim$ages[sim$yearEvents[e,"PixelGroupID"]] <- 0
    }
  }
  
  sim$opMatrixCBM[,"disturbance"]<-eventDMIDs
  
  sim$pools <- StepPools(pools=sim$pools, 
                         opMatrix = sim$opMatrixCBM, 
                         flowMatrices = sim$allProcesses)
  sim$ages <- sim$ages+1
  sim$cbmPools <- rbind(sim$cbmPools, cbind(level3DT$PixelGroupID, sim$ages, sim$pools))

  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  if (!suppliedElsewhere("pooldef", sim)){
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
    
    # These objects currently are required to be in the .GlobalEnv
    #   due to cpp code in RCBMGrowthIncrements.cpp. This should be
    #   changed in the cpp and also here so it is in the sim
    for(i in 1:length(sim$pooldef)){
      assign(sim$pooldef[i], i, envir = .GlobalEnv)#?.globals
    }
  }
  
  if (!suppliedElsewhere("cbmData", sim)){
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
  
  
  if(!suppliedElsewhere(sim$PoolCount))
    sim$PoolCount <- length(sim$pooldef)
  if(!suppliedElsewhere(sim$pools)){
    sim$pools <- matrix(ncol = sim$PoolCount, nrow=1, data=0)
    colnames(sim$pools)<- sim$pooldef
    sim$pools[,"Input"] = rep(1.0, nrow(sim$pools))
    }
  if(!suppliedElsewhere(sim$ages)){
    sim$ages <- c(0)#,2,3,140)
    sim$nStands <- length(sim$ages)
    standIdx <- 1:sim$nStands
  }
  if(!suppliedElsewhere(sim$gcids))
    sim$gcids <- c(1)#,2,3,101)
  if(!suppliedElsewhere(sim$historicDMIDs))
    sim$historicDMIDs <- c(214)#,1,1,1)
  if(!suppliedElsewhere(sim$lastPassDMIDS))
    sim$lastPassDMIDS <- c(214)#,1,1,1)
  if(!suppliedElsewhere(sim$delays))
    sim$delays <- c(0)#,0,0,0)
  if(!suppliedElsewhere(sim$minRotations))
    sim$minRotations <- rep(0, sim$nStands)
  if(!suppliedElsewhere(sim$maxRotations))
    sim$maxRotations <- rep(100, sim$nStands)
  if(!suppliedElsewhere(sim$returnIntervals))
    sim$returnIntervals <- c(200)#,110,120,130)
  if(!suppliedElsewhere(sim$spatialUnits))
    sim$spatialUnits <- rep(26, sim$nStands)
  if(!suppliedElsewhere(sim$ecozones))
    sim$ecozones <- rep(5, sim$nStands)
  if(!suppliedElsewhere(sim$disturbanceEvents)){sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))
  colnames(sim$disturbanceEvents)<-c("PixelGroupID", "Year", "DisturbanceMatrixId")
  }
  dataPath <- file.path(modulePath(sim),"data")
  if(!suppliedElsewhere(sim$dbPath))
    sim$dbPath <- file.path(dataPath, "cbm_defaults", "cbm_defaults.db")
  if(!suppliedElsewhere(sim$gcurveFileName))
    sim$gcurveFileName <- file.path(dataPath, "yieldRCBM.csv")#"SK_ReclineRuns30m", "LookupTables",
  if(!suppliedElsewhere(sim$gcurveComponentsFileName))
    sim$gcurveComponentsFileName <- file.path(dataPath, "yieldComponentSK.csv")#"SK_ReclineRuns30m", "LookupTables", 
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
  
}
### add additional events as needed by copy/pasting from above

Sys.setenv(PKG_CXXFLAGS = "-std=c++0x")
#sourceCpp(file='RCBMStep.cpp')
Rcpp::sourceCpp(file='RCBMGrowthIncrements.cpp', cacheDir = cachePath(sim), 
          env = envir(sim)[["spadesCBMcore"]])

