
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
    defineParameter("noAnnualDisturbances", "logical", FALSE, NA, NA, "If TRUE the sim$allProcesses and sim$opMatrix are created in the postSpinup event, just once. By default, these are recreated everyyear in the annual event"),
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
    expectsInput(objectName = "pools", objectClass = "matrix", desc = "empty matrix for storage of spinupResult", sourceURL = NA),
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
    expectsInput(objectName = "mySpuDmids", objectClass = "data.frame", desc = "the table containing one line per pixel"),
    #expectsInput(objectName = "disturbanceEvents", objectClass = "matrix", desc = "3 column matrix, PixelGroupID, Year (that sim year), and DisturbanceMatrixId. Not used in Spinup.", sourceURL = NA),
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "level3DT", objectClass = "data.table", desc = NA, sourceURL = NA),
    expectsInput(objectName = "spatialDT", objectClass = "data.table", desc = "the table containing one line per pixel")
    
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #createsOutput(objectName = "processes", objectClass = "list", desc = NA),
    #createsOutput(objectName = "decayRates", objectClass = "list", desc = NA),
    createsOutput(objectName = "opMatrixCBM", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "spinupResult", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "allProcesses", objectClass = "list", desc = "A list of the constant processes, anything NULL is just a placeholder for dynamic processes"),
    createsOutput(objectName = "pixelGroupC", objectClass = "data.table", desc = "This is the data table that has all the vectors to create the inputs for the annual processes"),
    createsOutput(objectName = "cbmPools", objectClass = "data.frame", desc = "Three parts: pixelGroup, Age, and Pools "),
    #createsOutput(objectName = "disturbanceEvents", objectClass = "matrix", desc = "3 column matrix, PixelGroupID, Year, and DisturbanceMatrixId. Not used in Spinup."),
    createsOutput(objectName = "pixelKeep", objectClass = "data.table", desc = "Keeps the pixelIndex from spatialDT with each year's PixelGroupID as a column. This is to enable making maps of yearly output."),
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
      colnames(sim$spinupResult) <- c( c("pixelGroup", "age"), sim$pooldef)
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
      colnames(sim$cbmPools) <- c( c("simYear","pixelGroup", "age"), sim$pooldef)
      write.csv(file = file.path(outputPath(sim),"cPoolsPixelYear.csv"), sim$cbmPools)
      
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
  
  ## THIS IS WRONG
  # sim$pools for the next round needs have the spinupResult and the pixels
  # that will be disturbed in seperate lines
  sim$pools <- sim$spinupResult
  ## DO I NEED TO MAKE THIS sim$??
  sim$pixelGroupC <- cbind(sim$level3DT,sim$spinupResult)
  
  if(P(sim)$noAnnualDisturbances){
  
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
    
    
    
    
  }
  sim$cbmPools<-NULL
  
  # Keep the pixels from each simulation year (in the postSpinup event)
  # in the end (cPoolsPixelYear.csv), this should be the same length at this vector
  sim$spatialDT <- sim$spatialDT[order(sim$spatialDT$pixelIndex),]
  sim$pixelKeep <- sim$spatialDT[,.(pixelIndex,pixelGroup)]
  setnames(sim$pixelKeep,c("pixelIndex","pixelGroup0"))
  
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
  # not sure if this will be needed once I have figured out the recalculation of the data.table
  if(!P(sim)$noAnnualDisturbances){
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
    
    
    
  }  
  
  ###################################
  # WORKING HERE IN DISTURBANCES
  ###################################
  # 
  # 1. Read-in the disturbances
  # this raster is where we get our disturbances 
  annualDisturbance <- raster(grep(sim$disturbanceRasters, pattern = paste0(time(sim)[1],".tif$"), value = TRUE))
  pixels <- getValues(sim$masterRaster)
  yearEvents <- getValues(annualDisturbance) %>% .[pixels != 0] #same length as spatialDT
  
# Add this year's events to the spatialDT, so each disturbed pixels has its event
  sim$spatialDT <- sim$spatialDT[order(sim$spatialDT$pixelIndex)]
  sim$spatialDT <- sim$spatialDT[,events := yearEvents]
  
  # ###############################
  # # TRYING TO USE THE LandR fnct updatePixelCohortData()
  # ###############################
  # # working "outside" the sim for the moment
  # # create the spatialDT as it would be in sim$spatialDT to this point
  # 
  # newPixelCohortData <- spatialDT[!is.na(events),.(pixelIndex, pixelGroup, spatial_unit_id, growth_curve_component_id, ages, events)]
  # # I think level3DT is the "cohortData"
  # cohortData <- level3DT
  # 
  # masterRaster <- spadesCBMout$masterRaster
  # masterRaster[masterRaster == 0] <- NA 
  # masterRaster[!is.na(masterRaster)] <- spatialDT$pixelGroup
  # 
  # b <- LandR::updateCohortData(newPixelCohortData,cohortData,masterRaster)
  # ## it is looking for LandR specific columns...
  # ### ERROR
  # # Regenerating open and pixels with B (likely after seed dispersal)
  # # Error in `[.data.table`(cohorts, , c("pixelIndex", columnsForPG), with = FALSE) : 
  # #   column(s) not found: ecoregionGroup, speciesCode, B
  # # Called from: `[.data.table`(cohorts, , c("pixelIndex", columnsForPG), 
  # #                             with = FALSE)
  # 
  
  ################################
  ### NEED TO ADD sim$ to all this after it works#########
  # Trying just adding the lines to groups that are disturbed.
  distPixels <- sim$spatialDT[!is.na(events),.(pixelIndex, pixelGroup, spatial_unit_id, growth_curve_component_id, ages, events)]
  ## ok so...let's try this: I think the ages can be changed prior to the
  ## processing in the C++ functions because the first thing that happens is
  ## disturbances and presently **all disturbances are stand replacing**. Set
  ## all ages to 0 in the disturbed pixels
  distPixels$ages <- 0
  # just in case keep the pixelIndex and pixelGroup
  # right now this is not used anywhere 
  distKeep <- distPixels[,1:3]
  
  # just add the rows with the 
  groupToAddC <- sim$pixelGroupC[which(sim$pixelGroupC$pixelGroup %in% unique(distPixels$pixelGroup)),]
  setkey(groupToAddC,pixelGroup,spatial_unit_id, growth_curve_component_id)
  
  
  #calculate the new pixelGroup values
  maxPixelGroup <- max(sim$spatialDT$pixelGroup)
  # NOTE: different carbon transactions will apply to different events, so, keep
  # them seperate pre-C++ processing
  distPixels$newGroup <- LandR::generatePixelGroups(distPixels[,-("pixelGroup")],maxPixelGroup,
                                         columns = c("spatial_unit_id", "growth_curve_component_id", "ages", "events"))
  # # adding the new pixelGroup to the pixelKeep
  trackPix <- sim$spatialDT$pixelGroup
  trackPix[which(!is.na(sim$spatialDT$events))] <- distPixels$newGroup
    sim$pixelKeep[,newPix := trackPix]
  setnames(sim$pixelKeep,"newPix",paste0("pixelGroup",time(sim)))
  # 
  # match the pixelGroup of the carbon (groupToAddC) with the pixelGroup of the
  # distPixels. This is to make sure we don't miss a newGroup...since we have
  # events there might be more of these.
  uniqueNewGroup <- unique(distPixels[,-("pixelIndex")]) #60
  setkey(uniqueNewGroup,pixelGroup,spatial_unit_id, growth_curve_component_id)
  
  toAdd <- merge(uniqueNewGroup,groupToAddC[,-("ages")],all.x=TRUE)#,on = c("pixelGroup")]
  toAdd <- toAdd[,pixelGroup:=newGroup] 
  ## HERE IS WHERE THE EVENTS GET TAKEN OUT...
  # BEFORE WE DO...need to figure out eventsDMIDs
  DMIDS <- merge(toAdd,sim$mySpuDmids, by=c("spatial_unit_id","events"),all.x=TRUE)[,disturbance_matrix_id]
  # not quite the right length yet
  
  toAdd <- toAdd[,-c("newGroup","events")]
  # rbind now matches column names for you
  pixelGroupForAnnual <- rbind(sim$pixelGroupC,toAdd)
  

  
  # Changing the vectors and matrices that need to be changed to process this year's growth
  sim$pools <- as.matrix(pixelGroupForAnnual[,Input:Products])
  eventDMIDS <- c(rep(0,dim(pixelGroupForAnnual)[1] - length(DMIDS)),DMIDS)
  ecoToSpu <- as.data.frame(sim$cbmData@spatialUnitIds[,c(1,3)])
  names(ecoToSpu) <- c("spatial_unit_id","ecozones")
  sim$ecozones <- merge(pixelGroupForAnnual,ecoToSpu)[,ecozones]
  
  ## from here change level3DT to pixelGroupForAnnual
  sim$ages <- pixelGroupForAnnual[,ages]
  sim$nStands <- length(sim$ages)
  

  sim$gcids <- pixelGroupForAnnual[,growth_curve_component_id]
  sim$spatialUnits <- pixelGroupForAnnual[,spatial_unit_id]

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

  
  # ! ----- EDIT BELOW ----- ! #
  # 
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
  
  # this has to be the same length as the DT going in for processing
  sim$opMatrixCBM[,"disturbance"]<-eventDMIDS

  sim$pools <- StepPools(pools=sim$pools, 
                         opMatrix = sim$opMatrixCBM, 
                         flowMatrices = sim$allProcesses)
  sim$pixelGroupC <- cbind(pixelGroupForAnnual[,ages:pixelGroup],sim$pools)
  sim$pixelGroupC$ages <- sim$pixelGroupC$ages+1
  sim$spatialDT$ages <- sim$spatialDT$ages+1
  sim$ages <- sim$pixelGroupC$ages

  sim$cbmPools <- rbind(sim$cbmPools, cbind(rep(time(sim)[1],length(sim$ages)),pixelGroupForAnnual$pixelGroup, sim$ages, sim$pools))

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
  # if(!suppliedElsewhere(sim$disturbanceEvents)){sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))
  # colnames(sim$disturbanceEvents)<-c("PixelGroupID", "Year", "DisturbanceMatrixId")
  #}
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

