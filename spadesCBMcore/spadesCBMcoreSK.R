
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
  reqdPkgs = list("Rcpp","raster", "quickPlot", "ggplot2"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),    
    defineParameter("spinupDebug", "logical", FALSE, NA, NA, "If TRUE spinupResult will be outputed to a text file (spinup.csv). FALSE means no ouput of the spinupResult"),
    defineParameter("noAnnualDisturbances", "logical", FALSE, NA, NA, "If TRUE the sim$allProcesses and sim$opMatrix are created in the postSpinup event, just once. By default, these are recreated everyyear in the annual event"),
    defineParameter("poolsToPlot", "character", "totalCarbon", NA, NA, 
                    desc = "which carbon pools to plot, if any. Defaults to total carbon"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "cbmData", objectClass = "dataset", desc = NA, sourceURL = NA),
    expectsInput(objectName = "masterRaster", objectClass = "raster", desc = "Raster has NAs where there are no species and the pixel groupID where the pixels were simulated. It is used to map results"),
    expectsInput(objectName = "processes", objectClass = "dataset", desc = NA, sourceURL = NA),
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
    createsOutput(objectName = "opMatrixCBM", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "spinupResult", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "allProcesses", objectClass = "list", desc = "A list of the constant processes, anything NULL is just a placeholder for dynamic processes"),
    createsOutput(objectName = "pixelGroupC", objectClass = "data.table", desc = "This is the data table that has all the vectors to create the inputs for the annual processes"),
    createsOutput(objectName = "cbmPools", objectClass = "data.frame", desc = "Three parts: pixelGroup, Age, and Pools "),
    #createsOutput(objectName = "disturbanceEvents", objectClass = "matrix", desc = "3 column matrix, PixelGroupID, Year, and DisturbanceMatrixId. Not used in Spinup."),
    createsOutput(objectName = "pixelKeep", objectClass = "data.table", desc = "Keeps the pixelIndex from spatialDT with each year's PixelGroupID as a column. This is to enable making maps of yearly output."),
    #createsOutput(objectName = "yearEvents", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "pools", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "ages", objectClass = "numeric", desc = "Ages of the stands after simulation"),
    createsOutput(objectName = "NPP", objectClass = "data.table", desc = "NPP for each pixelGroup"),
    createsOutput(objectName = "spatialDT", objectClass = "data.table", desc = "this is modified to associate the right pixel group to the pixel id after disturbances"),
    createsOutput(objectName = "level3DT", objectClass = "data.table", desc = "this is modified: ordered by pixelGroup"),
    createsOutput(objectName = "nStands", objectClass = "integer", desc = "number of pixelGroup in this annual run"),
    createsOutput(objectName = "gcids", objectClass = "vector", desc = "growth component id associated with each pixelGroup"),
    createsOutput(objectName = "spatialUnits", objectClass = "vector", desc = "spatial unit for each pixelGroup"),
    createsOutput(objectName = "ecozones", objectClass = "vector", desc = "ecozone for each pixelGroup"),
    createsOutput(objectName = "turnoverRates", objectClass = "data.table", desc = "table with turnover rates for SPUs")
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
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "spadesCBMcore", "plot", eventPriority = 9)
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
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spadesCBMcore", "plot",.last())
      sim <- scheduleEvent(sim, time(sim) + 1, "spadesCBMcore", "annual")
      if(time(sim)==end(sim))
        sim <- scheduleEvent(sim, end(sim), "spadesCBMcore", "savePools", .last())
      # ! ----- STOP EDITING ----- ! #
    },
    postSpinup = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- postSpinup(sim)
      sim$turnoverRates <- calcTurnoverRates(turnoverRates = sim$cbmData@turnoverRates,
                                             spatialUnitIds = sim$cbmData@spatialUnitIds, spatialUnits = sim$spatialUnits)
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spadesCBMcore", "plot")
      sim <- scheduleEvent(sim, time(sim), "spadesCBMcore", "annual")
      # ! ----- STOP EDITING ----- ! #
    },
    plot = {
      clearPlot()
      if (!time(sim) == start(sim)) {
        
        areaPlot(cbmPools = sim$cbmPools,
                masterRaster = sim$masterRaster)

        barPlot(cbmPools = sim$cbmPools,
                masterRaster = sim$masterRaster)
        NPPPlot(changeInNPP = sim$NPP,
                masterRaster = sim$masterRaster,
                spatialDT = sim$spatialDT,
                time = time(sim))
      }
      
      spatialPlot(cbmPools = sim$cbmPools,
                  poolsToPlot = P(sim)$poolsToPlot,
                  masterRaster = sim$masterRaster,
                  pixelkeep = sim$pixelKeep,
                  years = time(sim))

      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spadesCBMcore", "plot", eventPriority = 9)
    },
    savePools = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      colnames(sim$cbmPools) <- c( c("simYear","pixelCount","pixelGroup", "ages"), sim$pooldef)
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
  
  spinupResult <- Spinup(pools = sim$pools, 
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
  
  # setting CO2, CH4, CO and products to 0 before starting the simulations
  spinupResult[,23:dim(spinupResult)[2]] <- 0
  sim$spinupResult <- spinupResult
  return(invisible(sim))
}


postSpinup <- function(sim) {
  
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")
  # if(P(sim)$spinupDebug){
  #   opMatrix <- cbind(
  #     1:sim$nStands, #growth 1
  #     sim$ecozones, #domturnover
  #     sim$ecozones, #bioturnover
  #     1:sim$nStands, #overmature
  #     1:sim$nStands, #growth 2
  #     sim$spatialUnits, #domDecay
  #     sim$spatialUnits, #slow decay
  #     rep(1, sim$nStands) #slow mixing
  #   )
  # 
  #   sim$spinupResult <- Spinup(pools = sim$pools, 
  #                              opMatrix = opMatrix,
  #                              constantProcesses = sim$processes,
  #                              growthIncrements = sim$gcHash, 
  #                              ages = sim$ages, 
  #                              gcids = sim$gcids, 
  #                              historicdmids = sim$historicDMIDs, 
  #                              lastPassdmids = sim$lastPassDMIDS, 
  #                              delays = sim$delays, 
  #                              minRotations = sim$minRotations, 
  #                              maxRotations = sim$maxRotations,
  #                              returnIntervals = sim$returnIntervals$return_interval, 
  #                              rootParameters = as.data.frame(t(sim$cbmData@rootParameters[1,])),
  #                              turnoverParams = as.data.frame(t(sim$cbmData@turnoverRates[1,])),
  #                              biomassToCarbonRate = as.numeric(sim$cbmData@biomassToCarbonRate),
  #                              debug=FALSE)
  # }
  
  ## THIS IS WRONG
  # sim$pools for the next round needs have the spinupResult and the pixels
  # that will be disturbed in seperate lines
  sim$pools <- sim$spinupResult
  ## DO I NEED TO MAKE THIS sim$??
  sim$level3DT <- sim$level3DT[order(pixelGroup),]
  sim$pixelGroupC <- cbind(sim$level3DT,sim$spinupResult)
  
  # if(P(sim)$noAnnualDisturbances){
  # 
  #   sim$allProcesses <- list(
  #     Disturbance=sim$processes$disturbanceMatrices,
  #     Growth1=NULL, 
  #     DomTurnover=sim$processes$domTurnover,
  #     BioTurnover=sim$processes$bioTurnover,
  #     OvermatureDecline=NULL, 
  #     Growth2=NULL, 
  #     DomDecay=sim$processes$domDecayMatrices,
  #     SlowDecay=sim$processes$slowDecayMatrices,
  #     SlowMixing=sim$processes$slowMixingMatrix
  #   )
  #   
  #   sim$opMatrixCBM <- cbind(
  #     rep(0, sim$nStands), #disturbance
  #     1:sim$nStands, #growth 1
  #     sim$ecozones, #domturnover
  #     sim$ecozones, #bioturnover
  #     1:sim$nStands, #overmature
  #     1:sim$nStands, #growth 2
  #     sim$spatialUnits, #domDecay
  #     sim$spatialUnits, #slow decay
  #     rep(1, sim$nStands) #slow mixing
  #   )
  #   
  #   colnames(sim$opMatrixCBM) <- c("disturbance", "growth 1", "domturnover", 
  #                                  "bioturnover", "overmature", "growth 2",
  #                                  "domDecay", "slow decay", "slow mixing")
  #   
  #   
  #   
  #   
  # }
  sim$cbmPools<-NULL
  sim$NPP <- NULL
  
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


annual <- function(sim) {
  # not sure if this will be needed once I have figured out the recalculation of the data.table
  # special case if there are no disturbances
  # if(!P(sim)$noAnnualDisturbances){
  #   # set up the constant processes, anything NULL is just a 
  #   # placeholder for dynamic processes
  #   sim$allProcesses <- list(
  #     Disturbance=sim$processes$disturbanceMatrices,
  #     Growth1=NULL, 
  #     DomTurnover=sim$processes$domTurnover,
  #     BioTurnover=sim$processes$bioTurnover,
  #     OvermatureDecline=NULL, 
  #     Growth2=NULL, 
  #     DomDecay=sim$processes$domDecayMatrices,
  #     SlowDecay=sim$processes$slowDecayMatrices,
  #     SlowMixing=sim$processes$slowMixingMatrix
  #   )
  #   
  #   sim$opMatrixCBM <- cbind(
  #     rep(0, sim$nStands), #disturbance
  #     1:sim$nStands, #growth 1
  #     sim$ecozones, #domturnover
  #     sim$ecozones, #bioturnover
  #     1:sim$nStands, #overmature
  #     1:sim$nStands, #growth 2
  #     sim$spatialUnits, #domDecay
  #     sim$spatialUnits, #slow decay
  #     rep(1, sim$nStands) #slow mixing
  #   )
  #   
  #   colnames(sim$opMatrixCBM) <- c("disturbance", "growth 1", "domturnover", 
  #                                  "bioturnover", "overmature", "growth 2",
  #                                  "domDecay", "slow decay", "slow mixing")
  #   
  #   
  #   
  # }  

  ### Growing the undisturbed pixels
  pixelGroupForAnnual <- sim$pixelGroupC#[!(pixelGroup %in% groupOut),]
  pixelGroupForAnnual <- pixelGroupForAnnual[order(pixelGroup),]
  
  # Changing the vectors and matrices that need to be changed to process this year's growth
  sim$pools <- as.matrix(pixelGroupForAnnual[,Input:Products])
  # disturbances are processed below, outside the Rcpp functions
  eventDMIDs <- rep(0,dim(pixelGroupForAnnual)[1])##--##c( ...dim(pixelGroupForAnnual)[1] - length(DMIDS)),DMIDS)
  ecoToSpu <- as.data.frame(sim$cbmData@spatialUnitIds[,c(1,3)])
  names(ecoToSpu) <- c("spatial_unit_id","ecozones")
  ecozones <- merge(pixelGroupForAnnual,ecoToSpu)
  ecozones <- ecozones[order(pixelGroup),]
  sim$ecozones <- ecozones[,ecozones]
  sim$ages <- pixelGroupForAnnual[,ages]
  sim$nStands <- length(sim$ages)
  sim$gcids <- pixelGroupForAnnual[,growth_curve_component_id]
  sim$spatialUnits <- pixelGroupForAnnual[,spatial_unit_id]
  
  sim$opMatrixCBM <- cbind(
    rep(0,sim$nStands), #disturbance
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
  
  sim$opMatrixCBM[,"disturbance"]<-eventDMIDs
  
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
  #sim$opMatrixCBM[,"disturbance"]<-eventDMIDS

  sim$pools <- StepPools(pools=sim$pools, 
                         opMatrix = sim$opMatrixCBM, 
                         flowMatrices = sim$allProcesses)
  
  ###################################
  # DISTURBANCES COME IN HERE
  ###################################
  # 
  # 1. Read-in the disturbances
  # this raster is where we get our disturbances 
  
  annualDisturbance <- raster(grep(sim$disturbanceRasters, pattern = paste0(time(sim)[1],".grd$"), value = TRUE))
  pixels <- getValues(sim$masterRaster)
  yearEvents <- getValues(annualDisturbance) %>% .[pixels != 0] #same length as spatialDT
  
  # Add this year's events to the spatialDT, so each disturbed pixels has its event
  sim$spatialDT <- sim$spatialDT[order(sim$spatialDT$pixelIndex)]
  sim$spatialDT <- sim$spatialDT[,events := yearEvents]
  
  
  ################################
  
  ## get the disturbed pixels only
  # Trying just adding the lines to groups that are disturbed.
  distPixels <- sim$spatialDT[events>0,.(pixelIndex, pixelGroup, ages, rasterSps, Productivity,
                                         spatial_unit_id, growth_curve_component_id, growth_curve_id, events)]
  setkey(distPixels,pixelGroup)
  
  ## The ages can be changed prior to the
  ## processing in the C++ functions because the first thing that happens is
  ## disturbances and presently **all disturbances are stand replacing**. Set
  ## all ages to 0 in the disturbed pixels
  ## note that ages will be updated by 1 at the end of the annual event. This is
  ## necessary because the growth curves won't work is everything is at 0
  distPixels$ages <- 0
  
  # get the carbon info from the old pixelGroup for the disturbed pixels
  groupToAddC <- sim$pixelGroupC[which(sim$pixelGroupC$pixelGroup %in% unique(distPixels$pixelGroup)),-c("ages","rasterSps", "Productivity", "spatial_unit_id", "growth_curve_component_id", "growth_curve_id")]
  groupToAddC <- groupToAddC[,c("oldGroup","pixelGroup") := list(pixelGroup,NULL)]
  groupToAddC <- unique(groupToAddC)
  setkey(groupToAddC,oldGroup)
  
  #calculate the new pixelGroup values
  maxPixelGroup <- max(sim$spatialDT$pixelGroup)
  distPixels[,oldGroup:=pixelGroup]
  distPixels$newGroup <- LandR::generatePixelGroups(distPixels,maxPixelGroup,
                                                    columns = c("oldGroup","spatial_unit_id", "growth_curve_component_id", "ages", "events"))
  distPixels <- distPixels[,.(pixelIndex,ages,rasterSps,Productivity,spatial_unit_id, growth_curve_component_id,growth_curve_id, events, oldGroup,newGroup)]
  
  #adding the new pixelGroup to the pixelKeep
  trackPix <- sim$spatialDT[which(!(pixelIndex %in% distPixels$pixelIndex)),.(pixelIndex,pixelGroup)]
  newGroups <- distPixels[,.(pixelIndex,newGroup)] %>% .[,pixelGroup:=newGroup] %>% .[,newGroup:=NULL]
  trackPix2 <- rbind(trackPix,newGroups)
  trackPix2 <- trackPix2[order(pixelIndex),]
  
  sim$pixelKeep <- sim$pixelKeep[,newPix := trackPix2$pixelGroup]
  setnames(sim$pixelKeep,"newPix",paste0("pixelGroup",time(sim)))
  # MODIFIED FOR NO DISTURBANCES
  # sim$pixelKeep <- sim$pixelKeep[,newPix := sim$spatialDT$pixelGroup]
  # setnames(sim$pixelKeep,"newPix",paste0("pixelGroup",time(sim)))
  
  # change the vector of pixel group in $spatialDT to match trackPix for next annual cycle
  
  sim$spatialDT$pixelGroup <- trackPix2$pixelGroup
  # count the pixels in each new pixel group
  
  pixelCount <- sim$spatialDT[,.N,by=pixelGroup][order(pixelGroup),]
  pixelCount[which(is.na(pixelCount$N)),"N"] <- 0
  # THIS IS WHERE IS THERE IS A PIXEL GROUP WITH 0 PIXELS, WE SHOULD GET RID OF IT
  # group1 <- sort(unique(sim$spatialDT$pixelGroup))
  # group2 <- sort(unique(trackPix2$pixelGroup))
  # groupOut <- subset(group1, !(group1 %in% group2))
  #
  # match the pixelGroup of the carbon (groupToAddC) with the pixelGroup of the
  # distPixels. This is to make sure we don't miss a newGroup...since we have
  # events there might be some old groups that go to two groups.
  uniqueNewGroup <- unique(distPixels[,-("pixelIndex")]) 
  setkey(uniqueNewGroup,oldGroup)
  
  # this is where the equivalent of sim$level3DT and C pools for new groups are
  # put together
  
  toAdd <- merge(uniqueNewGroup,groupToAddC,all.x=TRUE)#,on = c("pixelGroup")]
  toAdd <- toAdd[,c("pixelGroup","newGroup") := list(newGroup,NULL)]
  toAdd <- toAdd[order(pixelGroup),]
  ## HERE IS WHERE THE EVENTS GET TAKEN OUT...
  # BEFORE WE DO...need to figure out eventsDMIDs
  
  DM <- merge(toAdd,sim$mySpuDmids, by=c("spatial_unit_id","events"),all.x=TRUE)
  DM <- DM[order(pixelGroup),]
  DMIDS <- DM$disturbance_matrix_id
  # not quite the right length yet
  
  toAdd <- toAdd[,c("oldGroup","events") := NULL]
  toAdd[,DMIDS := as.character(DMIDS)]  
  toAdd <- toAdd[order(pixelGroup),]
  
  ################# PROCESSING DISTURBANCES AND ANNUAL PROCESS FOR DISTURBED PIXELS ###################
  

  distMatrices <- mget(unique(toAdd$DMIDS),envir = sim$allProcesses$Disturbance)
  distMatrices <- lapply(distMatrices, as.data.table)
  distMats <- rbindlist(distMatrices, idcol = "DMIDS")
  # what is each column?
  #lapply(distMats,is)
  
  setnames(distMats, old = "value", new = "distValue")
  distMats[,row := as.character(row)]
  
  # need to do the melt first so that "row" in disturbances can be matched with Inputs:Products numbered 1:26
  # melt toAdd
  
  InputColNum <- grep("Input", colnames(toAdd))
  ProductsColNum <- grep("Products", colnames(toAdd))
  toAdd1 <- melt(toAdd, measure.vars = InputColNum:ProductsColNum, 
                 id.vars = c("pixelGroup", "DMIDS"))
  
  # change the Inputs:Products to 1:26 using this
  poolsToRows <- as.data.frame(cbind(pools = sim$pooldef,row = c(1:26)))
  names(poolsToRows) <- c("variable","row")
  toAdd2 <- toAdd1[poolsToRows,on="variable"]
  keycol <-c("pixelGroup","row")
  setorderv(toAdd2, keycol)
  
  toAdd3 <- toAdd2[distMats, on=c("DMIDS","row"), allow.cartesian = TRUE][,fluxOut:=(value*distValue)]
  setorderv(toAdd3,keycol)
  outC <- toAdd3[, .(outC = sum(fluxOut)), by=c("pixelGroup","row")]
  inC <-  toAdd3[, .(inC = sum(fluxOut)), by=c("pixelGroup","col")]
  inC[,row:=as.character(col)]
  #names(inC) <- c("pixelGroup","row","inC")
  fluxes <- inC[outC,on=c("pixelGroup","row")]
  # no NAs allowed
  fluxes$inC[which(is.na(fluxes$inC))] <- 0
  toAddDist <- toAdd2[fluxes,on=c("pixelGroup","row")][,.(calcDist = (value-outC+inC)),by=c("pixelGroup","row")]
  # pools can't go negative
  toAddDist[calcDist<0,"calcDist"] <- 0
  
  # ### THERE IS A LITTLE TINY BIT OF CARBON LEFT IN THE POOLS BECAUSE OF THE
  # ### INCONSISTENT NUMBER OF DECIMALS. NEED MAKE THE ROWS THAT ARE 100%
  # ### DISTURBED go to 0 - NOT DONE: the correct4$diffs that are NAs need to be
  # ### set to 0 before other c transactions occur. FIX ANOTHER TIME.
  # correct1 <- distMats[,.(sumR = sum(distValue)),by=c("DMIDS","row")]
  # correct2 <- distMats[,.(sumC = sum(distValue)),by=c("DMIDS","col")][,row:= as.character(col)]
  # correct3 <- correct2[correct1, on= c("DMIDS","row")]
  # correct4 <- correct3[,.(diffs = sumR - sumC), by=c("DMIDS","row")]
  # # failing here b/c the DMIDS are not in the toAddDist
  # #c5 <- correct4[,.(to0 = which(is.na(correct4$diffs)), by=c("DMIDS","row"))]
  
  
  toAddDist
  setorderv(toAddDist,keycol)
  ## DISTURBANCES CALCULATED
  
  # # for a visual check
  # distPg740 <- toAddDist[pixelGroup==740,.(row, calcDist)]
  # distPg740 <- distPg740[, row:= as.numeric(as.character(row))][order(row),]
  # chgVect <- cbind(toAdd1[pixelGroup==740],distPg740)
  
  
  ## calculate domTurn ########################
  # match with ecozones
  #toAdd[,ecoz:=as.character(spatial_unit_id)]  
  ecoToSpu <- as.data.table(ecoToSpu)
  tri1 <- merge(toAdd,ecoToSpu,by=c("spatial_unit_id"))
  tri2 <- tri1[,.(pixelGroup,ecozones)][,ecoz := as.character(ecozones)]
  ecoz <- ecoToSpu[ecoToSpu$spatial_unit_id %in% unique(toAdd$spatial_unit_id),"ecozones"]
  domTurnMatrices <- mget(as.character(t(ecoz)),envir = sim$allProcesses$DomTurnover)  
  domTurnMatrices <- lapply(domTurnMatrices, as.data.table)
  domTurnMats <- rbindlist(domTurnMatrices , idcol = "ecoz")
  
  setnames(domTurnMats, old = "value", new = "domTurnValue")
  domTurnMats[,row := as.character(row)]
  domT1 <- toAddDist[tri2,on="pixelGroup"]
  
  domT2 <- domT1[domTurnMats,on=c("ecoz","row") , allow.cartesian=TRUE][,fluxOut:=(calcDist*domTurnValue)]
  outC <- domT2[, .(outC = sum(fluxOut)), by=c("pixelGroup","row")]
  inC <-  domT2[, .(inC = sum(fluxOut)), by=c("pixelGroup","col")]
  inC[,row:=as.character(col)]
  #names(inC) <- c("pixelGroup","row","inC")
  domTfluxes <- inC[outC,on=c("pixelGroup","row")]

  toAddDomT <- toAddDist[domTfluxes,on=c("pixelGroup","row")][,.(calcDomT = (calcDist-outC+inC)),by=c("pixelGroup","row")]
  # pools can't go negative
  toAddDomT [calcDomT<0,"calcDomT"] <- 0
  ## END calculate domTurn ########################
  
  # # for a visual check
  # domTpg740 <- toAddDomT[pixelGroup==740,.(row, calcDomT)]
  # domTpg740 <- domTpg740[, row:= as.numeric(as.character(row))][order(row),]
  # chgVect[,domTpg740:=domTpg740$calcDomT]
  
  ## calculate bioTurn ########################
  # match with ecozones as above

  bioTurnMatrices <- mget(as.character(t(ecoz)),envir = sim$allProcesses$BioTurnover)  
  bioTurnMatrices <- lapply(bioTurnMatrices, as.data.table)
  bioTurnMats <- rbindlist(bioTurnMatrices , idcol = "ecoz")
  
  setnames(bioTurnMats, old = "value", new = "bioTurnValue")
  bioTurnMats[,row := as.character(row)]
  bioT1 <- toAddDomT[tri2,on="pixelGroup"]
  bioT2 <- bioT1[bioTurnMats,on=c("ecoz","row") , allow.cartesian=TRUE][,fluxOut:=(calcDomT*bioTurnValue)]
  outC <- bioT2[, .(outC = sum(fluxOut)), by=c("pixelGroup","row")]
  inC <-  bioT2[, .(inC = sum(fluxOut)), by=c("pixelGroup","col")]
  inC[,row:=as.character(col)]
  #names(inC) <- c("pixelGroup","row","inC")
  bioTfluxes <- inC[outC,on=c("pixelGroup","row")]
  
  toAddbioT <- toAddDomT[bioTfluxes,on=c("pixelGroup","row")][,.(calcbioT = (calcDomT-outC+inC)),by=c("pixelGroup","row")]
  # pools can't go negative
  toAddbioT[calcbioT<0,"calcbioT"]  <- 0
  ## END calculate bioTurn ########################
  
  # # for a visual check
  # bioTpg740 <- toAddbioT[pixelGroup==740,.(row, calcbioT)]
  # bioTpg740 <- bioTpg740[, row:= as.numeric(as.character(row))][order(row),]
  # chgVect[,bioTpg740:=bioTpg740$calcbioT]
  
  ## calculate domDecay ########################
  # match with spatial units
  domDecayMatrices <- mget(as.character(unique(toAdd$spatial_unit_id)),envir = sim$allProcesses$DomDecay)  
  domDecayMatrices <- lapply(domDecayMatrices, as.data.table)
  domDecayMats <- rbindlist(domDecayMatrices , idcol = "spu")
  
  setnames(domDecayMats, old = "value", new = "domDecayValue")
  domDecayMats[,row := as.character(row)]
  spuAdd <- toAdd[,.(pixelGroup,spatial_unit_id)][,spu := as.character(spatial_unit_id)]
  domD1 <- toAddbioT[spuAdd,on="pixelGroup"]
  domD2 <- domD1[domDecayMats,on=c("spu","row") , allow.cartesian=TRUE][,fluxOut:=(calcbioT*domDecayValue)]
  outC <- domD2[, .(outC = sum(fluxOut)), by=c("pixelGroup","row")]
  inC <-  domD2[, .(inC = sum(fluxOut)), by=c("pixelGroup","col")]
  inC[,row:=as.character(col)]
  #names(inC) <- c("pixelGroup","row","inC")
  domDfluxes <- inC[outC,on=c("pixelGroup","row")]
  
  toAddDomD <- toAddbioT[domDfluxes,on=c("pixelGroup","row")][,.(calcDomD = (calcbioT-outC+inC)),by=c("pixelGroup","row")]
  # pools can't go negative
  toAddDomD[calcDomD<0,"calcDomD"]  <- 0
  ## END calculate domDecay ########################
  
  # # for a visual check
  # domDpg740 <- toAddDomD[pixelGroup==740,.(row, calcDomD)]
  # domDpg740 <- domDpg740[, row:= as.numeric(as.character(row))][order(row),]
  # chgVect[,domDpg740:=domDpg740$calcDomD]
  
  
  ## calculate slowDecay ########################
  # match with spatial units
  slowDecayMatrices <- mget(as.character(unique(toAdd$spatial_unit_id)),envir = sim$allProcesses$SlowDecay)  
  slowDecayMatrices <- lapply(slowDecayMatrices, as.data.table)
  slowDecayMats <- rbindlist(slowDecayMatrices , idcol = "spu")
  
  setnames(slowDecayMats, old = "value", new = "slowDecayValue")
  slowDecayMats[,row := as.character(row)]
  #spuAdd <- toAdd[,.(pixelGroup,spatial_unit_id)][,spu := as.character(spatial_unit_id)]
  slowD1 <- toAddDomD[spuAdd,on="pixelGroup"]
  slowD2 <- slowD1[slowDecayMats,on=c("spu","row") , allow.cartesian=TRUE][,fluxOut:=(calcDomD*slowDecayValue)]
  outC <- slowD2[, .(outC = sum(fluxOut)), by=c("pixelGroup","row")]
  inC <-  slowD2[, .(inC = sum(fluxOut)), by=c("pixelGroup","col")]
  inC[,row:=as.character(col)]
  #names(inC) <- c("pixelGroup","row","inC")
  slowDfluxes <- inC[outC,on=c("pixelGroup","row")]
  
  toAddslowD <- toAddDomD[slowDfluxes,on=c("pixelGroup","row")][,.(calcSlowD = (calcDomD-outC+inC)),by=c("pixelGroup","row")]
  # pools can't go negative
  toAddslowD[calcSlowD<0,"calcSlowD"]  <- 0
  ## END calculate slowDecay ########################
  
  # # for a visual check
  # slowDpg740 <- toAddslowD[pixelGroup==740,.(row, calcSlowD)]
  # slowDpg740 <- slowDpg740[, row:= as.numeric(as.character(row))][order(row),]
  # chgVect[,slowDpg740:=slowDpg740$calcSlowD]
  
  ## calculate slowMix ########################
  # only one matrixHERE
  slowMixMatrices <- as.data.table(mget(as.character(1),envir = sim$allProcesses$SlowMixing))
  slowMixMats <-  setnames(slowMixMatrices,names(slowMixMatrices),new = c("row","col","slowMixValue"))
   slowMixMats[,row := as.character(row)]
  #spuAdd <- toAdd[,.(pixelGroup,spatial_unit_id)][,spu := as.character(spatial_unit_id)]
  slowM1 <- toAddslowD[slowMixMats,on="row", allow.cartesian=TRUE][,fluxOut:=(calcSlowD*slowMixValue)]
  outC <- slowM1[, .(outC = sum(fluxOut)), by=c("pixelGroup","row")]
  inC <-  slowM1[, .(inC = sum(fluxOut)), by=c("pixelGroup","col")]
  inC[,row:=as.character(col)]
  #names(inC) <- c("pixelGroup","row","inC")
  slowMfluxes <- inC[outC,on=c("pixelGroup","row")]
  
  toAddslowM <- toAddslowD[slowMfluxes,on=c("pixelGroup","row")][,.(calcSlowM = (calcSlowD-outC+inC)),by=c("pixelGroup","row")]
  # pools can't go negative
  toAddslowM[calcSlowM<0,"calcSlowM"]  <- 0
  toAddslowM[,row:=as.numeric(as.character(row))]
  
  ## END calculate slowMix ########################  
  
  # # for a visual check
  # slowMpg740 <- toAddslowM[pixelGroup==740,.(row, calcSlowM)]
  # slowMpg740 <- slowMpg740[, row:= as.numeric(as.character(row))][order(row),]
  # chgVect[,slowMpg740:=slowMpg740$calcSlowM]
 
  # from long to wide
  setorderv(toAddslowM, keycol)
  toAddOut <- dcast(toAddslowM,pixelGroup~row, value.var="calcSlowM")
  
  names(toAddOut) <- c("pixelGroup",as.character(poolsToRows$variable))
  toAddOut[order(pixelGroup),]
  countDist <- pixelCount[ pixelGroup %in% c((maxPixelGroup+1):max(pixelCount$pixelGroup)),N]
  ages <- unique(distPixels[,ages, by=newGroup])
  ages[order(newGroup),]
  
  distPixOut <- cbind(rep(time(sim)[1],dim(toAddOut)[1]),countDist,toAddOut$pixelGroup, ages[,2],toAddOut[,-1])
  
  
  
  ##################################

# Calculating NPP for this year using stockt and stockt1
  stockt <- sim$pixelGroupC[,.(
    pixelGroup,
    ages,
    spatial_unit_id,
    'PastSoftwoodMerch' = SoftwoodMerch,
    'PastSoftwoodFoliage' = SoftwoodFoliage,
    'PastSoftwoodOther' = SoftwoodOther,
    'PastSoftwoodCoarseRoots' = SoftwoodCoarseRoots,
    'PastSoftwoodFineRoots' = SoftwoodFineRoots,
    'PastHardwoodMerch' = HardwoodMerch,
    'PastHardwoodFoliage' = HardwoodFoliage,
    'PastHardwoodOther' = HardwoodOther,
    'PastHardwoodCoarseRoots' = HardwoodCoarseRoots,
    'PastHardwoodFineRoots' = HardwoodFineRoots 
  )] 
  
  setkey(stockt,pixelGroup)
  stockt1 <- unique(cbind(pixelGroupForAnnual[,!(Input:Products)],sim$pools))[,.(
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
  #This is recycling stockt. Need to do the other type of join
  stocks2t <- stockt[,-c("ages","spatial_unit_id")][stockt1]
  grossGrowth <- stocks2t[,.(
    pixelGroup,
    grossGrowthAG = (
      (SoftwoodMerch - PastSoftwoodMerch) +
        (SoftwoodFoliage - PastSoftwoodFoliage) +
        (SoftwoodOther - PastSoftwoodOther) +
        (HardwoodMerch - PastHardwoodMerch) +
        (HardwoodFoliage - PastHardwoodFoliage) +
        (HardwoodOther - PastHardwoodOther)),
    grossGrowthBG= (
      (SoftwoodCoarseRoots - PastSoftwoodCoarseRoots) + 
        (SoftwoodFineRoots - PastSoftwoodFineRoots) +
        (HardwoodCoarseRoots - PastHardwoodCoarseRoots) +
        (HardwoodFineRoots - PastHardwoodFineRoots))
  )]
  
  turnoverRates <- sim$turnoverRates[, spatial_unit_id := SpatialUnitID]
  turnoverComponents <- merge(stockt,turnoverRates,by="spatial_unit_id")
  turnover <- turnoverComponents[,.(
    AGturnover = (
      (PastSoftwoodMerch * StemAnnualTurnoverRate)+
        (PastSoftwoodFoliage * SoftwoodFoliageFallRate)+
        (PastSoftwoodOther * SoftwoodBranchTurnoverRate)+
        (PastHardwoodMerch * StemAnnualTurnoverRate)+
        (PastHardwoodFoliage * HardwoodFoliageFallRate)+
        (PastHardwoodOther * HardwoodBranchTurnoverRate)),
    BGturnover = (
      (PastSoftwoodCoarseRoots * CoarseRootTurnProp)+
        (PastSoftwoodFineRoots * FineRootTurnProp)+
        (PastHardwoodCoarseRoots * CoarseRootTurnProp)+
        (PastHardwoodFineRoots * FineRootTurnProp))
  ), by = pixelGroup]
  
  NPP <- merge(turnover,grossGrowth,by="pixelGroup")[,.(
    pixelGroup,
    NPP = (
      AGturnover+
        BGturnover+
        grossGrowthAG+
        grossGrowthBG)
  )]

  sim$NPP <- rbind(sim$NPP, cbind(simYear = rep(time(sim)[1],nrow(NPP)),NPP))
  
  # make the disturbed pixels like$pixelGroupC and add the rows at the end
  addDistC <- cbind(toAdd[,.(ages,rasterSps,spatial_unit_id,growth_curve_component_id,growth_curve_id,Productivity)],
                    toAddOut)
  sim$pixelGroupC <- rbind(unique(cbind(pixelGroupForAnnual[,!(Input:Products)],sim$pools)),addDistC)
  #sim$pixelGroupC$N <- sim$spatialDT[,.N,by=pixelGroup]$
  sim$pixelGroupC$ages <- sim$pixelGroupC$ages+1
  sim$spatialDT$ages <- sim$spatialDT$ages+1
  
  names(distPixOut) <- c( c("simYear","pixelCount","pixelGroup", "ages"), sim$pooldef)
  updatePools <-   cbind(rep(time(sim)[1],length(sim$ages)),pixelCount[1:length(sim$ages),2],pixelGroupForAnnual$pixelGroup, sim$ages, sim$pools)
  names(updatePools) <- c( c("simYear","pixelCount","pixelGroup", "ages"), sim$pooldef)
  sim$cbmPools <- rbind(sim$cbmPools,updatePools,distPixOut)
  

  return(invisible(sim))
}



.inputObjects = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  
  if (!suppliedElsewhere("pooldef", sim)){
    sim$pooldef <-  c("Input",
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
    #standIdx <- 1:sim$nStands
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
    sim$gcurveFileName <- file.path(dataPath, "spadesGCurvesSK.csv")#"SK_ReclineRuns30m", "LookupTables",
  if(!suppliedElsewhere(sim$gcurveComponentsFileName))
    sim$gcurveComponentsFileName <- file.path(dataPath, "yieldComponentSK.csv")#"SK_ReclineRuns30m", "LookupTables", 
  # ! ----- STOP EDITING ----- ! #
  if(!suppliedElsewhere(sim$processes))
   sim$processes <-
     list(
       domDecayMatrices = matrixHash(computeDomDecayMatrices(sim$decayRates, sim$cbmData@decayParameters, sim$PoolCount)),
       slowDecayMatrices = matrixHash(computeSlowDecayMatrices(sim$decayRates, sim$cbmData@decayParameters, sim$PoolCount)),
       slowMixingMatrix = matrixHash(computeSlowMixingMatrix(sim$cbmData@slowAGtoBGTransferRate, sim$PoolCount)),
       domTurnover = matrixHash(computeDomTurnoverMatrices(sim$cbmData@turnoverRates, sim$PoolCount)),
       bioTurnover = matrixHash(computeBioTurnoverMatrices(sim$cbmData@turnoverRates, sim$PoolCount)),
       disturbanceMatrices = matrixHash(loadDisturbanceMatrixIds(sim$cbmData@disturbanceMatrixValues, sim$cbmData@pools))
     )
   if(!suppliedElsewhere(sim$disturbanceRasters)){
   sim$disturbanceRasters <- list.files("data/forIan/SK_data/CBM_GIS/disturbance_testArea",
                                        full.names = TRUE) %>%
     grep(., pattern = ".grd$", value = TRUE)
   }
  if(!suppliedElsewhere(sim$spatialDT)){
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
    sim$masterRaster <- ldSpsRaster
    ## END Rasters--------------------------------------------------------------------
    
    # in one data table-------------------------------------
    level2DT <- as.data.table(cbind(ages,rasterSps,Productivity,spatial_unit_id))	  
    level2DT <- level2DT[level2DT$rasterSps>0]
    # END data.table----------------------------------------
    
    # not all species have 3 levels of productivity - adjust productivity---
    oneProdlevel <- c(1,2,4,6) ## danger hard coded these are the species with one level##
    Prod2 <- which(level2DT$rasterSps %in% oneProdlevel)
    level2DT$Productivity[Prod2] <- 1
    level2DT <- level2DT[level2DT$Productivity==3, Productivity:=2]
    level2DT$pixelIndex <- 1:nrow(level2DT)
    setkey(level2DT,rasterSps,Productivity,spatial_unit_id)
    level2DT <- level2DT[order(pixelIndex),]
    # END adjustment of productivity to match data----------------
    
    # add the gcID information-------------------------------
    #gcID <- read.csv(file.path(getwd(),"data/spadesGCurvesSK.csv"))#gcID_ref.csv
    gcID <- fread("data/spadesGCurvesSK.csv")#fread(sim$gcurveFileName)## danger hard coded##
    gcID <- unique(gcID[,.(rasterSps,species,growth_curve_component_id,spatial_unit_id,forest_type_id,growth_curve_id,Productivity)])
    setkey(gcID,rasterSps,Productivity,spatial_unit_id)
    # end add the gcID: each pixel has a growth curve now---
    
    # create the pixel group---------------
    spatialDT <- level2DT[gcID, on = c("rasterSps","Productivity","spatial_unit_id"),nomatch = 0]
    spatialDT <- spatialDT[order(pixelIndex),]
    spatialDT$pixelGroup <- LandR::generatePixelGroups(spatialDT,0,
                                                       columns = c("spatial_unit_id", "growth_curve_component_id", "ages"))
    spatialDT <- spatialDT[order(pixelIndex),]
    ## NEED TO ASK ELIOT ABOUT THIS: why does it create all these extra vars?
    # why the number starts at the number you are asking for? not logical to me -
    # max is the max value your groups should have.@..?
    spatialDT <- spatialDT[,.(ages, rasterSps, spatial_unit_id, pixelIndex,
                              growth_curve_component_id, growth_curve_id,Productivity, pixelGroup)]
    spatialDT <- spatialDT[order(pixelIndex),]
    sim$spatialDT <- spatialDT
  }
  
    
    if(!suppliedElsewhere(sim$mySpuDmids)){
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
    }
  if(!suppliedElsewhere(sim$level3DT)){  
  level3DT <- unique(sim$spatialDT[,-("pixelIndex")])%>% .[order(pixelGroup),]
  # might have to keep this when we integrate the disturbances
  sim$level3DT <- level3DT
  }
  
  return(invisible(sim))
  
}
### add additional events as needed by copy/pasting from above

Sys.setenv(PKG_CXXFLAGS = "-std=c++0x")
#sourceCpp(file='RCBMStep.cpp')
Rcpp::sourceCpp(file='RCBMGrowthIncrements.cpp', cacheDir = cachePath(sim), 
          env = envir(sim)[["spadesCBMcore"]])

