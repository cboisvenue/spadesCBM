
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "spadesCBMdefaults",
  description = "Reads in all the default values for CBM-CFS3 for Canada", #"insert module description here",
  keywords = c("CBM-CFS3", "forest carbon","Canada parameters"), # c("insert key words here"),
  authors = person("Celine", "Boisvenue", email = "Celine.Boisvenue@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9007", spadesCBMdefaults = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spadesCBMdefaults.Rmd"),
  reqdPkgs = list("RSQLite", "data.table"),
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
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA),
    expectsInput(objectName = "sqlDir", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA),
    createsOutput(objectName = "pooldef", objectClass = "character", desc = "Vector of names (characters) for each of the carbon pools, with `Input` being the first one"),
    createsOutput(objectName = "PoolCount", objectClass = "numeric", desc = "Length of pooldef"),
    createsOutput(objectName = "cbmData", objectClass = "dataset", desc = NA),
    createsOutput(objectName = "decayRates", objectClass = "matrix", desc = "decay rates per spatial unit?"),
    createsOutput(objectName = "processes", objectClass = "list", desc = "decay mixing turnover and disturbances")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.spadesCBMdefaults = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "spadesCBMdefaults", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "spadesCBMdefaults", "save")
    },
    plot = {

    },
    save = {

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


#Scott's version 
#assign("PoolCount", length(sim$pooldef), envir = envir(sim))#?.globals
sim$PoolCount <- length(sim$pooldef)

#step 1 read the cbm_defaults parameter data
spatialUnitIds <- as.matrix(getTable("spatialUnitIds.sql", sim$dbPath, sim$sqlDir))
disturbanceMatrix <- as.matrix(getTable("disturbanceMatrix.sql", sim$dbPath, sim$sqlDir))
# this is the S4 object that has ALL the parameters
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

#step 2 create constant matrices from parameter data
# these are the decay rates for all the 48 spatial units
sim$decayRates <- spatialUnitDecayRates(sim$cbmData@climate, sim$cbmData@decayParameters, sim$cbmData@domPools)

sim$processes <- 
  list(
    domDecayMatrices = matrixHash(computeDomDecayMatrices(sim$decayRates, sim$cbmData@decayParameters, sim$PoolCount)),
    slowDecayMatrices = matrixHash(computeSlowDecayMatrices(sim$decayRates, sim$cbmData@decayParameters, sim$PoolCount)),
    slowMixingMatrix = matrixHash(computeSlowMixingMatrix(sim$cbmData@slowAGtoBGTransferRate, sim$PoolCount)),
    domTurnover = matrixHash(computeDomTurnoverMatrices(sim$cbmData@turnoverRates, sim$PoolCount)),
    bioTurnover = matrixHash(computeBioTurnoverMatrices(sim$cbmData@turnoverRates, sim$PoolCount)),
    disturbanceMatrices = matrixHash(loadDisturbanceMatrixIds(sim$cbmData@disturbanceMatrixValues, sim$cbmData@pools))
  )

# ! ----- STOP EDITING ----- ! #


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
plot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1

### template for your event2

.inputObjects = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  if(!suppliedElsewhere(sim$sqlDir))
    sim$sqlDir <- file.path(file.path(modulePath(sim), "spadesCBMdefaults","data","cbm_defaults"))
  if(!suppliedElsewhere(sim$dbPath))
    sim$dbPath <- file.path(sim$sqlDir, "cbm_defaults.db")
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
