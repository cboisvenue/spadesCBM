# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "CBM_core",
  description = NA, # "insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Celine", "Boisvenue", email = "celine.boisvenue@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.2", CBM_core = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_core.Rmd"),
  reqdPkgs = list(
    "data.table", "ggplot2", "quickPlot", "magrittr", "raster", "Rcpp", "RSQLite",
    "CBMutils" # "PredictiveEcology/CBMutils"
  ),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("spinupDebug", "logical", FALSE, NA, NA, "If TRUE spinupResult will be outputed to a text file (spinup.csv). FALSE means no ouput of the spinupResult"),
    # defineParameter("noAnnualDisturbances", "logical", FALSE, NA, NA, "If TRUE the sim$allProcesses and sim$opMatrix are created in the postSpinup event, just once. By default, these are recreated everyyear in the annual event"),
    defineParameter("poolsToPlot", "character", "totalCarbon", NA, NA,
      desc = "which carbon pools to plot, if any. Defaults to total carbon"
    ),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    # expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "cbmData", objectClass = "dataset", desc = NA, sourceURL = NA),
    expectsInput(
      objectName = "masterRaster", objectClass = "raster",
      desc = "Raster has NAs where there are no species and the pixel groupID where the pixels were simulated. It is used to map results"
    ),
    expectsInput(objectName = "processes", objectClass = "dataset", desc = NA, sourceURL = NA),
    expectsInput(
      objectName = "pooldef", objectClass = "character",
      desc = "Vector of names (characters) for each of the carbon pools, with `Input` being the first one", sourceURL = NA
    ),
    expectsInput(
      objectName = "PoolCount", objectClass = "numeric",
      desc = "Length of pooldef", sourceURL = NA
    ),
    expectsInput(
      objectName = "pools", objectClass = "matrix",
      desc = "empty matrix for storage of spinupResult", sourceURL = NA
    ),
    expectsInput(
      objectName = "ages", objectClass = "numeric",
      desc = "Ages of the stands from the inventory in 1990", sourceURL = NA
    ),
    expectsInput(
      objectName = "gcids", objectClass = "numeric",
      desc = "The identification of which growth curves to use on the specific stands provided by...", sourceURL = NA
    ),
    expectsInput(
      objectName = "historicDMIDs", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating historical disturbance type, linked to the S4 table called cbmData. Only Spinup.", sourceURL = NA
    ),
    expectsInput(
      objectName = "lastPassDMIDS", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating final disturbance type, linked to the S4 table called cbmData. Only Spinup.", sourceURL = NA
    ),
    expectsInput(
      objectName = "delays", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating regeneration delay post disturbance. Only Spinup.", sourceURL = NA
    ),
    expectsInput(
      objectName = "minRotations", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating minimum number of rotations. Only Spinup.", sourceURL = NA
    ),
    expectsInput(
      objectName = "maxRotations", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating maximum number of rotations. Only Spinup.", sourceURL = NA
    ),
    expectsInput(
      objectName = "returnIntervals", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating the fixed fire return interval. Only Spinup.", sourceURL = NA
    ),
    expectsInput(
      objectName = "spatialUnits", objectClass = "numeric",
      desc = "The id given to the intersection of province and ecozones across Canada, linked to the S4 table called cbmData", sourceURL = NA
    ),
    expectsInput(
      objectName = "ecozones", objectClass = "numeric",
      desc = "Vector, one for each stand, indicating the numeric represenation of the Canadian ecozones, as used in CBM-CFS3", sourceURL = NA
    ),
    expectsInput(
      objectName = "disturbanceRasters", objectClass = "raster",
      desc = "Character vector of the disturbance rasters for SK"
    ),
    expectsInput(
      objectName = "mySpuDmids", objectClass = "data.frame",
      desc = "the table containing one line per pixel"
    ),
    # expectsInput(objectName = "disturbanceEvents", objectClass = "matrix",
    #              desc = "3 column matrix, PixelGroupID, Year (that sim year), and DisturbanceMatrixId. Not used in Spinup.", sourceURL = NA),
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "level3DT", objectClass = "data.table", desc = NA, sourceURL = NA),
    expectsInput(
      objectName = "spatialDT", objectClass = "data.table",
      desc = "the table containing one line per pixel"
    )
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "opMatrixCBM", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "spinupResult", objectClass = "data.frame", desc = NA),
    createsOutput(
      objectName = "allProcesses", objectClass = "list",
      desc = "A list of the constant processes, anything NULL is just a placeholder for dynamic processes"
    ),
    createsOutput(
      objectName = "pixelGroupC", objectClass = "data.table",
      desc = "This is the data table that has all the vectors to create the inputs for the annual processes"
    ),
    createsOutput(
      objectName = "cbmPools", objectClass = "data.frame",
      desc = "Three parts: pixelGroup, Age, and Pools "
    ),
    # createsOutput(objectName = "disturbanceEvents", objectClass = "matrix",
    #               desc = "3 column matrix, PixelGroupID, Year, and DisturbanceMatrixId. Not used in Spinup."),
    createsOutput(
      objectName = "pixelKeep", objectClass = "data.table",
      desc = "Keeps the pixelIndex from spatialDT with each year's PixelGroupID as a column. This is to enable making maps of yearly output."
    ),
    # createsOutput(objectName = "yearEvents", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "pools", objectClass = "matrix", desc = NA),
    createsOutput(objectName = "ages", objectClass = "numeric", desc = "Ages of the stands after simulation"),
    createsOutput(objectName = "NPP", objectClass = "data.table", desc = "NPP for each pixelGroup"),
    createsOutput(objectName = "emissionsProducts", objectClass = "data.table", desc = "Co2, CH4, CO and Products columns for each simulation year - filled up at each annual event."),
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
doEvent.CBM_core <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- spinup(sim) ## this is the spinup
      if (P(sim)$spinupDebug) {
        sim <- scheduleEvent(sim, start(sim), "CBM_core", "saveSpinup")
      }

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "postSpinup")
      # sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual") ## scheduled in postSpinup?
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "CBM_core", "plot", eventPriority = 9)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "CBM_core", "save")
    },
    saveSpinup = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      colnames(sim$spinupResult) <- c(c("pixelGroup", "age"), sim$pooldef)
      write.csv(file = file.path(outputPath(sim), "spinup.csv"), sim$spinupResult)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "CBM_core", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    annual = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- annual(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_core", "annual")
      if (time(sim) == end(sim)) {
        sim <- scheduleEvent(sim, end(sim), "CBM_core", "savePools", .last()) ## TODO: schedule saving in init or in savePools event
      }
      # ! ----- STOP EDITING ----- ! #
    },
    postSpinup = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- postSpinup(sim)
      sim$turnoverRates <- calcTurnoverRates(
        turnoverRates = sim$cbmData@turnoverRates,
        spatialUnitIds = sim$cbmData@spatialUnitIds, spatialUnits = sim$spatialUnits
      )
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "CBM_core", "plot")
      sim <- scheduleEvent(sim, time(sim), "CBM_core", "annual") ## TODO: schedule in init
      # ! ----- STOP EDITING ----- ! #
    },
    plot = {
      clearPlot()
      if (!time(sim) == start(sim)) {
        areaPlot(
          cbmPools = sim$cbmPools,
          masterRaster = sim$masterRaster
        )

        barPlot(
          cbmPools = sim$cbmPools,
          masterRaster = sim$masterRaster
        )

        NPPPlot(
          changeInNPP = sim$NPP,
          masterRaster = sim$masterRaster,
          spatialDT = sim$spatialDT,
          time = time(sim)
        )
      }

      spatialPlot(
        cbmPools = sim$cbmPools,
        poolsToPlot = P(sim)$poolsToPlot,
        masterRaster = sim$masterRaster,
        pixelkeep = sim$pixelKeep,
        years = time(sim)
      )


      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "CBM_core", "plot", eventPriority = 9)
    },
    savePools = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      colnames(sim$cbmPools) <- c(c("simYear", "pixelCount", "pixelGroup", "ages"), sim$pooldef)
      write.csv(file = file.path(outputPath(sim), "cPoolsPixelYear.csv"), sim$cbmPools)

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      colnames(sim$cbmPools) <- c(c("simYear", "pixelCount", "pixelGroup", "ages"), sim$pooldef)
      write.csv(file = file.path(outputPath(sim), "cPoolsPixelYear.csv"), sim$cbmPools)

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "CBM_core", "savePools")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
      "' in module '", current(sim)[1, "moduleName", with = FALSE], "'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization

spinup <- function(sim) {
  io <- inputObjects(sim, currentModule(sim))
  objectNamesExpected <- io$objectName
  available <- objectNamesExpected %in% ls(sim)
  if (any(!available)) {
    stop(
      "The inputObjects for CBM_core are not all available:",
      "These are missing:", paste(objectNamesExpected[!available], collapse = ", "),
      ". \n\nHave you run ",
      paste0("spadesCBM", c("defaults", "inputs", "m3ToBiomass"), collapse = ", "),
      "?"
    )
  }

  opMatrix <- cbind(
    1:sim$nStands, # growth 1
    sim$ecozones, # domturnover
    sim$ecozones, # bioturnover
    1:sim$nStands, # overmature
    1:sim$nStands, # growth 2
    sim$spatialUnits, # domDecay
    sim$spatialUnits, # slow decay
    rep(1, sim$nStands) # slow mixing
  )
  ### NEED TO DEAL WITH THIS HERE
  ## Are there stands over max age in the growth curves?If so, need to set to
  ## the max...may even the oldest stand for 1st spinup might have to be
  ## changed. This means that we are not tracking old
  ## stands but also, this problem will go away once we use LandR for the
  ## biomass increments
  # sim$ages[sim$ages>max(spadesCBMout$growth_increments[,2])] <- max(spadesCBMout$growth_increments[,2])
  ## END AGE

  spinupResult <- Spinup( ## TODO: cache this
    pools = sim$pools,
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
    rootParameters = as.data.frame(t(sim$cbmData@rootParameters[1, ])),
    turnoverParams = as.data.frame(t(sim$cbmData@turnoverRates[1, ])),
    biomassToCarbonRate = as.numeric(sim$cbmData@biomassToCarbonRate),
    debug = P(sim)$spinupDebug
  )

  # # setting CO2, CH4, CO and products to 0 before starting the simulations
  spinupResult[, 23:dim(spinupResult)[2]] <- 0
  sim$spinupResult <- spinupResult
  sim$spinupResult[which(is.na(sim$spinupResult))] <- 0
  return(invisible(sim))
}

postSpinup <- function(sim) {
  sim$pools <- sim$spinupResult
  # prepping the pixelGroups for processing in the annual event
  sim$level3DT <- sim$level3DT[order(pixelGroup), ]
  sim$pixelGroupC <- cbind(sim$level3DT, sim$spinupResult)

  sim$cbmPools <- NULL
  sim$NPP <- NULL
  sim$emissionsProducts <- NULL

  # Keep the pixels from each simulation year (in the postSpinup event)
  # in the end (cPoolsPixelYear.csv), this should be the same length at this vector
  ## got place for a vector length check!!
  sim$spatialDT <- sim$spatialDT[order(sim$spatialDT$pixelIndex), ]
  sim$pixelKeep <- sim$spatialDT[, .(pixelIndex, pixelGroup)]
  setnames(sim$pixelKeep, c("pixelIndex", "pixelGroup0"))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

annual <- function(sim) {
  ################################### -----------------------------------
  # DISTURBANCES: which pixels are disturbed and update the pixelGroup and data
  # tables in consequence
  ###################################
  #
  # 1. Read-in the disturbances
  # The example simulation (SK) has a raster stack covering 1984-2011 for an
  # area in SK. The raster stack like all inputs from user, is read in the
  # spadesCBMinputs module. However, one raster at a time is read in this annual
  # event, permitting the raters to come for each annual event from another
  # source.

  ## TO DO: disturbances for both SK and RIA were read-in for the whole
  ## simulation horizon in spadesCBMinputs. To permit "on-the-fly" disturbances,
  ## from other modules such as rasters they need to be read in here.

  # 1. Read-in the disturbances, stack read-in from spadesCBMinputs.R in example.
  annualDisturbance <- raster(grep(sim$disturbanceRasters, pattern = paste0(time(sim)[1], ".grd$"), value = TRUE))
  ##
  pixels <- getValues(sim$masterRaster)
  yearEvents <- getValues(annualDisturbance)[!is.na(pixels)]
  ## good check here would be: length(pixels[!is.na(pixels)] == nrow(sim$spatialDT)

  # 2. Add this year's events to the spatialDT, so each disturbed pixels has its event
  spatialDT <- sim$spatialDT[order(sim$spatialDT$pixelIndex)]
  pixelCount <- spatialDT[, .N, by = pixelGroup]
  ## TO DO: put in a check here where sum(.N) == length(pixels[!is.na(pixels)])

  ### do I have to make it sim$ here?
  spatialDT <- spatialDT[, events := yearEvents]
  # this could be big so remove it
  rm(yearEvents)

  # 3. get the disturbed pixels only
  distPixels <- spatialDT[events > 0, .(
    pixelIndex, pixelGroup, ages, spatial_unit_id,
    growth_curve_component_id, growth_curve_id,
    ecozones, events
  )]
  setkey(distPixels, pixelGroup)

  # 4. reset the ages for disturbed pixels in stand replacing disturbances
  ## In SK example: not all disturbances are stand replacing. Disturbance matrix
  ## 91 (events 3 and 5) are 20% mortality and does not need ages set to 0.
  cols <- c(3, 5)
  distPixels$ages[!(distPixels$events %in% cols)] <- 0

  # 5. new pixelGroup----------------------------------------------------
  # make a column of new pixelGroup that includes events and carbon from
  # previous pixel group since that changes the amount and destination of the
  # carbon being moved.
  # NOTE: disturbances in the SK example are not all stand replacing, "events", which
  # means type of disturbances, is not part of the factors in determining pixel
  # groups. If we start representing partial disturbances or have different
  # transitions resulting from specific disturbances, this will have to change.

  maxPixelGroup <- max(spatialDT$pixelGroup)

  # Get the carbon info from the pools in from previous year. The
  # sim$pixelGroupC is created in the postspinup event, and then updated at
  # the end of each annual event (in this script).
  pixelGroupC <- sim$pixelGroupC
  setkey(pixelGroupC, pixelGroup)

  cPoolsOnly <- pixelGroupC[, .(
    pixelGroup, Input, SoftwoodMerch, SoftwoodFoliage,
    SoftwoodOther, SoftwoodCoarseRoots, SoftwoodFineRoots,
    HardwoodMerch, HardwoodFoliage, HardwoodOther,
    HardwoodCoarseRoots, HardwoodFineRoots, AboveGroundVeryFastSoil,
    BelowGroundVeryFastSoil, AboveGroundFastSoil, BelowGroundFastSoil,
    MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil, SoftwoodStemSnag,
    SoftwoodBranchSnag, HardwoodStemSnag, HardwoodBranchSnag,
    CO2, CH4, CO, Products
  )]


  distPixelCpools <- merge(distPixels, cPoolsOnly)

  distPixelCpools$newGroup <- LandR::generatePixelGroups(distPixelCpools, maxPixelGroup,
    columns = c(
      "ages", "spatial_unit_id",
      "growth_curve_component_id",
      "ecozones", "events", "Input", "SoftwoodMerch",
      "SoftwoodFoliage", "SoftwoodOther", "SoftwoodCoarseRoots",
      "SoftwoodFineRoots",
      "HardwoodMerch", "HardwoodFoliage", "HardwoodOther",
      "HardwoodCoarseRoots", "HardwoodFineRoots", "AboveGroundVeryFastSoil",
      "BelowGroundVeryFastSoil", "AboveGroundFastSoil", "BelowGroundFastSoil",
      "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil", "SoftwoodStemSnag",
      "SoftwoodBranchSnag", "HardwoodStemSnag", "HardwoodBranchSnag",
      "CO2", "CH4", "CO", "Products"
    )
  )

  distPixelCpools <- distPixelCpools[, .(
    newGroup, pixelGroup, pixelIndex, events, ages, spatial_unit_id,
    growth_curve_component_id, growth_curve_id, ecozones, Input, SoftwoodMerch,
    SoftwoodFoliage, SoftwoodOther,
    SoftwoodCoarseRoots, SoftwoodFineRoots,
    HardwoodMerch, HardwoodFoliage,
    HardwoodOther, HardwoodCoarseRoots,
    HardwoodFineRoots, AboveGroundVeryFastSoil,
    BelowGroundVeryFastSoil, AboveGroundFastSoil,
    BelowGroundFastSoil, MediumSoil,
    AboveGroundSlowSoil, BelowGroundSlowSoil,
    SoftwoodStemSnag, SoftwoodBranchSnag,
    HardwoodStemSnag, HardwoodBranchSnag,
    CO2, CH4, CO, Products
  )]
  cols <- c("pixelGroup", "newGroup")
  distPixelCpools[, (cols) := list((newGroup), NULL)]

  # 6. Update long form pixel index all pixelGroups (old ones plus new ones for
  # disturbed pixels)
  updateSpatialDT <- rbind(spatialDT[events < 1, ], distPixelCpools[, 1:8]) %>% .[order(pixelIndex), ]
  pixelCount <- updateSpatialDT[, .N, by = pixelGroup]
  # adding the new pixelGroup to the pixelKeep. pixelKeep is 1st created in the
  # postspinup event and update in each annual event (in this script).
  sim$pixelKeep[, newPix := updateSpatialDT$pixelGroup]
  setnames(sim$pixelKeep, "newPix", paste0("pixelGroup", time(sim)[1]))

  # 7. Update the meta data for the pixelGroups. The first meta data is the
  # $level3DT created in the spadesCBMinputs module. When new pixels groups are
  # create the meta data gets updated here.

  # only the column pixelIndex is different between distPixelCpools and pixelGroupC
  metaDT <- unique(updateSpatialDT[, -("pixelIndex")]) %>% .[order(pixelGroup), ]
  setkey(metaDT, pixelGroup)

  # 8. link the meta data (metaDT) with the appropriate carbon pools
  # add c pools and event column for old groups
  part1 <- merge(metaDT, cPoolsOnly)
  # add c pools and event column from the new groups
  distGroupCpools <- unique(distPixelCpools[, -("pixelIndex")])
  setkey(distGroupCpools, pixelGroup)
  cols <- c(
    "pixelGroup", "ages", "spatial_unit_id", "growth_curve_component_id",
    "growth_curve_id", "ecozones", "events"
  )
  part2 <- merge(metaDT, distGroupCpools, by = cols)
  # table for this annual event processing
  pixelGroupForAnnual <- rbind(part1, part2) %>% .[order(pixelGroup), ]


  # 9. From the events column, create a vector of the disturbance matrix
  # identification so it links back to the CBM default disturbance matrices.

  # mySpuDmids was created in spadesCBMinputs
  mySpuDmids <- sim$mySpuDmids
  mySpuDmids[, "events" := rasterId][, rasterId := NULL]

  DM <- merge(pixelGroupForAnnual, mySpuDmids, by = c("spatial_unit_id", "events"), all.x = TRUE)
  DM$disturbance_matrix_id[is.na(DM$disturbance_matrix_id)] <- 0
  DM[order(pixelGroup), ]
  ## this is the vector to be fed into the sim$opMatrixCBM[,"disturbance"]<-DMIDS
  DMIDS <- DM$disturbance_matrix_id

  # END of dealing with disturbances and updating all relevant data tables.
  ################################### -----------------------------------


  #########################################################################
  #-----------------------------------------------------------------------
  # RUN ALL PROCESSES FOR ALL NEW PIXEL GROUPS#############################
  #########################################################################

  # 1. Changing the vectors and matrices that need to be changed to process this year's growth
  sim$pools <- as.matrix(pixelGroupForAnnual[, Input:Products])
  sim$ecozones <- pixelGroupForAnnual$ecozones
  sim$ages <- pixelGroupForAnnual[, ages]
  sim$nStands <- length(sim$ages)
  sim$gcids <- pixelGroupForAnnual[, growth_curve_component_id]
  sim$spatialUnits <- pixelGroupForAnnual[, spatial_unit_id]

  # 2. Make a matrix out of the updated vectors
  # this is a matrix that gives the index of the matrix to be used for this
  # annual event in $allProcesses.
  sim$opMatrixCBM <- cbind(
    DMIDS, # disturbance matrix identification vector
    1:sim$nStands, # growth 1
    sim$ecozones, # domturnover
    sim$ecozones, # bioturnover
    1:sim$nStands, # overmature
    1:sim$nStands, # growth 2
    sim$spatialUnits, # domDecay
    sim$spatialUnits, # slow decay
    rep(1, sim$nStands) # slow mixing
  )

  colnames(sim$opMatrixCBM) <- c(
    "disturbance", "growth 1", "domturnover",
    "bioturnover", "overmature", "growth 2",
    "domDecay", "slow decay", "slow mixing"
  )

  # 3. select the matrices that apply to this annual event and specific sim
  # allProcesses contains all the default matrices for disturbances in CBM, gets
  # the growth matrices for this annual event, and the specific matrices for the
  # other processes for this simulation. The order of the columns reflect the
  # order in which these processes are applied to the carbon pools.
  sim$allProcesses <- list(
    Disturbance = sim$processes$disturbanceMatrices,
    Growth1 = NULL,
    DomTurnover = sim$processes$domTurnover,
    BioTurnover = sim$processes$bioTurnover,
    OvermatureDecline = NULL,
    Growth2 = NULL,
    DomDecay = sim$processes$domDecayMatrices,
    SlowDecay = sim$processes$slowDecayMatrices,
    SlowMixing = sim$processes$slowMixingMatrix
  )

  # 4. compute the growth increments that are specific to the number of
  # pixelGroups in this annual event, and feed in the vectors specific to this
  # annual event

  growthAndDecline <- ComputeGrowthAndDeclineMatrices2(
    growthIncrements = sim$gcHash,
    ages = sim$ages,
    gcids = sim$gcids,
    pools = sim$pools,
    rootParameters = as.data.frame(t(sim$cbmData@rootParameters[1, ])),
    turnoverParams = as.data.frame(t(sim$cbmData@turnoverRates[1, ])),
    biomassToCarbonRate = as.numeric(sim$cbmData@biomassToCarbonRate),
    swMult = 0.5, hwMult = 0.5
  )

  sim$allProcesses$Growth1 <- growthAndDecline$Growth
  sim$allProcesses$Growth2 <- growthAndDecline$Growth
  sim$allProcesses$OvermatureDecline <- growthAndDecline$OvermatureDecline

  ### good check: dim(pools)[1]==dim(opMatrixCBM)[1]

  # 5. All the work happens here: update all the pools.
  sim$pools <- StepPools(
    pools = sim$pools,
    opMatrix = sim$opMatrixCBM,
    flowMatrices = sim$allProcesses
  )
  sim$pools[which(is.na(sim$pools))] <- 0

  ########################## END PROCESSES#########################################
  #-------------------------------------------------------------------------------

  #-----------------------------------------------------------------------------------
  ############ NPP ####################################################################
  # Calculating NPP for this year using stockt and stockt1 for undisturbed
  # pixelGroups, and increments for the disturbed pixelGroups

  # 1. NPP for undisturbed pixelGroups in this annual event
  nonDistline <- which(pixelGroupForAnnual$pixelGroup == maxPixelGroup)
  stockt <- pixelGroupForAnnual[1:nonDistline, .(
    pixelGroup,
    ages,
    spatial_unit_id,
    "PastSoftwoodMerch" = SoftwoodMerch,
    "PastSoftwoodFoliage" = SoftwoodFoliage,
    "PastSoftwoodOther" = SoftwoodOther,
    "PastSoftwoodCoarseRoots" = SoftwoodCoarseRoots,
    "PastSoftwoodFineRoots" = SoftwoodFineRoots,
    "PastHardwoodMerch" = HardwoodMerch,
    "PastHardwoodFoliage" = HardwoodFoliage,
    "PastHardwoodOther" = HardwoodOther,
    "PastHardwoodCoarseRoots" = HardwoodCoarseRoots,
    "PastHardwoodFineRoots" = HardwoodFineRoots
  )]

  setkey(stockt, pixelGroup)

  stockt1 <- cbind(pixelGroupForAnnual[1:nonDistline, !(Input:Products)], sim$pools[1:nonDistline, ])[, .(
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
  setkey(stockt1, pixelGroup)

  stocks2t <- stockt[, -c("ages", "spatial_unit_id")][stockt1]
  grossGrowth <- stocks2t[, .(
    pixelGroup,
    grossGrowthAG = (
      (SoftwoodMerch - PastSoftwoodMerch) +
        (SoftwoodFoliage - PastSoftwoodFoliage) +
        (SoftwoodOther - PastSoftwoodOther) +
        (HardwoodMerch - PastHardwoodMerch) +
        (HardwoodFoliage - PastHardwoodFoliage) +
        (HardwoodOther - PastHardwoodOther)),
    grossGrowthBG = (
      (SoftwoodCoarseRoots - PastSoftwoodCoarseRoots) +
        (SoftwoodFineRoots - PastSoftwoodFineRoots) +
        (HardwoodCoarseRoots - PastHardwoodCoarseRoots) +
        (HardwoodFineRoots - PastHardwoodFineRoots))
  )]

  # sim$turnoverRates are calculated in the postspinup event
  turnoverRates <- sim$turnoverRates[, spatial_unit_id := SpatialUnitID]
  turnoverComponents <- merge(stockt, turnoverRates, by = "spatial_unit_id")
  turnover <- turnoverComponents[, .(
    AGturnover = (
      (PastSoftwoodMerch * StemAnnualTurnoverRate) +
        (PastSoftwoodFoliage * SoftwoodFoliageFallRate) +
        (PastSoftwoodOther * SoftwoodBranchTurnoverRate) +
        (PastHardwoodMerch * StemAnnualTurnoverRate) +
        (PastHardwoodFoliage * HardwoodFoliageFallRate) +
        (PastHardwoodOther * HardwoodBranchTurnoverRate)),
    BGturnover = (
      (PastSoftwoodCoarseRoots * CoarseRootTurnProp) +
        (PastSoftwoodFineRoots * FineRootTurnProp) +
        (PastHardwoodCoarseRoots * CoarseRootTurnProp) +
        (PastHardwoodFineRoots * FineRootTurnProp))
  ), by = pixelGroup]

  NPPnonDist <- merge(turnover, grossGrowth, by = "pixelGroup")[, .(
    pixelGroup,
    NPP = (
      AGturnover +
        BGturnover +
        grossGrowthAG +
        grossGrowthBG)
  )]

  # 2. NPP for the disturbed pixels
  # NPP for all the pixelGroups that are greater than the maxPixelGroup is calculated as the sum of the
  # increments for that pixel group.
  ## Note: this will also work for the non-stand-replacing disturbances as we use
  ## "inputs" only and not differences between stocks.

  # make the matrices data.tables
  incsListDT <- lapply(sim$allProcesses$Growth1, as.data.table)
  incsListDTnrow <- lapply(sim$allProcesses$Growth1, nrow)
  incsNrow <- do.call("rbind", incsListDTnrow)
  # names(incsListDT) <- pixelGroupForAnnual$pixelGroup#paste0("pg",
  incDT <- rbindlist(incsListDT)
  # this is not working can't figure out why...
  # incDT$name <- rep(names(incsListDT),each=sapply(incsListDT,"nrow"))
  nameVec <- NULL
  for (i in 1:length(pixelGroupForAnnual$pixelGroup)) {
    thisPg <- rep(pixelGroupForAnnual$pixelGroup[i], times = incsNrow[i])
    nameVec <- c(nameVec, thisPg)
  }
  incDT$name <- nameVec

  # only the pixelGroups that are disturbed
  distNPP <- incDT[name > maxPixelGroup & value < 1, .(NPP = sum(value)), by = name]
  names(distNPP) <- names(NPPnonDist)

  # 3. All pixel group NPP in one table
  # sim$NPP is created in postspinup (as NULL)
  NPP <- rbind(NPPnonDist, distNPP)
  sim$NPP <- rbind(sim$NPP, cbind(simYear = rep(time(sim)[1], nrow(NPP)), NPP))
  ######### NPP END HERE ###################################
  #-----------------------------------------------------------------------------------

  ############# Update emissions and products -------------------------------------------
  # Emissions and re-zeroed every year as these pools should not define the
  # pixelGroups and both these values are most commonly required on a yearly
  # basis.

  # sim$emissionsProducts was first created in postspinup event and is update
  # here for each annual event. The sim$spinupResult emissions and Products was
  # re-zeroed at the end of the spinup event.

  # 1. Add the emissions and Products for this year
  emissionsProducts <- as.data.table(cbind(rep(time(sim)[1], length(pixelGroupForAnnual$pixelGroup)),pixelGroupForAnnual$pixelGroup,sim$pools[,23:26]))
  names(emissionsProducts) <- c("simYear","pixelGroup", "CO2", "CH4", "CO", "Products")
  sim$emissionsProducts <- rbind(sim$emissionsProducts, emissionsProducts)

  # 2. Re-zero the pools for emissions and Products
  sim$pools[, 23:dim(sim$pools)[2]] <- 0
  ############# End of update emissions and products ------------------------------------


  #### UPDATING ALL THE FINAL VECTORS FOR NEXT SIM YEAR ###################################
  #-----------------------------------
  # 1. Update long form (pixelIndex) and short form (pixelGroup) tables.
  sim$spatialDT <- updateSpatialDT
  sim$pixelGroupC <- rbind(unique(cbind(pixelGroupForAnnual[, !(Input:Products)], sim$pools)))

  # 2. increment ages
  sim$pixelGroupC$ages <- sim$pixelGroupC$ages + 1
  sim$spatialDT$ages <- sim$spatialDT$ages + 1

  # 3. Update the final simluation horizon table with all the pools/year/pixelGroup
  # names(distPixOut) <- c( c("simYear","pixelCount","pixelGroup", "ages"), sim$pooldef)
  updatePools <- cbind(
    rep(time(sim)[1], length(sim$pixelGroupC$ages)), pixelCount[, 2],
    sim$pixelGroupC$pixelGroup, sim$pixelGroupC$ages,
    sim$pixelGroupC[, Input:Products]
  )
  names(updatePools) <- c(c("simYear", "pixelCount", "pixelGroup", "ages"), sim$pooldef)

  sim$cbmPools <- rbind(sim$cbmPools, updatePools)
  ######## END OF UPDATING VECTORS FOR NEXT SIM YEAR #######################################
  #-----------------------------------

  return(invisible(sim))
}
