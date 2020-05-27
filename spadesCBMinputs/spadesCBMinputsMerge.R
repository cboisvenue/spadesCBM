
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "spadesCBMinputs",
  description = "A data preparation module to format all the user-provided input to the SpaDES forest-carbon modelling familly.", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Celine", "Boisvenue", email = "Celine.Boisvenue@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9007", spadesCBMinputs = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spadesCBMinputs.Rmd"),
  reqdPkgs = list("data.table","raster", "PredictiveEcology/LandR"),
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
  
  ## Rasters----------------------------------------------------------------------
  ### user provides age raster, raster linking each growth curve to pixels, a
  ### raster to match (masterRaster)
  masterRaster <- sim$masterRaster
  # see .inputObjects
  age <- sim$ageRaster
  gcIndex <- sim$gcIndexRaster
  spuRaster <- sim$spuRaster
  ecoRaster <- sim$ecoRaster
  # in one data table-------------------------------------
  #level2DT <- as.data.table(cbind(ages,rasterSps,Productivity,spatial_unit_id))	  
  level2DT <- Cache(data.table,spatial_unit_id = spuRaster[],ages = age[],pixelIndex = 1:ncell(age),
                    growth_curve_component_id= gcIndex[], growth_curve_id = gcIndex[],
                    ecozones = ecoRaster[])
  #level2DT <- level2DT[level2DT$rasterSps>0]
  level2DT <- level2DT[!is.na(ages) & !is.na(growth_curve_id)]
  spatialDT <- level2DT
  
  # END data.table----------------------------------------
  
  # create the pixel group---------------
  spatialDT <- spatialDT[order(pixelIndex),]
  spatialDT$pixelGroup <- LandR::generatePixelGroups(spatialDT,maxPixelGroup = 0,
                                                     columns = c("ages","spatial_unit_id", "growth_curve_component_id","ecozones" ))
  spatialDT <- spatialDT[order(pixelIndex),]
  spatialDT <- spatialDT[,.(ages, spatial_unit_id, pixelIndex,
                            growth_curve_component_id, growth_curve_id, ecozones,pixelGroup)]
  spatialDT <- spatialDT[order(pixelIndex),]
  ###RIA
  sim$spatialDT <- spatialDT
  # end create pixel groups-------------
  
  # make the data.table that will be used in simulations and checks that have 1
  # row per pixel group-------------------------------------------------------
  
  level3DT <- unique(spatialDT[,-("pixelIndex")])%>% .[order(pixelGroup),]
  # might have to keep this when we integrate the disturbances
  ###RIA 
  sim$level3DT <- level3DT
  # end level3DT-------------------------------------------------------------
  

  

  ############################################################
  ## SK: can't seem to solve why growth curve id 58 (white birch, good
  ## productivity) will not run with ages=1 it gets stuck in the spinup. Here is
  ## my therory: there is a mismatch in the pinup disturbances which creates
  ## problems with this specific growth curve (id 58). Because the first few
  ## years of growth are 0 it does not grow and it does not fill-up the soil
  ## pools. this is a problem to tackle once we have some insight into the cpp
  ## code
  ###########################################################
  # temp fix:
  sim$level3DT[ages==1 & growth_curve_component_id==58,ages:=3]
  
  sim$level3DT[order(pixelGroup),]
  ## TRYING TO DEBUG THE SPINUP: solution to set all ages that are 1 to 3.
  # shorten the level3DT - start with 100st lines
  #sim$level3DT <- sim$level3DT[1:5000,]
  sim$level3DT[ages==1,ages:=3]
  sim$ages <- sim$level3DT[,ages]
  ## DECISION: there are stands over 350 years old and we do not have growth
  ## curves past 350. So I am setting all ages about 350 to 350. This means that
  ## we are not tracking old stands but also, this problem will go away once we
  ## use LandR for the biomass increments
  sim$ages[sim$ages>350] <- 350
  sim$nStands <- length(sim$ages)
  
  ## the pooldef needs to be a sim$ because if will be used in the spatial data portion later
  sim$pools <- matrix(ncol = sim$PoolCount, nrow=sim$nStands, data=0)
  colnames(sim$pools)<- sim$pooldef
  sim$pools[,"Input"] = rep(1.0, nrow(sim$pools))
  
  sim$gcids <- sim$level3DT[,growth_curve_component_id]
  ## NEED an error message here if this vector any of the gcids are NOT 
  # in the increments (and therefore in the gcHash)
  # gcidUnique <- unique(sim$gcids)
  # sim$gcids[!(sim$gcids %in% gcidUnique)]
  sim$delays <-  rep.int(0,sim$nStands)
  sim$minRotations <- rep.int(10,sim$nStands)
  sim$maxRotations <- rep.int(30,sim$nStands)
  retInt <- merge(sim$level3DT[,],sim$cbmData@spinupParameters[,c(1,2)], by="spatial_unit_id", all.x=TRUE) %>% .[order(pixelGroup)]
  sim$returnIntervals <- retInt[,"return_interval"]
  sim$spatialUnits <- sim$level3DT[,spatial_unit_id]
  #spu <- as.data.frame(sim$cbmData@spatialUnitIds)
  
  # need an ecozone vector
  sim$ecozones <- sim$level3DT$ecozones
  
  ################################################################################
  ## matching the disturbances with the Disturbance Matrix IDs in CBM-CFS3 defaults
  ################################################################################
  # Matching disturbances to CBM disturbance matrix id---------------------------------
  # make the disturbance look-up table to the disturbance_matrix_id(s)
  # making sim$mySpuDmids
  
  ## SK specific from HERE TO
  # #raster values 1 to 5
  # #C:\Celine\GitHub\spadesCBM\data\forIan\SK_data\disturbance_Sask\ReadMe.txt
  # # Fire =  1
  # # Harvest = 2
  # # Lcondition = 3
  # # Road = 4
  # # Unclass = 5
  # 
  # Disturbance Matrices are specific to spatial_units_ids
  spu <- sim$unique(spatialDT$spatial_unit_id)
  # what disturbances in those spu(s)?
  # this function is in spadesCBMinputsFunctions.R
  # it lists all the possible disturbances in the CBM-CFS3 archive for that/those
  # spatial unit with the name of the disturbance in the 3rd colum.
  listDist <- spuDist(spu)

  #This extras all with wildfire in the name 
  fire <- listDist[grep("wildfire",listDist[,3], ignore.case=TRUE),1:3]
## NEED A CHECK AND MESSAGE HERE - more then one per spu? if so user has to pick
  
  ## Clearcut
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
  # ## TO HERE
  
  # need to match the historic and last past dist to the spatial unit
  # DECISION: both the last pass and the historic disturbance will be the same for these runs
  # setkey(sim$level3DT,spatial_unit_id)
  # setkey(as.data.table(fire[,1:2]),spatial_unit_id)
  # histLastDMIDs <- merge(sim$level3DT,fire)

  sim$mySpuDmids <- fread(file.path(getwd(),"/spadesCBMinputs/data/mySpuDmids.csv"))
  mySpuFires <- sim$mySpuDmids[grep("wildfire",sim$mySpuDmids$distName, ignore.case=TRUE),]

  myFires <- mySpuFires[spatial_unit_id %in% unique(sim$level3DT$spatial_unit_id),]
  setkey(myFires,spatial_unit_id)
  setkey(sim$level3DT,spatial_unit_id)
  
  histLastDMIDs <- merge(sim$level3DT,myFires)

  sim$historicDMIDs <- histLastDMIDs$disturbance_matrix_id
  sim$lastPassDMIDS <- histLastDMIDs$disturbance_matrix_id
  # and merge them on the level3DT$spatial_unit_id
  
  
  #sim$historicDMIDs <- rep.int(214,sim$nStands)#c(214)#,1,1,1)
  #sim$lastPassDMIDS <- rep.int(214,sim$nStands)#c(214)#,1,1,1)
   
  ## STILL HAVE TO FIGURE THIS OUT  - what identifies what disturbance ##RIA
  #sim$mySpuDmids <- cbind(mySpuDmids,events)
  

  ###RIA: I do not have the harvest or fire layers for the RIA. For now, I will
  #try to disturbe pixels using a fire composite that was created for Ana
  #Raymundo. The raster composite is here:
  #https://drive.google.com/open?id=10aQUa_QCS6UMvoeFV-WnBSFk8YCqOBsT
  sim$disturbanceRasters <- raster(file.path(dataPath,"/RIA2019/firesComposite.tif"))
    # list.files("data/forIan/SK_data/CBM_GIS/disturbance_testArea",
    #                                full.names = TRUE) %>%
    # grep(., pattern = ".grd$", value = TRUE)
    # 
  
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
  if (!suppliedElsewhere(sim$pooldef)) {
    sim$pooldef <- c("Input",
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
  sim$PoolCount <- length(sim$pooldef)
  }
  
    #needRTM <- FALSE
 # if (is.null(sim$masterRaster)) {
    if (!suppliedElsewhere("masterRaster",sim)){
    # local file here Cache(raster,file.path(getwd(),"data/forIan/SK_data/CBM_GIS/ldSp_TestArea.tif"))
    # here should be all the fixes needed (if any). For SK, the RTM is the
    # leading species raster with the 0 needing to be changed to NAs
    sim$masterRaster <- raster(file.path(getwd(),"/spadesCBMinputs/data/ldSp_TestArea.tif"))
    sim$masterRaster[masterRaster == 0] <- NA
    
      # help - can't get this to work.                  
      # prepInputs( # Cache
      # url ="https://drive.google.com/file/d/17D76wWir5dCFWb3AFEPiEMz5Q64JorPz/view?usp=sharing",
      # fun = raster::raster)
    }
    ## comments below taken from BiomassSpeciesData.R - will add messages when things work
    # if (!suppliedElsewhere("masterRaster", sim)) {      ## if one is not provided, re do both (safer?)
    #   needRTM <- TRUE
    #   message("There is no masterRaster supplied; will attempt to use rawBiomassMap")
    # } else {
    #   stop("masterRaster is going to be supplied, but ", currentModule(sim), " requires it ",
    #        "as part of its .inputObjects. Please make it accessible to ", currentModule(sim),
    #        " in the .inputObjects by passing it in as an object in simInit(objects = list(masterRaster = aRaster)",
    #        " or in a module that gets loaded prior to ", currentModule(sim))
    # }
#	}
  if(!suppliedElsewhere(sim$ageRaster)){
    sim$ageRaster <- raster(file.path(getwd(),"/spadesCBMinputs/data/age_TestArea.tif"))
      # Cache(prepInputs, 
      #                         url = "https://drive.google.com/file/d/1XhnF6s_V350Z916NKg9nX9Lw1WyTtC0s/view?usp=sharing",
      #                         fun = raster::raster)
      # 
  
  }
  
  if(!suppliedElsewhere(sim$gcIndexRaster)){
    # in Sk, the gcID is defined by the leading species the productivity level
    # there are 1,2 and 3 productivity levels, but we only have a few species that have 2 levels,
    # the others have one
    sim$gcIndexRaster <- raster(file.path(getwd(),"spadesCBMinputs/data/gcIndex.tif"))
    # Cache(prepInputs, 
    #                         url = "https://drive.google.com/file/d/1XhnF6s_V350Z916NKg9nX9Lw1WyTtC0s/view?usp=sharing",
    #                         fun = raster::raster)
    # 
  }
  if(!suppliedElsewhere(sim$spuRaster)){
    # canadaSpu <- shapefile("data/spUnit_Locator.shp")
    # spuShp <- postProcess(canadaSpu, rasterToMatch = masterRaster, targetCRS = crs(masterRaster),
    #                       useCache = FALSE, filename2 = NULL)
    # spuRaster <- fasterize::fasterize(sf::st_as_sf(spuShp), raster = masterRaster, field = "spu_id")
    
    sim$spuRaster <- raster(file.path(getwd(),"spadesCBMinputs/data/spUnits_TestArea.tif"))
    # For this raster the "big" spatial unit map for Canada would need to be
    # cropped to the study area
    
  }
  if(!suppliedElsewhere(sim$ecoRaster)){
    sim$ecoRaster <- raster(file.path(getwd(),"spadesCBMinputs/data/ecoRaster.tif"))
    # ecozones <- prepInputs(# targetFile = asPath(ecodistrictFilename),
    #   #archive = asPath("ecodistrict_shp.zip"),
    #   url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
    #   #alsoExtract = ecodistrictAE,
    #   destinationPath = file.path(getwd(),"spadesCBMinputs/data/"),
    #   rasterToMatch = masterRaster,
    #   overwrite = TRUE,
    #   fun = "raster::shapefile",
    #   filename2 = TRUE)#,
    # #userTags = cacheTags)
    # ecozones <- cropInputs(ecozones, rasterToMatch = masterRaster)
    # ecozonesRas <- fasterize::fasterize(sf::st_as_sf(ecozones), raster = masterRaster,
    #                                     field = "ECOZONE")
    
  }
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
