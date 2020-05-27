## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "spadesCBMm3ToBiomass",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.9", spadesCBMm3ToBiomass = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "spadesCBMm3ToBiomass.Rmd")),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.spadesCBMm3ToBiomass = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "spadesCBMm3ToBiomass", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "spadesCBMm3ToBiomass", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spadesCBMm3ToBiomass", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "spadesCBMm3ToBiomass", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "spadesCBMm3ToBiomass", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "spadesCBMm3ToBiomass", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  ## Read-in user
  #OLD#### process growth curves #########--------------------------------------------------------------
  
  # first reduce to the curves applicable to the study area----
  ecoToSpu <- as.data.table(sim$cbmData@spatialUnitIds)
  ecozones <- ecoToSpu[which(ecoToSpu$SpatialUnitID %in% unique(sim$level3DT$spatial_unit_id)),]
  gcID <- gcID[spatial_unit_id %in% ecozones$SpatialUnitID,]
  # END study area---------------------------------------------
  
  # reduce curves to the species we have--------------------------
  gcID <- gcID[rasterSps %in% sim$level3DT$rasterSps,]
  # END reduce to species-----------------------------------------
  
  # read-in the m3/ha values-----------------------------
  growthCurveComponents <- fread(sim$gcurveComponentsFileName)
  # END read-in m3/ha------------------------------------
  
  # read-in Boudewyn et al parameters for conversion from m3/ha to biomass in
  # the three main carbon pools that make-up the $growth_increments used to move
  # spadesCBM forward in growth from year to year
  # https://nfi.nfis.org/en/biomass_models-------------------------------------
  ## danger hard coded## need to change this to read URL or cache these.
  table3 <- read.csv("data/appendix2_table3.csv")#)file.path(paths(sim)$inputPath,"appendix2_table3.csv"
  table4 <- read.csv("data/appendix2_table4.csv")
  table5 <- read.csv("data/appendix2_table5.csv")
  table6 <- read.csv("data/appendix2_table6_v2.csv")
  
  # identify jurisdiction matching CBM-legacy numbering with Boudewyn
  # jurisdiction params----------------------------------------------
  # choices are: 
  # table3$juris_id and table4$juris_id and table6$jur
  # AB BC MB NB NF NS NT NU ON PE QC SK YK
  # table5$juris_id
  # AB BC NB NF NT
  cbmAdmin <- c(10,11,8,5,1,2,3,13,14,7,4,6,9,12)## danger hard coded##
  paramJur <- c("AB","BC","MB","NB","NF","NF","NS" ,"NT" ,"NU" ,"ON" ,"PE", "QC", "SK", "YK")
  adminMatch <- as.data.table(cbind(cbmAdmin,paramJur))
  jurisdiction <- as.character(adminMatch[which(cbmAdmin %in% unique(ecoToSpu[SpatialUnitID %in% unique(gcID$spatial_unit_id),2])),2])
  sktable3 <- as.data.table(table3[table3$juris_id==jurisdiction,])
  sktable4 <- as.data.table(table4[table4$juris_id==jurisdiction,])
  # table5 is weird since they did not have enough data for SK. I am selecting AB
  # instead. Another catch is that they don't have the same species match. I
  # manually check and ABIES is genus 3 (used below)
  #### PUT error message if the specified jurisdiction is not found #### GIVE CHOICES
  sktable5 <- as.data.table(table5[table5$juris_id=="AB",])
  sktable6 <- as.data.table(table6[table6$jur==jurisdiction,])
  # END jurisdiction-----------------------------------------------
  
  # read-in species match with canfi_species code and genus to get rigth
  # Boudewyn params---------------------------------------------------
  ## danger this is hard coded ## Species match will have to be checked by user
  spsMatch <- fread("data/spsMatchNameRasterGfileBiomParams.csv")#file.path(paths(sim)$inputPath,"spsMatchNameRasterGfileBiomParams.csv"
  # Match gcID$species to spsMatch$speciesName, then sktable3-4 have
  # $canfi_species, sktable5 $genus, sktable6 has $species which is equilvalent
  # to $canfi_species
  
  fullSpecies <- unique(gcID$species)
  swInc <- NULL
  hwInc <- NULL
  
  for(i in 1:length(fullSpecies)){
    speciesMeta <- gcID[species==fullSpecies[i],]
    for(j in 1:length(unique(speciesMeta$growth_curve_component_id))){
      meta <- speciesMeta[j,]
      id <- growthCurveComponents$GrowthCurveComponentID[which(growthCurveComponents$GrowthCurveComponentID == meta$growth_curve_component_id)][-1]
      ### IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ###
      age <- growthCurveComponents[GrowthCurveComponentID==meta$growth_curve_component_id,Age][-1]
      cumBiom <- as.matrix(convertM3biom(meta = meta,gCvalues = growthCurveComponents,spsMatch=spsMatch, 
                                         ecozones = ecozones,params3=sktable3, params4=sktable4, 
                                         params5=sktable5,params6=sktable6))
      # going from tonnes of biomass/ha to tonnes of carbon/ha here
      cumBiom <- cumBiom*0.5
      inc <- diff(cumBiom)
      if(meta$forest_type_id==1){
        incs  <- cbind(id,age,inc,rep(0,length(age)),rep(0,length(age)),rep(0,length(age)))
        swInc <- rbind(swInc,incs)
        #FYI:
        # cbmTables$forest_type
        # id           name
        # 1  1       Softwood
        # 2  2      Mixedwood
        # 3  3       Hardwood
        # 4  9 Not Applicable
      } else if(meta$forest_type_id==3){incs <- cbind(id,age,rep(0,length(age)),rep(0,length(age)),rep(0,length(age)),inc)
      hwInc <- rbind(hwInc,incs)}
    }
  }
  colnames(swInc) <- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
  colnames(hwInc) <- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
  increments <- as.data.table(rbind(swInc,hwInc)) %>% .[order(id),]
  interim <- as.matrix(increments)
  interim[is.na(interim)] <- 0
  increments <- as.data.table(interim)
  
  #################### HARD CODED FIXES TO THE CURVES OUT OF THE BOUDEWYN PARAMS THAT DON"T WORK#########
  ## BLACK SPRUCE (in ecozone 9) does not work so take ecozone 6
  ## id 49 becomes 28
  ## id 50 becomes 29
  ## white birch does not work at all, so take lower productivity trembling aspen
  ## ids 38 and 58 become 34
  
  increments[id==49,3:8] <- increments[id==28,3:8]
  increments[id==50,3:8] <- increments[id==29,3:8]
  increments[id==37,3:8] <- increments[id==34,3:8]
  increments[id==58,3:8] <- increments[id==34,3:8]
  ## NEGATIVES PRIOR TO 80 become 0
  #gc[value < 0 & age<80, value := 0]
  increments[age<80 & swmerch < 0, swmerch := 0]
  increments[age<80 & swfol < 0, swfol := 0]
  increments[age<80 & swother < 0, swother := 0]
  increments[age<80 & hwmerch < 0, hwmerch := 0]
  increments[age<80 & hwfol < 0, hwfol := 0]
  increments[age<80 & hwother < 0, hwother := 0]
  
  
  sim$growth_increments <- as.matrix(increments)
  # END process growth curves -------------------------------------------------------------------------------
  
  sim$gcHash <- matrixHash(sim$growth_increments)
  #create a nested hash (by gcid/by age)
  ## used in SpinUp function later...
  for(item in ls(sim$gcHash)){
    sim$gcHash[[item]] <- hash(sim$gcHash[[item]])
  }
  
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
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
