defineModule(sim, list(
  name = "CBM_vol2biomass",
  description = paste("A module to prepare the user-provided growth and yield information for use",
                      "in the family of models spadesCBM - CBM-CFS3-like simulation of forest",
                      "carbon in the platform SpaDES. This module takes in user-provided m3/ha",
                      "and meta data for teh growth curves and returns annual increments for",
                      "the aboveground live c-pools."),
  keywords = "",
  authors = c(
    person("Celine", "Boisvenue", email = "Celine.Boisvenue@canada.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.9", CBM_vol2biomass = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "CBM_vol2biomass.Rmd")),
  reqdPkgs = list(
    "mgcv", "ggplot2", "quickPlot", "ggpubr",
    "CBMutils" ## TODO: use PredictiveEcology/CBMutils
  ),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(
      ".plotInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".useCache", "logical", FALSE, NA, NA,
      paste(
        "Should this entire module be run with caching activated?",
        "This is generally intended for data-type modules, where stochasticity",
        "and time are not relevant"
      )
    )
  ),
  inputObjects = bind_rows(
    # expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    # this are variables in inputed data.tables:SpatialUnitID, EcoBoundaryID, juris_id, ecozone, jur, eco, name, GrowthCurveComponentID, plotsRawCumulativeBiomass, checkInc
    expectsInput(
      objectName = "table3", objectClass = "dataframe", desc = "Stem wood biomass model parameters for merchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv"
    ),
    expectsInput(
      objectName = "table4", objectClass = "dataframe", desc = "Stem wood biomass model parameters for nonmerchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv"
    ),
    expectsInput(
      objectName = "table5", objectClass = "dataframe", desc = "Stem wood biomass model parameters for sapling-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv"
    ),
    expectsInput(
      objectName = "table6", objectClass = "dataframe", desc = "Proportion model parameters from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv"
    ),
    expectsInput(
      objectName = "table7", objectClass = "dataframe", desc = "Caps on proportion models from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv"
    ),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "dataframe",
      desc = "Provides equivalent between provincial boundaries, CBM-id for provincial boundaries and CBM-spatial unit ids",
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"
    ),
    expectsInput(objectName = "gcMeta", objectClass = "dataframe", desc = "Provides equivalent between provincial boundaries,
                 CBM-id for provincial boundaries and CBM-spatial unit ids", sourceURL = NA),
    expectsInput(objectName = "gcMetaFile", objectClass = "character", desc = "File name and location for the user provided gcMeta dataframe", sourceURL = NA),
    expectsInput(objectName = "canfi_species", objectClass = "dataframe", desc = "File containing the possible species in the Boudewyn table - note
                 that if Boudewyn et al added species, this should be updated. Also note that such an update is very unlikely", sourceURL = NA),
    expectsInput(objectName = "userGcM3File", objectClass = "character", desc = "User file name for the files containing:
                 GrowthCurveComponentID,Age,MerchVolume. Default name userGcM3", sourceURL = NA),
    expectsInput(objectName = "userGcM3", objectClass = "dataframe", desc = "User file containing:
                 GrowthCurveComponentID,Age,MerchVolume. Default name userGcM3",
                 sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"),
    expectsInput(objectName = "ecozones", objectClass = "data.table", desc = "the table linking the spu id, with the
                  disturbance_matrix_id and the events. The events are the possible raster values from the disturbance rasters of Wulder and White"),
    expectsInput(objectName = "gcids", objectClass = "data.table", desc = "the table linking the spu id, with the
                  disturbance_matrix_id and the events. The events are the possible raster values from the disturbance rasters of Wulder and White"),
    expectsInput(objectName = "spatialUnits", objectClass = "data.table", desc = "the table linking the spu id, with the
                  disturbance_matrix_id and the events. The events are the possible raster values from the disturbance rasters of Wulder and White")
  ),
  outputObjects = bind_rows(
    # createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA),
    createsOutput(objectName = "volCurves", objectClass = "plot", desc = "Plot of all the growth curve provided by the user"),
    createsOutput(objectName = "plotsRawCumulativeBiomass", objectClass = "plot", desc = "Plot of cumulative m3/ha curves
                  translated into tonnes of carbon/ha, per AG pool, prior to any smoothing"),
    createsOutput(objectName = "checkInc", objectClass = "plot", desc = "Plot of 1/2 of the increment per AG pool,
                  calculated from the smoothed cumulative tonnes c/ha, derived into increments, per AG pool. "),
    createsOutput(objectName = "growth_increments", objectClass = "matrix", desc = "Matrix of the 1/2 increment that will be used to create the gcHash"),
    createsOutput(objectName = "gcHash", objectClass = "environment", desc = "Environment pointing to each gcID, that is itself an environment,
                  pointing to each year of growth for all AG pools.Hashed matrix of the 1/2 growth increment.
                  This is used in the c++ functions to increment AG pools two times in an annual event (in the spadesCBMcore.R module.")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.CBM_vol2biomass <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "CBM_vol2biomass", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "CBM_vol2biomass", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "CBM_vol2biomass", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "CBM_vol2biomass", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "CBM_vol2biomass", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "CBM_vol2biomass", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions

Init <- function(sim) {
  # user provides userGcM3: incoming cumulative m3/ha
  # plot
  sim$volCurves <- ggplot(data = sim$userGcM3, aes(x = Age, y = MerchVolume, group = GrowthCurveComponentID, colour = GrowthCurveComponentID)) +
    geom_line() ## TODO: move to plotInit event
  message("User: please look at the curve you provided via sim$volCurves")
  ## not all curves provided are used in the simulation - and ***FOR NOW*** each
  ## pixels only gets assigned one growth curve (no transition, no change in
  ## productivity).
  ## To run module independently, the gcID used in this translation can be specified here
  # if(!suppliedElsewhere("level3DT",sim)){
  #   userGcM3 <- sim$userGcM3
  # }else{
  userGcM3 <- sim$userGcM3[GrowthCurveComponentID %in% unique(sim$gcids), ]
  # }

  # START reducing Biomass model parameter tables -----------------------------------------------
  # reducing the parameter tables to the jurisdiction or ecozone we have in the study area
  ## To run module independently, the gcID used in this translation can be specified here
  # if(!suppliedElsewhere("spatialUnits",sim)){
  #   spu  <- ### USER TO PROVIDE SPU FOR EACH gcID###########
  # }else{
  spu <- unique(sim$spatialUnits)
  # }
  # if(!suppliedElsewhere("ecozones",sim)){
  #   eco <- ### USER TO PROVIDE SPU FOR EACH gcID###########
  # }else{

  eco <- unique(sim$ecozones)
  # }
  thisAdmin <- sim$cbmAdmin[sim$cbmAdmin$SpatialUnitID %in% spu & sim$cbmAdmin$EcoBoundaryID %in% eco, ]

  # "s" table for small table3, 4, 5, 6, 7 - tables limited to the targeted
  # ecozones and jurisdictions
  stable3 <- as.data.table(sim$table3[sim$table3$juris_id %in% thisAdmin$abreviation &
    sim$table3$ecozone %in% eco, ])
  stable4 <- as.data.table(sim$table4[sim$table4$juris_id %in% thisAdmin$abreviation &
    sim$table4$ecozone %in% eco, ])
  # table5 is different since there was not have enough data to fit models for
  # all provinces. Here we are hard-coding the closest equivalent province to
  # have a complete set.
  # This first If-statement is to catch the "no-province" match

  stable5.2 <- as.data.table(sim$table5[sim$table5$juris_id %in% thisAdmin$abreviation, ])
  if (!length(unique(stable5.2$juris_id)) == length(unique(thisAdmin$abreviation))) {
    ## DANGER HARD CODED: if NFIS changes table 5, this will no longer be valid
    # juris_id: there are only 5/13 possible
    # these are the provinces available: AB BC NB NF NT
    # for the non match these would be the equivalent
    # "PE" - NB
    # "QC" - NB
    # "ON" - NB
    # "MB" - AB
    # "SK" - AB
    # "YK" - NT
    # "NU" - NT
    abreviation <- c("PE", "QC", "ON", "MB", "SK", "YK", "NU")
    t5abreviation <- c("NB", "NB", "NB", "AB", "AB", "NT", "NT")
    abreviaReplace <- data.table(abreviation, t5abreviation)
    # replace the abbreviations and select
    thisAdmin5 <- merge(abreviaReplace, thisAdmin)
    thisAdmin5[, c("abreviation", "t5abreviation") := list(t5abreviation, NULL)]
    stable5.2 <- as.data.table(sim$table5[sim$table5$juris_id %in% thisAdmin5$abreviation, ])
  }
  # This second "if-statement" is to catch is the "no-ecozone" match
  ### THIS NEEDS TO BE TESTED
  if (nrow(stable5.2) > 0) {
    stable5 <- stable5.2[ecozone %in% unique(eco), ]
  } else {
    stop(
      "There are no matches found for the parameters needed to execute the Boudewyn models.",
      "Please manually find matches for table 5."
    )
  }
  if (!length(eco) == length(unique(stable5$ecozone))) {
    # there are 9/15 ecozones
    # These are the ones in table5
    # id               name
    # 4       Taiga Plains
    # 5  Taiga Shield West
    # 6 Boreal Shield West
    # 7  Atlantic Maritime
    # 9      Boreal Plains
    # 10  Subhumid Prairies
    # 12  Boreal Cordillera
    # 13   Pacific Maritime
    # 14 Montane Cordillera

    # these are the ones that are not
    # id               name
    # 8   Mixedwood Plains  - 7  Atlantic Maritime
    # 11   Taiga Cordillera - 4 taiga plains
    # 15      Hudson Plains - 6 Boreal Shield West
    # 16  Taiga Shield East - 5  Taiga Shield West
    # 17 Boreal Shield East - 6 Boreal Shield West
    # 18  Semiarid Prairies - 10  Subhumid Prairies

    EcoBoundaryID <- c(8, 11, 15, 16, 17, 18)
    ecoNotInT5 <- c(7, 4, 6, 5, 6, 10)
    ecoReplace <- data.table(ecoNotInT5, EcoBoundaryID)
    thisAdmin5.1 <- merge(ecoReplace, thisAdmin5, by = EcoBoundaryID)
    stable5 <- as.data.table(stable5[stable5$ecozone %in% thisAdmin5.1$EcoBoundaryID, ])
  }
  if (nrow(stable5) < 1) {
    stop("There is a problem finding a parameter match in table 5.")
  }

  stable6 <- as.data.table(sim$table6[sim$table6$jur %in% thisAdmin$abreviation &
    sim$table6$eco %in% eco, ])
  stable7 <- as.data.table(sim$table7[sim$table6$jur %in% thisAdmin$abreviation &
    sim$table6$eco %in% eco, ])
  # END reducing Biomass model parameter tables -----------------------------------------------

  # Read-in user provided meta data for growth curves. This could be a complete
  # data frame with the same columns as gcMetaEg.csv OR is could be only curve
  # id and species. This format is necessary to process the curves and use the
  # resulting increments
  gcMeta <- sim$gcMeta

  # checking how many columns in gcMeta, if not 6, columns need to be added
  if (!ncol(gcMeta) == 6) {
    # help the user go from their growth curve id and leading species to the six
    # columns: names(gcMeta)
    # [1] "growth_curve_id"           "growth_curve_component_id"
    # [3] "species"                   "canfi_species"
    # [5] "genus"                     "forest_type_id"
    # the data frame canfi_species.csv (in userData_Defaults_spadesCBM -
    # https://drive.google.com/drive/folders/1OBDTp1v_3b3D3Yvg1pHLiW6A_GRklgpD?usp=sharing)
    # has all the possible options for canfi_species (number), genus (4 letter
    # code) and species (three letter code).
    gcMeta2 <- gcMeta[, .(growth_curve_id, species)]
    gcMeta2[, growth_curve_component_id := growth_curve_id]
    # check if all the species are in the canfi_species table
    ### THIS HAS NOT BEEN TESTED YET
    if (nrow(gcMeta2) == length(which(gcMeta$species %in% sim$canfi_species$name))) {
      spsMatch <- sim$canfi_species[
        , which(name %in% gcMeta2$species),
        .(canfi_species, genus, name, forest_type_id)
      ]
      names(spsMatch) <- c("canfi_species", "genus", "species", "forest_type_id")
      setkey(gcMeta2, species)
      setkey(spsMatch, species)
      gcMeta3 <- merge(gcMeta2, spsMatch) # I do not think the order of the columns matter
      gcMeta <- gcMeta3
    }
    ### PUT SOMETHING HERE IF THE SPECIES DONT MATCH...NOT SURE WHAT - ERROR MESSAGE?
  }

  # assuming gcMeta has now 6 columns, it needs a 7th: spatial_unit_id. This
  # will be used in the convertM3biom() fnct to link to the right ecozone
  # and it only needs the gc we are using in this sim.
  gcThisSim <- as.data.table(unique(cbind(sim$spatialUnits, sim$gcids)))
  names(gcThisSim) <- c("spatial_unit_id", "growth_curve_component_id")
  setkey(gcThisSim, growth_curve_component_id)
  setkey(gcMeta, growth_curve_component_id)
  gcMeta <- merge(gcMeta, gcThisSim)

  ### CHECK - this in not tested
  if (!unique(unique(userGcM3$GrowthCurveComponentID) == unique(gcMeta$growth_curve_component_id))) {
    stop("There is a missmatch in the growth curves of the userGcM3 and the gcMeta")
  }

  # START processing curves from m3/ha to tonnes of C/ha then to annual increments
  # per above ground biomass pools -------------------------------------------

  ### NEED TO MAKE SURE THE PROVIDED CURVES ARE ANNUAL
  ### if not, we need to extrapolate to make them annual

  # Matching is 1st on species, then on gcId which gives us location (admin,
  # spatial unit and ecozone)
  fullSpecies <- unique(gcMeta$species)
  cumPools <- NULL

  for (i in 1:length(fullSpecies)) {
    # matching on species name
    speciesMeta <- gcMeta[species == fullSpecies[i], ]
    # for each species name, process one gcID at a time
    for (j in 1:length(unique(speciesMeta$growth_curve_component_id))) {
      meta <- speciesMeta[j, ]
      id <- sim$userGcM3$GrowthCurveComponentID[which(sim$userGcM3$GrowthCurveComponentID == meta$growth_curve_component_id)][-1]
      ## IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
      age <- sim$userGcM3[GrowthCurveComponentID == meta$growth_curve_component_id, Age][-1]
      # series of fncts results in curves of merch, foliage and other (SW or HW)
      cumBiom <- as.matrix(convertM3biom(
        meta = meta, gCvalues = sim$userGcM3, spsMatch = gcMeta,
        ecozones = thisAdmin, params3 = unique(stable3), params4 = unique(stable4),
        params5 = unique(stable5), params6 = unique(stable6), params7 = unique(stable7)
      ))
      # going from tonnes of biomass/ha to tonnes of carbon/ha here
      cumBiom <- cumBiom * 0.5 ## this value is in sim$cbmData@biomassToCarbonRate
      # calculating the increments per year for each of the three pools (merch,
      # foliage and other (SW or HW))
      # inc <- diff(cumBiom)
      # CBM processes half the growth before turnover and OvermatureDecline, and
      # half after.
      # names(outInputs$allProcesses)
      # [1] "Disturbance"       "Growth1"           "DomTurnover"       "BioTurnover"
      # [5] "OvermatureDecline" "Growth2"           "DomDecay"          "SlowDecay"
      # [9] "SlowMixing"
      cumBiom <- cbind(id, age, cumBiom)

      cumPools <- rbind(cumPools, cumBiom)
    }
  }

  # Check models that are directly out of the Boudewyn-translation----------------------------
  # Usually, these need to be, at a minimum, smoothed out.

  # plotting the curves of the direct translation ------------------------
  # adding the zeros back in
  cumPools <- as.data.table(cumPools)
  id <- unique(cumPools$id)
  add0s <- cbind(id,
    age = rep(0, length(id)), totMerch = rep(0, length(id)),
    fol = rep(0, length(id)), other = rep(0, length(id))
  )
  cumPoolsRaw <- rbind(cumPools, add0s)
  cumPoolsRaw <- cumPoolsRaw[order(id, age)]

  # plotting and save the plots of the raw-translation in the sim$
  rawPlots <- m3ToBiomIncOnlyPlots(inc = cumPoolsRaw)
  # From: http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/

  # do.call(ggarrange, rawPlots)
  sim$plotsRawCumulativeBiomass <- do.call(
    ggarrange,
    append(
      rawPlots,
      list(
        common.legend = TRUE,
        legend = "right",
        labels = names(rawPlots),
        font.label = list(size = 10, color = "black", face = "bold"),
        label.x = 0.5
      )
    )
  )
  # dev.new()
  annotate_figure(sim$plotsRawCumulativeBiomass,
                  top = text_grob("Cumulative merch fol other by gc id", face = "bold", size = 14)
  )
  message("User: please inspect the translation of your growth curves via sim$plotsRawCumulativeBiomass.")

  # sim$gg
  # end plotting direct translations----------------------------------------

  # Example of smoothing: two choices presented, smoothing of the cumulative
  # carbon/ha curves or smoothing of the increment curves of carbon/ha

  # ####### Smoothing of start here#######################################################
  # ## CHOICE 1. on cumulative curves ###################################################
  # # created smooth cumulative curves using GAMs
  # sMerch <- NULL
  # sFol <- NULL
  # sOther <- NULL
  #
  # ## TODO: module not loading the package mgcv even though it is in reqdPkgs
  # #library(mgcv)
  #
  #
  # id <- unique(cumPoolsRaw$id)
  # for (val in id) {
  #   # per column
  #   # totMerch
  #   oneSet <- cumPoolsRaw[id == val, ]
  #   wts <- c(
  #     100, rep(2, (which(oneSet$totMerch == max(oneSet$totMerch)) - 1)),
  #     rep(1, (length(oneSet$age) - which(oneSet$totMerch == max(oneSet$totMerch))))
  #   )
  #   k <- 20
  #   gamMerch <- gam(oneSet$totMerch ~ s(oneSet$age, k = k), weight = wts, method = "REML")
  #   df1 <- as.data.frame(cbind(age = oneSet$age, totMerch = gamMerch$fitted.values))
  #   # User: this would be a check of the estimates from Boudewyn with the fitted GAM values
  #   # ggplot(oneSet, aes(age, totMerch)) + geom_point() +
  #   #  geom_line(data=df1,aes(color="Fitted GAM cumulative totMerch"))
  #   sMerch <- rbind(sMerch, df1)
  #   # fol
  #   # oneSet <- cbind(cumPoolsRaw[id==id[i],.(age,fol)])
  #   wts <- c(
  #     100, rep(2, (which(oneSet$fol == max(oneSet$fol)) - 1)),
  #     rep(1, (length(oneSet$age) - which(oneSet$fol == max(oneSet$fol))))
  #   )
  #   k <- 20
  #   gamMerch <- gam(oneSet$fol ~ s(oneSet$age, k = k), weight = wts, method = "REML")
  #   df2 <- as.data.frame(cbind(age = oneSet$age, fol = gamMerch$fitted.values))
  #   # User: this would be a check of the estimates from Boudewyn with the fitted GAM values
  #   # ggplot(oneSet, aes(age, fol)) + geom_point() +
  #   #   geom_line(data=df2,aes(color="Fitted GAM cumulative fol"))
  #   sFol <- rbind(sFol, df2)
  #   # other
  #   # oneSet <- cbind(cumPoolsRaw[id==id[i],.(age,other)])
  #   wts <- c(
  #     100, rep(2, (which(oneSet$other == max(oneSet$other)) - 1)),
  #     rep(1, (length(oneSet$age) - which(oneSet$other == max(oneSet$other))))
  #   )
  #   k <- 20
  #   gamMerch <- gam(oneSet$other ~ s(oneSet$age, k = k), weight = wts, method = "REML")
  #   df3 <- as.data.frame(cbind(age = oneSet$age, other = gamMerch$fitted.values))
  #   # User: this would be a check of the estimates from Boudewyn with the fitted GAM values
  #   # ggplot(oneSet, aes(age, other)) + geom_point() +
  #   #   geom_line(data=df3,aes(color="Fitted GAM cumulative other"))
  #   sOther <- rbind(sOther, df3)
  # }
  # # putting it all together
  # smoothCumPools <- as.data.table(cbind(cumPoolsRaw$id, sMerch, sFol[, 2], sOther[, 2]))
  # names(smoothCumPools) <- names(cumPoolsRaw)
  # setkey(smoothCumPools, id)
  # # half the increments are use at the begining of simulations and half later in
  # # the simulation. The order is:
  # # names(sim$allProcesses)
  # # [1] "Disturbance"       "Growth1"           "DomTurnover"
  # # [4] "BioTurnover"       "OvermatureDecline" "Growth2"
  # # [7] "DomDecay"          "SlowDecay"         "SlowMixing"
  # # here, calculating the increment, then dividing it in 2.
  # cols <- c("totMerch", "fol", "other")
  # newCols <- c("lMerch", "lFol", "lOther")
  # lagPools <- smoothCumPools[, (newCols) := lapply(.SD, data.table::shift), .SDcols = cols, by = "id"]
  # lagPools[is.na(lagPools)] <- 0
  # incCols <- c("incMerch", "incFol", "incOther")
  # incPools <- lagPools[, (incCols) := list(totMerch - lMerch, fol - lFol, other - lOther)][, .(id, age, incMerch, incFol, incOther)]
  #
  # forestType <- gcMeta[, .(id = growth_curve_component_id, forest_type_id)]
  # #       #FYI:
  # #       # cbmTables$forest_type
  # #       # id           name
  # #       # 1  1       Softwood
  # #       # 2  2      Mixedwood
  # #       # 3  3       Hardwood
  # #       # 4  9 Not Applicable
  # setkey(forestType, id)
  # incPools <- merge(incPools, forestType)
  # swCols <- c("swmerch", "swfol", "swother")
  # hwCols <- c("hwmerch", "hwfol", "hwother")
  #
  # totalIncrements <- incPools[forest_type_id == 1, (swCols) := list((incMerch), (incFol), (incOther))][forest_type_id == 3, (hwCols) := list((incMerch), (incFol), (incOther))]
  # totalIncrements[is.na(totalIncrements)] <- 0
  # outCols <- c("incMerch", "incFol", "incOther", "forest_type_id")
  # incCols <- c(swCols, hwCols)
  # totalIncrements[, (outCols) := NULL]
  # increments <- totalIncrements[, (incCols) := list(
  #   swmerch / 2, swfol / 2,
  #   swother / 2, hwmerch / 2, hwfol / 2, hwother / 2
  # )] %>% .[order(id, age), ]
  # incPlots <- m3ToBiomIncOnlyPlots(inc = increments)
  #
  # # From: http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/
  # # do.call(ggarrange, rawPlots)
  # sim$checkInc <- do.call(
  #   ggarrange,
  #   append(
  #     incPlots,
  #     list(
  #       common.legend = TRUE,
  #       legend = "right",
  #       labels = names(rawPlots),
  #       font.label = list(size = 10, color = "black", face = "bold"),
  #       label.x = 0.5
  #     )
  #   )
  # )
  # # dev.new()
  # annotate_figure(sim$checkInc,
  #   top = text_grob("Halved increments for merch fol other by gc id", face = "bold", size = 14)
  # )
  # message("User: please inspect the halved increments that are used in your simulation via sim$checkInc.")

  ## CHOICE 2.on increment curves ###################################################

  setkey(cumPoolsRaw, id)
  # half the increments are use at the begining of simulations and half later in
  # the simulation. The order is:
  # names(sim$allProcesses)
  # [1] "Disturbance"       "Growth1"           "DomTurnover"
  # [4] "BioTurnover"       "OvermatureDecline" "Growth2"
  # [7] "DomDecay"          "SlowDecay"         "SlowMixing"
  # here, calculating the increment, then dividing it in 2.
  cols <- c("totMerch", "fol", "other")
  newCols <- c("lMerch", "lFol", "lOther")
  lagPools <- cumPoolsRaw[, (newCols) := lapply(.SD, data.table::shift), .SDcols = cols, by = "id"]
  lagPools[is.na(lagPools)] <- 0
  incCols <- c("incMerch", "incFol", "incOther")
  incPools <- lagPools[, (incCols) := list(totMerch - lMerch, fol - lFol, other - lOther)][, .(id, age, incMerch, incFol, incOther)]
  rawIncPlots <- m3ToBiomIncOnlyPlots(inc = incPools)
  plotsRawInc <- do.call(
    ggarrange,
    append(
      rawIncPlots,
      list(
        common.legend = TRUE,
        legend = "right",
        labels = names(rawIncPlots),
        font.label = list(size = 10, color = "black", face = "bold"),
        label.x = 0.5
      )
    )
  )
  # dev.new()
  annotate_figure(plotsRawInc,
                  top = text_grob("RawIncrements merch fol other by gc id", face = "bold", size = 14)
  )

  ## calculate the GAMs on increments
  # created smooth cumulative curves using GAMs
  sIncMerch <- NULL
  sIncFol <- NULL
  sIncOther <- NULL

  ## TODO: module not loading the package mgcv even though it is in reqdPkgs
  library(mgcv)


  id <- unique(incPools$id)
  for (val in id) {
    # per column
    # incMerch
    oneSet <- incPools[id == val, ]
    wts <- c(
      100, rep(2, (which(oneSet$incMerch == max(oneSet$incMerch)) - 1)),
      rep(1, (length(oneSet$age) - which(oneSet$incMerch == max(oneSet$incMerch))))
    )
    k <- 20
    gamMerch <- gam(oneSet$incMerch ~ s(oneSet$age, k = k), weight = wts, method = "REML")
    df1 <- as.data.frame(cbind(age = oneSet$age, incMerch = gamMerch$fitted.values))
    # User: this would be a check of the estimates from Boudewyn with the fitted GAM values
    # ggplot(oneSet, aes(age, incMerch)) + geom_point() +
    #  geom_line(data=df1,aes(color="Fitted GAM cumulative incMerch"))
    sIncMerch <- rbind(sIncMerch, df1)
    # incFol
    # oneSet <- cbind(cumPoolsRaw[id==id[i],.(age,incFol)])
    wts <- c(
      100, rep(2, (which(oneSet$incFol == max(oneSet$incFol)) - 1)),
      rep(1, (length(oneSet$age) - which(oneSet$incFol == max(oneSet$incFol))))
    )
    k <- 20
    gamMerch <- gam(oneSet$incFol ~ s(oneSet$age, k = k), weight = wts, method = "REML")
    df2 <- as.data.frame(cbind(age = oneSet$age, incFol = gamMerch$fitted.values))
    # User: this would be a check of the estimates from Boudewyn with the fitted GAM values
    # ggplot(oneSet, aes(age, incFol)) + geom_point() +
    #   geom_line(data=df2,aes(color="Fitted GAM cumulative incFol"))
    sIncFol <- rbind(sIncFol, df2)
    # other
    # oneSet <- cbind(cumPoolsRaw[id==id[i],.(age,incOther)])
    wts <- c(
      100, rep(2, (which(oneSet$incOther == max(oneSet$incOther)) - 1)),
      rep(1, (length(oneSet$age) - which(oneSet$incOther == max(oneSet$incOther))))
    )
    k <- 20
    gamMerch <- gam(oneSet$incOther ~ s(oneSet$age, k = k), weight = wts, method = "REML")
    df3 <- as.data.frame(cbind(age = oneSet$age, incOther = gamMerch$fitted.values))
    # User: this would be a check of the estimates from Boudewyn with the fitted GAM values
    # ggplot(oneSet, aes(age, incOther)) + geom_point() +
    #   geom_line(data=df3,aes(color="Fitted GAM cumulative incOther"))
    sIncOther <- rbind(sIncOther, df3)
  }
  # putting it all together

  smoothIncPools <- as.data.table(cbind(incPools$id, sIncMerch, sIncFol[, 2], sIncOther[, 2]))
  names(smoothIncPools) <- names(incPools)
  smoothIncPlots <- m3ToBiomIncOnlyPlots(inc = smoothIncPools )
  # plot the smooth increments pools - any better?
  plotsSmoothIncs <- do.call(
    ggarrange,
    append(
      smoothIncPlots,
      list(
        common.legend = TRUE,
        legend = "right",
        labels = names(smoothIncPlots),
        font.label = list(size = 10, color = "black", face = "bold"),
        label.x = 0.5
      )
    )
  )
  # dev.new()
  annotate_figure(plotsSmoothIncs,
                  top = text_grob("RawIncrements merch fol other by gc id", face = "bold", size = 14)
  )


  sim$smoothIncPools <- smoothIncPools
  sim$plotsSmoothIncs <- plotsSmoothIncs

  forestType <- gcMeta[, .(id = growth_curve_component_id, forest_type_id)]
  #       #FYI:
  #       # cbmTables$forest_type
  #       # id           name
  #       # 1  1       Softwood
  #       # 2  2      Mixedwood
  #       # 3  3       Hardwood
  #       # 4  9 Not Applicable
  setkey(forestType, id)
  smoothIncPools <- merge(smoothIncPools, forestType, by = "id")
  swCols <- c("swmerch", "swfol", "swother")
  hwCols <- c("hwmerch", "hwfol", "hwother")

  totalIncrementsSmooth <- smoothIncPools[forest_type_id == 1, (swCols) := list((incMerch), (incFol), (incOther))][forest_type_id == 3, (hwCols) := list((incMerch), (incFol), (incOther))]
  totalIncrementsSmooth[is.na(totalIncrementsSmooth)] <- 0
  outCols <- c("incMerch", "incFol", "incOther", "forest_type_id")
  incCols <- c(swCols, hwCols)
  totalIncrementsSmooth[, (outCols) := NULL]
  increments <- totalIncrementsSmooth[, (incCols) := list(
    swmerch / 2, swfol / 2,
    swother / 2, hwmerch / 2, hwfol / 2, hwother / 2
  )] %>% .[order(id, age), ]
  ########finish here ##################

  ## these are the fixes needed to make the smoothed increments curves usable
  ## for simulation in the default example provided.
  hardFixes <- readline(prompt = "Are you use the default example for this simulation? \n Press Y for yes or N for no.")
  if (toupper(hardFixes) == "Y") {
     ### Danger hard coded fixes to see if the age=0 issue ("stuck in spinup")
  ### Using the smoothed increment
  ## replace id 34 with 35, and 55 with 56.
  increments[id == 34,3:8] <- increments[id == 35,3:8]
  increments[id == 55,3:8] <- increments[id == 56,3:8]
  }



  sim$growth_increments <- as.matrix(increments)
  # END process growth curves -------------------------------------------------------------------------------

  sim$gcHash <- matrixHash(sim$growth_increments)
  # create a nested hash (by gcid/by age)
  ## used in SpinUp function later...
  for (item in ls(sim$gcHash)) {
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
  # Plot(sim$object)

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

  # cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("gcids", sim)) {
    ## this is where the pixelGroups and their spu eco etc.
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to limit the number of growth curves used.")
    sim$gcids <- c(
      52, 52, 58, 52, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55, 58,
      61, 28, 29, 31, 34, 35, 37, 40, 49, 50, 52, 55, 58, 61, 28, 29,
      31, 34, 37, 40, 49, 50, 52, 55, 56, 58, 61, 28, 29, 31, 34, 40,
      49, 50, 52, 55, 58, 61, 28, 34, 49, 52, 55, 40, 28, 31, 34, 40,
      49, 50, 52, 55, 61, 28, 31, 34, 40, 49, 50, 52, 55, 61, 52, 55,
      58, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55, 58, 61, 28, 31, 34,
      37, 40, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37, 40, 49, 50, 52,
      55, 61, 28, 31, 34, 40, 49, 52, 55, 61, 28, 61, 52, 61, 62, 28,
      31, 34, 40, 49, 50, 52, 55, 61, 31, 34, 49, 52, 55, 28, 31, 34,
      40, 49, 50, 52, 55, 58, 61, 62, 28, 29, 31, 34, 40, 49, 50, 52,
      55, 61, 28, 34, 40, 49, 50, 52, 55, 61, 62, 28, 34, 40, 61, 49,
      31, 40, 49, 61, 28, 29, 31, 34, 40, 49, 50, 52, 58, 61, 28, 31,
      34, 40, 49, 50, 52, 55, 61, 49, 52, 55, 28, 31, 34, 40, 49, 50,
      52, 55, 58, 61, 28, 31, 34, 40, 49, 50, 52, 55, 61, 40, 49, 50,
      52, 61, 28, 31, 31, 61, 28, 31, 34, 49, 50, 55, 61, 28, 31, 34,
      49, 61, 28, 34, 52, 61, 31, 49, 52, 55, 55, 40, 28, 49, 28, 31,
      34, 49, 52, 28, 31, 58, 61, 28, 31, 34, 49, 50, 61, 52, 49, 52,
      55, 58, 31, 34, 37, 49, 52, 55, 52, 55, 58, 31, 34, 49, 52, 55,
      56, 58, 31, 34, 49, 52, 55, 56, 58, 61, 49, 52, 55, 52, 55, 28,
      34, 49, 55, 28, 31, 34, 37, 52, 55, 49, 52, 55, 28, 31, 34, 37,
      49, 52, 55, 58, 28, 31, 34, 37, 49, 52, 55, 58, 28, 31, 34, 37,
      49, 52, 55, 34, 37, 50, 52, 52, 28, 31, 34, 37, 52, 55, 28, 31,
      34, 37, 49, 52, 55, 58, 52, 55, 28, 31, 34, 37, 40, 49, 52, 55,
      58, 28, 31, 34, 37, 40, 49, 52, 55, 58, 61, 28, 31, 34, 37, 49,
      52, 55, 58, 28, 31, 34, 37, 52, 55, 31, 52, 55, 31, 28, 31, 34,
      37, 40, 49, 52, 55, 58, 28, 31, 34, 37, 40, 49, 50, 52, 55, 58,
      52, 55, 28, 31, 34, 37, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37,
      40, 49, 50, 52, 55, 58, 28, 31, 34, 37, 40, 49, 50, 52, 55, 58,
      28, 31, 34, 37, 49, 52, 55, 58, 34, 49, 55, 28, 31, 28, 31, 34,
      49, 52, 55, 58, 28, 31, 34, 37, 49, 50, 52, 55, 58, 61, 49, 52,
      28, 31, 34, 37, 40, 49, 50, 52, 55, 58, 28, 29, 31, 34, 35, 37,
      40, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37, 40, 49, 50, 52, 55,
      58, 61, 28, 31, 34, 49, 52, 55, 58, 52, 28, 28, 34, 49, 55, 58,
      61, 28, 34, 37, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37, 40, 49,
      50, 52, 55, 58, 61, 28, 31, 34, 37, 49, 50, 52, 55, 58, 61, 28,
      31, 34, 49, 50, 52, 55, 58, 61, 28, 40, 49, 55, 58, 49, 34, 28,
      31, 34, 49, 50, 52, 55, 58, 28, 31, 34, 37, 40, 49, 50, 52, 55,
      58, 61, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55, 58, 61, 28, 29,
      31, 34, 37, 40, 49, 50, 52, 55, 56, 58, 61, 28, 29, 31, 34, 37,
      40, 49, 50, 52, 55, 58, 61, 28, 29, 31, 34, 40, 49, 50, 52, 55,
      61, 31, 50, 49, 52, 61, 28, 31, 34, 49, 50, 52, 55, 58, 61, 28,
      31, 34, 37, 40, 49, 50, 52, 55, 58, 61, 52, 28, 31, 34, 37, 40,
      49, 50, 52, 55, 58, 61, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55,
      58, 61, 28, 31, 34, 40, 49, 50, 52, 55, 58, 61, 28, 34, 49, 50,
      52, 55, 58, 61, 49, 50, 55, 61, 49, 52, 55, 58, 61, 28, 29, 31,
      34, 40, 49, 50, 52, 55, 58, 61, 28, 29, 31, 34, 37, 40, 49, 50,
      52, 55, 58, 61
    )
  }

  if (!suppliedElsewhere("ecozones", sim)) {
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to determine which ecozones these curves are in.")
    sim$ecozones <- c(
      9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 6, 6, 6, 6,
      6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6,
      6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 6, 6, 6, 6, 9, 9,
      9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 9, 9, 6, 6, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 6, 6, 6, 9, 6,
      6, 6, 9, 9, 9, 9, 6, 6, 6, 9, 9, 6, 6, 9, 9, 6, 9, 9, 9, 9, 6,
      6, 9, 6, 6, 6, 9, 9, 6, 6, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9,
      9, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 6, 6, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6,
      9, 9, 9, 6, 6, 9, 9, 9, 6, 6, 6, 6, 9, 9, 6, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 6, 9, 9, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 6, 6,
      6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 9, 9, 6, 6, 6,
      6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6,
      6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6,
      9, 9, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9,
      9, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6,
      9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 6, 6, 6, 9, 9,
      9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6,
      9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 6, 9, 6, 6, 6, 9, 9, 9, 9, 6, 6,
      9, 6, 6, 6, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 6, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9,
      6, 6, 6, 6, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9,
      6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 6, 9, 9,
      9, 6, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9
    )
  }
  if (!suppliedElsewhere("spatialUnits", sim)) {
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to determine which CBM-spatial units these curves are in.")
    sim$spatialUnits <- c(
      28, 28, 28, 28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28,
      28, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27,
      27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28,
      28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 27, 28, 28, 28, 28, 27,
      27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28,
      28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 28, 28,
      27, 27, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 28, 28,
      28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 28, 28,
      28, 28, 27, 27, 27, 28, 27, 27, 27, 28, 28, 28, 28, 27, 27, 27,
      28, 28, 27, 27, 28, 28, 27, 28, 28, 28, 28, 27, 27, 28, 27, 27,
      27, 28, 28, 27, 27, 28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28,
      28, 28, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 27,
      27, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27,
      28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 27, 27, 27, 27,
      28, 28, 28, 27, 27, 28, 28, 28, 27, 27, 27, 27, 28, 28, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28,
      28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 28,
      28, 28, 28, 27, 27, 27, 27, 28, 28, 27, 28, 28, 27, 27, 27, 27,
      27, 27, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28,
      27, 27, 27, 27, 28, 28, 28, 28, 27, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28,
      27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 28, 28, 28,
      28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28,
      28, 28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28, 28, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27,
      27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 27, 28, 28, 28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27,
      27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 28, 28,
      28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 27, 28, 28,
      28, 28, 28, 28
    )
  }

  # userGcM3 and userGcM3File, these files are the m3/ha and age info by growth
  # curve ID, columns should be GrowthCurveComponentID	Age	MerchVolume
  ## TO DO: add a data manipulation to adjust if the m3 are not given on a yearly basis
  if (!suppliedElsewhere("userGcM3", sim)) {
    if (!suppliedElsewhere("userGcM3File", sim)) {
      sim$userGcM3File <- file.path("data", "userGcM3.csv") ## TODO: use prepInputs from url
      sim$userGcM3 <- fread(sim$userGcM3File)
      message(
        "User has not supplied growth curves (m3 by age or the file name for the growth curves). ",
        "The default will be used which is for a region in Saskatchewan."
      )
    }
    names(sim$userGcM3) <- c("GrowthCurveComponentID", "Age", "MerchVolume")
  }

  # tables from Boudewyn
  if (!suppliedElsewhere("table3", sim)) {
    # sim$table3 <- fread(extractURL("table3"))
    # this does not work
    # t3URL <- extractURL("table3")
    # sim$table3 <- fread(t3URL)
    # this does not work either, but the one below does ***HELP FIX PLEASE SO IT
    # CAN READ THE URL DIRECTLY***
    sim$table3 <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv")
  }
  if (!suppliedElsewhere("table4", sim)) {
    ### HELP: the .csv has a colum with commas! it puts that column in two columns...
    # table4 <- fread(extractURL("table4"))
    # work around
    sim$table4 <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv")
  }
  if (!suppliedElsewhere("table5", sim)) {
    ### HELP: the .csv has a colum with commas! it puts that column in two
    # columns... table5 <- fread(extractURL("table5")) work around
    sim$table5 <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv")
  }
  if (!suppliedElsewhere("table6", sim)) {
    ### HELP: the .csv has a colum with commas! it puts that column in two columns...
    # table6 <- fread(extractURL("table6"))
    # work around
    sim$table6 <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv")
  }
  if (!suppliedElsewhere("table7", sim)) {
    ### HELP: the .csv has a colum with commas! it puts that column in two columns...
    # table7 <- fread(extractURL("table7"))
    # work around
    sim$table7 <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv")
  }

  if (!suppliedElsewhere("gcMeta", sim)) {
    if (!suppliedElsewhere("gcMetaFile", sim)) {
      sim$gcMetaFile <- file.path(dPath, "gcMetaEg.csv") ## TODO: use prepInputs with url
      # or could use this "https://drive.google.com/file/d/1LYnShgd0Q7idNNKX9hHYju4kMDwMSkW5/view?usp=sharing"
      sim$gcMeta <- fread(sim$gcMetaFile)
    }
  }

  # cbmAdmin: this is needed to match species and parameters. Boudewyn et al 2007
  # abbreviation and cbm spatial units and ecoBoudnary id is provided with the
  # adminName to avoid confusion.
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    sim$cbmAdmin <- fread(file.path(dPath, "cbmAdmin.csv")) ## TODO: use prepInputs with url
  }

  # canfi_species: for the BOudewyn parameters, the species have to be matched
  # with the ones in the Boudewyn tables. The choices HAVE to be one of these.
  # This contains three columns, canfi_species, genus and species form the
  # publication and I added (manually) one more: forest_type_id. That variable is a CBM-CFS3
  # indicator as follows:
  # cbmTables$forest_type
  # id           name
  # 1  1       Softwood
  # 2  2      Mixedwood
  # 3  3       Hardwood
  # 4  9 Not Applicable
  if (!suppliedElsewhere("canfi_species", sim)) {
    sim$canfi_species <- fread(file.path(dPath, "canfi_species.csv")) ## TODO: use prepInputs with url
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
