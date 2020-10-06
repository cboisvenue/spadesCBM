#------------------------------------------------------
# rewrite of the spadesCBMcore post c++ fix
# run independently spadesCBMdefaults.Rmd, spadesCBMinputs.Rmd, and
# spadesCBMm3ToBiomass.Rmd to run through this code. There is a check at the
# bottom to make sure all is "as is expected".
# September 2020
# CBoisvenue
#----------------------------------------------------

######### rewriting annual event here
  #Rcpp::sourceCpp(file='RCBMGrowthIncrements.cpp') ## NOTE: use this function from CBMutils pkg


##annual <- function(sim) {
  ###################################
  # DISTURBANCES COME IN HERE
  ###################################
  #
  # 1. Read-in the disturbances
  # this raster is where we get our disturbances

  ## TO DO: disturbances for both SK and RIA were read-in for the whole
  ## simulation horizon in spadesCBMinputs. To permit "on-the-fly" disturbances, from other modules such as
  ## rasters they need to be read in here.

  # 1. Read-in the disturbances

  # this raster is where we get our disturbances

  ### put it back time(sim)[1]
  annualDisturbance <- raster(grep(outInputs$disturbanceRasters, pattern = paste0(1990,".grd$"), value = TRUE))
  ##
  pixels <- getValues(outInputs$masterRaster)
  yearEvents <- getValues(annualDisturbance)[!is.na(pixels)]
  ## good check here: same length as spatialDT

  # Add this year's events to the spatialDT, so each disturbed pixels has its event
  ### Back to sim$
  #sim$
  spatialDT <- outInputs$spatialDT[order(outInputs$spatialDT$pixelIndex)]
  ###sim$pixelKeep
  ### normally created in the postspinup event - remove next line
  pixelKeep <- spatialDT[,.(pixelIndex,pixelGroup)]
  pixelCount <- spatialDT[,.N,by=pixelGroup]
  #sim$sim$
  spatialDT <- spatialDT[,events := yearEvents]
  # this could be big so remove it
  #rm(yearEvents)

  ################################

  ## get the disturbed pixels only
  # Trying just adding the lines to groups that are disturbed.
  ###sim$spatialDT
  distPixels <- spatialDT[events>0,.(pixelIndex, pixelGroup, ages, spatial_unit_id,
                                     growth_curve_component_id, growth_curve_id,
                                     ecozones,events)]
  setkey(distPixels,pixelGroup)

  #### reset the ages for disturbed pixels
  distPixels$ages <- 0
  # number of 0s before: table(spatialDT$ages)[1] - 351
  #spatialDT$ages[which(spatialDT$events>0)] <- 0
  # number of 0s after: table(spatialDT$ages)[1] - 1712

  # new pixelGroup----------------------------------------------------
  # make a column of new pixelGroup that includes events since that changes how
  # the carbon will be moved NOTE: because disturbances are currently stand
  # replacing, "events", which means type of disturbances, is not part of the
  # factors in determining pixel groups. If we start representing partial
  # disturbances or have different transitions resulting from specific
  # disturbances, this will have to change.

  maxPixelGroup <- max(spatialDT$pixelGroup)
  # distPixels[,oldGroup := pixelGroup]
  # distPixels[,pixelGroup := NULL]
  #
  ## TRYING SOMETHING HERE - bring in c pools...
  ## Dealing with the carbon pools---------------------------------------------------------------
  ## linking the meta pixelGroup level table to the carbon pool from the

  ## from previous year
  # get the carbon info from the pools in. The sim$pixelGroupC will be created
  # in the postspinup event.
  ###sim$pixelGroupC
  spinupResult <- fread(file.path(getwd(),"outputs/spinupResults.csv"))
  spinupResult[,V1:=NULL]
  # emissions and products from spinup should be set to 0.
  ### TO DO: track emmissions and products on a yearly basis
  spinupResult[,CO2:Products] <- 0
  ###sim$spinupResult
  pixelGroupC <- cbind(outInputs$level3DT,
                       spinupResult)
  setkey(pixelGroupC,pixelGroup)
  ###
  cPoolsOnly <- pixelGroupC[,.(pixelGroup,Input, SoftwoodMerch, SoftwoodFoliage,
                               SoftwoodOther, SoftwoodCoarseRoots, SoftwoodFineRoots,
                               HardwoodMerch, HardwoodFoliage, HardwoodOther,
                               HardwoodCoarseRoots, HardwoodFineRoots, AboveGroundVeryFastSoil,
                               BelowGroundVeryFastSoil, AboveGroundFastSoil, BelowGroundFastSoil,
                               MediumSoil, AboveGroundSlowSoil, BelowGroundSlowSoil, SoftwoodStemSnag,
                               SoftwoodBranchSnag, HardwoodStemSnag, HardwoodBranchSnag,
                               CO2, CH4, CO, Products)]


  distPixelCpools <- merge(distPixels,cPoolsOnly)
  range(distPixelCpools$pixelGroup)
  #[1]  11 736
  ### TO HERE

  distPixelCpools$newGroup <- LandR::generatePixelGroups(distPixelCpools,maxPixelGroup,
                                                    columns = c("ages","spatial_unit_id",
                                                                "growth_curve_component_id",
                                                                "ecozones","events","Input", "SoftwoodMerch",
                                                                "SoftwoodFoliage", "SoftwoodOther", "SoftwoodCoarseRoots",
                                                                "SoftwoodFineRoots",
                                                                "HardwoodMerch", "HardwoodFoliage", "HardwoodOther",
                                                                "HardwoodCoarseRoots", "HardwoodFineRoots", "AboveGroundVeryFastSoil",
                                                                "BelowGroundVeryFastSoil", "AboveGroundFastSoil", "BelowGroundFastSoil",
                                                                "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil", "SoftwoodStemSnag",
                                                                "SoftwoodBranchSnag", "HardwoodStemSnag", "HardwoodBranchSnag",
                                                                "CO2", "CH4", "CO", "Products"))

  distPixelCpools <- distPixelCpools[,.(newGroup, pixelGroup, pixelIndex, events, ages, spatial_unit_id,
                            growth_curve_component_id, growth_curve_id, ecozones, Input,SoftwoodMerch,
                            SoftwoodFoliage,SoftwoodOther,
                            SoftwoodCoarseRoots,SoftwoodFineRoots,
                            HardwoodMerch,HardwoodFoliage,
                            HardwoodOther,HardwoodCoarseRoots,
                            HardwoodFineRoots,AboveGroundVeryFastSoil,
                            BelowGroundVeryFastSoil,AboveGroundFastSoil,
                            BelowGroundFastSoil,MediumSoil,
                            AboveGroundSlowSoil,BelowGroundSlowSoil,
                            SoftwoodStemSnag,SoftwoodBranchSnag,
                            HardwoodStemSnag,HardwoodBranchSnag,
                            CO2,CH4,CO,Products)]
  cols <- c("pixelGroup","newGroup")
  distPixelCpools[,(cols) := list((newGroup),NULL)]

  # long form pixel index all groups
  updateSpatialDT <- rbind(spatialDT[events<1,],distPixelCpools[,1:8])%>% .[order(pixelIndex),]
  #adding the new pixelGroup to the pixelKeep
  ###sim$pixelKeep
  pixelKeep[,newPix := updateSpatialDT$pixelGroup]
  ###sim$pixelKeep ##
  setnames(pixelKeep,"newPix",paste0("pixelGroup",1990))

  ## only the column pixelIndex is different between distPixelCpools and pixelGroupC
  metaDT <- unique(updateSpatialDT[,-("pixelIndex")])%>% .[order(pixelGroup),]
  setkey(metaDT,pixelGroup)
  ## add c pools and event column
  # for old groups
  part1 <- merge(metaDT,cPoolsOnly)
  # from the new groups
  distGroupCpools <- unique(distPixelCpools[,-("pixelIndex")])
  setkey(distGroupCpools,pixelGroup)
  cols <- c("pixelGroup","ages","spatial_unit_id","growth_curve_component_id",
                    "growth_curve_id","ecozones","events")
  part2 <- merge(metaDT,distGroupCpools, by=cols)
  pixelGroupForAnnual <- rbind(part1, part2) %>% .[order(pixelGroup),]



  ## HERE IS WHERE THE EVENTS GET TAKEN OUT...
  # BEFORE WE DO...need to figure out events disturbance matrices id.
  # If the matrix has to be found by name, something like this can be used.
  #  sim$mySpuDmids$events <- as.numeric(grepl("wildfire",sim$mySpuDmids$distName,ignore.case = TRUE))
  # need to add the link between the "1" in the event column (from the fireComposite raster) to the
  ### read this into sim$
  mySpuDmids <- outInputs$mySpuDmids
  mySpuDmids[,"events":= rasterId][,rasterId := NULL]

  DM <- merge(pixelGroupForAnnual,mySpuDmids, by=c("spatial_unit_id","events"),all.x=TRUE)
  DM$disturbance_matrix_id[is.na(DM$disturbance_matrix_id)] <- 0
  DM[order(pixelGroup),]
  ## this is the vector to be fed into the sim$opMatrixCBM[,"disturbance"]<-DMIDS
  DMIDS <- DM$disturbance_matrix_id


  #########################################################################
  # ALL PROCESSES FOR ALL PIXEL GROUPS#####################################
  #########################################################################

  # Changing the vectors and matrices that need to be changed to process this year's growth
  ##sim$pools
  pools <- as.matrix(pixelGroupForAnnual[,Input:Products])
  # no disturbances for these pixels
  eventDMIDs <- DMIDS
  ###sim$ecozones
  ecozones <- pixelGroupForAnnual$ecozones
  ###sim$ages
  ages <- pixelGroupForAnnual[,ages]
  ###sim$nStands  sim$ages
  nStands <- length(ages)
  ###sim$gcids
  gcids <- pixelGroupForAnnual[,growth_curve_component_id]
  ###sim$spatialUnits
  spatialUnits <- pixelGroupForAnnual[,spatial_unit_id]
  ###sim$opMatrixCBM
  opMatrixCBM <- cbind(
    DMIDS, #disturbance sim$opMatrixCBM[,"disturbance"]<-eventDMIDs
    1:nStands, #growth 1
    ecozones, #domturnover
    ecozones, #bioturnover
    1:nStands, #overmature
    1:nStands, #growth 2
    spatialUnits, #domDecay
    spatialUnits, #slow decay
    rep(1, nStands) #slow mixing
  )
  ###sim$opMatrixCBM
  colnames(opMatrixCBM) <- c("disturbance", "growth 1", "domturnover",
                             "bioturnover", "overmature", "growth 2",
                             "domDecay", "slow decay", "slow mixing")
  ###sim$allProcesses
  allProcesses <- list(
    ### sim$
    Disturbance=outDefaults$processes$disturbanceMatrices,
    Growth1=NULL,
    DomTurnover=outDefaults$processes$domTurnover,
    BioTurnover=outDefaults$processes$bioTurnover,
    OvermatureDecline=NULL,
    Growth2=NULL,
    DomDecay=outDefaults$processes$domDecayMatrices,
    SlowDecay=outDefaults$processes$slowDecayMatrices,
    SlowMixing=outDefaults$processes$slowMixingMatrix
  )


  # ! ----- EDIT BELOW ----- ! #
  #
  # compute the growth increments

  ### myBiomassout$ and sim$ and outDefaults
  growthAndDecline <- ComputeGrowthAndDeclineMatrices2(
    growthIncrements = myBiomassOut$gcHash,
    ages = ages,
    gcids = gcids,
    pools = pools,
    rootParameters = as.data.frame(t(outDefaults$cbmData@rootParameters[1,])),
    turnoverParams = as.data.frame(t(outDefaults$cbmData@turnoverRates[1,])),
    biomassToCarbonRate = as.numeric(outDefaults$cbmData@biomassToCarbonRate),
    swMult = 0.5, hwMult = 0.5)
  ###sim$allProcesses
  allProcesses$Growth1=growthAndDecline$Growth
  allProcesses$Growth2=growthAndDecline$Growth
  allProcesses$OvermatureDecline=growthAndDecline$OvermatureDecline

  # this has to be the same length as the DT going in for processing
  #sim$opMatrixCBM[,"disturbance"]<-eventDMIDS
  ###sim$ everything
  pools2 <- StepPools(pools=pools,
                     opMatrix = opMatrixCBM,
                     flowMatrices = allProcesses)
  ##########################END PROCESSES#########################################

  ### note: the vectors for the end of the annual event still need to be made
  ### and ready for the next year

  #####################################################################################
  ### HERE CHECK IF THE ANNUAL PROCESSES HAVE WORKED##############################
  ## pools before processing: 2 pixels groups with no dist and two with a clear cut
  metaBefore <- rbind(pixelGroupForAnnual[4:5,],pixelGroupForAnnual[events==2,][1:2,])

  before4 <-rbind(pools[4:5,],pools[which(pixelGroupForAnnual$events==2),][1:2,])
  after4 <- rbind(pools2[4:5,],pools2[which(pixelGroupForAnnual$events==2),][1:2,])

  diffs <- after4-before4

  theseGc <- metaBefore$growth_curve_component_id
  thisOld <- metaBefore$ages

  increments <- as.data.table(myBiomassOut$growth_increments)
  increments[(id %in% theseGc & age %in% thisOld),]
  # twice the increments that are in the increments table per year
  #inc2x <- cbind(increments[,1:2],2*increments[,3:8])

  theseCombos <- as.data.table(cbind(theseGc,thisOld))
  names(theseCombos) <- c("id","age")
  incCombos <- merge(theseCombos,increments)

  ## check is the increment matrices match increments calculated out of
  ## myBiomassOut$growth_increments for swmerch (no disturbances)
  incCombos[4,swmerch] - 2*allProcesses$Growth1[[4]][1,3]
  # value
  # 0
  incCombos[1,swmerch] - 2*allProcesses$Growth1[[5]][1,3]
  # value
  # 0

  # correct matrix id
  rowsForpixelGroups <- which(pixelGroupForAnnual$pixelGroup %in% metaBefore$pixelGroup)
  thisOpMatrices <- opMatrixCBM[rowsForpixelGroups,]

  ## will just check swmerch (all conifers myBiomassOut$gcMeta[growth_curve_id
  ## %in% metaBefore$growth_curve_id,])

  ## for no dist: swmerch + growth1 + BioTurn + OvermatureDecline + growth2

  ## right matrices: for pixelGroup in line 4 of pixelGroupsForAnnual -
  ## allProcesses$Growth1[[4]] + allProcesses$BioTurnover$'9' +
  ## OvermatureDecline[[4]] + allProcesses$Growth2[[4]]
  ## for pixelGroup in line 5 of pixelGroupsForAnnual -
  ## allProcesses$Growth1[[5]] + allProcesses$BioTurnover$'9' +
  ## OvermatureDecline[[5]] + allProcesses$Growth2[[5]]

  ## in the case of line 4 - no overmatureDecline, and growth1 == growth2
  allProcesses$Growth1[[4]]==allProcesses$Growth2[[4]]
  calcAfter <- (before4[1,2] + allProcesses$Growth1[[4]][1,3]) *
    allProcesses$BioTurnover$'9'[allProcesses$BioTurnover$'9'[,1]==2 & allProcesses$BioTurnover$'9'[,2]==2,][[2,3]] +
    allProcesses$Growth1[[4]][1,3]
  after4[1,"SoftwoodMerch"]-calcAfter
  # SoftwoodMerch
  # 0
  ## in the case of line 4 - no overmatureDecline, and growth1 == growth2
  allProcesses$Growth1[[5]]==allProcesses$Growth2[[5]]
  calcAfter2 <- (before4[2,2] + allProcesses$Growth1[[5]][1,3]) *
    allProcesses$BioTurnover$'6'[allProcesses$BioTurnover$'6'[,1]==2 & allProcesses$BioTurnover$'6'[,2]==2,][[2,3]] +
    allProcesses$Growth1[[5]][1,3]
  after4[2,"SoftwoodMerch"]-calcAfter2
  # SoftwoodMerch
  # 0
  #
  ### Disturbed pixels: swmerch*dist + growth1 + BioTurn + OvermatureDecline + growth2
  ## rows happen to match pixelGroup (since this is right out of the spinup)
  length(allProcesses$Disturbance)
  #[1] 426
  calcAfter3 <- (before4[3,2]-
                   (sum(allProcesses$Disturbance$'409'[allProcesses$Disturbance$'409'[,1]==2,][,3])*
                      before4[3,2]) + allProcesses$Growth1[[757]][1,3]*
                   allProcesses$BioTurnover$'9'[allProcesses$BioTurnover$'9'[,1]==2 & allProcesses$BioTurnover$'9'[,2]==2,][[2,3]] +
                   allProcesses$Growth1[[757]][1,3])

  after4[3,"SoftwoodMerch"]-calcAfter3
  # to products?
  afterProducts1 <- before4[3,2]*allProcesses$Disturbance$'409'[allProcesses$Disturbance$'409'[,1]==2,][[2,3]]+
    before4[3,19]*0.5
  after4[3,26]-afterProducts1
  # Products
  # 0

  ## last disturbance
  calcAfter4 <- (before4[4,2]-
                   (sum(allProcesses$Disturbance$'409'[allProcesses$Disturbance$'409'[,1]==2,][,3])*
                      before4[4,2]) + allProcesses$Growth1[[758]][1,3]*
                   allProcesses$BioTurnover$'9'[allProcesses$BioTurnover$'9'[,1]==2 & allProcesses$BioTurnover$'9'[,2]==2,][[2,3]] +
                   allProcesses$Growth1[[758]][1,3])

  after4[4,"SoftwoodMerch"]-calcAfter4
  # to products?
  afterProducts2 <- before4[4,2]*
    allProcesses$Disturbance$'409'[allProcesses$Disturbance$'409'[,1]==2,][[2,3]]+
    before4[4,19]*0.5
  after4[4,26]-afterProducts2

  ### END OF CHECK IF THE ANNUAL PROCESSES HAVE WORKED##############################
  ## they have ALL WORKED!
  #####################################################################################

  ### BELOW STILL NEEDS TO BE CHECKED
  ##########NPP: Calculating NPP for this year using stockt and stockt1#############
  ## NPP for all the pixel groups that are above maxPixelGroup is the sum of the
  ## increments for that pixel group.

  ###sim$pixelGroupC
  nonDistline <- which(pixelGroupForAnnual$pixelGroup==maxPixelGroup)
  stockt <- pixelGroupForAnnual[1:nonDistline,.(
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
  ### sim$pools instead of pools2
  stockt1 <- cbind(pixelGroupForAnnual[1:nonDistline,!(Input:Products)],pools2[1:nonDistline,])[,.(
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
  #sim$turnoverRates
  ### sim$turnoverRates are calculated in the postspinup event
  ###calcTurnoverRates ------------------------------------------------------------------
  # matching the turnover rates to the spatial unit

  calcTurnoverRates <- function(turnoverRates, spatialUnitIds, spatialUnits) {
    turnoverRates <- as.data.table(turnoverRates)
    SPU <- as.data.table(spatialUnitIds)
    SPU <- SPU[SpatialUnitID %in% unique(spatialUnitIds)]
    SPU <- merge(SPU, turnoverRates, by = "EcoBoundaryID", all.y = FALSE)
    SPU <- SPU[SpatialUnitID %in% unique(spatialUnits),]
    return(SPU)
  }
  ### END calcTurnoverRates ------------------------------------------------------------------
  turnoverRates <- calcTurnoverRates(turnoverRates = outInputs$cbmData@turnoverRates,
                                     spatialUnitIds = outInputs$cbmData@spatialUnitIds, spatialUnits = outInputs$spatialUnits)
  turnoverRates <- turnoverRates[, spatial_unit_id := SpatialUnitID]
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

  NPPnonDist <- merge(turnover,grossGrowth,by="pixelGroup")[,.(
    pixelGroup,
    NPP = (
      AGturnover+
        BGturnover+
        grossGrowthAG+
        grossGrowthBG)
  )]

  ### now add NPP for the disturbed pixels
  # make the matrices data.tables
  incsListDT <- lapply(allProcesses$Growth1,as.data.table)
  incsListDTnrow <- lapply(allProcesses$Growth1,nrow)
  incsNrow <- do.call("rbind",incsListDTnrow)
  #names(incsListDT) <- pixelGroupForAnnual$pixelGroup#paste0("pg",
  incDT <- rbindlist(incsListDT)
  # this is not working can't figure out why...
  # incDT$name <- rep(names(incsListDT),each=sapply(incsListDT,"nrow"))
  nameVec <- NULL
  for(i in 1:length(pixelGroupForAnnual$pixelGroup)){
    thisPg <- rep(pixelGroupForAnnual$pixelGroup[i],times=incsNrow[i])
    nameVec <- c(nameVec,thisPg)
  }
  incDT$name  <-  nameVec

  # only the pixelGroups that are disturbed
  distNPP <- incDT[name>maxPixelGroup & value<1,.(NPP = sum(value)),by=name]
  names(distNPP) <- names(NPPnonDist)
  ### sim$NPP is created in postspinup
  NPP <- rbind(NPPnonDist,distNPP)
  sim$NPP <- rbind(sim$NPP, cbind(simYear = rep(time(sim)[1],nrow(NPP)),NPP))
  ######### NPP END HERE ###################################


  ####UPDATING ALL THE FINAL VECTORS FOR NEXT SIM YEAR ###################################
  # make the disturbed pixels like$pixelGroupC and add the rows at the end
  addDistC <- cbind(toAdd[,.(ages,spatial_unit_id,growth_curve_component_id,growth_curve_id,ecozones,pixelGroup)],
                    distPoolsOut)
  sim$pixelGroupC <- rbind(unique(cbind(pixelGroupForAnnual[,!(Input:Products)],sim$pools)),addDistC)
  #sim$pixelGroupC$N <- sim$spatialDT[,.N,by=pixelGroup]$
  sim$pixelGroupC$ages <- sim$pixelGroupC$ages+1
  sim$spatialDT$ages <- sim$spatialDT$ages+1
  names(distPixOut) <- c( c("simYear","pixelCount","pixelGroup", "ages"), sim$pooldef)
  updatePools <-   cbind(rep(time(sim)[1],length(sim$pixelGroupC$ages)),pixelCount[1:length(sim$pixelGroupC$ages),2],sim$pixelGroupC$pixelGroup, sim$pixelGroupC$ages, sim$pixelGroupC[,Input:Products])
  names(updatePools) <- c( c("simYear","pixelCount","pixelGroup", "ages"), sim$pooldef)

  sim$cbmPools <- rbind(sim$cbmPools,updatePools)
  ######## END OF UPDATING VECTORS FOR NEXT SIM YEAR #######################################






