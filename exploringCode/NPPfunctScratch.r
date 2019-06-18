# trying to figure out how to get the NPP "right" in the annual event

#MAY NOT NEED THE IF STATEMENT since pixelGroupC is calculated from level3DT and spinupResults
#if (time(sim) == start(sim)) {
stockt <- cbind(spadesCBMout$level3DT,spadesCBMout$spinupResult)[,.(
  pixelGroup,
  ages,
  spatial_unit_id,
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
setkey(stockt,pixelGroup)
#} else{
## this will be stockt
stockt1 <- spadesCBMout$pixelGroupC[,.(
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
#}
#THIS WILL BE STOCKT1
 #unique(cbind(pixelGroupForAnnual[,!(Input:Products)],sim$pools))
stockt2 <- as.data.table(spadesCBMout$cbmPools)[simYear==1990,.(
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
setkey(stockt2,pixelGroup)

turnoverRates <- spadesCBMout$turnoverRates[, spatial_unit_id := SpatialUnitID]
turnoverComponents <- merge(stockt,turnoverRates,by="spatial_unit_id")
turnover <- turnoverComponents[,.(
  pixelGroup,
  AGturnover = (
    (SoftwoodMerch * StemAnnualTurnoverRate)+
    (SoftwoodFoliage * SoftwoodFoliageFallRate)+
    (SoftwoodOther * SoftwoodBranchTurnoverRate)+
    (HardwoodMerch * StemAnnualTurnoverRate)+
    (HardwoodFoliage * HardwoodFoliageFallRate)+
    (HardwoodOther * HardwoodBranchTurnoverRate)),
  BGturnover = (
    (SoftwoodCoarseRoots * CoarseRootTurnProp)+
    (SoftwoodFineRoots * FineRootTurnProp)+
    (HardwoodCoarseRoots * CoarseRootTurnProp)+
    (HardwoodFineRoots * FineRootTurnProp))
    )]


stocks2t <- merge(stockt[,-c("ages","spatial_unit_id")],stockt2[,-"ages"],by = "pixelGroup",sort=TRUE)
grossGrowth <- stocks2t[,.(
  pixelGroup,
  grossGrowthAG = (
    (SoftwoodMerch.y-SoftwoodMerch.x)+
    (SoftwoodFoliage.y-SoftwoodFoliage.x)+
    (SoftwoodOther.y-SoftwoodOther.x)+
    (HardwoodMerch.y-HardwoodMerch.x)+
    (HardwoodFoliage.y-HardwoodFoliage.x)+
    (HardwoodOther.y-HardwoodOther.x)),
  grossGrowthBG= (
    (SoftwoodCoarseRoots.y-SoftwoodCoarseRoots.x)+
    (SoftwoodFineRoots.y-SoftwoodFineRoots.x)+
    (HardwoodCoarseRoots.y-HardwoodCoarseRoots.x)+
    (HardwoodFineRoots.y-HardwoodFineRoots.x))
    )]
NPP <- merge(turnover,grossGrowth,by="pixelGroup")[!(pixelGroup %in% groupOut),.(
  pixelGroup,
  NPP = (
    AGturnover+
    BGturnover+
    grossGrowthAG+
    grossGrowthBG)
)]

# THIS IS WHAT IS IN THERE NOW
# if (time(sim) == start(sim)) {
#   sim$changeInNPP <- cbind(sim$level3DT,sim$spinupResult)[,.(
#     #pixelGroup,
#     grossGrowthAG = sum(SoftwoodMerch,
#     SoftwoodFoliage,
#     SoftwoodOther,
#     SoftwoodCoarseRoots,
#     SoftwoodFineRoots,      
#     HardwoodMerch,
#     HardwoodFoliage,
#     HardwoodOther,
#     HardwoodCoarseRoots,
#     HardwoodFineRoots)
#       
#     )
#     HERE!!
#     sim$spinupResult[,2:11]
#     #pixelGroupC[, .(
#     # AGC = sum(
#     #   
#     #   ),
#     BGC = sum(
#       
#       AboveGroundVeryFastSoil,
#       BelowGroundVeryFastSoil,
#       AboveGroundFastSoil,
#       BelowGroundFastSoil,
#       AboveGroundSlowSoil,
#       BelowGroundSlowSoil
#       )
#     ), by = pixelGroup]
# } else {
#   turnoverRates <- sim$turnoverRates[, spatial_unit_id := SpatialUnitID]
#   #Figure out how to join these values with table and multiply by them instead of hardcode
#   #Is level3DT updated? 
#   changeInNPP <- sim$changeInNPP
#   #Need to get SPU of each pixelGroup to track down ecoboundary. Faster way?
#   changeInNPP <- merge(changeInNPP, sim$pixelGroupC, by = c("pixelGroup"))
#   newCarbon <- merge(changeInNPP, turnoverRates, by = "spatial_unit_id")
#   newCarbon <- newCarbon[, .(
#     newAGC = sum(
#       SoftwoodMerch,
#       SoftwoodFoliage,
#       SoftwoodOther,
#       HardwoodMerch,
#       HardwoodFoliage,
#       HardwoodOther
#      ),
#     newBGC = sum(
#       HardwoodCoarseRoots,
#       HardwoodFineRoots,
#       AboveGroundVeryFastSoil,
#       BelowGroundVeryFastSoil,
#       AboveGroundFastSoil,
#       BelowGroundFastSoil,
#       AboveGroundSlowSoil,
#       BelowGroundSlowSoil
#      ),
#     turnover = sum(
#       SoftwoodMerch * StemAnnualTurnoverRate,
#       SoftwoodFoliage * SoftwoodFoliageFallRate,
#       SoftwoodOther * SoftwoodBranchTurnoverRate,
#       SoftwoodCoarseRoots * CoarseRootTurnProp,
#       SoftwoodFineRoots * FineRootTurnProp,
#       HardwoodMerch * StemAnnualTurnoverRate,
#       HardwoodFoliage * HardwoodFoliageFallRate,
#       HardwoodOther * HardwoodBranchTurnoverRate,
#       HardwoodCoarseRoots * CoarseRootTurnProp,
#       HardwoodFineRoots * FineRootTurnProp
#      )
#     ), by = pixelGroup]
#   bothYears <- changeInNPP[newCarbon, on = 'pixelGroup']
#   deltaNPP <- bothYears[, .(
#     AGC = newAGC,
#     BGC = newBGC,
#     bgNPP = newBGC - BGC,
#     agNPP = newAGC - AGC,
#     totalNPP = newBGC + newAGC - BGC - AGC + turnover
#     ), by = pixelGroup]
#   sim$changeInNPP <- deltaNPP
# }