# working out how to insert the disturbances


#### CREATING THE toAdd data frame ------------------------------------
# pixels disturbed in 1990
annualDisturbance <- raster(grep(spadesCBMout$disturbanceRasters, pattern = paste0(1990,".grd$"), value = TRUE))
pixels <- getValues(spadesCBMout$masterRaster)
yearEvents <- getValues(annualDisturbance) %>% .[pixels != 0] #same length as spatialDT

# Add this year's events to the spatialDT, so each disturbed pixels has its event
spatialDT <- spadesCBMout$spatialDT[order(spadesCBMout$spatialDT$pixelIndex)]
spatialDT <- spatialDT[,events := yearEvents]

## fudging things to make it work
thisOne <- spadesCBMout$pixelKeep[pixelIndex %in% spatialDT[events>0,pixelIndex],pixelGroup0]

distPixels <- spatialDT[events>0,.(pixelIndex, pixelGroup, ages, rasterSps, Productivity,
                                       spatial_unit_id, growth_curve_component_id, growth_curve_id, events)]
distPixels[,pixelGroup := thisOne]
#fudgin seems to have worked...
setkey(distPixels,pixelGroup)

## The ages can be changed prior to the
## processing in the C++ functions because the first thing that happens is
## disturbances and presently **all disturbances are stand replacing**. Set
## all ages to 0 in the disturbed pixels
distPixels$ages <- 1

groupToAddC <- spadesCBMout$pixelGroupC[which(spadesCBMout$pixelGroupC$pixelGroup %in% unique(thisOne)),
                                        -c("ages","rasterSps", "Productivity", "spatial_unit_id", "growth_curve_component_id", "growth_curve_id")]

groupToAddC <- groupToAddC[,c("oldGroup","pixelGroup") := list(pixelGroup,NULL)]
groupToAddC <- unique(groupToAddC)
setkey(groupToAddC,oldGroup)
library(LandR)
maxPixelGroup <- 739
distPixels[,oldGroup:=pixelGroup]
distPixels$newGroup <- LandR::generatePixelGroups(distPixels,maxPixelGroup,
                                                  columns = c("oldGroup","spatial_unit_id", "growth_curve_component_id", "ages", "events"))
distPixels <- distPixels[,.(pixelIndex,ages,rasterSps,Productivity,spatial_unit_id, growth_curve_component_id,growth_curve_id, events, oldGroup,newGroup)]
uniqueNewGroup <- unique(distPixels[,-("pixelIndex")]) 
setkey(uniqueNewGroup,oldGroup)

# this is where the equivalent of sim$level3DT and C pools for new groups are
# put together

toAdd <- merge(uniqueNewGroup,groupToAddC,all.x=TRUE)#,on = c("pixelGroup")]
toAdd <- toAdd[,c("pixelGroup","newGroup") := list(newGroup,NULL)]
toAdd <- toAdd[order(pixelGroup),]

DM <- merge(toAdd,spadesCBMout$mySpuDmids, by=c("spatial_unit_id","events"),all.x=TRUE)
DM <- DM[order(pixelGroup),]
DMIDS <- DM$disturbance_matrix_id
# not quite the right length yet

toAdd <- toAdd[,c("oldGroup","events") := NULL]
toAdd <- toAdd[order(pixelGroup),]

## END got it --------------------------------------------------


############## DISTURBANCE AND ANNUAL PROCESSES ##########
# these are my functions
cTransferDist
cTransfer
## in spadesCBMinputs add a column to mySpuDmids with `number of dist`, 
mySpuDmids <- spadesCBMout$mySpuDmids
matchID <- c("$Disturbance$`378`")
,`371`,`409`,`409`,`26`,`26`,`91`,`91`,`91`,`91`)
matchEco
matchSpu
## not working...will load the necessary matrices in the inputs module
disturbances <- list(length(5))
# 1 is Wildfire DMID 378 (SPU 28) and 371 (SPU 27)
# 2 is Clearcut harvesting with salvage DMID 409 (one for both SPU)
# 4 is Deforestation â€” Transportation â€” Salvage, uprooting and burn DMID 26
# 3 Lcondition Generic 20% mortality DMID 91
# 5	Unclassified Generic 20% mortality DMID 91
orderDistMat <- c(1,2,3,3,4,4,5,5,5,5)
mySpuDmids <- as.data.table(cbind(mySpuDmids,orderDistMat))
disturbances[[1]] <- spadesCBMout$allProcesses$Disturbance$`378`
disturbances[[2]] <- spadesCBMout$allProcesses$Disturbance$`371`
disturbances[[3]] <- spadesCBMout$allProcesses$Disturbance$`409`
disturbances[[4]] <- spadesCBMout$allProcesses$Disturbance$`26`
disturbances[[5]] <- spadesCBMout$allProcesses$Disturbance$`91`
#these by ecozones
domTurnMat <- list(length(2))
domTurnMat[[1]] <- spadesCBMout$allProcesses$DomTurnover$`6`
domTurnMat[[2]] <- spadesCBMout$allProcesses$DomTurnover$`9`
bioTurnMat <- list(length(2))
bioTurnMat[[1]] <- spadesCBMout$allProcesses$BioTurnover$`6`
bioTurnMat[[2]] <- spadesCBMout$allProcesses$BioTurnover$`9`
# these by spatial unit
domDecayMat <- list(length(2))
domDecayMat[[1]] <- spadesCBMout$allProcesses$DomDecay$`27`
domDecayMat[[2]] <- spadesCBMout$allProcesses$DomDecay$`28`
# these by spatial unit
slowDecayMat <- list(length(2))
slowDecayMat[[1]] <- spadesCBMout$allProcesses$SlowDecay$`27`
slowDecayMat[[2]] <- spadesCBMout$allProcesses$SlowDecay$`28`

# each line is a pixelgroup
# so this needs to be done for each pixel group
toAddOut <- NULL
for(i in 1:length(toAdd$pixelGroup)){
  preD <- toAdd[i,Input:Products]
  distProp <- as.data.table(disturbances[[unique(mySpuDmids[disturbance_matrix_id==DMIDS[i],orderDistMat])]])
  distCalc <- cTransferDist(standIn =t(preD),transProp = distProp)
  if(toAdd$spatial_unit_id[i]==27) {a=1} else a=2
  domTurn <- cTransfer(standIn = distCalc$calcDist,transProp = domTurnMat[[a]])
  bioTurn <- cTransfer(standIn = domTurn$calcDist,transProp = bioTurnMat[[a]])
  domDecay <- cTransfer(standIn = bioTurn$calcDist,transProp = domDecayMat[[a]])
  slowDecay <- cTransfer(standIn = domDecay$calcDist,transProp = slowDecayMat[[a]])
  slowMix <- cTransfer(standIn = slowDecay$calcDist,transProp = spadesCBMout$allProcesses$SlowMixing$`1`)
  toAddOut <- rbind(toAddOut,as.data.table(t(slowMix[,2])))
}



## need help here to select








  