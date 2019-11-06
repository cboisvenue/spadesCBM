
###################################
# WORKING HERE IN DISTURBANCES
###################################
# 
# 1. Read-in the disturbances
# this raster is where we get our disturbances 
annualDisturbance <- raster(grep(spadesCBMout$disturbanceRasters, pattern = paste0(time(spadesCBMout)[1],".tif$"), value = TRUE))
##Ian: this is not the same dimensions as the masterRaster (3703100 vs 3705000) - why??

# yearEvents <- getValues(annualDisturbance)
# length(which(!is.na(yearEvents)))
pixels <- getValues(spadesCBMout$masterRaster)
yearEvents <- getValues(annualDisturbance) %>% .[pixels != 0] #same length as spatialDT
table(yearEvents)
spatialDT <- spadesCBMout$spatialDT
spatialDT[,events := yearEvents]
table(spatialDT[,"events"])


# now we need the disturbance_matrix_id (PixelGroupID, Year, DistrubanceMatrixID)
#this I used in one of my functions...will be usefull to find the right ID
#this may be better in the inputs module
spu <- unique(spadesCBMout$spatialDT$spatial_unit_id)
#raster values 1 to 5
#GitHub\spadesCBM\data\forIan\SK_data\SK_ReclineRuns30m\LookupTables\DisturbanceTypeLookup.csv
# 1 is Wildfire
# 2 is Clearcut harvesting with salvage
# 3 is Deforestation â€” Transportation â€” Salvage, uprooting and burn
# 4 Generic 20% mortality
# 5	Generic 20% mortality

listDist <- spuDist(spu)
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
events <- c(1,1,2,2,3,3,4,4,5,5)
mySpuDmids <- cbind(mySpuDmids,events)

#1.5  Keep the pixels from each simulation year (in the postSpinup event)
# in the end (output1stand.csv), this should be the same length at this vector
spatialDT <- spatialDT[order(spatialDT$rowOrder),]
pixelKeep <- spatialDT[,.(rowOrder,PixelGroupID)]
setnames(pixelKeep,c("rowOrder","PixelGroupID0"))



# 2. Changing the PixelGroupID here
spatialDT$PixelGroupID <- as.numeric(factor(paste(spatialDT$spatial_unit_id,
                                                  spatialDT$growth_curve_component_id,
                                                  spatialDT$ages,spatialDT$events)))
# checking
length(unique(spatialDT$PixelGroupID)) # has 1066 lines

# 3. Adding the new unique PixelGroupID to the pixelKeep data.table
pixelKeep[,newPix := spatialDT$PixelGroupID]
setnames(pixelKeep,"newPix",paste0("PixelGroupID",time(spadesCBMout)))
### Need to redo this at one point...
# 4. Rebuild the level3DT post disturbances

level3DT1 <- unique(spatialDT[,-("rowOrder")]) # has 1066 lines


# 5. Changing vectors that need changing

ecozones <- unique(spatialDT[, .(PixelGroupID,ecozones)])[,ecozones]

# ecoToSpu <- as.data.frame(sim$cbmData@spatialUnitIds[which(spu$SpatialUnitID %in% unique(gcID$spatial_unit_id)),c(1,3)])
# names(ecoToSpu) <- c("spatial_unit_id","ecozones")
# ecoz <- merge.data.frame(sim$level3DT[,],ecoToSpu,by="spatial_unit_id", all.x=TRUE)
# sim$ecozones <- ecoz[,"ecozones"]

ages <- level3DT1[,ages]

# sim$ages <- sim$level3DT[,ages]
nStands <- length(ages)
# sim$nStands <- length(sim$ages)

pools <- matrix(ncol=spadesCBMout$PoolCount, nrow = nStands, data=0)
## the pooldef needs to be a sim$ because if will be used in the spatial data portion later
# sim$pools <- matrix(ncol = sim$PoolCount, nrow=sim$nStands, data=0)
colnames(pools) <- spadesCBMout$pooldef
# colnames(sim$pools)<- sim$pooldef
pools[,"Input"] <- rep(1.0,nrow(pools))
# sim$pools[,"Input"] = rep(1.0, nrow(sim$pools))

gcids <- level3DT1[,growth_curve_component_id]
# sim$gcids <- sim$level3DT[,growth_curve_component_id]
spatialUnits <- level3DT1[,spatial_unit_id]
# sim$spatialUnits <- sim$level3DT[,spatial_unit_id]#rep(26, sim$nStands)

####HERE####

opMatrixCBM <- cbind(
    rep(0, nStands), #disturbance
    1:nStands, #growth 1
    ecozones, #domturnover
    ecozones, #bioturnover
    1:nStands, #overmature
    1:nStands, #growth 2
    spatialUnits, #domDecay
    spatialUnits, #slow decay
    rep(1, nStands) #slow mixing
  )
colnames(opMatrixCBM) <- c("disturbance", "growth 1", "domturnover",
                               "bioturnover", "overmature", "growth 2",
                               "domDecay", "slow decay", "slow mixing")

#sim$opMatrixCBM
# sim$opMatrixCBM <- cbind(
#   rep(0, sim$nStands), #disturbance
#   1:sim$nStands, #growth 1
#   sim$ecozones, #domturnover
#   sim$ecozones, #bioturnover
#   1:sim$nStands, #overmature
#   1:sim$nStands, #growth 2
#   sim$spatialUnits, #domDecay
#   sim$spatialUnits, #slow decay
#   rep(1, sim$nStands) #slow mixing
# )
# 
# colnames(sim$opMatrixCBM) <- c("disturbance", "growth 1", "domturnover", 
#                                "bioturnover", "overmature", "growth 2",
#                                "domDecay", "slow decay", "slow mixing")

# maybe this too...or maybe not...Growth1 and Growth2 are the ones that relate to the PixelGroupID
# this has no dimensions related to the lenght of level3DT
# set up the constant processes, anything NULL is just a 
# placeholder for dynamic processes
allProcesses <- list(
  Disturbance=spadesCBMout$processes$disturbanceMatrices,
  Growth1=NULL,
  DomTurnover=spadesCBMout$processes$domTurnover,
  BioTurnover=spadesCBMout$processes$bioTurnover,
  OvermatureDecline=NULL,
  Growth2=NULL,
  DomDecay=spadesCBMout$processes$domDecayMatrices,
  SlowDecay=spadesCBMout$processes$slowDecayMatrices,
  SlowMixing=spadesCBMout$processes$slowMixingMatrix
)
# sim$allProcesses <- list(
#   Disturbance=sim$processes$disturbanceMatrices, 
#   Growth1=NULL, 
#   DomTurnover=sim$processes$domTurnover,
#   BioTurnover=sim$processes$bioTurnover,
#   OvermatureDecline=NULL, 
#   Growth2=NULL, 
#   DomDecay=sim$processes$domDecayMatrices,
#   SlowDecay=sim$processes$slowDecayMatrices,
#   SlowMixing=sim$processes$slowMixingMatrix
# )


# this is the rest of the annual event

#eventDMIDs <- rep(0,nStands)
table(yearEvents, useNA = "always")
#yearEvents <- spadesCBMout$disturbanceEvents[which(spadesCBMout$disturbanceEvents[,"Year"]==time(spadesCBMout)),c("PixelGroupID","DisturbanceMatrixId"),drop=FALSE]
#don't need this...
# if(nrow(sim$yearEvents)>0){
#   for(e in 1:nrow(sim$yearEvents)) {
#     eventDMIDs[sim$yearEvents[e,"PixelGroupID"]] <- sim$yearEvents[e,"DisturbanceMatrixId"]
#     sim$ages[sim$yearEvents[e,"PixelGroupID"]] <- 0
#   }
# }

# can just take the column from the newly remade level3DT$events but with NAs replaced by 0
# this has the raster values (1 to 5), we need the DMIDs
level3DT2 <- merge(level3DT1,mySpuDmids, by=c("spatial_unit_id","events"),all.x=TRUE)

eventDMIDS <- level3DT2[,disturbance_matrix_id]
# get the DMIDs fromt the already in sim mySpuDmids
eventDMIDS[which(is.na(eventDMIDS))] <- 0

opMatrixCBM[,"disturbance"]<-eventDMIDS
#change the ages vector to disturbed=0
ages <- level3DT2[,ages]
ages[which(eventDMIDS>0)] <- 0
sim$pools <- StepPools(pools=sim$pools, 
                       opMatrix = sim$opMatrixCBM, 
                       flowMatrices = sim$allProcesses)
sim$ages <- sim$ages+1
sim$cbmPools <- rbind(sim$cbmPools, cbind(level3DT$PixelGroupID, sim$ages, sim$pools))

return(invisible(sim))



################### OLD#########################################
# figuring out what the recliner raters are
# July 2018
# CBoisvenue
# 

library(raster)
library(SpaDES.tools)
dist1991r <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/disturbance/1991.tif"))
dev()
Plot(dist1991r)
dist1991all <- getValues(dist1991r)
table(dist1991all)

# 1612  1630  1693  1737  1743  1744  1760  1761  1762  1774  1950  1957  1975  1997  1998 
# 6     7    18    22    31    10    17    12     7     9   350    97   274   108   140 
# 2007  2010 10276 10292 10307 10308 10350 13645 13646 17917 17921 
# 33    37     5    11    10     8    19    17     9    13    12 

#pick another
dist1995r <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/disturbance/1995.tif"))
dev()
Plot(dist1995r)
dist1995all <- getValues(dist1995r)
table(dist1995all)
