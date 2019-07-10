# cheking out the DMIDs used in our simulations
#GitHub\spadesCBM\data\forIan\SK_data\SK_ReclineRuns30m\LookupTables\DisturbanceTypeLookup.csv
# 1 is Wildfire DMID 378 (SPU 28) and 371 (SPU 27)
# 2 is Clearcut harvesting with salvage DMID 409 (one for both SPU)
# 4 is Deforestation â€” Transportation â€” Salvage, uprooting and burn DMID 26
# 3 Lcondition Generic 20% mortality DMID 91
# 5	Unclassified Generic 20% mortality DMID 91

# these are the disturbance matrix ids used
dists <- unique(spadesCBMout$mySpuDmids$disturbance_matrix_id)

# use the functions in extraFunctions
listDists <- seeDist(dists)
str(listDists)

# ecozone names
ecoToSpu <- as.data.frame(spadesCBMout$cbmData@spatialUnitIds[,c(1,3)])
# 6 Boreal Shield West SPU 27
# 9 Boreal Plains SPU 28 

# frequency of disturbances:
#Get frequency table from disturbance rasters
library(raster)
library(magrittr)
myBigList <- grep(pattern = "*.grd", 
                  x = list.files("data/forIan/SK_data/CBM_GIS/disturbance_testArea/", full.names = TRUE),
                  value = TRUE)
myBigList <- lapply(myBigList, raster::raster)
myBigStack <- raster::stack(myBigList)
freqTables <- raster::freq(myBigStack)
# this gives my a frequency table for each of the year in which we have disturbances.
# our current simulations are freqTable[6] to freqTable[21]

# what pixels are disturbed in 1990?------------------------------------
pixels <- getValues(spadesCBMout$masterRaster)
dist1990 <- getValues(myBigStack[[6]]) %>% .[pixels != 0]
table(dist1990)
# dist1990
# 0             1       2       3       4       5 
# 1346168       6     682     136      77     460 
# 6 burnt pixels
# 682 harvested pixels
# 44 deforested pixels
# 460+136 Generic 20% mortality
#-----------------------------------------------------------------------




### Checking the matrices--------------------------------------------------------------------------
# fire in the Boreal Plains (NIR2011)------------------------
# dmid 371
fireBP <- listDists[[2]][,c(2,4,3,5,6)]
write.csv(file = file.path(outputPath(spadesCBMout),"fireBorealPlains.csv"),fireBP)
# looking for burnt pixels
# matching the pixelIndex in pixel keep
burntPixelsBP <- getValues(myBigStack[[6]]) %>% .[pixels != 0]
burntPixels1 <- which(burntPixelsBP==1)
length(burntPixels1)
#[1] 6

# tracking which group those pixels belong to throught the simulation
fire1Pixels1990 <- spadesCBMout$pixelKeep[burntPixels1,]
fire1Groups <- unique(fire1Pixels1990[,2:16])
#321,329 out of the spinup
# changed to 761 and 763 respectively - stay in those groups until the end of the simulation
# both groups are in spu 28 (so DMID 371)
#Trying to recreate what happens in the annual...b/c something is wrong, doesn't match
distGroups <- spadesCBMout$level3DT[pixelGroup==321 |pixelGroup==329 ,]
pixelGroupC0 <- cbind(spadesCBMout$level3DT,spadesCBMout$spinupResult)
cStep1 <- pixelGroupC0[which(pixelGroupC0$pixelGroup %in% c(321)),-c("ages","rasterSps", "Productivity", "spatial_unit_id", "growth_curve_component_id", "growth_curve_id")]
#pixelGroupC[which(sim$pixelGroupC$pixelGroup %in% unique(distPixels$pixelGroup)),-c("ages","rasterSps", "Productivity", "spatial_unit_id", "growth_curve_component_id", "growth_curve_id")]
setkey(cStep1,oldGroup)
cStep1 <- cStep1[,oldGroup := pixelGroup] %>% .[,pixelGroup := NULL]
distGroups[,oldGroup:=pixelGroup]
distGroups[,pixelGroup:=NULL]
distGroups$newGroup <- c(763,761)
setkey(distGroups,oldGroup)

cStep3 <- merge(distGroups,cStep1,all.x=TRUE)


pixelGroup321 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==321)))
pixelGroup329 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==329)))
# 4 pixels burnt from 321 and 2 from 329 in 1990

pixelGroup761 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==761)))
pixelGroup763 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==763)))
# No changes to those pixels after that
## THE ONLY REASON THAT THESE 6 PIXELS ARE NOT IN THE SAME GROUP IS THAT THEY DO
## NOT START OFF WITH THE SAME AMOUNT OF C IN THEIR POOLS

# this is the pixel groups out of the spinup (759)
group0 <- unique(spadesCBMout$pixelKeep$pixelGroup0)
#these are the pixel groups that include the pixels disturbed, and the first
#group in the cPoolsPixelYear.csv. There are 826 unique groups.
group1 <- unique(spadesCBMout$pixelKeep$pixelGroup1990)

# take the 4 disturbed pixels in 1990, for group 321 that becomes group 761,
# disturb it as per fireBP and check values in $cbmPools
# carbon out of the spinup
su321 <- melt(spadesCBMout$spinupResult[which(spadesCBMout$level3DT$pixelGroup==321),-1])
g321c1 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==321,6:30])
# the difference between the spinup and the 1990 pools should be the growth only for the 321 group
growth321 <- g321c1[,2] - su321$value
# three first values should be the carbon curve values
# growth curve id for group 321 is 49
gc49 <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==49,]
plot(gc49[,2],gc49[,3])
gc49age90 <- gc49[gc49[,2]==90,]
gcCheck <- growth321[1:3]-t(gc49age90[3:5])
##CHECK - those are the values of the $growth_increments
# double check growth for the next year
g321c2 <- melt(spadesCBMout$cbmPools[simYear==1991&pixelGroup==321,6:30])
growth321_2 <- g321c2[,2]-g321c1[,2]
gc49age91 <- gc49[gc49[,2]==91,]
gcCheck2 <- growth321_2[1:3]-t(gc49age91[3:5])

# note that the curves are wonky after beign put through CBMVolumeToBiomass....

# group 761 should start out with the carbon in the group 321 in the spinup, get
# disturbed by fire, and grow age 0 following only the 1st three pools as they
# are the simplest
# check that the spu is 28 and the gc is 49
spadesCBMout$pixelGroupC[pixelGroup==761,]
# CHECK
## BUT THE STAND HAS 32 tonnes of carbon in the merch at age 3...wrong!###############

g761c1 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==761,6:30])
gc49age0 <- gc49[gc49[,2]==0,]
g7610 <- gc49age0[3:5]
end761 <- g761c1[1:3,]
pnames <- as.character(g761c1$variable[1:3])
startC761 <- su321[1:3,]

merchOp <- startC761[1]-(startC761[1]*sum(fireBP[grep(pattern=pnames[1],fireBP$sourceName),5])) + g7610[1]
### PROBLEM: THE PIXELS ARE NOT DISTURBED, THEY START WITH THE C FROM THE
### PREVIOUS YEAR (IN THIS CASE SPINUP VALUES FROM pixelGroup444) AND "GROW" ON
### CURVE 49 AT AGE 0
# IAN REDOING THE DIST RASTER
# I WILL LOOK AT ANOTHER DIST FOR CLUES

## FIRE 1 DIST 371 DOES NOT SEEM TO BE HAPPENING

#--------------------------------------

## looking at harvest------------------------------------------------------------------------------
harv <- listDists[[3]][,c(2,4,3,5,6)]
write.csv(file = file.path(outputPath(spadesCBMout),"clearCutSK.csv"),harv)

# pixels <- getValues(spadesCBMout$masterRaster)
# # matching the pixelIndex in pixel keep
harvPixels <- getValues(myBigStack[[6]]) %>% .[pixels != 0]
harvPixels1 <- which(harvPixels==2)
length(harvPixels1)
#[1] 682
 
# tracking which group those pixels belong to throught the simulation
harvPixels1990 <- spadesCBMout$pixelKeep[harvPixels1,]
harv2Groups <- unique(harvPixels1990[,2:18])
countHarvSU <- harvPixels1990[,.N,by=pixelGroup0]
countHarv90 <- harvPixels1990[,.N,by=pixelGroup1990]
#looking at 447 that becomes 777 with 20 pixels harvested
# the 20 pixels harvested stay in those groups until the end of the simulation
# only one DMID 409
#Trying to recreate what happens in the annual...b/c something is wrong, doesn't match
distGroups <- spadesCBMout$level3DT[pixelGroup==447,]
#pixelGroupC0 <- cbind(spadesCBMout$level3DT,spadesCBMout$spinupResult)
# cStep1 <- pixelGroupC0[which(pixelGroupC0$pixelGroup %in% c(321)),-c("ages","rasterSps", "Productivity", "spatial_unit_id", "growth_curve_component_id", "growth_curve_id")]
# #pixelGroupC[which(sim$pixelGroupC$pixelGroup %in% unique(distPixels$pixelGroup)),-c("ages","rasterSps", "Productivity", "spatial_unit_id", "growth_curve_component_id", "growth_curve_id")]
# setkey(cStep1,oldGroup)
# cStep1 <- cStep1[,oldGroup := pixelGroup] %>% .[,pixelGroup := NULL]
# distGroups[,oldGroup:=pixelGroup]
# distGroups[,pixelGroup:=NULL]
# distGroups$newGroup <- c(763,761)
# setkey(distGroups,oldGroup)
# 
# cStep3 <- merge(distGroups,cStep1,all.x=TRUE)
# 
# 
# pixelGroup321 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==321)))
# pixelGroup329 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==329)))
# # 4 pixels burnt from 321 and 2 from 329 in 1990
# 
# pixelGroup761 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==761)))
# pixelGroup763 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==763)))
# # No changes to those pixels after that
# ## THE ONLY REASON THAT THESE 6 PIXELS ARE NOT IN THE SAME GROUP IS THAT THEY DO
# ## NOT START OFF WITH THE SAME AMOUNT OF C IN THEIR POOLS
# 
# # this is the pixel groups out of the spinup (759)
# group0 <- unique(spadesCBMout$pixelKeep$pixelGroup0)
# #these are the pixel groups that include the pixels disturbed, and the first
# #group in the cPoolsPixelYear.csv. There are 826 unique groups.
# group1 <- unique(spadesCBMout$pixelKeep$pixelGroup1990)
# 
# # take the 4 disturbed pixels in 1990, for group 321 that becomes group 761,
# # disturb it as per fireBP and check values in $cbmPools
# # carbon out of the spinup
# su321 <- melt(spadesCBMout$spinupResult[which(spadesCBMout$level3DT$pixelGroup==321),-1])
# g321c1 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==321,6:30])
# # the difference between the spinup and the 1990 pools should be the growth only for the 321 group
# growth321 <- g321c1[,2] - su321$value
# # three first values should be the carbon curve values
# # growth curve id for group 321 is 49
# gc49 <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==49,]
# plot(gc49[,2],gc49[,3])
# gc49age90 <- gc49[gc49[,2]==90,]
# gcCheck <- growth321[1:3]-t(gc49age90[3:5])
# ##CHECK - those are the values of the $growth_increments
# # double check growth for the next year
# g321c2 <- melt(spadesCBMout$cbmPools[simYear==1991&pixelGroup==321,6:30])
# growth321_2 <- g321c2[,2]-g321c1[,2]
# gc49age91 <- gc49[gc49[,2]==91,]
# gcCheck2 <- growth321_2[1:3]-t(gc49age91[3:5])
# 
# # note that the curves are wonky after beign put through CBMVolumeToBiomass....
# 
# # group 761 should start out with the carbon in the group 321 in the spinup, get
# # disturbed by fire, and grow age 0 following only the 1st three pools as they
# # are the simplest
# # check that the spu is 28 and the gc is 49
# spadesCBMout$pixelGroupC[pixelGroup==761,]
# # CHECK
# ## BUT THE STAND HAS 32 tonnes of carbon in the merch at age 3...wrong!###############
# 
# g761c1 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==761,6:30])
# gc49age0 <- gc49[gc49[,2]==0,]
# g7610 <- gc49age0[3:5]
# end761 <- g761c1[1:3,]
# pnames <- as.character(g761c1$variable[1:3])
# startC761 <- su321[1:3,]
# 
# merchOp <- startC761[1]-(startC761[1]*sum(fireBP[grep(pattern=pnames[1],fireBP$sourceName),5])) + g7610[1]
##------------------------------------------------------------------------------------------------
# fire in the Boreal Plains------------------------
# this is disturbance 371 (SPU 27)
# the proportions of c transfered are not the same as the fireBS
fireBP <- listDists[[2]][,c(2,4,3,5,6)]
write.csv(file = file.path(outputPath(spadesCBMout),"fireBorealPlains.csv"),fireBP)
# are there differences between the fires?
fireDiffs <- fireBP == fireBS
propDiffs <- fireBP[,5]-fireBS[,5]
mean(propDiffs)
#[1] -1.70067e-08
#They are almost equal...



# follow a 'non-disturbed' pixelGroup to check the pixelGroup 1








# ccut
ccut <-  listDists[[3]]

# defor
defor <-  listDists[[4]]

#mortality 20%
mort20 <-  listDists[[5]]




### TOADD
trackPix3 <- sim$spatialDT[which(!(pixelIndex %in% distPixels$pixelIndex)),.(pixelIndex,pixelGroup)]
newGroups <- distPixels[,.(pixelIndex,newGroup)] %>% .[,pixelGroup:=newGroup] %>% .[,newGroup:=NULL]
trackPix4 <- rbind(trackPix3,newGroups)
trackPix4 <- trackPix4[order(pixelIndex),]

#trackPix[which(!is.na(sim$spatialDT$events))] <- distPixels$newGroup
sim$pixelKeep <- sim$pixelKeep[,newPix := trackPix4$pixelGroup]
setnames(sim$pixelKeep,"newPix",paste0("pixelGroup",time(sim)))

# change the vector of pixel group in $spatialDT to match trackPix for next annual cycle
group1 <- sort(unique(sim$spatialDT$pixelGroup))
sim$spatialDT$pixelGroup <- trackPix4
# count the pixels in each new pixel group
pixelCount <- sim$spatialDT[,.N,by=pixelGroup]

group2 <- sort(unique(trackPix))
groupOut <- subset(group1, !(group1 %in% group2))
# 


grep(pattern=pnames[2],fireBS$sourceName)
grep(pattern=pnames[3],fireBS$sourceName)
# this is substrated from the atmosphere:1.354479
minusAtm <- sum(growth321[c(1:5)])
#This is the difference between the spinup and 1990 for the Atm pools (C02,CH4,CO)
deltaAtm <- sum(growth321[22:24])# right now just in CO2
