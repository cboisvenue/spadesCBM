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


########### Ran spadesCBM 1990:1995 with the disturbance rasters###############################
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

## what do the harvested pixels look like?---------------------------------
harvPixInd <- which(dist1990==2)
# from which pixelGroup to which for all these pixels
trackHarv1990 <- spadesCBMout$pixelKeep[harvPixInd,]
# what age for these pixelGroups at spinup?
ageHarv1990 <- spadesCBMout$level3DT[pixelGroup %in% trackHarv1990$pixelGroup0,]
# picked one
pg537desc <- spadesCBMout$level3DT[pixelGroup==537,]
# ages rasterSps Productivity spatial_unit_id growth_curve_component_id growth_curve_id pixelGroup
#   50         5            1              28                        55              55        537
# This is a Pop_Tre stand, at age 50, growth curve 55
# 537 is the pg out of the spinup

# pg537 through time
pg5371990 <- trackHarv1990[pixelGroup0==537]
# pg787 if the pixel group after teh 1990 harvest disturbance
# check the the description is the same...
spadesCBMout$pixelGroupC[pixelGroup==787]
# ages rasterSps Productivity spatial_unit_id growth_curve_component_id growth_curve_id pixelGroup
# 1:    6         5            1              28                        55              55        787

# starting amount of C (both 537 and 787 start with the same amount of C)
pixelGroupC0.537 <- cbind(spadesCBMout$level3DT,spadesCBMout$spinupResult)%>% .[pixelGroup==537]
suC537 <- melt(spadesCBMout$spinupResult[which(spadesCBMout$level3DT$pixelGroup==537),-1])
# double checked

#check the gc
gc55 <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==55,]
clearPlot()
Plot(gc55[,2],gc55[,6])
Plot(gc55[,2],gc55[,7])
Plot(gc55[,2],gc55[,8])
# just as weird as all the others...

# does the 537 correspond to this? (this checks if the overmature decline has kicked in)
# in 1990, 537 grows by the gc increment
gc55.50 <- gc55[gc55[,2]==50,]
# the suC537 + 3 pools growth, should equal
c537.1990 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==537,6:30])
growth537.1990 <- c537.1990$value-suC537$value
gcCheck1 <- growth537.1990[6:8]-gc55.50[6:8]
# yes, 537 grows by that much

# what happens to 787 in 1990?
# It should be harvested according to the DMID 409
c787.1990 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==787,6:30])
# c787.1990
# variable        value
# 1:           SoftwoodMerch    0.0000000
# 2:         SoftwoodFoliage    0.0000000
# 3:           SoftwoodOther    0.0000000
# 4:     SoftwoodCoarseRoots    0.0000000
# 5:       SoftwoodFineRoots    0.0000000
# 6:           HardwoodMerch   35.4285142
# 7:         HardwoodFoliage    1.3234401
# 8:           HardwoodOther    8.7897469
# 9:     HardwoodCoarseRoots   10.7487207
# 10:       HardwoodFineRoots    1.8864570
# 11: AboveGroundVeryFastSoil   12.5924905
# 12: BelowGroundVeryFastSoil    2.5071665
# 13:     AboveGroundFastSoil   17.8466290
# 14:     BelowGroundFastSoil    6.2491608
# 15:              MediumSoil   35.6325656
# 16:     AboveGroundSlowSoil   32.1257725
# 17:     BelowGroundSlowSoil   97.9476053
# 18:        SoftwoodStemSnag    0.0000000
# 19:      SoftwoodBranchSnag    0.0000000
# 20:        HardwoodStemSnag    9.2256158
# 21:      HardwoodBranchSnag    0.5625601
# 22:                     CO2 4975.0270615
# 23:                     CH4    4.4250473
# 24:                      CO   39.8238937
# 25:                Products   37.4208884

# IT GROWS MORE THEN THE pg537...
########### Ran spadesCBM 1990:1995 with the disturbance rasters###############################

## STEP 2---------------------------------------------------------------------------------
# 1.change the script in annual for no disturbance - Run 1990:1993
# does the growth happen as it should?
# check pixelGroup 101
checkGrowth <- function(pg=c(101)){
  pGdesc <- spadesCBMout$level3DT[pixelGroup %in% pg[[1]],]
  pGdesc
  suCpg <- melt(spadesCBMout$spinupResult[which(spadesCBMout$level3DT$pixelGroup==pg),-1])
  pg1990 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==pg,6:30])
  
  gCurvePg <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==spadesCBMout$level3DT[[which(spadesCBMout$level3DT$pixelGroup==pg),5]],]
  
  # need to figure out if it is HW or SW
  growthC <- pg1990$value - suCpg$value
  
  if(gCurvePg[2,3]==0){
    mFolO <- c(6:8)
  }else    mFolO <- c(3:5)
  
  growthCheck <- growthC[mFolO] - gCurvePg[which(gCurvePg[,2]==pGdesc$ages),mFolO]
  return(growthCheck)

}
#I have now mannually calculated the vol to biom conversion for balsam fir
#select pixelGroup that are balsam fir and check ## NONE DO HAVE BALSAM FIR!
# reran everything with black spruce medium
BSid <- c(8,9,29,30,50,51,71,72,92,93)
#Only need to check 29,30,51
BScurves <- unique(spadesCBMout$level3DT[pixelGroup %in% pg[[1]],growth_curve_component_id])
# maje sure those of "my" curves
gCurvesBS <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1] %in% BScurves,]
which(gCurvesBS[1:250,3]!=gCurvesBS[251:500,3])
#integer(0)
which(gCurvesBS[1:250,3]!=gCurvesBS[501:750,3])
#integer(0)
pg <- spadesCBMout$level3DT[rasterSps==3&growth_curve_component_id %in% BSid,7]
# 64 pixel groups use those curves

# this was to check random pg
#growthCheck <- checkGrowth(pg=1)
# swmerch         swfol       swother 
# -0.0016801097  0.0004728645 -0.0002689583 
#checkGroups <- round(runif(10,min = 0, max=759),0)

growthChex <- list()

for(i in i:dim(pg)[1]){
  growthChex[[i]] <- checkGrowth(pg[[1]][i])
}
resultsBS <- NULL
for(i in i:dim(pg)[1]){
  resultsBS <- rbind(resultsBS,growthChex[[i]][1])
}


# add species
gcID <- read.csv(spadesCBMout$gcurveFileName)#file.path(getwd(),"data/spadesGCurvesSK.csv"))
# the above shows pixelGroup 101 to be white birch...but the cPoolsPixelYear.csv shows it to be a SW...and it grows?
gcSpsName <- unique(gcID$species)
gcSpsID <- unique(gcID$species_id)
SpsIDMatch <- vector(length=length(gcSpsName))
for(i in 1: length(gcSpsName)){
  SpsIDMatch[i] <- gcID[gcID$species==gcSpsName[i],4]%>% .[1]
}

species <- as.character(gcSpsName)
gcSpsMatch <- as.data.table(cbind(species,gcSpsName,SpsIDMatch))
gcSpsName

pGdesc <- cbind(spadesCBMout$level3DT[pixelGroup %in% checkGroups,],results1)
pGdesc <- merge(pGdesc,gcSpsMatch[,rasterSps:=as.numeric(gcSpsName)], by="rasterSps")
write.csv(pGdesc,file = "C:/Celine/GitHub/spadesCBM/data/growthChex.csv")

## next steps:
## check these
# TA works at age 40(gc55) and 43(gc34) but fails at 13(gc55)...
# JP works at 0 but fails at 100 (gc52)
# BS fails at 108(gc28)
# WS fails at 10113
# WB has very little...

gc55 <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==55,]
gc34 <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==34,]
write.csv(gc55,file = "C:/Celine/GitHub/spadesCBM/data/TAgrowth1.csv")
write.csv(gc34,file = "C:/Celine/GitHub/spadesCBM/data/TAgrowth2.csv")
TAid <- gcID[rasterSps==5,]
write.csv(TAid,file = "C:/Celine/GitHub/spadesCBM/data/TAid.csv")

## note overmature decline only happens when totoal increment is less than 0.

## THEY DO NOT MATCH...##############################

#checking out the inputs module
gc1 <- read.csv(spadesCBMout$gcurveFileName)
growthCurves <- as.matrix(gc1[,c(3,2,5,4,6)])
gcComp <- as.matrix(read.csv(spadesCBMout$gcurveComponentsFileName))
growth_increments<-NULL
for(gcid in unique(growthCurves[,"growth_curve_id"])) { 
  curve <- processGrowthCurve(gcid, growthCurves, gcComp,sim = spadesCBMout)
  growth_increments <- rbind(growth_increments,
                             cbind(rep(gcid,(nrow(curve)-1)), cbind(curve[0:(nrow(curve)-1),1], diff(curve[,2:ncol(curve)]))))
  
}
ginc <- as.data.table(growth_increments)
names(ginc) <- c("curveNum","ages","swmerch","swfol","swo","hwmerch", "hwfol","hwo")

gcMax <- ginc[,.(Mswmerch=max(swmerch),Mswfol=max(swfol),Mswo=max(swo),
                 Mhwmerch=max(hwmerch),Mhwfol=max(hwfol),Mhwo=max(hwo)),by=curveNum]

# does this match HW SW species?
gSpsCheck1 <- as.data.table(gc1[,c(2,9,10,4)])
names(gSpsCheck1) <- c("curveNum","species","rasterSps","species_cbm")

incSps1 <- merge(gSpsCheck1,gcMax)
View(incSps1)
## The increments seem logical


# FIRST ATTEMP BELOW###############################################################################
# first check on DMIDs below---------------------------------------------#############################


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


#######################################################################
## STOP - side track - the su and pg1990 shows a SW species (3 Black Spruce) bu the GC shows a HW???
gcID <- read.csv(spadesCBMout$gcurveFileName)#file.path(getwd(),"data/spadesGCurvesSK.csv"))
# the above shows pixelGroup 101 to be white birch...but the cPoolsPixelYear.csv shows it to be a SW...and it grows?
gcSpsName <- unique(gcID$species)
gcSpsID <- unique(gcID$species_id)
SpsIDMatch <- vector(length=length(gcSpsName))
for(i in 1: length(gcSpsName)){
  SpsIDMatch[i] <- gcID[gcID$species==gcSpsName[i],4]%>% .[1]
}

species <- as.character(gcSpsName)
gcSpsMatch <- as.data.table(cbind(species,gcSpsName,SpsIDMatch))
# manually made some checks on the species and growth curves matches ~/outputs/speciesCheckFor101.xlsx
# there are 105 curves, only 10 distinct curves (as per Boisvenue et al 2016b).
#here is the match in the gcID file
gcIDSps <- gcID[,c(2,9,10,4)]
# the rasterSps match the raster definition

# Checking in the cbm_defaults if those species ids (SpsIDMatch) are correct 
# run line 8:48 of readInSQLiteData
sps
spsCheck <- sps[sps$species_id %in% gcSpsID,]
### ALL SPECIES MATCH

# do the species in the growth_curve_component_id?
match1 <- read.csv("C:/Celine/GitHub/spadesCBM/data/curveSpsMatch.csv")
match1 <- match1[order(match1$growth_curve_component_id),]
checkMatch2 <- which(match1$species!=gcID$species)
### species match.
# I was messing up the growth_curve_component_id and teh piXelGroups...:/
check101 <- which(growthCurveComponents[,1]==101)
plot(growthCurveComponents[check101,2],growthCurveComponents[check101,3])
#######################################################################
