# cheking out the DMIDs used in our simulations
#GitHub\spadesCBM\data\forIan\SK_data\SK_ReclineRuns30m\LookupTables\DisturbanceTypeLookup.csv
# 1 is Wildfire DMID 371 and 
# 2 is Clearcut harvesting with salvage 409
# 3 is Deforestation â€” Transportation â€” Salvage, uprooting and burn
# 4 Generic 20% mortality
# 5	Generic 20% mortality

# these are the disturbance matrix ids used
dists <- unique(spadesCBMout$mySpuDmids$disturbance_matrix_id)

# use the functions in extraFunctions
listDists <- seeDist(dists)
str(listDists)

# frequency of disturbances:
#Get frequency table from disturbance rasters
library(raster)
library(magrittr)
myBigList <- grep(pattern = "*.tif", 
                  x = list.files("data/forIan/SK_data/CBM_GIS/disturbance_testArea/", full.names = TRUE),
                  value = TRUE)
myBigList <- lapply(myBigList, raster::raster)
myBigStack <- raster::stack(myBigList)
freqTables <- raster::freq(myBigStack)
# this gives my a frequency table for each of the year in which we have disturbances.
# our curretn simulations are freqTable[6] to freqTable[21]
# these freTables have 3703100 pixels and masterRaster has 3705000...

### Checking the matrices--------------------------------------------------------------------------
# fire in the Boreal Shield West------------------------
fireBS <- listDists[[1]][,c(2,4,3,5,6)]
write.csv(file = file.path(outputPath(spadesCBMout),"fireBorealShield.csv"),fireBS)
# looking for burnt pixels
pixels <- getValues(spadesCBMout$masterRaster)
# mathcing the pixelIndex in pixel keep
burntPixelsBS <- getValues(myBigStack[[6]]) %>% .[pixels != 0]
burntPixels1 <- which(burntPixelsBS==1)
length(burntPixels1)
#[1] 14

fire1Pixels1990 <- spadesCBMout$pixelKeep[burntPixels1,]
fire1Groups <- unique(fire1Pixels1990[,2:16])
#331,444,482,776,788
# 12 pixels of the 444 group were burnt in 1990.
# those 12 pixels become the new group 776 until the end of the simulation
# one pixel from group 331 and one pixel from group 482 are burnt in 1990
# those two pixels become group 764 and 788 until the end of the simulation
pixelGroup331 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==331)))
pixelGroup444 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==444)))
pixelGroup482 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==482)))
pixelGroup776 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==776)))
pixelGroup788 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==788)))
plot(pixelGroup331)
plot(pixelGroup444)
plot(pixelGroup482)
plot(pixelGroup776)
plot(pixelGroup788)
# conclusion: all pixel groups stay stable or go down pixel group 444 starts
# with 6318 in the spinup then goes to 6306 from 1990 to 1992, looses 52, has at
# 6254 in 1993 and looses 779, has 6133 in 1994, 6132 for 1995 to 1997, 6039 in
# 1998, 5661 in 1999 and 2000, 5962 in 2001, 5872 in 2002, 5784 in 2003, 5782 in
# 2004 and 5709 in 2005. Similarly with group 331 and 482. pixel group 776 (12)
# 788 (1) maintain their #pix that until the end of the simulation


# this is the pixel groups out of the spinup (759)
group0 <- unique(spadesCBMout$pixelKeep$pixelGroup0)
#these are the pixel groups that include the pixels disturbed, and the first group in the cPoolsPixelYear.csv
group1 <- unique(spadesCBMout$pixelKeep$pixelGroup1990)

# take the 12 disturbed pixels in 1990, for group 444 that becomes group 776,
# disturb it as per fireBS and check values in $cbmPools
# carbon out of the spinup
su444 <- melt(spadesCBMout$spinupResult[which(spadesCBMout$level3DT$pixelGroup==444),-1])
g444c1 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==444,6:30])
# the difference between the spinup and the 1990 pools should be the growth only for the 444 group
growth444 <- g444c1[,2] - su444$value
# three first values should be the carbon curve values
# growth curve id for group 444 is 52
gc52 <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==52,]
plot(gc52[,2],gc52[,3])
gc52age3 <- gc52[gc52[,2]==3,]
gcCheck <- growth444[1:3]-t(gc52age3[3:5])
##CHECK - those are the values of the $growth_increments
# double check growth for the next year
g444c2 <- melt(spadesCBMout$cbmPools[simYear==1991&pixelGroup==444,6:30])
growth444_2 <- g444c2[,2]-g444c1[,2]
gc52age4 <- gc52[gc52[,2]==4,]
gcCheck2 <- growth444_2[1:3]-t(gc52age4[3:5])

# note that the curves are wonky after beign put through CBMVolumeToBiomass....

# this is substrated from the atmosphere:1.354479
minusAtm <- sum(growth444[c(1:5)])
#This is the difference between the spinup and 1990 for the Atm pools (C02,CH4,CO)
deltaAtm <- sum(growth444[22:24])# right now just in CO2

# group 776 should start out with the carbon in the group444 in the spinup, get
# disturbed by fire, and grow age 0 following only the 1st three pools as they
# are the simplest
g776c1 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==776,6:30])
gc52age0 <- gc52[gc52[,2]==0,]
g7760 <- gc52age0[3:5]
end776 <- g776c1[1:3,]
pnames <- as.character(g776c1$variable[1:3])
ini776 <- su444[1:3,]
merchOp <- ini776[1]-(ini776[1]*sum(fireBS[grep(pattern=pnames[1],fireBS$sourceName),5])) + g7760[1]

grep(pattern=pnames[2],fireBS$sourceName)
grep(pattern=pnames[3],fireBS$sourceName)






gc52age1 <- gc52[gc52[,2]==1,]

## IT DOES NOT NEED TO FIX THIS

# check the fireBS disturbance anyway
# DOES NOT GET DISTURBED EITHER














# fire in the Boreal Plains
fireBP <- listDists[[2]][,c(2,4,3,5,6)]
write.csv(file = file.path(outputPath(spadesCBMout),"fireBorealPlains.csv"),fireBS)
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
