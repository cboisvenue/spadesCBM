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
length(myBigList)
#[1] 27
#goes from 1985:2011
length(1985:2011)
# our current simulations are freqTable[6] 1990 to freqTable[21] 1993


########### Ran spadesCBM 1990:1993 with the disturbance rasters, so [[6]] to [[9]]#############
# this is what comes out of the spinup, 739 pixel groups
spinupOut <- as.data.table(spadesCBMout$spinupResult)
# these are all the pools for all the pixels groups throughout the simulations
cbmPools <- as.data.table(spadesCBMout$cbmPools)

# Check growth for three non-disturbed pixel groups with no overmature decline--------------------
# get the age of pixel groups <50 in the initial pixel groups (these carry
# through the simulations, i.e., no pixel groups disappear)
spsMatch <- fread("C:/Celine/GitHub/spadesCBM/data/spsMatchNameRasterGfileBiomParams.csv")
gInc <- as.data.table(spadesCBMout$growth_increments)
# plot m3/ha
volInc <- fread(spadesCBMout$gcurveComponentsFileName)
library(ggplot2)
volCurves <- ggplot(data=volInc, aes(x=Age,y=MerchVolume,group=GrowthCurveComponentID, colour=GrowthCurveComponentID)) +
  geom_line()

# plot gInc - corresponding "translated" increments
# note that there are still problems with these...will fix that later
swMerch <- ggplot(data=gInc, aes(x=age,y=swmerch,group=id, colour=id)) +
  geom_line()
swFol <- ggplot(data=gInc, aes(x=age,y=swfol,group=id, colour=id)) +
  geom_line()
swOther <- ggplot(data=gInc, aes(x=age,y=swother,group=id, colour=id)) +
  geom_line()
hwMerch <- ggplot(data=gInc, aes(x=age,y=hwmerch,group=id, colour=id)) +
  geom_line()
hwFol <- ggplot(data=gInc, aes(x=age,y=hwfol,group=id, colour=id)) +
  geom_line()
hwOther <- ggplot(data=gInc, aes(x=age,y=hwother,group=id, colour=id)) +
  geom_line()
## for now, don't take "early" negative values
posInc <- gInc[!(gInc$age<50 & (gInc$swmerch<0 | gInc$hwmerch<0)),]

# choosing level3DT means the that we are looking at the increment between the
# spinupOut and the cbmPools in 1990
# ages less than 50 more than 0
cbmPools[ages<50 & ages>0,]

# use this to have a "start" age of <50, and to select the growth_component_id
level3DT <- spadesCBMout$level3DT
#posIncCheck <- function(ageLim=50,inc=posInc, year1sim=growthCheck1,year2sim=)
growthCheck1 <- level3DT[ages<50,]
gcIdCheck1 <- unique(growthCheck1$growth_curve_component_id)
# picking a growth curve randomly (this is specieXproductivity)
g1 <- growthCheck1[growth_curve_component_id==gcIdCheck1[round(runif(1,1,length(gcIdCheck1)),0)],]
# picking a specific age randomly - age, gcID, pixelGroup
g2 <- g1[round(runif(1,1,dim(g1)[1]),0),c(ages,growth_curve_component_id,pixelGroup)]

# calculate growth for merch fol other from $growth_increment table
gCalc <- posInc[id==g2[2] & age %in% (g2[1]+1): (g2[1]+(length(1990:1993)-1)),]
gSim <- diff(as.matrix(cbmPools[pixelGroup== g2[3],c("SoftwoodMerch","SoftwoodFoliage","SoftwoodOther",
                              "HardwoodMerch","HardwoodFoliage","HardwoodOther")]))
diff0 <- gCalc[,-c(1:2)] - gSim
if (length(which(diff0!=0))!= 0){a <- "not equal"} else
  a <- "equal"

# END growth checks out--------------------------------------------------------------


# check disturbances---------------------------------------------------------------------------------
# each year, the disturbed pixels get regrouped into new pixel groups. Each new
# group gets the carbon from their previous pixel groups. Their age is set to 0
# before processing for that annual cycle. Example 739 in the first year (1990, which
# is [[6]]) - starting at pixelgroup 740, the ages are set to 0
cbmPools[736:800,]

###### this shows that the right pixelGroups are selected for disturbance

## check disturbances c transfers-------------

freqTables[[6]] # shows all disturbed pixels for 1990
# what pixels are simluated AND disturbed in 1990?------------------------------------
pixels <- getValues(spadesCBMout$masterRaster)
dist1990 <- getValues(myBigStack[[6]]) %>% .[pixels != 0]
table(dist1990)
# dist1990
# 0             1       2       3       4       5 
# 1346168       6     682     136      77     460 

#C:\Celine\GitHub\spadesCBM\data\forIan\SK_data\disturbance_Sask\ReadMe.txt
# Fire =  1
# Harvest = 2
# Lcondition = 3
# Road = 4
# Unclass = 5
# 6 burnt pixels
# 682 harvested pixels
# 77 deforested pixels
# 460+136 Generic 20% mortality
# we have these spatial units in our sim
unique(spadesCBMout$spatialUnits)
#27 28

# are the fire disturbances different?-----------------------------------
ls.str(listDists) ## careful, this does not show you the disturbances in order in the list
d378 <- as.data.table(listDists[[1]])
d371 <- as.data.table(listDists[[2]])
dim(d371)
dim(d378)
length(which(d371$proportion==d378$proportion)) == length(d371$proportion)
# YES: treat them as seperate disturbances, one fire for each spatial unit---
burnPix <- which(dist1990==1)

# harvest -----------------------
d409 <- as.data.table(listDists[[3]])
# defor--------------------
d26 <- as.data.table(listDists[[4]])
# generic 20% mortality --------------
d91 <- as.data.table(listDists[[5]])

## HARVEST in 1990 -----------------------------------------

## pixel index of harvested pixels
harvPixInd <- which(dist1990==2)
# pixelGroup to which for all these pixels
trackHarv1990 <- spadesCBMout$pixelKeep[harvPixInd,1:3]
# 682 harvested pixels

# carbon in here WAIT not sure this is the right index..
#preDist <- spinupOut[unique(trackHarv1990$pixelGroup0),]##
# should match carbon in here + changes due to disturbances
postDist <- cbmPools[simYear==1990 & pixelGroup %in% trackHarv1990$pixelGroup1990,]

# get pixel group meta data
# only on posInc growth_curve_component_id, age<50
harv1990 <- spadesCBMout$level3DT[pixelGroup %in% trackHarv1990$pixelGroup0 & ages<50 &
                                    growth_curve_component_id %in% posInc$id,]
# this is a 49 year old Jack Pine pixel group
## only one! Pixel group 287

# pre-disturbance pixel group number matches the row values in level3DT and spinupOut
level3DT[287,]==harv1990
# so this is the pre-disturbance carbon
preD <- spinupOut[harv1990$pixelGroup,]
# what pixel group did it become?
postPixG <- unique(trackHarv1990[pixelGroup0==harv1990$pixelGroup,]$pixelGroup1990)
## pixel group 805
postD <- cbmPools[simYear==1990 & pixelGroup == postPixG,]
## something very weird happening...so I will look at all the sim years for that group
pg805 <- cbmPools[pixelGroup == postPixG,]

# check pixel group? 
spadesCBMout$pixelKeep[,.N,by=pixelGroup1990][pixelGroup1990==805,]
spadesCBMout$pixelKeep[,.N,by=pixelGroup1991][pixelGroup1991==805,]
spadesCBMout$pixelKeep[,.N,by=pixelGroup1992][pixelGroup1992==805,]
spadesCBMout$pixelKeep[,.N,by=pixelGroup1993][pixelGroup1993==805,]

## Calculating disturbance to pre-disturbance values -----------------------------------
## using data.table

## CHECK numbering and names---------------------------
# checking the pool numbering - what is the difference between these two? in
# terms of pool numbering
distProp <- as.data.table(spadesCBMout$allProcesses$Disturbance$`409`)
distProp[order(row),]
d409a <- d409[,.(row=as.numeric(as.character(source_pool_id)),col=as.numeric(as.character(sink_pool_id)),
                 source_pool_id,sink_pool_id,proportion)][order(row),]
distProp$row[1:dim(d409a)[1]] == (d409a$row+1)
## the row value in the sparse matrices found here
## $AllProceses$Disturbance$`number` are the source_pool_id in disturances out
## of the seeDist() function
### now deciding: the carbon goes from "row" to "col"
colnames(preD) == spadesCBMout$pooldef
# because of the "input" column, the numbering matches
## END CHECK numbering and names-----------------------

# DISTURBANCE-------------------------------------
standIn <- as.data.table(cbind(t(preD),row=c(1:length(preD))))

rowJoin <- standIn[distProp, on="row"][,fluxOut:=(V1*value)]

outC <- rowJoin[,.(outC = sum(fluxOut)), by=row]
inC <-  rowJoin[,.(inC = sum(fluxOut)), by=col]
names(inC) <- c("row","inC")

fluxes <- merge(outC,inC,by="row", all=TRUE)

fluxes$inC[which(is.na(fluxes$inC))] <- 0
standOut <- standIn[fluxes,on="row"][,.(calcDist = V1-outC+inC),by="row"]
## END DISTURBANCE -------------------------------

# Calculate growth merch, fol, other------------------------
# spinup to 1990 will be called 1990

# double check growth component id
level3DT[287,growth_curve_component_id]==spadesCBMout$pixelGroupC[pixelGroup==805,growth_curve_component_id]
level3DT[287,growth_curve_component_id]
#plot the curve
gc52 <- gInc[id==level3DT[287,growth_curve_component_id],]
library(reshape2)
gc <- melt(gc52[,2:5],id.vars = "age")
all3 <- ggplot(data=gc, aes(x=age,y=value,group=variable,colour=variable)) + geom_line()


### STOP############ trying to fix the biomass increment curves
## seems to be a growth curve problem again
#get the meta for the gc
gcMeta <- fread(spadesCBMout$gcurveFileName)
gcMeta <- unique(gcMeta[,c(2,3,4,5,6,9,10)])

## there are 50...
# which look ok?
# positive merch <25 years old?
swMerchOk <- unique(gInc[age<50 & swmerch>=0,id])

swMerch <- ggplot(data=gInc[id %in% swMerchOk,], aes(x=age,y=swmerch,group=id, colour=id)) +
  geom_line()
swFol <- ggplot(data=gInc, aes(x=age,y=swfol,group=id, colour=id)) +
  geom_line()
swOther <- ggplot(data=gInc, aes(x=age,y=swother,group=id, colour=id)) +
  geom_line()
hwMerch <- ggplot(data=gInc, aes(x=age,y=hwmerch,group=id, colour=id)) +
  geom_line()
hwFol <- ggplot(data=gInc, aes(x=age,y=hwfol,group=id, colour=id)) +
  geom_line()
hwOther <- ggplot(data=gInc, aes(x=age,y=hwother,group=id, colour=id)) +
  geom_line()
## for now, don't take "early" negative values
posInc <- gInc[!(gInc$age<50 & (gInc$swmerch<0 | gInc$hwmerch<0)),]




