# Figuring out the TASKS in spatialInfor.rmd
# this file is to be ignored
# CBoisvenue June 12, 2018


### 6- Last pass disturbance  
#The user has to identify the last disturbance that happened to the stand/pixel that is 
#at the age in the age raster (in 1). For example, this could be a fire or a clearcut. 
#In each spatial unit across Canada, there are a number of defined matrices for clearcut 
#and fire. So spatial unit needs to be associated with a pixel - as in 5, this can be 
#done via the gcID assignment (in 4) since they have spatial units associated with them. 
#Once the spatial unit per pixels is known, the table 
#`spadesCBMout@.envir$cbmData@disturbanceMatrixAssociation` can provide the 
#disturbance_type_id and the disturbance_matrix_id that are possible for the spatial unit. 
#The cbm_default.db table disturbance_type gives the name and a description of the 
#disturbance (using the disturbance_type_id) from which the user can select..  

#***TASKS***  
#  1. write script to facilitate the choice of last pass disturbance.


##Step 1: determine which spu you are in


## For us, take them from the growth information
# ***NEW TASKS***could we provide spu rasters? adminXeco with the same numbering as the table in 

# Read in the gc files AND read-in the cbm_defaults.db to create the spu (to line 44)

gcComponent <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldComponentRCBM.csv"))
gcIn <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv"))

## here trying to match the spatial_unit_id in gcIn with the spatial_unit in cbm_defaults

# ***run readInSQLiteData.r which creates the data.frame spu (combines all ids with 
# province and ecozone names)***

# figure out what spatial_unit these gc are in with the matching of 
# gcIn$spatial_unit_id and cbmTables$spatial_unit

gcSpu <- spu[which(spu$spu_id %in% unique(gcIn[,1])),]

##Got the spus.
#     spu_id admin_boundary_id     province eco_boundary_id            ecozone
# 8      26                 9 Saskatchewan               5  Taiga Shield West
# 11     27                 9 Saskatchewan               6 Boreal Shield West
# 22     28                 9 Saskatchewan               9      Boreal Plains
# 25     29                 9 Saskatchewan              10  Subhumid Prairies
# 48     30                 9 Saskatchewan              18  Semiarid Prairies

library(raster)
dist85 <- raster("data/forIan/SK_data/SK_ReclineRuns30m/layers/projected_change_type/dist1985.tif")
library(SpaDES)
dev()
Plot(dist85)

########## First try at filling-in the spatial info June 25, 2018 ###################
# 1- ages: `sim$ages <- c(0)#,2,3,140)`  
# 2- number of stands or of pixels: `sim$nStands <- length(sim$ages)`  
# 3- identifier for each stand: `standIdx <- 1:sim$nStands`  
# 4- which growth curve is associated with this stand: `sim$gcids <- c(1)`  
# 5- what is the historical Disturbance matrix identification number (CBM-jargon) associated with this `stand/pixel: sim$historicDMIDs <- c(214)`  
# 6- the disturbance matrix identification number that points to the last disturbance that affect each pixel/stand prior to it growing to the age it is at at the begining of the simulations: `sim$lastPassDMIDS <- c(214)#,1,1,1)`  
# 7- what is the length (in years) of the regeneration delay for this perticular stand/pixel: `sim$delays <- c(0)`  
# 8- what is the minimum number of rotations allowed to be simulated in the spinup (CBM-jargon) of the model prior to the lastPass disturbance (CBM-jargon): `sim$minRotations <- rep(0, sim$nStands)`  
# 9- what is the maximum number of rotations allowed to be simulated in the spinup (CBM-jargon) of the model prior to the lastPass disturbance (CBM-jargon): `sim$maxRotations <- rep(100, sim$nStands)`  
# 10- what is the fire return interval for each of these stands/pixels: `sim$returnIntervals <- c(200)`  
# 11- in what spatial unit (see table above) is this stand/pixel located: `sim$spatialUnits <- rep(26, sim$nStands)`  
# 12- in what ecozone is this stand/pixel: `sim$ecozones <- rep(5, sim$nStands)`  
# 13- identify the stands/pixels (by the identifier in 3 above) that are disturbed, when this disturbance is going to occur in your simulations, and associate the Disturbance matrix Identification number (CBM-jargon) that should be applied: `sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))`  
# `colnames(sim$disturbanceEvents)<-c("standIndex", "Year", "DisturbanceMatrixId")`  

# notes for trying things: Ian created a small version of all the rasters
# They live here: GitHub\spadesCBM\data\forIan\SK_data\CBM_GIS

###################### MAKING A DATA.TABLE FIRST ##########################################
library(data.table)
library(raster)

age <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/age_TestArea.tif"))
#This works
ages <- getValues(age)
# read-in species
ldSpsRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/ldSp_TestArea.tif"))
rasterSps <- getValues(ldSpsRaster) # 5 0 3 4 6 7
# read-in productivity  levels
prodRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/prod_TestArea.tif"))
RasterValue <- getValues(prodRaster)#1 2 3 0
# read-in spatial units
spuRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
spatial_unit_id <- getValues(spuRaster) #28 27

# make it a data.table
level2DT <- as.data.table(cbind(ages,rasterSps,RasterValue,spatial_unit_id))
level2DT1 <- unique(level2DT) # 820 4
level2DT1 <- level2DT1[level2DT1$rasterSps>0,] # 759   4
setkey(level2DT1,rasterSps,RasterValue,spatial_unit_id)


# add the gcID
gcID <- read.csv(file.path(getwd(),"data/forIan/SK_data/gcID_ref.csv"))
gcID <- as.data.table(gcID[,-1])
setkey(gcID,rasterSps,RasterValue,spatial_unit_id)

level3DT <- merge(level2DT1, gcID, all.x=TRUE) #759   8
# all unique combinations
# dim(unique(level3DT))
# [1] 759   8

########################### DATA.TABLE COMPLETE #########################################


### 1- Ages  
simages <- level3DT[,ages]
#sim$ages <- getValues(age)

### 2- Number of stands/pixels
simnStands <- length(simages)
#sim$nStands <- length(sim$ages)

### 3- Stand index
# NOTE: this is actually our level3 homogenising polygons
standIdx <- 1:simnStands
#standIdx <- 1:sim$nStands


### 4- Assign a growth curve id to each stand/pixel  
simgcid <- level3DT[,growth_curve_component_id]
#sim$gcids <- c(1)#,2,3,101)

### 5- Historical Disturbance Matrix Identification Number 
# leave these disturbances for now.
simhistoricDMIDs <- rep.int(214,simnStands)
#sim$historicDMIDs <- rep.int(214,sim$nStands)#,1,1,1)

## OR
## CELINE TO DO
# spatial_unit_id <- unique(level3DT$spatial_unit_id)
# distMatrices <- spadesCBMSim@.envir$cbmData@disturbanceMatrixAssociation[
#   which(spadesCBMSim@.envir$cbmData@disturbanceMatrixAssociation[,1] %in% spatial_unit_id),]
## still have to figure out which is fire

### 6- Last pass disturbance 
# as in 5
simlastPassDMIDS <- rep.int(214,simnStands)
#sim$lastPassDMIDS <- c(214,sim$nStands)#,1,1,1)

### 7- Delay 
simdelays <- c(0,simnStands)
#sim$delays <- c(0,sim$nStands)#,0,0,0)

### 8- Minimum rotations for spinup  
### 9- Maximum rotations for spinup  
### 10- Distrubance return interval for spinup
# these can be found here
spadesCBMSim@.envir$cbmData@spinupParameters
## but keeping they are all the same...
simminRotations <- rep(10, simnStands)
#sim$minRotations <- rep(0, sim$nStands)
simmaxRotations <- rep(30, simnStands)
#sim$maxRotations <- rep(100, sim$nStands)

simreturnIntervals <-merge(level3DT,spadesCBMSim@.envir$cbmData@spinupParameters[,c(1,2)], by="spatial_unit_id", all.x=TRUE)[,9]
#sim$returnIntervals <- c(200)#,110,120,130)

### 11- spatial Units
simspatialUnits <- level3DT[,spatial_unit_id]
#sim$spatialUnits <- rep(26, sim$nStands)

### 12- ecozones
ecoToSpu <- gcSpu[,c(1,4)]
names(ecoToSpu) <- c("spatial_unit_id","ecozones")
simecozones <- merge(level3DT,ecoToSpu,by="spatial_unit_id", all.x=TRUE)[,9]
#sim$ecozones <- rep(5, sim$nStands)

### 13- Disturbances
## for now keep this the same
sim$disturbanceEvents <- cbind(1:sim$nStands,rep(2050,sim$nStands),rep(214,sim$nStands))
colnames(sim$disturbanceEvents)<-c("standIndex", "Year", "DisturbanceMatrixId")

### line 636 of level3DT problem #######################################
<<<<<<< HEAD
spadesCBMout@.envir$level3DT[636,]
# rasterSps RasterValue spatial_unit_id ages growth_curve_component_id species_id
# 1:         6           1              28    1                        58         76
# prodClass     species
# 1:         G White birch
## run line 77 to 105

table(rasterSps)
table(RasterValue)
table(spatial_unit_id)
level3DT[,.N,by="growth_curve_component_id"]
level3DT[,.N,by="species_id"]
level3DT[,.N,by="prodClass"]
gcid58 <- spadesCBMout@.envir$growth_increments[spadesCBMout@.envir$growth_increments[,1]==58,]
## CAN'T FIGURE IT OUT YET
=======


>>>>>>> parent of 9df08c4... Auto stash before merge of "sask" and "origin/sask"
