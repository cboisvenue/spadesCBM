# RIA for NE BC --------------------------------------
# Checking out the data I currently have: ~\data\RIA2019
# Started  November 7, 2019
# CBoisvenue
#-----------------------------------------------------

# this folder has all the data I currently have:
dataDir <- "G:/RES_Work/Work/SpaDES/RIA/data/"
# some of it was copied here: ~\data\RIA2019
# processed data will be stored here: ~\data\RIA2019

library(raster)
library(data.table)

###### SPU and eczones #############################################################
## SPUs for the 5 TSAs##############################################################

## old data has an spu file and Ian has an spu file, are they the same?
#library(rgdal)
# tsa08<-readOGR(dsn="C:/Directory_Containing_Shapefile",layer="MyMap")
# plot(data.shape)
# I think this is the shapefile of the SPUs
test1 <- shapefile(file.path(dataDir,"old/ftStJohn_growthCurves/spUnit_Locator/spUnit_Locator.shp"))
#"data/forIan/SK_data/CBM_GIS/SpadesCBM_TestArea.shp")
Plot(test1)
# check against Ian's
ianSpu <- shapefile("data/spUnit_Locator.shp")
Plot(ianSpu)
# YES: these are the same

### Ian provided RIAspu.png and TSAsSpuRia.png - this last one has both SPUs and TSA boundaries
## thoses are here: G:\RES_Work\Work\SpaDES\RIA\data
# Read in the .dbf?
library(foreign)
spuDbf <- as.data.table(read.dbf("data/spUnit_Locator.dbf"))

# there seems to be 4 ecozones in the RIA Boreal Cordillera, Taiga Plains,
# Montane Cordillera, and Boreal Plains
# determined by looking at the .png provided by Ian Eddy
ecoCheck1 <- unique(spuDbf[ProvncN=="British Columbia",.(EcoBndr,ecozone)])
targEcoz <- c(4,9,12,14)
TSAecoz <- as.data.table(cbind(c(rep("Fort Nelson",2),rep("MacKenzie",2),"Prince George",
                   rep("Dawson Creek",2),rep("Fort St John",4)),c(12,4,12,14,14,14,9,12,9,14,4)))
names(TSAecoz) <- c("TSAnames","ecozones")
RIAspu <- unique(spuDbf[ProvncN=="British Columbia" & ecozone!="Pacific Maritime" & EcoBndr %in% targEcoz,]$spu_id)

## CAREFUL HARD CODED ##
# From Greg Paradis and Ian Eddy's map
spuId <- c(40,38,40,42,42,40,38,42,39,42,39)

TSAnames <- c("Fort Nelson","Fort Nelson", "MacKenzie", "MacKenzie", "Prince George", 
              "Fort St John", "Fort St John", "Fort St John", "Fort St John", "Dawson Creek", "Dawson Creek")
TSAid <- c(8,8,16,16,24,40,40,40,40,41,41)

TSAspu <- cbind(TSAnames,TSAid,spuId)
#names(TSAspu) <- c("TSAnames","TSAid","SpatialUnitID")
spuEco <- as.data.table(spadesCBMout$cbmData@spatialUnitIds)
RIAspuEco <- spuEco[SpatialUnitID %in% as.data.table(TSAspu)$spuId,]
eco <- c(12,4,12,14,14,12,4,14,9,14,9)
TSAspuEco <- cbind(TSAspu,eco)
# dont't write this yet - it does not have the AUs
write.csv(TSAspuEco,file="data/RIA2019/TSAspuEco.csv",row.names = FALSE)
### Need to link the TSAspuEco to AU which are the identifiers for the growth curves for each modelled pixel
#############################

  ############from Ian Eddy ####NEED TO STREAMLINE ##############
  ## Note that the raster to match is here G:\RES_Work\Work\SpaDES\RIA\data\RIA5tsaRTM 
  # the study area (shape file) is G:\RES_Work\Work\SpaDES\RIA\data\RIA_fiveTSA and both are here
  # https://drive.google.com/drive/folders/1I_G4DcLpMRwUwSE4rsEGnYWi5vLtEG06
  #don't have this  
  #mylandscape <- readRDS("C:/users/ieddy/Downloads/landscape_Initial.rds") #haven't shared this yet
    # ecozones <- shapefile("C:/Ian/data/Canada Ecosystem/Ecozones/ecozones.shp") %>% #canadian ecozones
    # spTransform(., CRSobj =  crs(mylandscape$au)) #get in same projection
    # ecozonesSA <- crop(ecozones, y = mylandscape$au) %>% #crop these bad boys
    # sf::st_as_sf(.) #and make them an sf object - see below
    # #make ecozonesSA into a raster - there are many functions for tabulating rasters over a vector, 
    # # and many of them are terrible. The easiest way off the top of my head is by rasterizing the vector
    # ecozonesRas <- fasterize::fasterize(ecozonesSA, raster = mylandscape$au, field = 'ECOZONE')
    # CelinesTable <- data.table(au = getValues(mylandscape$au), 
    #                            ecozone = getValues(ecozonesRas)) %>%
    # na.omit(.) %>% #get rid of Nas
    # .[!duplicated(.)] #get rid of duplicate values
    # CelinesTable
    # sort(unique(CelinesTable$au))
    # data.table::fwrite(CelinesTable, file = "C:/Ian/PracticeDirectory/CelinesTable.csv")
  ############
  # instead read this in
ecoAu <- fread("data/RIA2019/CelinesTable.csv")
names(ecoAu) <- c("AU", "ecozone")

## SPUs file complete###################################################################
###### AU, SPU and eczones END ###########################################################



### DISTURBANCES - picking which ones in the CBM list #####################################
# There are functions in spadesCBMextraFunctions.r that help with finding the
# right disturbance matrices id in spadesCBMextraFunctions.r ###########################
  
### histDist()---------------------------------------------
#Historical disturbances in CBM-CFS3 are used for "filling-up" the soil-related
#carbon pools during spinup. In all spatial
#units in Canada, the historical disturbance is set to fire. A stand-replacing
#fire disturbance is used in a disturb-grow cycle, where stands are disturbed
#and regrown with turnover, overmature, decay, functioning until the dead
#organic matter pools biomass values stabilise.
#This function histDist(), identifies the stand-replacing wildfire disturbance
#in each spatial unit. By default the most recent is selected, but the user can
#change that.
RIAhistDist <- histDist(unique(TSAspu[,3]))
#write.csv(RIAhistDist,file="data/RIA2019/RIAhistDist.csv",row.names = FALSE)
# This will be a vector the length of the number of pixel groups
#### HIST DIST COMPLETED ####################################################
### DECISION: the fire disturbance matrices in the simulations will be the same

## Last pass dist will have to wait until we have more info#################

# spuDist ################################################S
#This function identifies the ID number (CBM-CFS3 legacy) that are possible in
#the specific spatial unit you are in. You give is spatial units you are
#targetting (mySpu) and it give you the disturbance matrix id that are
#possible/default in that specific spu and a descriptive name of that disturbance matrix
#it creates a data.frame of length number of disturbances, with three columns: spatial_unit_id, 
#disturbance_matrix_id, and a desciption of the disturbance.

RIAdmidsAll <- spuDist(unique(TSAspu[,3]))
dim(RIAdmidsAll)
#[1] 312   3
# we need a harvesting for each spatial units, fire will be the wild fire one
# used in the spinup
clearCut <- RIAdmidsAll[grep("Clear",RIAdmidsAll[,3], ignore.case=TRUE),]
# I visiually looked I the list and picked "Clear Cutting Matrix with Salvage"
### NOTE: this means there is no slashburn
clearCut$`cbmTables[[6]][dmid$disturbance_matrix_id, 3]`

# select the chosen matrices
clearCut <- RIAdmidsAll[grep("Clear Cutting Matrix with Salvage",RIAdmidsAll[,3], ignore.case=TRUE),]
mySpuDmids <- rbind(RIAhistDist,clearCut)
names(mySpuDmids) <- c("spatial_unit_id","disturbance_matrix_id","distName")
### NOTE: there will have to be a matching of the dist identification from the
### disturbance rasters or list and the 3rd column presently called distName
#
#########################################################
## THERE WAS A MISTAKE HERE, SOME SPU WERE MISSING (ALL THE ONES OUTSIDE BC)
## According to the SPU raster, we have 13 SPUs not 4
DMIDs <- unique(spuRas[])

RIAfires2 <- histDist(DMIDs[which(!(DMIDs %in% mySpuDmids$spatial_unit_id))])
RIAall2 <- spuDist(DMIDs[which(!(DMIDs %in% mySpuDmids$spatial_unit_id))])
clearCut2 <- RIAall2[grep("clearcut with 94%",RIAall2[,3], ignore.case=TRUE),]
mySpuDmids2 <- rbind(RIAfires2,clearCut2)
names(mySpuDmids2) <- c("spatial_unit_id","disturbance_matrix_id","distName")
mySpuDmids <- rbind(mySpuDmids,mySpuDmids2)
#write.csv(mySpuDmids,"data/RIA2019/mySpuDmids.csv",row.names = FALSE)
mySpuDmids <- read.csv("data/RIA2019/mySpuDmids.csv")

### DISTURBANCES - picking which ones in the CBM list END #####################################

############# growth curves: read them in, select just the ones used in the RIA sims, 
# translate into biomass with Boudewyn, CHECK this translations and insert hard fixes ##########################

## these files are from Greg Paradis 
# this file contains ALL the bc curves
gcBC <- fread("data/RIA2019/yld.csv")
# the vector of the Analysis Units used in the RIA2019
riaAU <- fread("data/RIA2019/ria_au_values.txt")
names(riaAU) <- "AU"

gcRIAall <- gcBC[AU %in% riaAU$AU]
# start the meta data file (equivalent to spadesGCurvesSK.csv)
################### there will be more columns to add here onces we have the
################### link between curves and pixels equivalent to growth_curve_id
################### in spadesGCurvesSK.csv################## (might be anaylsis units??)
gcMeta <- gcRIAall[,c(1:5)]

# creating the equivalent of yieldComponentsSK.csv. Three coumns only: curve id
# (here AU?), age, Merchvolume

gcRIAwide <- gcRIAall[,c(3,7:42)]
# this is wide make it long
gcRIAyields <- melt.data.table(gcRIAwide, id.vars = "AU",measure.vars = c("X0","X10","X20","X30","X40","X50","X60","X70","X80","X90",
                                                                          "X100","X110","X120","X130","X140","X150","X160",
                                                                          "X170","X180","X190","X200","X210","X220","X230","X240",
                                                                          "X250","X260","X270","X280","X290","X300","X310","X320","X330",
                                                                         "X340","X350"))

setorder(gcRIAyields,AU)

gcRIAm3 <- gcRIAyields[,age := rep(seq(0,350,by=10),204)]
gcRIAm3[,-"variable"]
#this is the equivalent of the yieldComponentSK.csv
#write.csv(gcRIAm3[,-"variable"],file = "data/RIA2019/gcRIAm3.csv",row.names = FALSE)
##############################

gcRIAm3 <- as.data.table(read.csv("data/RIA2019/gcRIAm3.csv"))
## Plot it out
  library(ggplot2)
  volCurves <- ggplot(data=gcRIAm3, aes(x=age,y=value,group=AU, colour=AU)) +
    geom_line()
## NOTES: curves look messy...some very low, but 2 near or above 1000m3!!
  ## check curves over 900m3
  vol900AU <- unique(gcRIAm3[which(gcRIAm3$value>900),AU])
  ## over 300m3/ha
  vol300AU <- unique(gcRIAm3[which(gcRIAm3$value>300),AU])

  ### ARE ALL CURVES DIFFERENT? DOES ANYTHING JOIN THEM?
  ## trying to see if there are duplicates in the 204 curves
  gcCheck1 <- gcRIAwide[,-"AU"]
  dim(gcCheck1)
  gcCheck2 <- unique(gcCheck1)
  dim(gcCheck2)
  # even if the excell check showed some curves alike, this seems to tell me they are all different.
  ### NEED TO PROCESS (translate to biomass) 204 CURVES
###### growth curves END #######################################################################################


#### process growth curves ############################--------------------------------------------------------------
# read-in Boudewyn et al parameters for conversion from m3/ha to biomass in
# the three main carbon pools that make-up the $growth_increments used to move
# spadesCBM forward in growth from year to year
# https://nfi.nfis.org/en/biomass_models-------------------------------------
## danger hard coded## need to change this to read URL or cache these.
table3 <- fread("C:/Celine/GitHub/spadesCBM/data/appendix2_table3.csv")#)file.path(paths(sim)$inputPath,"appendix2_table3.csv"
table4 <- fread("C:/Celine/GitHub/spadesCBM/data/appendix2_table4.csv")
table5 <- fread("C:/Celine/GitHub/spadesCBM/data/appendix2_table5.csv")
table6 <- fread("C:/Celine/GitHub/spadesCBM/data/appendix2_table6_v2.csv",fill=TRUE)

# identify jurisdiction matching CBM-legacy numbering with Boudewyn
# jurisdiction params----------------------------------------------
# choices are: 
# table3$juris_id and table4$juris_id and table6$jur
# AB BC MB NB NF NS NT NU ON PE QC SK YK
# table5$juris_id
# AB BC NB NF NT
cbmAdmin <- c(10,11,8,5,1,2,3,13,14,7,4,6,9,12)## danger hard coded##
paramJur <- c("AB","BC","MB","NB","NF","NF","NS" ,"NT" ,"NU" ,"ON" ,"PE", "QC", "SK", "YK")
adminMatch <- as.data.table(cbind(cbmAdmin,paramJur))
#write.csv(adminMatch, file="data/adminMarchBoudewynParams.csv",row.names = FALSE)

jurisdiction <- "BC"#as.character(adminMatch[which(cbmAdmin %in% unique(ecoToSpu[SpatialUnitID %in% unique(gcID$spatial_unit_id),2])),2])
RIAtable3 <- as.data.table(table3[table3$juris_id==jurisdiction,])
RIAtable4 <- as.data.table(table4[table4$juris_id==jurisdiction,])
# table5 is weird since they did not have enough data for SK. I am selecting AB
# instead. Another catch is that they don't have the same species match. I
# manually check and ABIES is genus 3 (used below)
#### PUT error message if the specified jurisdiction is not found #### GIVE CHOICES
RIAtable5 <- as.data.table(table5[table5$juris_id==jurisdiction,])
RIAtable6 <- as.data.table(table6[table6$jur==jurisdiction,])
# END jurisdiction-----------------------------------------------

# read-in species match with canfi_species code and genus to get rigth
# Boudewyn params---------------------------------------------------


#### build the spsMatch needed to match between the leading species identified in
#### the curves and the Boudewyn params that "translate" the m3/ha into biomass
#### ####################################################################################################
    # THESE ARE ALL THE DECISIONS ON THE 21 sps in gcMeta
    # need to match species from gcMeta to Bourdewyn params
    # so LDSPP from the gcMeta matched with canfi_species and genus
    ## the AUs will have to be in there also, so there will be 204 of them
    RIAsps <- unique(gcMeta$LDSPP)
    # table3 and table4 are the same for BC
    tbl3sps <- unique(RIAtable3[,.(canfi_species,genus,species)])
    unique(RIAtable4[,.(canfi_species,genus,species)])==tbl3sps
    # table5 genus
    tbl5sps <- unique(RIAtable5[,.(canfi_genus,genus)])
    # table6 looks like the same numbers as canfi_species in table3 and 4
    tbl6sps <- unique(RIAtable6[,.(species)])

    ## danger this is hard coded ## Species match is a visual process for the moment
    # look at each species and build tbl3sps[c(21,10,5,)]
    RIAsps[1]
    # "Aspen"
    which(tbl3sps$species=="TRE")
    #21
    spsMatchRIA <- cbind(RIAsps[1],tbl3sps[21,])
    RIAsps[2]
    #"Subalpine fir"
    which(tbl3sps$species=="LAS")
    #10
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[2],tbl3sps[10,]))
    RIAsps[3]
    #"Lodgepole Pine"
    tbl3sps[which(tbl3sps$genus=="PINU")]
    which(tbl3sps$species=="CON")
    #5 (this is the var latifolia - the other one is var contorta)
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[3],tbl3sps[5,]))


    RIAsps[5]
    #"Poplar"
    # these will all be given the Cottonwood parameters as the values in the curves
    # visually match those better then the aspen
    RIAsps[6]
    # "Cottonwood"
    tbl3sps[which(tbl3sps$genus=="POPU")]
    which(tbl3sps$species=="BAL")
    # 22
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[5],tbl3sps[22,]))
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[6],tbl3sps[22,]))


    RIAsps[7]
    # "Paper birch"
    tbl3sps[which(tbl3sps$genus=="BETU")]
    which(tbl3sps$species=="PAP")
    # tbl3sps[which(tbl3sps$species=="PAP"),]
    # canfi_species genus species
    # 1:          1303  BETU     PAP
    # 2:          1308  BETU     PAP
    # These have the same params in table 3, but one says it has a count of 27 - so I am picking that one 1303
    which(tbl3sps$canfi_species==1303)
    # 23
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[7],tbl3sps[23,]))


    RIAsps[8]
    # "Alpine larch"
    # Note: only Larix laricina is the only larch that occurs that far north
    tbl3sps[which(tbl3sps$genus=="LARI")]
    which(tbl3sps$species=="LAR")
    #17
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[8],tbl3sps[17,]))

    RIAsps[9]
    # "Black spruce"
    # there is only one
    tbl3sps[which(tbl3sps$genus=="PICE")]
    which(tbl3sps$species=="MAR")
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[9],tbl3sps[2,]))

    RIAsps[10]
    # "Engelmann spruce"
    # there is only one
    tbl3sps[which(tbl3sps$genus=="PICE")]
    which(tbl3sps$species=="ENG")
    #3
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[10],tbl3sps[3,]))


    RIAsps[11]
    # "White spruce"
    # there is only one
    tbl3sps[which(tbl3sps$genus=="PICE")]
    which(tbl3sps$species=="GLA")
    #4
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[11],tbl3sps[4,]))


    RIAsps[12]
    # "Western white pine"
    # there is only one BUT Western White pine's distribution does not reach these
    # latitudes...will go with what they say since the BC level database (yld.csv)
    # has a Whitebark Pine
    tbl3sps[which(tbl3sps$genus=="PINU")]
    which(tbl3sps$species=="MON")
    #29
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[12],tbl3sps[29,]))


    RIAsps[13]
    # "Balsam fir"
    # Balsam fir (Abies balsamea) does not occur in BC and all the other firs do not
    # occur this far north. Selecting Abies lasiocarpa
    tbl3sps[which(tbl3sps$genus=="ABIE")]
    which(tbl3sps$species=="LAS")
    #10
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[13],tbl3sps[10,]))


    RIAsps[14]
    # "Amabilis fir"
    # there is only one BUT Pacific silver fir' is a coastal species's distribution does not reach these
    # latitudes...will go with what they say since the BC level database (yld.csv)
    # has a Whitebark Pine
    tbl3sps[which(tbl3sps$genus=="ABIE")]
    which(tbl3sps$species=="AMA")
    #9
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[14],tbl3sps[9,]))


    RIAsps[15]
    # "Redcedar"
    # there is only one
    tbl3sps[which(tbl3sps$genus=="THUJ")]
    which(tbl3sps$species=="PLI")
    #20
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[15],tbl3sps[20,]))


    RIAsps[16]
    # "Douglas-fir"
    # there are two but they have the same canfi_species. so canfi_specie 500
    tbl3sps[which(tbl3sps$genus=="PSEU")]
    which(tbl3sps$species=="MEN")
    #14
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[16],tbl3sps[14,]))


    RIAsps[17]
    # "Mountain hemlock"
    # there is only one BUT it's distribution does not reach these
    # latitudes...will go with what they say since the BC level database (yld.csv)
    tbl3sps[which(tbl3sps$genus=="TSUG")]
    which(tbl3sps$species=="MER")
    # 13
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[17],tbl3sps[13,]))


    RIAsps[18]
    # "Western hemlock"
    # there is only one BUT it's distribution does not reach these
    # latitudes...will go with what they say since the BC level database (yld.csv)
    tbl3sps[which(tbl3sps$genus=="TSUG")]
    which(tbl3sps$species=="HET")
    #12
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[18],tbl3sps[12,]))


    RIAsps[19]
    # "Tamarack"
    # Note: only Larix laricina is the only larch that occurs that far north
    tbl3sps[which(tbl3sps$genus=="LARI")]
    which(tbl3sps$species=="LAR")
    #17
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[19],tbl3sps[17,]))


    RIAsps[20]
    # "Whitebark pine"
    # there is only one
    tbl3sps[which(tbl3sps$genus=="PINU")]
    which(tbl3sps$species=="ALB")
    #6
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[20],tbl3sps[6,]))

    RIAsps[21]
    # "Willow"
    # there is only one
    tbl3sps[which(tbl3sps$genus=="SALI")]
    #34
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[21],tbl3sps[34,]))


    RIAsps[4]
    #"Spruce"
    # there are 28 curves labelled "Spruce"
    dim(gcMeta[LDSPP=="Spruce",])
    #[1] 28  5
    dim(gcMeta[LDSPP=="Engelmann spruce",])
    #[1] 11  5
    dim(gcMeta[LDSPP=="White spruce",])
    #[1] 16  5
    dim(gcMeta[LDSPP=="Black spruce",])
    #[1] 10  5
    # generic "Spruce" will be given a speciefic spruce name to get the best chances
    # of the Boudewyn params working (? I think this is a true statement but I have
    # not checked)
    # visual matching: I sorted all the "Spruces" by their volume at 50 and looked
    # at the other three spruces species and roughly matche the volumes
    # All 28 "Spruce" will be assigned "White Spruce" (#4)

    # These AUs will be changed to Engelman: 80,308,389 (#3)
    # and these to "Black Spruce" (#2): 1008, 1026,1085,1103,1161,1192,1203
    # After the merge is complete
    RIAsps[4]
    spsMatchRIA <- rbind(spsMatchRIA,cbind(RIAsps[4],tbl3sps[4,]))
    names(spsMatchRIA) <- c("LDSPP","canfi_species","genus","species")

    gcMeta <- gcMeta[spsMatchRIA, on="LDSPP"]
    eng <- c(80,308,389)
    black <- c(1008, 1026,1085,1103,1161,1192,1203)

    gcMeta[AU %in% eng,c(6:8)] <- tbl3sps[3,]
    gcMeta[AU %in% black,c(6:8)] <- tbl3sps[2,]

    # NEED to add forest_type (1:SW, 3:HW)
    RIAspsForestType <- as.data.table(cbind(as.character(RIAsps), c(3,1,1,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1)))
    names(RIAspsForestType) <- c("LDSPP","forest_type_id")
    gcMeta <- gcMeta[RIAspsForestType,on="LDSPP"]
#################
#write.csv(gcMeta,file="data/RIA2019/gcMeta.csv",row.names = FALSE)
#gcMeta <- fread("data/RIA2019/gcMeta.csv")
gcMeta[!(which(gcMeta$AU %in% ecoAu$AU)),]
which(riaAU$V1 %in% ecoAu$AU)
# 29 missing AUs in the ecoAU match...
#### ####################################################################################################
# END - THESE ARE ALL THE DECISIONS ON THE 21 sps in gcMeta

###### from m3.ha to biomass and carbon###############################
## RIA: will have to run each of the 204 AUs is the commun denominator (unique gc by AU)
    # 
    swInc <- NULL
    hwInc <- NULL
    ## could I write this in an lapply to generalise?? YES

    # only process the 173 that have a match in the ecoAu
    gcMeta1 <- gcMeta[which(gcMeta$AU %in% ecoAu$AU),]
    # these are no parameters in table 5 for "SALI" genus
    gcMeta1 <- gcMeta1[-(which(genus=="SALI")),]
    ## 29 AUs that Greg said were in the RIA are not in the ecoAu list
    # gcMeta[!(which(gcMeta$AU %in% ecoAu$AU)),]
    # ## and table3 does not have canfi_species 201 in ecozone 9...it is in ecozones 12 13 or 14
    # # table(TSAspuEco[,4])
    # # 12 14  4  9
    # # 3  4  2  2
     ecoAu[AU==218,"ecozone"] <- 14
    # # no canfi_species 3500 in eco 12 in table 3
     ecoAu[AU==320,"ecozone"] <- 14

    # now processing 173

     # this is the file to process the increments ***NEED TO ADD AU 320 and 479 back in with the info from AU274
     write.csv(gcMeta1,file="data/RIA2019/gcMeta1.csv",row.names = FALSE)   
     ## this is the file for the runs
     ## need to add back 2 lines (the "SALI" taken out are used in the runs)
     gcMetaRuns <- rbind(gcMeta,gcMetaComplete[AU %in%  c(320,479),])
     write.csv(gcMetaRuns,file="data/RIA2019/gcMetaRUns.csv",row.names = FALSE)  

    for(i in 1:length(gcMeta1$AU)){
      ## can't run the ecozones like this..in the mean time using the first ecozone in the TSAspuEco
      meta <- gcMeta1[i,]
      # match the AU call it id with the actual m3
      id <- gcRIAm3$AU[which(gcRIAm3$AU == meta$AU)]
      age <- gcRIAm3[gcRIAm3$AU==id,"age"]
      # temp remove age 0 to see if the Boudewyn et al params handle m3 values of 0
      # need the ecozones attached to the AUs
      cumBiom <- as.matrix(convertM3biomRIA(meta = meta,gCvalues = gcRIAm3,
                                         ecozones = ecoAu,params3=RIAtable3, params4=RIAtable4,
                                         params5=RIAtable5,params6=RIAtable6))

      cumBiom[which(is.na(cumBiom))] <- 0
      # going from tonnes of biomass/ha to tonnes of carbon/ha here
      cumBiom <- cumBiom*0.5
      inc <- diff(cumBiom)

      if(meta$forest_type_id==1){
        incs  <- cbind(id,age,rbind(rep(0,dim(inc)[2]),inc),rep(0,length(age)),rep(0,length(age)),rep(0,length(age)))
        swInc <- rbind(swInc,incs)
        #FYI:
        # cbmTables$forest_type
        # id           name
        # 1  1       Softwood
        # 2  2      Mixedwood
        # 3  3       Hardwood
        # 4  9 Not Applicable
      } else if(meta$forest_type_id==3){incs <- cbind(id,age,rep(0,length(age)),rep(0,length(age)),rep(0,length(age)),rbind(rep(0,dim(inc)[2]),inc))
      hwInc <- rbind(hwInc,incs)}

    }

    colnames(swInc) <- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
    colnames(hwInc) <- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
    increments <- as.data.table(rbind(swInc,hwInc)) %>% .[order(id),]
    interim <- as.matrix(increments)
    interim[is.na(interim)] <- 0
    increments <- as.data.table(interim)

     inc173AUs <- m3ToBiomIncOnlyPlots(inc=increments)
    #       # summary(increments[,swmerch:hwother])
    #       # swmerch               swfol               swother              hwmerch          
    #       # Min.   :-8.016e+09   Min.   :-1.294e+09   Min.   :-1.692e+09   Min.   :-1308.9061  
    #       # 1st Qu.: 0.000e+00   1st Qu.: 0.000e+00   1st Qu.: 0.000e+00   1st Qu.:    0.0000  
    #       # Median : 0.000e+00   Median : 0.000e+00   Median : 0.000e+00   Median :    0.0000  
    #       # Mean   : 3.000e+00   Mean   : 0.000e+00   Mean   : 1.000e+00   Mean   :    0.9103  
    #       # 3rd Qu.: 2.000e+00   3rd Qu.: 0.000e+00   3rd Qu.: 0.000e+00   3rd Qu.:    0.0000  
    #       # Max.   : 8.016e+09   Max.   : 1.294e+09   Max.   : 1.692e+09   Max.   : 1321.8905  
    #       # hwfol              hwother         
    #       # Min.   :-70.64033   Min.   :-442.1977  
    #       # 1st Qu.:  0.00000   1st Qu.:   0.0000  
    #       # Median :  0.00000   Median :   0.0000  
    #       # Mean   :  0.02935   Mean   :   0.2162  
    #       # 3rd Qu.:  0.00000   3rd Qu.:   0.0000  
    #       # Max.   : 75.73454   Max.   : 446.2988  
    # 
    # ######### PROBLEMS WITH TRANSLATIONS###################
    # ### will replace the increments with
    # ### big negative values
    # ### unbeleivable shapes
    # ######################################################
    # 
    # # the Plot functions does not like it when names are numbers
    # # here I change the names to be able to plot many windows at a time
    names(inc173AUs) <- paste0("AU", names(inc173AUs))
    clearPlot()
    Plot(inc173AUs[1:28])
    dev.new()
    Plot(inc173AUs[29:50])
    dev.new()
    Plot(inc173AUs[57:88])
    dev.new()
    Plot(inc173AUs[89:100])
    dev.new()
    Plot(inc173AUs[101:115])
    dev.new()
    clearPlot()
    Plot(inc173AUs[116:148])
    dev.new()
    clearPlot()
    Plot(inc173AUs[149:173])

    ## By visual assessment: there are 69 (of 173) curves that are unacceptable.
    # ## They either have negative values or ridiculous shapes
    # ## Try to figure out rules to replace those with similar curves that work.
    # ## Similar here means by species ann other info in the gcMeta1 data.table
    # ### CAREFUL MANUAL INPUT HERE###############################
    # # by visual assessment, these are the curves that cannot be used:
    incOut <- fread("data/RIA2019/incOutVisual.csv")
    incOut[, names := paste0(V1,V2)]
    incOut[,"V1" :=  NULL]
    names(incOut) <- c("AU","names")

    # double check
    plotIncOut <- inc173AUs[which(names(inc173AUs) %in% incOut$names)]
    clearPlot()
    Plot(plotIncOut[1:23])
    clearPlot()
    Plot(plotIncOut[24:46])
    clearPlot()
    Plot(plotIncOut[47:69])

    # 
    # ## RULES to decide is increments need to be replaces:
    # # do these have anything in commun?
    # ## started from gcMeta1
     outMeta1 <- gcMeta1[ AU %in% incOut$AU,]
    # summary(outMeta1)
    # # since we are working in the north (similar temps/prod) I will go by species.
     outSps <- unique(outMeta1$LDSPP) # 15 species
    # # can I map the AUs? (NO! no map yet)
    # # so match by species and check if there are curves in the same ecozone that works.
    # 
    # # aspen##############################################################################################
    #     # All aspen
    #     aspNames <- paste0("AU",gcMeta1[LDSPP == outSps[1],AU])
    #     which(names(inc173AUs) %in% aspNames) # 14 aspen in total
    #     aspPlots <- as.list(inc173AUs[which(names(inc173AUs) %in% aspNames)])
    #     # visually check all aspen
    #     clearPlot()
    #     Plot(aspPlots)
    #     gcMeta1[LDSPP == outSps[1],]
    #     ## They all seem too productive!!
    #     # I want to see what the aspen looks like in my increments in SK
    #     aspIncsRuns <- spadesCBMout$growth_increments[which(spadesCBMout$growth_increments[,1] %in% spadesCBMout$level3DT[rasterSps==3,growth_curve_component_id]),]
    #       spadesCBMout$level3DT[rasterSps==3,growth_curve_component_id]
    #     plotRunsAsp <- m3ToBiomIncOnlyPlots(inc=aspIncsRuns)
    #     names(plotRunsAsp) <- paste0("id",names(plotRunsAsp))
    #     dev.new()
    #     clearPlot()
    #     Plot(plotRunsAsp)
    #     incsRuns <- spadesCBMout$growth_increments
    #     # much much smoother...
    #     # what is the range of incs in the runs?
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[1],AU],.N, by=AU]
    #     aspEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[1],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     # make up a new aspEco that uses only the Boreal Cordillera and the Montane
    #     # Cordillera to see if it would make a differeance in the curves
    #     aspEco[,c("4","9") := NULL]
    #     aspEz <- melt(aspEco,id.vars = "AU", measure.vars = c("12","14"), variable.name = "ecozone")
    #     aspEz[,"ecozone" :=NULL]
    #     names(aspEz) <- c("AU","ecozone")
    #     aspEz <- aspEz[1:14,]
    #     aspEz[is.na(aspEz$ecozone),"ecozone"] <- 14
    #     ### JUST ASPENT
    #     aspInc <- NULL
    #     # only process the 173 that have a match in the ecoAu
    #     aspMeta1 <- gcMeta[which(gcMeta$AU %in% aspEz$AU),]
    #     
    #     #14
    #     for(i in 1:length(aspMeta1$AU)){
    #       ## can't run the ecozones like this..in the mean time using the first ecozone in the TSAspuEco
    #       meta <- aspMeta1[i,]
    #       # match the AU call it id with the actual m3
    #       id <- gcRIAm3$AU[which(gcRIAm3$AU == meta$AU)]
    #       age <- gcRIAm3[gcRIAm3$AU==id,"age"]
    #       cumBiom <- as.matrix(convertM3biomRIA(meta = meta,gCvalues = gcRIAm3, 
    #                                             ecozones = aspEz,params3=RIAtable3, params4=RIAtable4, 
    #                                             params5=RIAtable5,params6=RIAtable6))
    #       
    #       cumBiom[which(is.na(cumBiom))] <- 0
    #       # going from tonnes of biomass/ha to tonnes of carbon/ha here
    #       cumBiom <- cumBiom*0.5
    #       inc <- diff(cumBiom)
    #       incs <- cbind(id,age,rep(0,length(age)),rep(0,length(age)),rep(0,length(age)),rbind(rep(0,dim(inc)[2]),inc))
    #       aspInc <- rbind(aspInc,incs)}
    #       
    #     colnames(aspInc) <- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
    #     aspInc <- as.data.table(aspInc)
    #     aspInc[is.na(aspInc)] <- 0
    #     
    #     plotApsNewEco <- m3ToBiomIncOnlyPlots(inc=aspInc)
    #     names(plotApsNewEco) <- paste0("id",names(plotApsNewEco))
    #     dev.new()
    #     clearPlot()
    #     Plot(plotApsNewEco)
    #     ### NO BETTER
    # 
    #     ## check aspen growth curves
    #     aspM3 <- gcRIAm3[AU %in% aspEz$AU,]    
    #     aspCurves <- ggplot(data=aspM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    # ## Aspen decision: NO CHANGE. There are no better curves. No smooth ones, no "less productive" ones####################
    # ## Could use the other ecozones...not much difference though#######################################
    # 
    # # subFir##############################################################################################
    #     # All subFir
    #     subFNames <- paste0("AU",gcMeta1[LDSPP == outSps[2],AU])
    #     which(names(inc173AUs) %in% subFNames) # 17 subFir in total
    #     subFPlots <- as.list(inc173AUs[which(names(inc173AUs) %in% subFNames)])
    #     # visually check all subFir
    #     clearPlot()
    #     Plot(subFPlots)
    #     gcMeta1[LDSPP == outSps[2],]
    #     ## They all seem too productive!!
    # 
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[2],AU],.N, by=AU]
    #     subFEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[2],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    # #DECISION: replace AU 61, AU 61 and AU268 with AU265
    #     
    #     ## check subFir growth curves
    #     subFM3 <- gcRIAm3[AU %in% subFEco$AU,]    
    #     subFCurves <- ggplot(data=subFM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    # ###### subFir decision: replace AU 60, AU 61 and AU268 with AU265#########################
    # 
    # # lodgepole##############################################################################################
    #     # All lodgepole
    #     lPineNames <- paste0("AU",gcMeta1[LDSPP == outSps[3],AU])
    #     which(names(inc173AUs) %in% lPineNames) # 20 lodgepole in total
    #     lPinePlots <- as.list(inc173AUs[which(names(inc173AUs) %in% lPineNames)])
    #     # visually check all lodgepole
    #     clearPlot()
    #     Plot(lPinePlots)
    #     gcMeta1[LDSPP == outSps[3],]
    #     ## They all seem too productive!!
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[3],AU],.N, by=AU]
    #     lPineEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[3],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     
    #     ## check lodgepole growth curves
    #     lPineM3 <- gcRIAm3[AU %in% lPineEco$AU,]    
    #     lPineCurves <- ggplot(data=lPineM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    # ###### lodgepole decisionDECISION: replace AU15 with AU 378, AU1025, 1102, 1191, 1202 with AU602#########################
    # 
    # # poplar##############################################################################################
    #     # All poplar
    #     poplarNames <- paste0("AU",gcMeta1[LDSPP == outSps[4],AU])
    #     which(names(inc173AUs) %in% poplarNames) # 15 poplar in total
    #     poplarPlots <- as.list(inc173AUs[which(names(inc173AUs) %in% poplarNames)])
    #     # visually check all poplar
    #     clearPlot()
    #     Plot(poplarPlots)
    #     gcMeta1[LDSPP == outSps[4],]
    #     ## They all seem too productive!!
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[4],AU],.N, by=AU]
    #     poplarEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[4],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     
    #     ## check poplar growth curves
    #     poplarM3 <- gcRIAm3[AU %in% poplarEco$AU,]    
    #     poplarCurves <- ggplot(data=poplarM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    # ###### poplar DECISION: replace ALL with AU51#########################
    #     
    # # cotton##############################################################################################
    #     # All cotton
    #     cottonNames <- paste0("AU",gcMeta1[LDSPP == outSps[5],AU])
    #     which(names(inc173AUs) %in% cottonNames) # 15 cotton in total
    #     cottonPlots <- as.list(inc173AUs[which(names(inc173AUs) %in% cottonNames)])
    #     # visually check all cotton
    #     clearPlot()
    #     Plot(cottonPlots)
    #     gcMeta1[LDSPP == outSps[5],]
    #     ## They all seem too productive!!
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[5],AU],.N, by=AU]
    #     cottonEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[5],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     
    #     ## check cotton growth curves
    #     cottonM3 <- gcRIAm3[AU %in% cottonEco$AU,]    
    #     cottonCurves <- ggplot(data=cottonM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    #   ###### cotton DECISION: replace AU52, Au254, AU685, AU686 with AU51#######################################
    #     
    # # birch##############################################################################################
    #     # All birch
    #     birchNames <- paste0("AU",gcMeta1[LDSPP == outSps[6],AU])
    #     which(names(inc173AUs) %in% birchNames) # 15 birch in total
    #     birchPlots <- as.list(inc173AUs[which(names(inc173AUs) %in% birchNames)])
    #     # visually check all birch
    #     clearPlot()
    #     Plot(birchPlots)
    #     gcMeta1[LDSPP == outSps[6],]
    #     ## They all seem too productive!!
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[6],AU],.N, by=AU]
    #     birchEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[6],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     
    #     ## check birch growth curves
    #     birchM3 <- gcRIAm3[AU %in% birchEco$AU,]    
    #     birchCurves <- ggplot(data=birchM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    # ###### birch DECISION: AU66, AU67, AU68, AU355, AU 707, AU708, AU1022 with AU274 #######################################
    #     
    # # aLarch##############################################################################################
    #     # All aLarch
    #     aLarchNames <- paste0("AU",gcMeta1[LDSPP == outSps[7],AU])
    #     which(names(inc173AUs) %in% aLarchNames) # 15 aLarch in total
    #     aLarchPlots <- as.list(inc173AUs[which(names(inc173AUs) %in% aLarchNames)])
    #     # visually check all aLarch
    #     clearPlot()
    #     Plot(aLarchPlots)
    #     gcMeta1[LDSPP == outSps[7],]
    #     ## They all seem too productive!!
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[7],AU],.N, by=AU]
    #     aLarchEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[7],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     
    #     ## check aLarch growth curves
    #     aLarchM3 <- gcRIAm3[AU %in% aLarchEco$AU,]    
    #     aLarchCurves <- ggplot(data=aLarchM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    # ###### aLarch DECISION: AU722 and AU1023 with AU72#######################################
    #     
    #     # bSpruce##############################################################################################
    #     # All bSpruce
    #     bSpruceNames <- paste0("AU",gcMeta1[LDSPP == outSps[8],AU])
    #     which(names(inc173AUs) %in% bSpruceNames) # 15 bSpruce in total
    #     bSprucePlots <- as.list(inc173AUs[which(names(inc173AUs) %in% bSpruceNames)])
    #     # visually check all bSpruce
    #     clearPlot()
    #     Plot(bSprucePlots)
    #     gcMeta1[LDSPP == outSps[8],]
    #     ## They all seem too productive!!
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[8],AU],.N, by=AU]
    #     bSpruceEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[8],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     
    #     ## check bSpruce growth curves
    #     bSpruceM3 <- gcRIAm3[AU %in% bSpruceEco$AU,]    
    #     bSpruceCurves <- ggplot(data=bSpruceM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    #     ###### bSpruce DECISION: AU81-82-83 and 391 with AU310 and AU674-737-738 with 772 #######################################
    # 
    # # eSpruce##############################################################################################
    #     # All eSpruce
    #     eSpruceNames <- paste0("AU",gcMeta1[LDSPP == outSps[9],AU])
    #     which(names(inc173AUs) %in% eSpruceNames) # 15 eSpruce in total
    #     eSprucePlots <- as.list(inc173AUs[which(names(inc173AUs) %in% eSpruceNames)])
    #     # visually check all eSpruce
    #     clearPlot()
    #     Plot(eSprucePlots)
    #     gcMeta1[LDSPP == outSps[9],]
    #     ## They all seem too productive!!
    #     
    #     # would curves be better with another ecozone?
    #     ecoAu[AU %in% gcMeta1[LDSPP == outSps[9],AU],.N, by=AU]
    #     eSpruceEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[9],AU]], AU~ecozone)
    #     ecoCheck1
    #     # EcoBndr            ecozone
    #     # 1:      12  Boreal Cordillera
    #     # 2:       9      Boreal Plains
    #     # 3:      14 Montane Cordillera
    #     # 4:      13   Pacific Maritime
    #     # 5:       4       Taiga Plains
    #     
    #     
    #     ## check eSpruce growth curves
    #     eSpruceM3 <- gcRIAm3[AU %in% eSpruceEco$AU,]    
    #     eSpruceCurves <- ggplot(data=eSpruceM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #       geom_line()
    # ############## DECSION eSpruce: replace AU85-312-774 with AU393###########################
    # 
    # #WRitting a function for each species
    # # # this is a visual check but buy species
    #     checkBiomBySps <- function(treeSps=outSps[1],meta=gcMeta1,m3Ha=gcRIAm3,incPlots=inc173AUs,ecoz=ecoAu){
    #       # get a name to match with the plot names
    #       # this list of names has to match the names of the plots of the increments for all the translated growth curves
    #       pixNumToName <- paste0("AU",meta[LDSPP == treeSps,AU])
    #       # all the plots for that species
    #       plotsListForSps <- incPlots[which(names(incPlots) %in% pixNumToName)]
    #       return(plotsListForSps)
    #     }
    # 
    # ################# White Spruce #####################
    # wSprucePlots <- checkBiomBySps(treeSps=outSps[10])
    # clearPlot()
    # Plot(wSprucePlots)
    # gcMeta1[LDSPP == outSps[10],]
    # wSpruceM3 <- gcRIAm3[AU %in% gcMeta1[LDSPP == outSps[10],AU],]
    # wSpruceCurves <- ggplot(data=wSpruceM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #   geom_line()
    # ####### Decision: replace AU 89-90-91-317-318-775-777 with AU745#######################
    #     
    # ## Balsam Fir#########################################################
    # bFirPlots <- checkBiomBySps(treeSps=outSps[11])
    # clearPlot()
    # Plot(bFirPlots)
    # gcMeta1[LDSPP == outSps[11],]
    # bFirM3 <- gcRIAm3[AU %in% gcMeta1[LDSPP == outSps[11],AU],]
    # bFirCurves <- ggplot(data=bFirM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #   geom_line()
    # ##only two of these. check ecozones
    # ecoAu[AU %in% gcMeta1[LDSPP == outSps[11],AU],.N, by=AU]
    # bFirEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[11],AU]], AU~ecozone)
    # ################ ONLY one eco - decsion: no change!##################
    # 
    # ############## Amabilis Fir############################
    # amFirPlots <- checkBiomBySps(treeSps=outSps[12])
    # clearPlot()
    # Plot(amFirPlots)
    # gcMeta1[LDSPP == outSps[12],]
    # amFirM3 <- gcRIAm3[AU %in% gcMeta1[LDSPP == outSps[12],AU],]
    # amFirCurves <- ggplot(data=amFirM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #   geom_line()
    # #######Decsion: no change there is only one############
    #     
    # ############## Recedar ############################
    # redCPlots <- checkBiomBySps(treeSps=outSps[13])
    # clearPlot()
    # Plot(redCPlots)
    # gcMeta1[LDSPP == outSps[13],]
    # redCM3 <- gcRIAm3[AU %in% gcMeta1[LDSPP == outSps[13],AU],]
    # redCCurves <- ggplot(data=redCM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #   geom_line()
    # #######Decsion: no change, only shapes are funny, values are ok############
    # 
    # ############## whitebark ############################
    # wbPPlots <- checkBiomBySps(treeSps=outSps[14])
    # clearPlot()
    # Plot(wbPPlots)
    # gcMeta1[LDSPP == outSps[14],]
    # wbPM3 <- gcRIAm3[AU %in% gcMeta1[LDSPP == outSps[14],AU],]
    # wbPCurves <- ggplot(data=wbPM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #   geom_line()
    # # both curves bad - any other ecozones?
    # ecoAu[AU %in% gcMeta1[LDSPP == outSps[14],AU],.N, by=AU]
    # wbPEco <- dcast(ecoAu[AU %in% gcMeta1[LDSPP == outSps[14],AU]], AU~ecozone)
    # #######Decsion: all (2) bad, using alpine Larch replace AU296-727 with AU72 ############
    # 
    # ############## spruce ############################
    # sprucePlots <- checkBiomBySps(treeSps=outSps[15])
    # clearPlot()
    # Plot(sprucePlots)
    # gcMeta1[LDSPP == outSps[15],]
    # spruceM3 <- gcRIAm3[AU %in% gcMeta1[LDSPP == outSps[15],AU],]
    # spruceCurves <- ggplot(data=spruceM3, aes(x=age,y=value,group=AU, colour=AU)) +
    #   geom_line()
    # ########Decision: replace AU17-21-78-79-80-306-307-769-770-771-1008-1026-1085-1103-1161-1192-1203 with AU610 ################
    # 
    # 
    # ### HARD CODED FIXES FOR THE current growth curves
    # 
    # #################### HARD CODED FIXES TO THE CURVES OUT OF THE BOUDEWYN PARAMS THAT DON"T WORK#########
    #   # AU 60, AU 61 and AU268 with AU265
    sfir <- c(60,61,268)
    increments[id %in% sfir,3:8] <- rbind(increments[id==265,3:8],increments[id==265,3:8],increments[id==265,3:8])
      # AU15 with AU 378, AU1025, 1102, 1191, 1202 with AU602
    increments[id==15,3:8] <- increments[id==378,3:8]
    lpol <- c(1025, 1102, 1191, 1202)
    increments[id %in% lpol,3:8] <- rbind(increments[id==602,3:8],increments[id==602,3:8],increments[id==602,3:8],increments[id==602,3:8])
      # poplar DECISION: replace ALL with AU51
    pop <- gcMeta1[LDSPP == outSps[4],AU]
    increments[id %in% pop,3:8] <- rbind(increments[id==51,3:8],increments[id==51,3:8],increments[id==51,3:8],increments[id==51,3:8],
                                         increments[id==51,3:8],increments[id==51,3:8],increments[id==51,3:8],
                                         increments[id==51,3:8],increments[id==51,3:8],increments[id==51,3:8],
                                         increments[id==51,3:8],increments[id==51,3:8],increments[id==51,3:8],
                                         increments[id==51,3:8],increments[id==51,3:8])
      # replace AU52, Au254, AU685, AU686 with AU51
    cott <- c(52,254,685,686)
    increments[id %in% cott,3:8] <- rbind(increments[id==51,3:8],increments[id==51,3:8],increments[id==51,3:8],increments[id==51,3:8])
      # AU66, AU67, AU68, AU355, AU 707, AU708, AU1022 with AU274
    birc <- c(66,67,68,355,707,708,1022)
    increments[id %in% birc,3:8] <- rbind(increments[id==274,3:8],increments[id==274,3:8],increments[id==274,3:8],increments[id==274,3:8],
                                         increments[id==274,3:8],increments[id==274,3:8],increments[id==274,3:8])
      # AU722 and AU1023 with AU72
    alar <- c(722,1023)
    increments[id %in% alar,3:8] <- rbind(increments[id==72,3:8],increments[id==72,3:8])
      # AU81-82-83 and 391 with AU310 and AU674-737-738 with 772
    bsp1 <- c(81,82,83)
    increments[id %in% bsp1,3:8] <- rbind(increments[id==391,3:8],increments[id==391,3:8],increments[id==391,3:8])
    bsp2 <- c(674,737,738)
    increments[id %in% bsp2,3:8] <- rbind(increments[id==772,3:8],increments[id==772,3:8],increments[id==772,3:8])
      # AU85-312-774 with AU393
    esp <- c(85,312,774)
    increments[id %in% esp,3:8] <- rbind(increments[id==393,3:8],increments[id==393,3:8],increments[id==393,3:8])
      # AU 89-90-91-317-318-775-777 with AU745
    wsp <- c(89,90,91,317,318,775,777)
    increments[id %in% wsp,3:8] <- rbind(increments[id==745,3:8],increments[id==745,3:8],increments[id==745,3:8],increments[id==745,3:8],
                                          increments[id==745,3:8],increments[id==745,3:8],increments[id==745,3:8])
      # AU296-727 with AU72
    wbar <- c(296,727)
    increments[id %in% wbar,3:8] <- rbind(increments[id==72,3:8],increments[id==72,3:8])
      # AU17-21-78-79-80-306-307-769-770-771-1008-1026-1085-1103-1161-1192-1203 with AU610
    spr <- c(17,21,78,79,80,306,307,769,770,771,1008,1026,1085,1103,1161,1192,1203)
    increments[id %in% spr,3:8] <- rbind(increments[id==610,3:8],increments[id==610,3:8],increments[id==610,3:8],increments[id==610,3:8],
                                         increments[id==610,3:8],increments[id==610,3:8],increments[id==610,3:8],
                                         increments[id==610,3:8],increments[id==610,3:8],increments[id==610,3:8],
                                         increments[id==610,3:8],increments[id==610,3:8],increments[id==610,3:8],
                                         increments[id==610,3:8],increments[id==610,3:8],increments[id==610,3:8],increments[id==610,3:8])

    ## NEGATIVES PRIOR TO 80 become 0
    #gc[value < 0 & age<80, value := 0]
    increments[age<80 & swmerch < 0, swmerch := 0]
    increments[age<80 & swfol < 0, swfol := 0]
    increments[age<80 & swother < 0, swother := 0]
    increments[age<80 & hwmerch < 0, hwmerch := 0]
    increments[age<80 & hwfol < 0, hwfol := 0]
    increments[age<80 & hwother < 0, hwother := 0]

    #post correction increments check plots
    inc173AUsFixed <- m3ToBiomIncOnlyPlots(inc=increments)
    names(inc173AUsFixed) <- paste0("AU", names(inc173AUsFixed))
    clearPlot()
    Plot(inc173AUsFixed[1:28])
    dev.new()
    Plot(inc173AUsFixed[29:56])
    dev.new()
    Plot(inc173AUsFixed[57:88])
    dev.new()
    Plot(inc173AUsFixed[89:100])
    dev.new()
    Plot(inc173AUsFixed[101:115])
    clearPlot()
    Plot(inc173AUsFixed[116:148])
    clearPlot()
    Plot(inc173AUsFixed[149:173])

    ## ADDING THE SALI BACK IN WITH GROWTH INFO FROM AU274
    inc274a <- increments[id==274,]
    inc274b <- increments[id==274,]
    inc274a$id <- 320
    inc274b$id <- 479
    incSali <- rbind(inc274a,inc274b)
    increment <- rbind(increments,incSali)
    write.csv(increment,"data/RIA2019/increments.csv",row.names = FALSE)
#####################################################
increments <- fread("data/RIA2019/increments.csv")


            
## FUNCTIONS FOR PLOTTING #################
# this saves them all to jpegs.
# lapply(names(inc173AUs), FUN = function(x) {
# 
#   thePlot <- inc173AUs[[x]]
#   ggsave(plot = thePlot, filename = paste0("data/RIA2019/", "plot",x, ".jpeg"), device = "jpeg")
# })

# just looking at the increments:
m3ToBiomIncOnlyPlots <- function(inc=increments){
  gInc <- as.data.table(inc)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim,]
  gc <- melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:8)
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop){
    ggplot(data=gc[id == idLoop], 
           aes(x=age,y=value,group=variable,colour=variable)) + geom_line() + theme(legend.position = "none")
  })
  
  return(plots)
}


## FUNCTIONS to process growth curves into growth increments--------------------------------------------
# eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
# the gross merchantable volume/ha. Parameters a and b are in table3
b_m <- function(paramTable1, vol){
  b_m <- unique(paramTable1$a) * vol ^ unique(paramTable1$b)
  return(b_m)
}
# eq2 is for non-merch sized trees.
nmfac <- function(table4,eq1){
  nmFac <- unique(table4$k) + (unique(table4$a) * eq1 ^ unique(table4$b))
  b_nm <- nmFac * eq1
  b_n <- b_nm - eq1
  return(cbind(b_n,b_nm))
}
# eq3 is for the saplings and it needs b_nm from the previous eq2
sapfac <- function(table5, eq2){
  sapFac <- table5$k + (table5$a * eq2[,2] ^ table5$b)
  b_snm <- sapFac * eq2[,2]
  b_s <- b_snm - eq2[,2]
  return(b_s)
}
# calculate the 4 proportions that should be returned
# will eventually add species, ecozone
# vol = gross merchantable volume per ha
# lvol = natural logarithm of (vol+5)
biomProp <- function(table6,vol){
  lvol <- log(vol+5)
  a <- c(7:9)
  b <- c(10:12)
  c <- c(13:15)
  pstem <- 1 / ( 1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
                   exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
                   exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  
  pbark <- exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  
  pbranches <- exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  
  pfol <- exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  propVect <- cbind(pstem,pbark,pbranches,pfol)   
  return(propVect)
}
convertM3biomRIA <- function(meta,gCvalues,ecozones,params3, params4, params5,params6){
  oneCurve <- gCvalues[AU==meta$AU,]
  spec <- meta$canfi_species
  ## need a map to match Au to ecozone (could be via TSA)
  ez <- ecozones[AU==meta$AU,ecozone]
  gen <- meta$genus
  
  ## CAREFULL selecting the first ecozone right now. 
  ## Do I have to calculate biomass as many times are there are ecozones?
  ## I am choosing not to.##############
  params3 <- params3[canfi_species== spec & ecozone == ez[1],] 
  params4 <- params4[canfi_species== spec & ecozone == ez[1],]
  # table 5 is different than the others
  #browser()  
  params5 <- params5[genus == gen & ecozone == ez[1],]
  params6 <- params6[species == spec & eco == ez[1],]

  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  eq1 <- b_m(params3, oneCurve$value)
  # eq2 returns a two colum matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm, the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merche-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  eq2 <- nmfac(params4, eq1 = eq1)
  #some NAs where it was 0s. Leave these in place
  # eq3 is for biomass of the saplings, the smallest of the nonmerch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  eq3 <- sapfac(params5, eq2 = eq2)
  #eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees
  merch <- eq1+eq2[,1] + eq3
  merch[which(is.nan(merch))] <- NA
  # calculate the 4 proportions that should be returned: proportion for
  # stemwood, prop for bark, prop for branches, and prop for foliage.
  pVect <- biomProp(table6 = params6, vol = oneCurve$value)  
  # translating this into biomass values for the carbon pools
  totMerch <- merch/pVect[,1]
  bark <- totMerch*pVect[,2]
  branch <- totMerch*pVect[,3]
  fol <- totMerch*pVect[,4]
  other <- branch+bark
  biomCumulative <- as.matrix(cbind(totMerch,fol,other))
  return(biomCumulative)
  
}
# convertM3biom <- function(meta,gCvalues,spsMatch,ecozones,params3, params4, params5,params6){
#   oneCurve <- gCvalues[GrowthCurveComponentID==meta$growth_curve_component_id,]
#   spec <- spsMatch[speciesName==meta$species,]$canfi_species
#   ez <- ecozones[SpatialUnitID==meta$spatial_unit_id,]$EcoBoundaryID
#   gen <- spsMatch[speciesName==meta$species,]$genus
#   
#   params3 <- params3[canfi_species== spec & ecozone == ez,] 
#   params4 <- params4[canfi_species== spec & ecozone == ez,]
#   # table 5 is different than the others
#   params5 <- params5[genus == gen & ecozone == ez,]
#   params6 <- params6[species == spec & eco == ez,]
#   
#   # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
#   # the gross merchantable volume/ha. Parameters a and b are in table3
#   eq1 <- b_m(params3, oneCurve$MerchVolume)
#   # eq2 returns a two colum matrix giving the biomass of the non-merch sized
#   # trees (b_n) and b_nm, the sum of the total stem wood biomass of merch size
#   # live plus, the stem wood live of non merche-sized trees, given the total
#   # stem wood biomass per ha of live merch size trees (in tonnes/ha)
#   eq2 <- nmfac(params4, eq1 = eq1)
#   #some NAs where it was 0s. Leave these in place
#   # eq3 is for biomass of the saplings, the smallest of the nonmerch trees. The
#   # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
#   # live sapling-sized trees in tonnes/ha
#   eq3 <- sapfac(params5, eq2 = eq2)
#   #eq3[which(is.na(eq3))] <- 0
#   # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees
#   merch <- eq1+eq2[,1] + eq3
#   merch[which(is.nan(merch))] <- NA
#   # calculate the 4 proportions that should be returned: proportion for
#   # stemwood, prop for bark, prop for branches, and prop for foliage.
#   pVect <- biomProp(table6 = params6, vol = oneCurve$MerchVolume)  
#   # translating this into biomass values for the carbon pools
#   totMerch <- merch/pVect[,1]
#   bark <- totMerch*pVect[,2]
#   branch <- totMerch*pVect[,3]
#   fol <- totMerch*pVect[,4]
#   other <- branch+bark
#   biomCumulative <- as.matrix(cbind(totMerch,fol,other))
#   return(biomCumulative)
#   
# }
# # END process growth curve functions-------------------------------------------------------------  

## fixing things########

## PROBLEM: the gcid issue - Error in Spinup(pools = sim$pools, opMatrix = opMatrix, constantProcesses = sim$processes,  : 
#specified gcid does not exist
# my idea to look into this: my list of gcid in gcMeta is missing some of the gcids in level3DT
# investigate
# load level3DT
level3DT <- fread("C:\\Celine\\github\\spadesCBM\\data\\RIA2019\\level3DT.csv")

gcMeta <- fread("C:\\Celine\\github\\spadesCBM\\data\\RIA2019\\gcMeta1.csv")
DTgcid <- sort(unique(level3DT$growth_curve_id))
gcMetaGcids <- sort(unique(gcMeta$AU))
DTgcid[!(DTgcid %in% gcMetaGcids)]
gcMetaGcids[gcMetaGcids %in% DTgcid]

DTgcid[!(DTgcid %in% gcMetaGcids)]
#[1] 320 479
level3DT[growth_curve_id %in% c(320,479),]
# does the original data from Greg have these two?
gcBC <- fread("data/RIA2019/yld.csv")
gcBC[AU %in%  c(320,479),]
# Yieldscode THLB  AU  LDSPP SPCode Wdks X0 X10 X20 X30 X40  X50  X60  X70
# 1:         *Y    ? 320 Willow  WIVOL    1  0   0   0 0.1 0.6  5.2 15.8 32.7
# 2:         *Y    ? 479 Willow  WIVOL    1  0   0   0 0.0 9.6 37.3 67.5 92.7
# X80   X90  X100  X110  X120 X130  X140  X150  X160  X170  X180  X190  X200
# 1:  53.8  76.7  99.3 120.5 139.6  155 166.7 174.8 178.7 178.9 177.7 176.0 174.3
# 2: 112.4 129.7 141.6 150.3 157.0  163 166.5 166.9 165.8 165.5 164.9 164.4 164.1
# X210  X220 X230  X240  X250  X260  X270  X280  X290  X300  X310  X320  X330
# 1: 172.7 170.7  169 167.6 166.5 165.6 164.9 164.6 164.5 164.4 164.5 164.6 164.6
# 2: 163.8 162.8  162 161.6 161.3 161.1 160.8 160.6 160.4 160.3 160.2 160.2 160.2
# X340  X350
# 1: 164.6 164.6
# 2: 160.2 160.2
gcMetaComplete <- fread("C:\\Celine\\github\\spadesCBM\\data\\RIA2019\\gcMeta.csv")

gcMetaComplete[AU %in%  c(320,479),]

gcRIAm3 <- as.data.table(read.csv("C:/Celine/github/spadesCBM/data/RIA2019/gcRIAm3.csv"))
unique(gcRIAm3[gcRIAm3$AU %in% unique(gcBC[LDSPP=="Paper birch",AU]),AU])
## DECISION: add AU 320 and 479 with the increments from AU274 (replacing the SALI LDSPP with Paper Birch growth)
gcMetaComplete[AU==274,]
###### birch DECISION: AU66, AU67, AU68, AU355, AU 707, AU708, AU1022 with AU274 #######################################
### THIS SEEMS TO BE FIXED - gets to spinup now

### SPINUP does not complete: 
#this is similar to the problem with the one curve and one age I had with the SK
#project. This is what I think is happening: because of the error in the c++
#code where disturbances are not happening the way they should (carbon from this
#pool to that pool), the stand can't ## "grow" and fill the soil pools (which is
#the point of the spinup)
# To fix this or try to identify if this is the issue I will: 
# 1- try  to give the spinup a different age vector (older) since a lot of the
# cureves don't have early years.
# 2- reduce the lenght of the level3DT to run this a few pixel groups at a time
# to see which curves "fail".
## CHANGES: I noticed that some ages were past 350 - and our curves do not pass
## 350, so I added a live to reset the >350 ages to 350 for the spinup
level3DT <- fread("C:\\Celine\\github\\spadesCBM\\data\\RIA2019\\level3DT.csv")
# 1:4950 WORKED
##Problem between 4951:4975
gcidCheck1 <- unique(level3DT[4951:4975,growth_curve_id])
gcMetaRuns <- fread("C:\\Celine\\github\\spadesCBM\\data\\RIA2019\\gcMetaRuns.csv")

gcMetaRuns[AU %in% gcidCheck1,]
increments <- fread("data/RIA2019/increments.csv")
## found an error in the forest_type_id that was manually assigned, picea had a
## 3 and should have had a 1, betual had a 1 and should have had a 3
## that did not fix the problem.
# first time use of AU 79, so check if that is the problem?
