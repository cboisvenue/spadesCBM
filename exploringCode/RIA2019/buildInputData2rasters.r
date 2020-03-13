# figuring out the raster stack sent by Greg Paradis March 2nd, 2020
# questions:
# does my AU-fixed list correspond to the AU list in the raster?
# can I re-sample the age and AU to match raserToMatch



######HERE ## need to create my level3DT
rasters <- raster::stack("G:/RES_Work/Work/SpaDES/RIA/data/fromGreg/ria_landscapestack_init.tif") 
names(rasters) <- c("TSA","THLB","AU","blockId","age")

rasters$TSA
rasters$THLB
rasters$AU
rasters$blockId # this one has huge values and won't Plot()...not sure if I need this
rasters$age



masterRaster <- raster("G:/RES_Work/Work/SpaDES/RIA/data/RIA5tsaRTM/RIA5tsaRTM.tif")
masterValues <- values(masterRaster)
## Note that the raster to match is here G:\RES_Work\Work\SpaDES\RIA\data\RIA5tsaRTM 
# the study area (shape file) is G:\RES_Work\Work\SpaDES\RIA\data\RIA_fiveTSA and both are here
#Ian's stuff
age <- postProcess(x = rasters$age, rasterToMatch = masterRaster)#, filename2 = "pixelAges.tif"
AU <- postProcess(x = rasters$AU, rasterToMatch = masterRaster)#, filename2 = "pixelAges.tif"

AUcompare <- values(AU)

tAUraster <- table(AUcompare)
AUrasterNumeric <- as.numeric(names(tAUraster))
gcMeta <- fread("data/RIA2019/gcMeta.csv")
length(unique(gcMeta$AU))
metaAU <- unique(gcMeta$AU)
length(metaAU) #204
#could not process all the AUs because they did not all have ecozones
ecoAu <- fread("data/RIA2019/CelinesTable.csv")
names(ecoAu) <- c("AU", "ecozone")
gcMeta1 <- gcMeta[which(gcMeta$AU %in% ecoAu$AU),]
meta1AU <- unique(gcMeta1$AU)
length(meta1AU) #175

length(AUrasterNumeric)
length(which(meta1AU %in% AUrasterNumeric))
length(which( AUrasterNumeric %in% meta1AU))
# they seem to all be in there
