# figuring out the raster stack sent by Greg Paradis March 2nd, 2020
# questions:
# does my AU-fixed list correspond to the AU list in the raster?
# can I re-sample the age and AU to match raserToMatch



######HERE ## need to create my level3DT
# masterRaster <- raster("G:/RES_Work/Work/SpaDES/RIA/data/RIA5tsaRTM/RIA5tsaRTM.tif")
# masterValues <- values(masterRaster)

masterRaster <- Cache(prepInputs,url = "https://drive.google.com/open?id=1dl1iS3eXWcMcc7ASI8eIrKyn-x3OM_Cx")

rasters <- Cache(prepInputs,url = "https://drive.google.com/file/d/1DN31xcXh97u6v8NaVcy0O3vzKpLpld69/view?usp=sharing",
                      fun = "raster::stack", rasterToMatch = masterRaster, useGDAL = FALSE) # this was Eliot's
#rasters <- raster::stack("G:/RES_Work/Work/SpaDES/RIA/data/fromGreg/ria_landscapestack_init.tif") 
names(rasters) <- c("TSA","THLB","AU","blockId","age")

rasters$TSA
rasters$THLB
rasters$AU
rasters$blockId # this one has huge values and won't Plot()...not sure if I need this
rasters$age

age <- postProcess(x = rasters$age, rasterToMatch = masterRaster, filename2 = "data/RIA2019/pixelAges.tif")#, filename2 = "pixelAges.tif"
AU <- postProcess(x = rasters$AU, rasterToMatch = masterRaster, filename2 = "data/RIA2019/pixelGcid.tif")#, filename2 = "pixelAges.tif"
TSA <- postProcess(x = rasters$TSA, rasterToMatch = masterRaster, filename2 = "data/RIA2019/pixelTSA.tif")


## Note that the raster to match is here G:\RES_Work\Work\SpaDES\RIA\data\RIA5tsaRTM 
# the study area (shape file) is G:\RES_Work\Work\SpaDES\RIA\data\RIA_fiveTSA and both are here
#Ian's stuff

# Need TSAid also to match with spu
writeRaster(TSA,"data/RIA2019/pixelTSA.tif")

# figure out the numbering of the pixels
ageCompare <- values(age)
tAgeRaster <- table(ageCompare,useNA="ifany")
AUcompare <- values(AU)
tAUraster <- table(AUcompare,useNA="ifany")
tMaster <- table(masterValues,useNA="ifany")
TSAcompare <- table(TSA[],useNA = "ifany")


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


# trying to read the TSAsSpuRIA.png
# library(png)
# library(raster)
# spuImage <- readPNG("data/RIA2019/RIAspu.png", native = TRUE)
# spuR <- rasterImage(spuImage)
# TSAspuRas <- postProcess(x=raster(TSAspuImage), rasterToMatch = masterRaster)

ecozones <- prepInputs(# targetFile = asPath(ecodistrictFilename),
                         #archive = asPath("ecodistrict_shp.zip"),
                         url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                         #alsoExtract = ecodistrictAE,
                         destinationPath = dataPath,
                         rasterToMatch = masterRaster,
                         overwrite = TRUE,
                         fun = "raster::shapefile",
                         filename2 = TRUE)#,
                         #userTags = cacheTags)
+ecozones <- cropInputs(ecozones, rasterToMatch = masterRaster)
ecozonesRas <- fasterize::fasterize(sf::st_as_sf(ecozones), raster = masterRaster,
                                    field = "ECOZONE")
writeRaster(ecozonesRas,"data/RIA2019/ecozones.tif")
canadaSpu <- shapefile("data/spUnit_Locator.shp")
spuShp <- postProcess(canadaSpu, rasterToMatch = masterRaster, targetCRS = crs(masterRaster),
                      useCache = FALSE, filename2 = NULL)
spuRas <- fasterize::fasterize(sf::st_as_sf(spuShp), raster = masterRaster, field = "spu_id")
writeRaster(spuRas,"data/RIA2019/spu.tif")

# fires for now
firesComposite <- prepInputs(# targetFile = asPath(ecodistrictFilename),
  #archive = asPath("ecodistrict_shp.zip"),
  url = "https://drive.google.com/open?id=10aQUa_QCS6UMvoeFV-WnBSFk8YCqOBsT",
  #alsoExtract = ecodistrictAE,
  destinationPath = dataPath,
  rasterToMatch = masterRaster,
  overwrite = TRUE,
  fun = "raster::raster",
  filename2 = TRUE)#,
#userTags = cacheTags)
writeRaster(firesComposite,"data/RIA2019/firesComposite.tif")
