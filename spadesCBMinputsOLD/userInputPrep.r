# making one raster for productivity with the gcID for the SK runs
# this would be similar to what users would have to do to prep their data


ldSpsRaster <- raster(file.path(getwd(),"/spadesCBMinputs/data/ldSp_TestArea.tif"))

prodRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/prod_TestArea.tif"))
  oneProdlevel <- c(1,2,4,6)## danger hard coded - these are the species with one level##
  Prod2 <- which(ldSpsRaster[] %in% oneProdlevel)## ADD sim$ here
  prodRaster[Prod2] <- 1## ADD sim$ here
  no3 <- which(prodRaster[]==3)
  prodRaster[no3] <- 2

masterRaster <- raster(file.path(getwd(),"/spadesCBMinputs/data/ldSp_TestArea.tif"))
masterRaster[masterRaster == 0] <- NA

canadaSpu <- shapefile(file.path(getwd(),"/spadesCBMinputs/data/spUnit_Locator.shp"))
spuShp <- postProcess(canadaSpu, rasterToMatch = masterRaster, targetCRS = crs(masterRaster),
                        useCache = FALSE, filename2 = NULL)
spuRaster <- fasterize::fasterize(sf::st_as_sf(spuShp), raster = masterRaster, field = "spu_id")
  
# read in the # growth and yield info
gcID <- fread("data/spadesGCurvesSK.csv")
gcID <- unique(gcID[,.(rasterSps,species,growth_curve_component_id,spatial_unit_id,forest_type_id,growth_curve_id,Productivity)])
setkey(gcID,rasterSps,Productivity,spatial_unit_id)

# end add the gcID: each pixel has a growth curve now---
allPixDT <- data.table(pixelIndex = 1:ncell(ldSpsRaster),rasterSps = ldSpsRaster[],
                       Productivity = prodRaster[],
                       spatial_unit_id = spuRaster[])

# create the pixel group---------------
allPixDTgc <- allPixDT[gcID, on = c("rasterSps","Productivity","spatial_unit_id"),nomatch = 0]

gcIndex <- raster(masterRaster)
gcIndex[allPixDTgc$pixelIndex] <- allPixDTgc$growth_curve_id
clearPlot()
Plot(gcIndex)
writeRaster(x = gcIndex
  ,file.path(getwd(),"/spadesCBMinputs/data/gcIndex.tif"))

## Matching specified for Boudewyn translation from m3 to Biomass
spsMatch <- fread("data/spsMatchNameRasterGfileBiomParams.csv")
gcPrep <- gcID[spsMatch,on="rasterSps"]
gcMetaEg <- gcPrep[,.(growth_curve_id,growth_curve_component_id,species,
                      canfi_species,genus,forest_type_id)]
write.csv(gcMetaEg,file = file.path(getwd(),"spadesCBMinputs/data/gcMetaEg.csv"),
          row.names = FALSE)


# ecozone raster

ecozones <- prepInputs(# targetFile = asPath(ecodistrictFilename),
  #archive = asPath("ecodistrict_shp.zip"),
  url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
  #alsoExtract = ecodistrictAE,
  destinationPath = file.path(getwd(),"spadesCBMinputs/data/"),
  rasterToMatch = masterRaster,
  overwrite = TRUE,
  fun = "raster::shapefile",
  filename2 = TRUE)#,
#userTags = cacheTags)
ecozones <- cropInputs(ecozones, rasterToMatch = masterRaster)
ecozonesRas <- fasterize::fasterize(sf::st_as_sf(ecozones), raster = masterRaster,
                                    field = "ECOZONE")
writeRaster(x=ecozonesRas,
            file.path(getwd(),"/spadesCBMinputs/data/ecoRaster.tif"))
## TO DO NEED TO ADD the canfi_species and genus to match the Boudewyn et al species
# checking the LandR::spsEquivalences_CA
data("sppEquivalencies_CA", package = "LandR", envir = environment())
sppEquiv <- as.data.table(sppEquivalencies_CA)
