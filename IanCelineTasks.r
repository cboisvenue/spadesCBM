##----------------------------------------------- 
#List of tasks or discussion
#points between CBoisvenue and IEddy for the spadesCBM family development
#
#CBoisvenue 
#------------------------------------------------

# Oct.17,2018
# Creating maps from outputs
#We have a table "level3DT", that has a unique pixel groups We need to be able
#to make maps with this output maps per year and per pools or combinations of
#pools 
#Status: There was discussion of making sure "standindex" column in the
#output1stand.csv (in outputs folder) matched the pixel groups. And apparently
#Ian wrote code about this but Celine can't find it

####Task1####
require(raster)
require(magrittr)
require(data.table)
##Need to recreate level3DT

#1. Get test area GIS data
#1 i age
ageMap <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/age_TestArea.tif"))
ages <- getValues(ageMap)
#1 ii species
ldSpsMap <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/ldSp_TestArea.tif"))
species <- getValues(ldSpsMap)
#1 iii. productivity
prodMap <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/prod_TestArea.tif"))
prod <- getValues(prodMap)

#iv . Spatial units - Must run spadesCBM to get spuNames 
#This exists for the small study area - we can always generate the raster for other areas as in Task3
spuMap <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
spatial_unit_id <- as.integer(getValues(spuMap)) #28 27

#2. Make level 2DT
level2DT <- as.data.table(cbind("LeadingSp" = species, "Productivity" = prod, spatial_unit_id, "Age" = ages))
level2DT <- level2DT[LeadingSp != 0] #Remove NAs

#3. Add growth curves
#3 i get growth curve table
gcID <- read.csv(file.path(getwd(),"data/forIan/SK_data/gcID_ref.csv"))
gcID <- as.data.table(gcID[,-1])%>%
  .[,.("LeadingSp" = rasterSps,"Productivity" = RasterValue, growth_curve_component_id, spatial_unit_id)]
setkey(gcID, growth_curve_component_id, LeadingSp, Productivity, spatial_unit_id)


#3 ii filter the spatial data by unique values
level3DT <- unique(level2DT) # remove row order for unique operation
level3DT <- level3DT[level3DT$LeadingSp>0,] # removes values that should be NA
setkey(level3DT, LeadingSp, Productivity,spatial_unit_id)

#4 Make level 3DT
level3DT <- level3DT[gcID, on = c("LeadingSp", "Productivity", "spatial_unit_id"), nomatch = 0] #dim: 759 5   

#4 ii add PixelGroupID: 
level3DT$PixelGroupID <- as.numeric(factor(paste(level3DT$spatial_unit_id,
                                                 level3DT$growth_curve_component_id,
                                                 level3DT$Age)))

#5 Prepare raster that will have location of pixelGroupIds
#5 i make spatial data table 
level2DT$rowOrder <- 1:nrow(level2DT) #failsafe to make sure rows aren't sorted

setkey(level2DT, LeadingSp, Productivity, spatial_unit_id)

setkey(gcID, NULL) #apparently you have to unkey before a join
setkey(level2DT, NULL)

spatialDT <- gcID[level2DT, on = c("LeadingSp", "Productivity", "spatial_unit_id")]
spatialDT <- spatialDT[order(rowOrder)]

#5 ii add pixelGroupIds to spatialDT
spatialDT$PixelGroupID <- as.numeric(as.factor(paste(spatialDT$spatial_unit_id,
                                                     spatialDT$growth_curve_component_id,
                                                     spatialDT$Age)))
#####TEST####

#5 iii Generate 'master raster' that has location of NAs.
# use species map because it has NA where there are no trees, unlike productivity
masterRaster <- ldSpsMap
masterRaster[species == 0] <- NA
masterRaster[!species == 0] <- spatialDT$PixelGroupID
plot(masterRaster)

#6 Add output of SpadesCBM sim run - in this example I use BelowGroundSlowSoil
#6 i get table from sim run
poolsDT <- as.data.table(spadesCBMout$cbmPools)

#6 ii subset by age = max age, because sim "grew" stands up to age
poolsDT <- poolsDT[, .("PixelGroupID" = standindex, BelowGroundSlowSoil, "new.age" = age)] 
poolsDT <- poolsDT[, max.age := max(new.age), by = .(PixelGroupID)][new.age == max.age, .(BelowGroundSlowSoil, PixelGroupID, new.age)]
setkey(poolsDT, PixelGroupID)
setkey(spatialDT, PixelGroupID)

#6 iii Join these two tables
masterTable <- poolsDT[spatialDT, on = c("PixelGroupID")]
head(masterTable)
masterTable[order(rowOrder)]

BGSS_Map <- masterRaster
plot(BGSS_Map)

BGSS_Map[!is.na(BGSS_Map)] <- masterTable$BelowGroundSlowSoil

plot(BGSS_Map)



# Oct.17,2018
# Disturbances
# Status: a bogus table of disturbances where the same disturbance type was applied to
# each pixel group worked.
# Next: we need to try one of the rasters from Wulder and White used in Boisvenue et al 2016
# Ian, where are these? are they cut-up to size for the test area?
# Next: modifying the level3DT after each disturbance...
# do we follow what LandR-biomass does?
# Discussion needed



# Less pressing
# spu locator map
# create a cbm-default spu map (raster) and give the user the change to manually
# (on a map) or with coordinates to determine which spu they are in

#1 Create the spatial unit shapefile for all of Canada
#1 i Source the data with correct spatial unit IDs
inputDir <- file.path(getwd(),"data/12_Spades_run") 
dbPath = file.path(inputDir,"cbm_defaults","cbm_defaults.db")
sqlite.driver <- dbDriver("SQLite")
cbmDefaults <- dbConnect(sqlite.driver, dbname = dbPath)
alltables = dbListTables(cbmDefaults)
cbmTables <- list()

for(i in 1:length(alltables)){
  cbmTables[[i]] <- dbReadTable(cbmDefaults,alltables[i])
}

names(cbmTables) <- alltables
adminNames <- cbmTables$admin_boundary
names(adminNames) <- c("admin_boundary_id","stump_parameter_id","name")
ecoNames <- cbmTables$eco_boundary
names(ecoNames) <- c("eco_boundary_id","stump_parameter_id","name")
spuNames <- merge(adminNames,cbmTables$spatial_unit,by="admin_boundary_id")
names(spuNames) <- c("admin_boundary_id","stump_parameter_id", "province","id",
                     "eco_boundary_id","root_parameter_id", "climate_time_series_id","spinup_parameter_id")
spuNames1 <- merge(ecoNames,spuNames,by="eco_boundary_id")
names(spuNames1) <- c("eco_boundary_id","stump_parameter_id.x","ecozone","admin_boundary_id","stump_parameter_id.y", 
                      "province","spu_id","root_parameter_id", "climate_time_series_id","spinup_parameter_id")
spu <- spuNames1[,c(7,4,6,1,3,5,8,9,10)]
spu <- spu[order(spu$spu_id),]

#1 ii grab shapefile with correct administrative and ecological boundaries
spUnits_Can <- shapefile("data/forIan/SK_data/SK_ReclineRuns30m/layers/pspu.shp")
require(dplyr)

#1 iii Join the two tables
spUnits_Can@data <- left_join(spUnits_Can@data, spu, by = c("ProvinceID" = "admin_boundary_id",
                                                                 "EcoBoundar" = "eco_boundary_id"))

#NAs are located in arctic ecoregions
#spUnits_Can is now a shapefile with spatial units in the spu_id field.

#2.  retrieve spatial units for a hypothetical user study area: 
#2 i Make a study area

randomUserArea <- randomPolygon(x = rgeos::gCentroid(spUnits_Can[spUnits_Can$ProvinceNa == "Quebec",]), 
                                hectares = 1000)
#this will make a polygon somewhere in the center of Quebec - the hectares is inaccurate due to crs

#ii Retrieve Spatial Unit 
spUnits <- crop(spUnits_Can, randomUserArea)
spUnits$spu_id

#3 Make a function that produces a raster with spUnits
retrieveSpuRaster <- function(spatialUnitsFile, UserArea, rasterRes = c(250,250)){
  
  if (!identicalCRS(spatialUnitsFile, UserArea)) {
    crs(spatialUnitsFile) <- crs(UserArea)
  }
  browser()
  temp <- crop(spatialUnitsFile, UserArea)
  
  template <- rasterize(extent(temp), res = rasterRes, crs = crs(UserArea))
  spuRaster <- rasterize(temp, template, field = "spu_id")
  
  return(spuRaster)
}

out <- retrieveSpuRaster(spatialUnitsFile = spUnits_Can, UserArea = randomUserArea, rasterRes = c(250,250))
plot(out)
##Success##
###We could write this Canada-wide shapefile to disk so that we don't have to do all these join steps everytime
##The function won't work unless you have it loaded...
