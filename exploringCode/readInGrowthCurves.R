#----------------------------------------------------------
# Growth Curves exploration
## Descriptive - what is in there and what does it mean?
## ANALYSES -  what does the spadesCBMinputs module do with these curves?
## SPECIES - how did we go from 9 curves to a file with 105 unique growth curve ids?
# April 6, 2018, June 20, 2018
# CBoisvenue
#----------------------------------------------------------


# read-in the came growth curves as is the 1st working version of the 3-module spadeCBM family

## DESCRIPTIVE: what is in those files? how does it relate to cbm_defaults?

# in the data folder of the spadesCBMinputs module
gcComponent <- as.matrix(read.csv(file.path("data/forIan/SK_data/SK_ReclineRuns30m/LookupTables/yieldComponentRCBM.csv")))
gcIn <- as.matrix(read.csv(file.path("data/forIan/SK_data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv")))

## here trying to match the spatial_unit_id in gcIn with the spatial_unit in cbm_defaults
# ***run readInSQLiteData.r which creates the data.frame spu (combines all ids with province and ecozone names)***
# figure out what spatial_unit these gc are in with the matching of 
# gcIn$spatial_unit_id and cbmTables$spatial_unit
#x <- which(spu$spu_id %in% unique(gcIn[,1]))
gcSpu <- spu[which(spu$spu_id %in% unique(gcIn[,1])),]

# > gcSpu
# spu_id admin_boundary_id     province eco_boundary_id            ecozone
#     26                 9 Saskatchewan               5  Taiga Shield West
#     27                 9 Saskatchewan               6 Boreal Shield West
#     28                 9 Saskatchewan               9      Boreal Plains
#     29                 9 Saskatchewan              10  Subhumid Prairies
#     30                 9 Saskatchewan              18  Semiarid Prairies

gcSps <- sps[which(sps$species_id %in% unique(gcIn[,4])),c(1,2)]

# > gcSps
# species_id         species
#          2    Black spruce
#          6    White spruce
#         14       Jack pine
#         29      Balsam fir
#         63 Trembling aspen
#         65   Balsam poplar
#         76     White birch

gcforest <- unique(sps[which(sps$forest_type_id %in% unique(gcIn[,5])),c(3,4)])

##ANALYSES: what does the spadesCBMinputs module do with these curves?
# reproducing it below
library(CBMVolumeToBiomass)

# each unique gc in gcIn$growth_curve_id is translated into biomass
# this functionv(processGrowthCurve) calls a function in CBMVolume to Biomass (VolumeToBiomassConvert)

## This is in the spadesCBMinputsFunctions.r parsing file
processGrowthCurve <- function(gcid,growthCurves,growthCurveComponents) {
  
  matchingRows <- t(as.matrix(growthCurves[growthCurves[,"growth_curve_id"]==gcid,]))
  swSpeciesCode = 0
  hwSpeciesCode = 0
  swAgeVolumePairs = as.matrix(0)
  hwAgeVolumePairs = as.matrix(0)
  for(row in 1:nrow(matchingRows)){
    if(matchingRows[row,"forest_type_id"]==1) {
      swSpeciesCode <- matchingRows[row, "species_id"]
      swAgeVolumePairs <- growthCurveComponents[growthCurveComponents[,"GrowthCurveComponentID"] == matchingRows[row,"growth_curve_component_id"], c("Age","MerchVolume")]
    } else {
      hwSpeciesCode <- matchingRows[row, "species_id"]
      hwAgeVolumePairs <- growthCurveComponents[growthCurveComponents[,"GrowthCurveComponentID"] == matchingRows[row,"growth_curve_component_id"], c("Age","MerchVolume")]
    }
  }
  carbonCurve <- VolumeToBiomassConvert(dbpath = dbPath, 
                                        spatial_unit_id = matchingRows[1,"spatial_unit_id"],
                                        sw_species_code = swSpeciesCode, 
                                        swAgeVolume = swAgeVolumePairs, 
                                        hw_species_code = hwSpeciesCode, 
                                        hwAgeVolume = hwAgeVolumePairs)
  return (carbonCurve)
}
## END OF FUNCTION################################################

growth_increments<-NULL

for(gcid in unique(gcIn[,"growth_curve_id"])){
  gcid=1
  curve <- processGrowthCurve(gcid,gcIn,gcComponent)
  growth_increments <- rbind(growth_increments,
                                 cbind(rep(gcid,(nrow(curve)-1)), cbind(curve[0:(nrow(curve)-1),1], diff(curve[,2:ncol(curve)]))))
  
}

colnames(growth_increments)<- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")

## These are  2 functions in the spadesCBMinputsFunctions.r parsing file
hash <- function(x) {
  e <- new.env(hash = TRUE, size = nrow(x),
               parent = emptyenv());
  apply(x, 1, function(col) {
    assign(toString(col[1]), col[2:length(col)], envir = e)
  });
  
  return(e)
}

# used in spadeCBMInputs (in g&y reading) AND in spadesCBMdefaults (in creating sim$processes)
matrixHash <- function(x){
  keys = unique(x[,1])
  e <- new.env(hash = TRUE, size=length(keys), parent = emptyenv())
  apply(as.matrix(keys), 1, function(key) {
    assign(toString(key), x[x[,1]==key,2:ncol(x)], envir = e)
  });
  return(e)
}

## END OF FUNCTION############################

gcHash <- matrixHash(growth_increments)

#create a nested hash (by gcid/by age)
## used in SpinUp function later...
for(item in ls(sim$gcHash)){
  sim$gcHash[[item]] <- hash(sim$gcHash[[item]])
}

## END OF FUNCTION############################

## SPECIES - how did we go from 9 curves to a file with 105 unique growth curve ids?

#There are 7 unique species_id in the gcIn
# here are there names
gcSps <- sps[which(sps$species_id %in% unique(gcIn[,4])),]

# There are 5 unique spatial_unit
gcSpu <- spu[which(spu$spu_id %in% unique(gcIn[,1])),]
#There are 2 forest types: softwood and hardwood (not mixed)
forest_type_id <- unique(gcIn[,5])

# are there lines that are the same if we ignore the growth_curve_id and growth_curve_component_id?
dim(unique(gcIn[,c(-2,-3)])) 
# 35 and 3
## IDEA: this could be the productivity levels? if so...only the spruces really have 
## different growth curves

# how may lines for each species?

library(dplyr)
class(gcIn)
gcDF <- as.data.frame(gcIn)
gcDF %>% tally() #test

# there are 15 lines per species
gcDF %>% count(gcDF$species_id)
# Forest type is strickly associated with leading species
gcDF %>% count(gcDF$species_id, gcDF$forest_type_id)
# There are three growth_curve_id by 
gcDF %>% count(gcDF$spatial_unit_id, gcDF$species_id, gcDF$forest_type_id) %>% print(n=35)

# how many of those are different?
# only Black spruce (2) and white spruce (6) should have 2 that are different...
# first check the the other species are all using the same curve in their three instances 
# in the 5 spatial spatial units

# which growth_curve_component_id identify

library(tidyr)
spsCurves <- NULL
gcIDspsProd <- NULL

for(i in 1:length(gcSps$species_id)){
  curves <- unique(gcDF$growth_curve_component_id[gcDF$species_id==gcSps$species_id[i]])
  getComponents <- as.data.frame(gcComponent[gcComponent[,1]%in% curves,])
  curvesWide <- getComponents %>% spread(Age, MerchVolume)
    # these lines are to create a table of leading species by prod level by gcIDs
    gcIDwork <- cbind(gcSps$species_id[i],curvesWide)
    gcIDspsProd <- rbind(gcIDspsProd,gcIDwork)
  # these lines create the table needed to make sense of which species is what curve  
  curveCheck <- unique(curvesWide[,-1])
  addSps <- cbind(gcSps[i,1:2],curveCheck)
  spsCurves <- rbind(spsCurves,addSps)
}

gcIDspsProd %>% count(gcIDspsProd$`gcSps$species_id[i]`) %>% print(n=7)
names(gcIDspsProd) <- c("species_id","growth_curve_component_id",names(gcIDspsProd)[3:253])


# There are 10 curves

# how many levels of productivity on the productivity raster?
library(raster)
prod <- raster(file.path("data/forIan/SK_data/SK_ReclineRuns30m/layers/site_productivity.tif"))
prod_val <- getValues(prod)
unique(prod_val) # NA  1  2  3  0
# from lookup Tables in here:
# data/forIan/SK_data/SK_ReclineRuns30m/LookupTables/productivityLookup.csv
# RasterValue	prodClass
# 1	G
# 2	M
# 3	P
prodLookup <- read.csv(file.path(
                      "data/forIan/SK_data/SK_ReclineRuns30m/LookupTables/productivityLookup.csv"))


# match the productivity levels with each curve

# get rid of the 0/NA raster value
prodLookup <- prodLookup[prodLookup$RasterValue>0,]
# by looking at the spsCurves, I can tell that spruces, and Trembling Aspen each have two levels
# the first in the table of each level is the higher values one (so the "G")
dim(spsCurves)
species_id= sort(rep.int(unique(spsCurves$species_id),3))
prodClass <- cbind(species_id,prodLookup)
spsProdCurves <- merge(prodClass,spsCurves, by="species_id")
# getting rid of the extra matches. Rows 2,3,5, 8,9,11, 20,21,23
spsProdCurves <- spsProdCurves[c(-2,-3,-5,-8,-9,-11,-20,-21,-23),]

# need to have one place to lookup the links between raster species (1 to 7), cbm species (species_id),
# prodClass, RasterValue for the prodClass, and spatial unit, all linked to the growth_curve_component_id
# adding the prodClass and its Raster values
gcID_ref <- cbind(gcIDspsProd[,1:2],prodLookup)
# adding the raster species from ~data\forIan\SK_data\SK_ReclineRuns30m\LookupTables/species_map_to_codes_v2_6.xls
sps_ref <- cbind(gcSps[,1:2],rasterSps=c(3,7,4,1,5,2,6))
gcID_ref <- merge.data.frame(gcID_ref,sps_ref)
# adding the spatial units id
gcID_ref <- merge.data.frame(gcDF[,1:2],gcID_ref)
write.csv(gcID_ref,file=file.path("data/forIan/SK_data/gcID_ref.csv"))

          