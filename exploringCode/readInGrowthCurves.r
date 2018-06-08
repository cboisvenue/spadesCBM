#----------------------------------------------------------
# Growth Curves exploration
# Arpil 6, 2018
# CBoisvenue
#----------------------------------------------------------


# read-in the came growth curves as is the 1st working version of the 3-module spadeCBM family

## DESCRIPTIVE: what is in those files? how does it relate to cbm_defaults?

# in the data folder of the spadesCBMinputs module
gcComponent <- as.matrix(read.csv("C:/Ian/Boisvenue/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldComponentRCBM.csv"))
gcIn <- as.matrix(read.csv("C:/Ian/Boisvenue/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv"))

## here trying to match the spatial_unit_id in gcIn with the spatial_unit in cbm_defaults
# run readInSQLiteData.r which creates the data.frame spu (combines all ids with province and ecozone names)
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


