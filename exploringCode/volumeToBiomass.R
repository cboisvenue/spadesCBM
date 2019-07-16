#----------------------------------- Volume to Biomass conversion needed for
#spadesCBM Context: the CBMVolumeToBiomass Library initially used in spadesCBM
#runs seem to have some inconsistencies. This is a work around/check to be able
#to use the BOudewyn st al parameters for conversion from m3/ha to biomass in
#the three main carbon pools that make-up the $growth_increments used to move
#spadesCBM forward in growth from year to year
#
# first draft
# took the parameters from here: https://nfi.nfis.org/en/biomass_models
# need to have a spadesCBM run completed (spadesCBMout)
#
# CBoisvenue
# July 16, 2019
#------------------------------------

#this is the file that seems to have inconsistancies and that we need to modify: 
growth.inc <- spadesCBMout$growth_increments
#id age    swmerch      swfol    swother hwmerch hwfol hwother

# there are only 10 individual growth curves going into the CBMVolumeToBiomass
# translation. These were checked, they are exactly the same (only ten curves
# total) in the input files data/spadesGCurvesSK.csv and
# "C:/Celine/GitHub/spadesCBM/data/yieldComponentSK.csv"
# 

# Following the Boudewyn et al models (p7 and 8 of the 2007 publication), the
# proportion in stemwood bark branches foliage each have an equation. The
# equations for each component have the same form. 
# vol = gross merchantable volume per ha
# lvol = natural logarithm of (vol+5)
# 9 model parameters a1, a2, a3,b1,b2,b3,c1,c2,c3 that are specific to the
# jurisdiction, ecozone and lead tree species

bparams <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table6_v2.csv"))
skParams <- bparams[bparams$jur=="SK",]

#what are the spus in SK? and what ecoboundaries do we have?
spu <- as.data.frame(sim$cbmData@spatialUnitIds)
spu[spu$AdminBoundaryID==9&spu$EcoBoundaryID %in% c(6,9),]

# what species do we have?--------------------------------------------------------
gcID <- read.csv(spadesCBMout$gcurveFileName)#file.path(getwd(),"data/spadesGCurvesSK.csv"))
# the above shows pixelGroup 101 to be white birch...but the cPoolsPixelYear.csv shows it to be a SW...and it grows?
gcSpsName <- unique(gcID$species)
gcSpsID <- unique(gcID$species_id)
SpsIDMatch <- vector(length=length(gcSpsName))
for(i in 1: length(gcSpsName)){
  SpsIDMatch[i] <- gcID[gcID$species==gcSpsName[i],4]%>% .[1]
}
species <- as.character(gcSpsName)
gcSpsMatch <- as.data.table(cbind(species,gcSpsName,SpsIDMatch))

# what are the species code for the parameters?
paramSps <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table7.csv"))
paramSps <- paramSps[paramSps$prov=="SK"&paramSps$ecozone %in% c(6,9),]

## need to match our species with those species per ecozones (there are no
## differences for species code between these two ecozones)
# doing this manually.
genus <- unique(paramSps$genus)
paramSps[paramSps$genus=="ABIE",]# 302 for Balsam Fir in both ecozones
paramSps[paramSps$genus==genus[6],1:5]# 1203 for Balsam poplar in both ecozones
                                      # 1201 for Trembling aspen in both ecozones
paramSps[paramSps$genus==genus[1],1:5]# 101 for Black Spruce in both ecozones
                                      # 105 for White Spruce in both ecozones
paramSps[paramSps$genus==genus[2],1:5]# 203 for Jack Pine in both ecozones
paramSps[paramSps$genus==genus[7],1:5]# 1303 for White Birch in both ecozones

names(gcSpsMatch) <- c("speciesName", "gcSpsName","SpsIDMatch","rasterSps")
species <- c(302,1203,101,203,1201,1303,105)
spsMatch <- cbind(gcSpsMatch,species)
#--------------------------------------------------------------------------------

# trying with just one species: Balsam fir growth curve id 1 (and
# 2,3,22,23,24,43,44,45,64,65,66,85,86,87)
# growthComponents
gComp <- read.csv(spadesCBMout$gcurveComponentsFileName)
gComp1 <- gComp[gComp$GrowthCurveComponentID==1,]
# looks like it is upposed to
plot(gComp1$Age,gComp1$MerchVolume)
params1 <- skParams[skParams$species==302&skParams$eco==6,]
# try one volume at year 100
vol100 <- gComp1[gComp1$Age==100,3]

# 4 proportions  should be returned
# will eventually add species, ecozone
biomProp <- function(vol){
  biomVect <- vector(length = 4)
  lvol <- log(vol+5)
  a <- c(5:7)
  b <- c(8:10)
  c <- c(11:13)
  for(i in length(biomVect){
    stem <- 1/(1+exp(params1[,a[1]]+params1[,a[2]]*vol+params1[,a[3]]*lvol)+
               exp(params1[,b[1]]+params1[,b[2]]*vol+params1[,b[3]]*lvol)+
               exp(params1[,c[1]]+params1[,c[2]]*vol+params1[,c[3]]*lvol))
    
  })
  
}




### This is background info...in the spadesCBMinputs module, this is how we
### "translate" the growth info into biomass - note the function below using the
### library CBMVolumeToBiomass
gcID <- read.csv(sim$gcurveFileName)
## HAVE TO maintain this format
growthCurves <- as.matrix(gcID[,c(3,2,5,4,6)])
growthCurveComponents <- as.matrix(read.csv(sim$gcurveComponentsFileName))

sim$growth_increments<-NULL
for(gcid in unique(growthCurves[,"growth_curve_id"])) { 
  curve <- processGrowthCurve(gcid, growthCurves, growthCurveComponents,sim = sim)
  sim$growth_increments <- rbind(sim$growth_increments,
                                 cbind(rep(gcid,(nrow(curve)-1)), cbind(curve[0:(nrow(curve)-1),1], diff(curve[,2:ncol(curve)]))))
  
}

colnames(sim$growth_increments)<- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
sim$gcHash <- matrixHash(sim$growth_increments)
#create a nested hash (by gcid/by age)
## used in SpinUp function later...
for(item in ls(sim$gcHash)){
  sim$gcHash[[item]] <- hash(sim$gcHash[[item]])
}

# this is how the groth curves are processed in spadesCBM now
processGrowthCurve <- function(gcid,growthCurves,growthCurveComponents,sim) {
  
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
  carbonCurve <- VolumeToBiomassConvert(dbpath = sim$dbPath, 
                                        spatial_unit_id = matchingRows[1,"spatial_unit_id"],
                                        sw_species_code = swSpeciesCode, 
                                        swAgeVolume = swAgeVolumePairs, 
                                        hw_species_code = hwSpeciesCode, 
                                        hwAgeVolume = hwAgeVolumePairs)
  return (carbonCurve)
}
#FYI:
# cbmTables$forest_type
# id           name
# 1  1       Softwood
# 2  2      Mixedwood
# 3  3       Hardwood
# 4  9 Not Applicable
