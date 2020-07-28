#----------------------------------- Volume to Biomass conversion needed for
#spadesCBM Context: the CBMVolumeToBiomass Library initially used in spadesCBM
#runs seem to have some inconsistencies. This is a work around/check to be able
#to use the Boudewyn st al parameters for conversion from m3/ha to biomass in
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
# "data/yieldComponentSK.csv"
# 
library(ggplot2)
gComp <- as.data.table(read.csv(spadesCBMout$gcurveComponentsFileName))

a <- ggplot(data=gComp, aes(x=Age,y=MerchVolume,group=GrowthCurveComponentID, colour=GrowthCurveComponentID)) +
  geom_line()

# Following the Boudewyn et al models (p7 flowchart in 2007 publication)-----------------------------

# read-in all parameters---------------------------
# model parameters are specific to the jurisdiction, ecozone and lead tree species
# can be updated here: https://nfi.nfis.org/en/biomass_models
table3 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table3.csv"))
sktable3 <- table3[table3$jur=="SK",]
table4 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table4.csv"))
sktable4 <- table4[table4$jur=="SK",]
# table5 is weird since they did not have enough data for SK. I am selecting AB
# instead. Another catch is that they don't have the same species match. I
# manually check and ABIES is genus 3 (used below)
table5 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table5.csv"))
sktable5 <- table5[table5$juris_id=="AB",]
table6 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table6_v2.csv"))
sktable6 <- table6[table6$jur=="SK",]
# read-in parameters done---------------------------

#what are the spus in SK? and what ecoboundaries do we have?-------------
spu <- as.data.frame(sim$cbmData@spatialUnitIds)
spu[spu$AdminBoundaryID==9&spu$EcoBoundaryID %in% c(6,9),]
# end spu and eco--------------------------------------------------------

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

names(gcSpsMatch) <- c("speciesName", "rasterSps","SpsIDMatch")
species <- c(302,1203,101,203,1201,1303,105)
spsMatch <- cbind(gcSpsMatch,species)
write.csv(file = file.path(paths(spadesCBMout)$inputPath,"spsMatchNameRasterGfileBiomParams.csv"), spsMatch, row.names = FALSE)
#End species------------------------------------------------------------------------

# Balsam fir volume at 100 years old: there are no balsam fir in our study area..(sigh)
## Balsam fir 100 successful
# trying again but with black spruce med curves----------------------------------------------------------
# trying with just one species:  growth curve id 8 (black spruce is gcID
# 8,9,29,30,50,51,71,72,92,93)

# try the whole curve now--------------------------------------------
# growthComponents
gComp <- read.csv(spadesCBMout$gcurveComponentsFileName)
gComp14 <- gComp[gComp$GrowthCurveComponentID==14,]
# looks like it is upposed to
plot(gComp14$Age,gComp14$MerchVolume)

# one species Balsam fir in one ecozone since the params are the same, read-in all parameters
# black Spruce
# now Jack PIne 
params3 <- sktable3[sktable3$canfi_species==1201&sktable3$eco==6,]
params4 <- sktable4[sktable4$canfi_species==1201&sktable4$eco==6,]
# table 5 is different than the others
params5 <- sktable5[sktable5$canfi_genu==9&sktable5$ecozone==6,]
params6 <- sktable6[sktable6$species==1201&sktable6$eco==6,]

# try one volume at year 100
vol100 <- gComp14[,3]

# eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
# the gross merchantable volume/ha. Parameters a and b are in table3
eq1 <- b_m(params3,vol100)

# eq2 is for non-merch sized trees.
eq2 <- nmfac(params4,eq1 = eq1)
#some NAs where it was 0s
eq2[which(is.na(eq2))] <- 0

# eq3 is for the saplings and it needs b_nm from the previous eq2
eq3 <- sapfac(params5,eq2 = eq2)
eq3[which(is.na(eq3))] <- 0

# middle box flowchart3
merch <- eq1 + eq2[, 1] + eq3

pVect <- biomProp(table6=params6,vol = vol100)  

totbiom <- merch / pVect[, 1]
bark <- totbiom * pVect[, 2]
branch <- totbiom * pVect[, 3]
fol <- totbiom * pVect[, 4]
other <- branch + bark

# End of one age for Balsam fir at age 100, biomass seem reasonable-----------------------
# then black spruce
# Jack pine
id <- rep(14, length(merch))
curve <- cbind(id,merch,fol,other)
# merch, fol, other looks fine once translated for Balsam fir--------------------------------

# calculate the increments for Balsam fir/Black spruce?jack pine --------------------------------------

growth_increments <- NULL

growth_increments <- diff(curve)
growth_increments[,1] <- id[1:dim(growth_increments)[[1]]]

# growth increments for Balsam fir are calculated...replace in inputs module (before hash)
# writing this to file, I will replace gcID 1 and 2,3,22,23,24,43,44,45,64,65,66,85,86,87 with this and test
#write.csv(file = file.path(paths(spadesCBMout)$inputPath,"balsamFirInc.csv"), growth_increments, row.names = FALSE)
# now black spruce
# now jack pine
# aspen medium
write.csv(file = file.path(paths(spadesCBMout)$inputPath,"aspenMedInc.csv"), growth_increments, row.names = FALSE)


#how I am replacing the balsam fir growth increments with these, this has to
#happend on line 127 of inputs module
# balsamInc <- read.csv(file.path(paths(spadesCBMout)$inputPath,"balsamFirInc.csv"))
# BFid <- c(1,2,3,22,23,24,43,44,45,64,65,66,85,86,87)
# growth.inc <- spadesCBMout$growth_increments
# for(i in 1:length(BFid)){
#   growth.inc[growth.inc[,1] == BFid[i],3] <- balsamInc[,2]
#   growth.inc[growth.inc[,1] == BFid[i],4] <- balsamInc[,3]
#   growth.inc[growth.inc[,1] == BFid[i],5] <- balsamInc[,4]
#   }
bSpruceInc <- read.csv(file.path(paths(spadesCBMout)$inputPath,"blackSpruceInc.csv"))
BSid <- c(8,9,29,30,50,51,71,72,92,93)
growth.inc <- spadesCBMout$growth_increments
for(i in 1:length(BSid)){
  growth.inc[growth.inc[,1] == BSid[i],3] <- bSpruceInc[,2]
  growth.inc[growth.inc[,1] == BSid[i],4] <- bSpruceInc[,3]
  growth.inc[growth.inc[,1] == BSid[i],5] <- bSpruceInc[,4]
}

#FYI:
# cbmTables$forest_type
# id           name
# 1  1       Softwood
# 2  2      Mixedwood
# 3  3       Hardwood
# 4  9 Not Applicable
