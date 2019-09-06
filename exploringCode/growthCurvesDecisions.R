# GrowthCurve decisions
# September 5, 2019
# CBoisvenue

# number of species in our present study area?
studySps <- unique(spadesCBMout$level3DT$rasterSps)
# from volumeToBiomass.R
spsMatch[which(spsMatch$rasterSps %in% studySps),]
# there are 5 species in the study area
# speciesName rasterSps SpsIDMatch species
# 1:    Black spruce         3          2     101
# 2:       Jack pine         4         14     203
# 3: Trembling aspen         5         63    1201
# 4:     White birch         6         76    1303
# 5:    White spruce         7          6     105


# unique group of growth curves (input) per species?
# from publication: 
# black spruce has 2 curves (good and med)
# jack pine has 1
# Trembling aspen has 2 curves (Good and Med)
# White Birch has 1 curve
# White Spruce has 2 curves
## 8 curves total

# identify the growth_curve_component_id that is related to each species
gcID <- as.data.table(read.csv(spadesCBMout$gcurveFileName))
gcID[,.N,by=c("species","prodClass")]
# 5 by prodClassXspecies - 7 species X 3 prodClass X 5 curves each (b/c of
# original CBM classifiers) = 105 curves
# check if the P class equals the M class for species with more then one curve
# Black spruce (all will be the same decision)
bsIDs <- gcID[which(gcID$species=="Black spruce"),growth_curve_id]
bsGrowthC <- gComp[which(gComp$GrowthCurveComponentID %in% bsIDs),]
library(ggplot2)
bs <- ggplot(data=bsGrowthC, aes(x=Age,y=MerchVolume,group=GrowthCurveComponentID, colour=GrowthCurveComponentID)) +
  geom_line()

# only two curves
bsGrowthC[bsGrowthC$Age==250,]
# the 2nd and 3rd numbers are the same: numbers are in 3s (G, M, P prodClass in order). So P and M are the same

# identify the growth_curve_component_id that should be the same for black spruce

# 75 growth curves before translation
studySpsId <- gcID[which(gcID$rasterSps %in% studySps),growth_curve_id]
gComp <- as.data.table(read.csv(spadesCBMout$gcurveComponentsFileName))
studySpsCurves <- gComp[which(gComp$GrowthCurveComponentID %in% studySpsId)]
# but only 8 different ones
library(ggplot2)
b <- ggplot(data=studySpsCurves, aes(x=Age,y=MerchVolume,group=GrowthCurveComponentID, colour=GrowthCurveComponentID)) +
  geom_line()


## POST translation######

# are the increments the same each time we run spadesCBM?
trans1 <- spadesCBMout$growth_increments
trans2 <- spadesCBMout1$growth_increments
which(trans2!=trans2)
#integer(0)
length(which(trans2==trans2))
#[1] 210000
length(which(trans1==trans2))
#[1] 210000
which(trans1!=trans2)
#integer(0)
which(trans2!=trans1)
#integer(0)
# the increments are the same

# are there also just 8 curves post translation?
growthInc <- as.data.table(spadesCBMout$growth_increments)
# look at the the 3 biomass pools (the ones that get translated) as one value
# (totInc)
biomInc <- growthInc[,.(id,age,totInc = rowSums(growthInc[,3:8]))]
c <- ggplot(data=biomInc, aes(x=age,y=totInc,group=id,colour=id)) +geom_line()
### NO: there are many more curves...!!! THIS IS A HUGE PROBLEM

## DECISION#################
#we are going to build a volume to Biomass spades module this will take more
#time than is available for the RIA deliverables so a smotthing algorithm will
#be used to make sure that the biomass/ha values are reasonable in the early
#years of growth





