# Task 3
#3.create pre-calculated values for mapping: total carbon, live carbon, dead
#carbon, above ground carbon, below ground carbon, NPP. 

# May 1, 2019
# CBoisvenue

#the fluxes are going to be tricky because the most logical way to calculate the
#fluxes would be in the cpp code. There is even a place holder for that (lines
#393-398).
# SO WORKING ON NPP ONLY HERE

#I see three avenuesto figure out the best way to get NPP:
#1. extract the components during processing in the R scripts (like increment and turnover) and
#cumulate them in another data table (memory intensive)
#2. calculate the components of NPP directly (increment and biomass turnover) from the cPools outputed in
#the cPoolsPixelYear.csv
#3. calculate all the flux matrices in teh Cpp code (out of my reach rigth now)

# SELECTED #2: will calculate the NPP components from the output
# cPoolsPixelYear.csv. Note that if we don't output all years this won't work,
# we will have to calculate it in the yearly processing.

# SORTING OUT A FEW THINGS FIRST

# What is know about NPP in a CBM context-----------------------------
# CBM does not do photosynthesis or autotrophic respiration
#NPP is calculated as the sum of net growth (i.e. growth that results in
#positive increment) and growth that replaces material lost to biomass turnover
#during the year.
#NPP=increment + turnover
#increment = sum(GrossGrowthAG)+sum(GrossGrowthBG)
#GrossGrowthAG = increment going to Merch, Otherwood, foliage
#GrossGrowthBG = increment going to Fine Root Coarse Root

#there is no GPP or total ecosystem respiration estimated in CBM
#NEP=NPP-sum(DOMpool decomp to atmosphere)
#litterfall = sum(biomassToSoil)
#Netgrowth would be = NPP- litterfall
#--------------------------------------------------------------------

# UNITS-------------------------------------------------------------
#the $growth_increments are in tonnes of carbon per ha and these MATCH the values if
#you calculate the differences between two years in the cPoolsYears.csv for the
#Merc, Fol, Other, Froots, Croots.
#see work\SpaDES\work\workingOutNPP.xls
#so the carbonCurve ("curve" on line 121 of spadesCBMinputs) returned from the
#R function processGrowthCurve() is in tonnes of carbon per ha and is the total, not
#the increment for the Merch, Fol, and Other (not the roots, they are
#calculated later). For these three pools the "curve" of carbonCurve, match the
#values in the output (cPoolsPixelYear.csv)
#CONCLUSION: I can calculate the increment by summing the deltas from the output as per option #2.
#-----------------------------------------------------------------------
# creating a map of UNITS ---------------------
# 1.the growth curves are in m3/ha (cumulative volume over time)
# 2. the growth curves are fed into the Boudewyn algorithms
# (library(CBMVOlumeToBiomass)) with its results multiplied by 0.5. That gives us the "curve", line 121 in
# spadesCBMinputs and that is in tonnes of carbon/ha
# 3. the differences between the years will be in tonnes of carbon per hectare
# 4. checking this in workingOutNPP.xlsx IncrementTurnoverManually worksheet:
# average NPP is 2.5987. If this is in tonnes of C per hectare per year, that
# would be *100 (1 tonne=1000000g, 1ha=10000) in gC m-2 yr-1, so 259.87 - spot on! i.e., that makes sense


# calculating increment-------------------------------------------------------------
# example of this for one stand that does not have disturbances in excel
# work\SpaDES\work\workingOutNPP.xls
# the year stands are disturbed, don't calculate NPP (could put a range limit
# like is "total NPP/yr has to be <5)
# IncrementTurnoveManually, used cPoolsPixelYear.csv, order by pixelGroup and age, calculate
# the different between (year+1) and year for
# SoftwoodMerch	SoftwoodFoliage	SoftwoodOther	SoftwoodCoarseRoots	SoftwoodFineRoots.
# The sum of that = increment
## TO DO FOR ALL STANDS in cPoolsPixelYear.csv
###WORKING ON grossGrowth HERE##
library(data.table)
outC <- as.data.table(read.csv(file.path(outputPath(spadesCBMout),"cPoolsPixelYear.csv")))

# sort by pixel group and simYear## HERE FOR INCREMENTS
grossGrowthAG <- NULL
grossGrowthAG<- rbind(grossGrowthAG,##HERE## WRONG AFTER THIS
                               cbind(rep(gcid,(nrow(curve)-1)), cbind(curve[0:(nrow(curve)-1),1], diff(curve[,2:ncol(curve)]))))
#NOW: TURNOVER?
# Rates are here: spadesCBMout$cbmData@turnoverRates spadesCBMout$level3DT has
# spatial_unit_id spadesCBMout$cbmData@spatialUnitIds has the EcoBoundaryID (in
# rates) that links to the SpatialUnitID (in level3DT)

# there is a tricky part to turnover: there is a flow that occurs on a growth
# curve decline these involve the "Split" parameter in trates. There are three
# of these parameters: CoarseRootAGSplit, FineRootAGSplit,
# OtherToBranchSnagSplit. These spilts only apply to increments when the
# increments is less then 0. See OverMatureDecline worksheet in work\SpaDES\work\Matrices.xls
###HERE
trates <- spadesCBMout$cbmData@turnoverRates
level3DT <- spadesCBMout$level3DT #$pixelGroupC after the 1st year of sim
spuLink <- spadesCBMout$cbmData@spatialUnitIds

ourspu <- unique(level3DT$spatial_unit_id)
oureco <- spuLink[which(spuLink[,1] %in% ourspu),c(1,3)]
ourRates <- cbind(oureco[,1],trates[which(trates[,1] %in% oureco[,2]),])
colnames(ourRates)[1] <- "spatial_unit_id"
# check if the rates are all equal (?maybe this step is not necessary since we
# would do it by spu/ecoB anyway...)
ourRates[1,2:13]==ourRates[2,2:13]
# only one rate for these two spu

# lining-up rates so that they match the order of the pools in the cPoolsPixelYear.csv
ratesApply <- ourRates[,c(1,2,5,3,6,9,11,5,4,7,9,11)]


# scratch notes-------------------------------------------------------------------------
#person: Celine

# working with the spadesCBMout object for now
# this means that the spades call in the spadesCBM.Rmd has to be run.

# growth increment information
# what is this really?
# this is created in spadesCBMinputs.r. the 
growthInc <- spadesCBMout$growth_increments
growthInc <- as.data.frame(growthInc)

# HUGE PROBLEM BUT I ALREADY KNOW THIS...
#SOLUTION: re-write the CBMVolumeToBiomass library as a module.
#There are negative increment values
# summary(growthInc)
# id           age           swmerch             swfol               swother         
# Min.   :  1   Min.   :  0.0   Min.   :-0.16125   Min.   :-0.0145020   Min.   :-0.598913  
# 1st Qu.: 27   1st Qu.: 62.0   1st Qu.:-0.02105   1st Qu.:-0.0009973   1st Qu.:-0.004029  
# Median : 53   Median :124.5   Median : 0.00000   Median : 0.0000000   Median : 0.000000  
# Mean   : 53   Mean   :124.5   Mean   : 0.06598   Mean   : 0.0081129   Mean   : 0.020478  
# 3rd Qu.: 79   3rd Qu.:187.0   3rd Qu.: 0.06038   3rd Qu.: 0.0035512   3rd Qu.: 0.010658  
# Max.   :105   Max.   :249.0   Max.   : 1.14126   Max.   : 0.5733025   Max.   : 0.546792  
# hwmerch             hwfol             hwother       
# Min.   :-0.28597   Min.   :-0.03011   Min.   :-0.3758  
# 1st Qu.: 0.00000   1st Qu.: 0.00000   1st Qu.: 0.0000  
# Median : 0.00000   Median : 0.00000   Median : 0.0000  
# Mean   : 0.06633   Mean   : 0.00360   Mean   : 0.0186  
# 3rd Qu.: 0.00000   3rd Qu.: 0.00000   3rd Qu.: 0.0000  
# Max.   : 1.61907   Max.   : 0.52608   Max.   : 0.6687  

# plot the curves to show which ones have negatives

# strating with 1 curve
id1 <- growthInc[growthInc$id==28,]

plot(id1$age,id1$swmerch)

# this function is how the growthInc are calculated
#it uses the CBMVolumeToBiomass::VolumeToBiomassConvert()
# the Boudewyn et al 2007 equations do from m3/ha to tonnes of biomass per ha

library(CBMVolumeToBiomass)
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


gcID <- read.csv(file.path("data/spadesGCurvesSK.csv"))
growthCurves <- as.matrix(gcID[,c(3,2,5,4,6)])#as.matrix(read.csv(sim$gcurveFileName))
growthCurveComponents <- as.matrix(read.csv(spadesCBMout$gcurveComponentsFileName))
#gcid is a unique identifier for each growth curve (1 to 105)
gcid=28

# given that the Boudewyn et al coefficients gives you tonnes of biomass per
# ha...is this tonnes of carbon per hectare?
process28 <- processGrowthCurve(gcid,growthCurves,growthCurveComponents,spadesCBMout)
# NO this is in tonnes of biomass per ha
# post Cpp functions, that is in carbon

# Can I calculate all this using just the outputed pools? ($cbmPools) or do I have to pull it out of the matrices?
outPools <- spadesCBMout$cbmPools
head(outPools)
pg101 <- HERE

# checking out the "growth processes and decline matrices" in the core module
#I think this will be the last year's (so 2005) growth 1
allProcesses <- spadesCBMout$allProcesses
# names(allProcesses)
# [1] "Disturbance"       "Growth1"           "DomTurnover"       "BioTurnover"      
# [5] "OvermatureDecline" "Growth2"           "DomDecay"          "SlowDecay"        
# [9] "SlowMixing" 
length(allProcesses$Growth1)
# this is also a list
# [1] 4574
# it has one "transfer from one pool (row) to another (col)
length(allProcesses$Disturbance)
# [1] 426

#last level 3 DT
lastDT <- spadesCBMout$pixelGroupC
dim(lastDT)

# processes...created in spadesCBMdefaults.r
processes <- spadesCBMout$processes



