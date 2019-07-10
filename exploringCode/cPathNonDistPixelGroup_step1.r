# follow one non-disturbed pixel group
# 1.growth of AG ok?
# 2.increses in GHG to the atmosphere reasonable? 
# 3.Can I track the changes? $allProcesses$growth etc?

# Need to have a run completed (spadesCBMout accessible)

# 1.growth of AG ok?-----------------------------------------------------------------------------------
# do the 759 pixelGroups at the beginning still all have pixels in 2005?
pgSpinup <- spadesCBMout$level3DT$pixelGroup
nonDistPg <- spadesCBMout$pixelKeep[,.N,by=pixelGroup2005] %>% .[pixelGroup2005 %in% pgSpinup,]

summary(nonDistPg$N)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    91.5   369.0  1662.4  1413.5 48631.0 

#age of those - b/c we don't want to check the overMature decline...need young enough stands
nonDistPgAges <- spadesCBMout$level3DT[spadesCBMout$level3DT$pixelGroup %in% nonDistPg$pixelGroup2005,c(1,7)]

# pick one
# Pixel group 101
pg101 <- apply(spadesCBMout$pixelKeep[,-1],2,function(x) length(which(x==101)))
# 12 pixels never disturbed
pg101desc <- spadesCBMout$level3DT[pixelGroup==101,]
# ages rasterSps Productivity spatial_unit_id growth_curve_component_id growth_curve_id pixelGroup
#  39         3            1              27                        28              28        101

# check growth pg101
# c from the spinup
c0pg101 <- melt(spadesCBMout$spinupResult[which(spadesCBMout$level3DT$pixelGroup==101),-1])
sumPg101.0 <- sum(c0pg101$value[1:21])#[1] 125.657
# c in 1990
c1pg101 <- melt(spadesCBMout$cbmPools[simYear==1990&pixelGroup==101,6:30])

# delta c
deltaCpg13.1 <- c1pg101[,2]-c0pg101$value
#the difference between the spinup and the 1990 pools should be the growth only
#for the 101 group at least for the first three pools ("SoftwoodMerch"
#"SoftwoodFoliage" "SoftwoodOther")

# three first values should be the carbon curve values
# growth curve id for group 101 is 28
gc28 <- spadesCBMout$growth_increments[spadesCBMout$growth_increments[,1]==28,]
plot(gc28[,2],gc28[,3])
gc28age39 <- gc28[gc28[,2]==39,]
gcCheck <- deltaCpg13.1[1:3]-t(gc28age39[3:5])
### GROWTH CHECKS OUT on year 1
# try one more year
c2pg101 <- melt(spadesCBMout$cbmPools[simYear==1991&pixelGroup==101,6:30])
deltaCpg13.2 <- c2pg101[,2]-c1pg101[,2]
gc28age40 <- gc28[gc28[,2]==40,]
gcCheck2 <- deltaCpg13.2[1:3]-t(gc28age40[3:5])
# END 1.growth of AG ok? YES-----------------------------------------------------------------------------

# 2.increses in GHG to the atmosphere reasonable? ------------------------------
# atmosphere pools at the end of spinup (stay with pg101 for now)
# these values are out of the spinup and should be substracted from 
ghg0.101 <- c0pg101[22:24,]
GHGout.1 <- deltaCpg13.1[22:24]
GHGout.2 <- deltaCpg13.2[22:24]

#Total emmisions for all 15 years of simulations in tonnes of carbon per hectare for this pixel group
totGHG <- spadesCBMout$cbmPools[simYear==2005&pixelGroup==101,27:29]-ghg0.101


###MISC######################################################################################
#??what the hell? growth curve ID and growth curve components are NOT equal??
## SIDE TRACK
growthCurveComponents <- as.matrix(read.csv(spadesCBMout$gcurveComponentsFileName))
gcID <- read.csv(spadesCBMout$gcurveFileName)
gcID$growth_curve_component_id==gcID$growth_curve_id
## nope...all good...read the output wrong :/

# do $spinupResult and $level3DT have the some order?
head(spadesCBMout$spinupResult)
head(spadesCBMout$level3DT)
## YES - went back through the 
