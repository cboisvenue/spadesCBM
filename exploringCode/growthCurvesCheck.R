# My calculations do not seem to improve...
# Maybe that is NOT how increments work. I say this because the root calculation
# check done in the workingOutNpp.xls still came out about right. There may be
# some "mass balance maintenance" that adjust things and I can't see where that
# happend because I can't see the c++ code.

# trying another tactic
# just check $growth_increments
# there should only be 10 curves total. They repeat for the 105 curves we have
# that is true in the growth curves that are inputed (checked this thoroughly).
library(ggplot2)
gComp <- as.data.table(read.csv(spadesCBMout$gcurveComponentsFileName))

a <- ggplot(data=gComp, aes(x=Age,y=MerchVolume,group=GrowthCurveComponentID, colour=GrowthCurveComponentID)) +
  geom_line()

# Now I will check if any of the conversion from what is supposed to be the same
# curves are 1) actually the same (I don't think they are), and 2) if they are
# not, figure out if one is "better behaved" they the rest

growthInc <- as.data.table(spadesCBMout$growth_increments)

biomInc <- growthInc[,.(id,age,totInc = rowSums(growthInc[,3:8]))]

b <- ggplot(data=biomInc, aes(x=age,y=totInc,group=id,colour=id)) +geom_line()
## There are WAY more then 10 curves there..?

# Check my conversions of balsam fir and black spruce med-------------------------------------------
balsamFirInc <- as.data.table(read.csv(file.path(paths(spadesCBMout)$inputPath,"balsamFirInc.csv")))
bSpruceInc <- as.data.table(read.csv(file.path(paths(spadesCBMout)$inputPath,"blackSpruceInc.csv")))

BFtotInc <- balsamFirInc[,.(id, age=1:250,totInc=rowSums(balsamFirInc[,2:4]))]
#the 1st three lines are obviously wrong (need to check the calculations??), but
#the rest looks good.
c <- ggplot(BFtotInc,aes(x=age, y=totInc)) +geom_line()
c1<- ggplot(BFtotInc[3:250,],aes(x=age, y=totInc)) +geom_line()

BStotInc <- bSpruceInc[,.(id, age=1:250,totInc=rowSums(bSpruceInc[,2:4]))]
#the 1st three lines are obviously wrong (need to check the calculations??), but
#the rest looks good.
d <- ggplot(BStotInc,aes(x=age, y=totInc)) +geom_line()
d1 <- ggplot(BStotInc[3:250,],aes(x=age, y=totInc)) +geom_line()
# checks done---------------------------------------------------------------------------------------


# Comparing the conversions from the library CBMBiomassToVolume with "my"
# conversion from the script volumeToBiomss.r--------------------------------------------------
# attached the species to the growth curve ids
gcID <- as.data.table(read.csv(spadesCBMout$gcurveFileName))
gcSps <- gcID[,.N,by=species]
bSpruceID <- gcID[which(gcID$species==gcSps$species[3]),growth_curve_id]
# making sure some are in our current study area:
bSpruceRun <- spadesCBMout$level3DT[growth_curve_id %in% bSpruceID,]
# yes

e <- ggplot(biomInc[which(biomInc$id %in% bSpruceID),],
            aes(x=age,y=totInc,group=id,colour=id)) +geom_line()
e 
d1
f <- ggplot(biomInc[which(biomInc$id==7),],
            aes(x=age,y=totInc,group=id,colour=id)) +geom_line()
# CONCLUSION: none of the curve matches "my" conversion. ID #7 seems the most
# reasonable but still have "blips"---------------------------------------

# can "fix" the fisrt two lines of mine?
BStotInc[1,3] <- BStotInc[2,3]/2
g <- ggplot(BStotInc,aes(x=age, y=totInc)) +geom_line()
# do we need to put a 0 at age 0?
# proportions at year 5 (first year with non-negatives in the pools)
vals <- c(0.029340666,-0.017150972,0.005576922)
bSpruceInc[1,2] <- vals[1]
bSpruceInc[1,3] <- vals[2]
bSpruceInc[1,4] <- vals[3]
# run this now that I know it is fixed and
#1. re plot the black spruce to make sure they are all the same
#2 re check the growth
# these are the ones I changed
BSid <- c(8,9,29,30,50,51,71,72,92,93)
checkRun <- unique(spadesCBMout$level3DT[growth_curve_id %in% BSid,growth_curve_id])
goodBS <- biomInc[id %in% checkRun,]
h <-  ggplot(goodBS,aes(x=age, y=totInc,group=id, colour=id)) +geom_line()
## OK - there is one curve. Pixel Groups using those curves should work

checkRun
BSpg <- spadesCBMout$level3DT[which(spadesCBMout$level3DT$growth_curve_id %in% checkRun),7]

### CONCLUSION: either there is something that I am missing in how each year the
### pools get incremented or there is an error in the matrix multiplications or
### hashing...

# one last thing to check: are all these stands old?
BSages <- spadesCBMout$level3DT[which(spadesCBMout$level3DT$growth_curve_id %in% checkRun),ages]
# looks at the one at age 33
youngBS <- spadesCBMout$level3DT[which(spadesCBMout$level3DT$growth_curve_id %in% checkRun&ages==33),]
pg <- youngBS$pixelGroup
checkGrowth(pg)
# swmerch       swfol     swother 
# -0.39287095  0.06407009 -0.12866206 
## STill not...