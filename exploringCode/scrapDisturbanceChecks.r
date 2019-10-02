# try2 - check disturbances Determining if disturbances are correctly applied to
# pixel groups
#
# went back to the original code RCBM: will check if the disturbance is applied
# correctly. Ran the code, simulations between 2000:2052, for 4 stands, with
# disturbance (DMID 214) scheduled for 2050. Note that the historical AND
# lastpass disturbance is also 214

# C:\Celine\GitHub\RCBM
# output.csv is the spinup
# output1.csv are the pools

# cbmTables$disturbance_matrix[which(cbmTables$disturbance_matrix$id=="214"),]
# id                                         name
# 214 214 Salvage uprooting and burn for Boreal Plains
# description
# 214 Salvage uprooting and burn for Boreal Plains
# this is used in spinup as well as in a disturbance in 2050 for all stands
# spinup returnIntervals <- c(200,110,120,130)

library(data.table)

# 1.check the disturbances in the simulations
# read in pools results from RCBM
rcbmPools <- fread("C:/Celine/GitHub/RCBM/output1.csv")
simYrs <- sort(rep(c(2000:2052),times=4))
rcbmPools[,simYrs:=simYrs]

# get a year before and a year after the disturbance
distPools <- rcbmPools[simYrs %in% 2049:2051,]

# look at one stand
stand1Dist <- distPools[standindex==1,]

# diffinv() performs the inverse function
stand1deltaDist <- diff(as.matrix(stand1Dist[,SoftwoodMerch:Products]))

# SoftwoodMerch
# proportion diff -25.21403 on the c it started with 25.6611205
# this has been disturbed and has grown
pstand1 <- -25.21403/25.6611205
#-0.9825771


#### Step through the carbon-transfers: NEED TO MAKE THIS A FUNCTION
# the processes in order (according to the R scripts) are: 
# disturbance, growth1, domturnover, bioturnover, overmature, 
# growth2 , domDecay, slow decay, slow mixing
## just one pool SWmerch
swMerch1before <- stand1Dist[simYrs==2049,SoftwoodMerch]

## DISTURBANCE SWMERCH------------------------------------
# check that all the dist matrices are the same between spadesCBM and RCBM
# true for spadesCBM 
# spadesCBMout$cbmData@disturbanceMatrixValues[which(spadesCBMout$cbmData@disturbanceMatrixValues[,1]==214),]
#       disturbance_matrix_id source_pool_id sink_pool_id proportion
# [1,]                   214              1           15     0.0300
# [2,]                   214              1           22     0.1530
# [3,]                   214              1           23     0.0017
# [4,]                   214              1           24     0.0153
# [5,]                   214              1           26     0.8000
# [6,]                   214              2           12     0.3300

# note the +1 to all the pool #s
# spadesCBMout$allProcesses$Disturbance$`214`
#       row col  value
# [1,]   2  16 0.0300
# [2,]   2  23 0.1530
# [3,]   2  24 0.0017
# [4,]   2  25 0.0153
# [5,]   2  26 0.8000
# [6,]   3  13 0.3300

# true for RCBM
# allProcesses$Disturbance$`214`
#       row col  value
# [1,]   2  16 0.0300
# [2,]   2  23 0.1530
# [3,]   2  24 0.0017
# [4,]   2  25 0.0153
# [5,]   2  26 0.8000
# [6,]   3  13 0.3300
# [7,]   3  23 0.6030
# [8,]   3  24 0.0067
# [9,]   3  25 0.0603

# in RCBM "allProcesses" == spadesCBM$allProcesses in spadesCBM
swMerchOut <- spadesCBMout$allProcesses$Disturbance$`214`[which(spadesCBMout$allProcesses$Disturbance$`214`[,1]==2),]
# check if it gets all removed
sum(swMerchOut[,3])
# YES, all is removed: swMerch is at 0 at this point

#swFoliage? swOther?
swFolOut <- spadesCBMout$allProcesses$Disturbance$`214`[which(spadesCBMout$allProcesses$Disturbance$`214`[,1]==3),]
sum(swFolOut[,3])
swOtherOut <- spadesCBMout$allProcesses$Disturbance$`214`[which(spadesCBMout$allProcesses$Disturbance$`214`[,1]==4),]
sum(swOtherOut[,3])
## ALL AT 0 after disturbances

## END DISTURBANCE -----------------------------------------

# SWMERCH SWFOL SWOTher-------------------------------------------------
# using the RCBM simulations, I am trying to track what happens to the carbon
# from one year to the next in a disturbance year. Here I look at the three
# pools that grow in stand1: swMerch, swFol, swOther.

# RCBM growth curve for stand 1 is in spu 26 and is for species_id 29 - which is Balsam fir in CBM
# we are not modelling spu 26 in our spadesCBM simulations (no balsam fir in
# study area and not in spu 26), hence no equivalent in $level3DT
#spadesCBMout$level3DT[which(spadesCBMout$level3DT$rasterSps==1),]# & spadesCBMout$)]
# spadesCBM matches RCBM? CAN'T CHECK

# hand copy the values from the RCBM simulations for growth1 for that simulation year
# allProcesses$Growth1[[1]] is equal to allProcesses$Growth2[[1]]
#       row col      value
# [1,]   1   2 0.07442192
# [2,]   1   3 0.01348630
# [3,]   1   4 0.03742821
# [4,]   1   6 0.01138443
# [5,]   1   5 0.01644025
# [6,]   1   1 1.00000000
# [7,]   2   2 1.00000000
#  allProcesses$Growth1[[1]]
#       row col      value
# [1,]   1   2 0.07442192
# [2,]   1   3 0.01348630
# [3,]   1   4 0.03742821
# [4,]   1   6 0.01138443
# [5,]   1   5 0.01644025
# [6,]   1   1 1.00000000
# [7,]   2   2 1.00000000

# gcids <- c(1,2,3,101) - so gcid 1 for stand1
# growth at age 0 - year of disturbance
# head(growth_increments)
#       id age    swmerch      swfol    swother hwmerch hwfol hwother
# [1,]  1   0 0.04368955 0.01010729 0.02596237       0     0       0
# [2,]  1   1 0.10207861 0.02008869 0.05452850       0     0       0
# [3,]  1   2 0.14884385 0.02697261 0.07485641       0     0       0

# given the column numbering checked in
# G:\RES_Work\Work\SpaDES\spadesCBM\distMatricesRowColExplicit.xlsx, row1 and
# col2 value gives the increment to swmerch. This should divided by 2 for the $Growth1 and $Growth2
#### ERROR THIS DOES NOT MATCH###
0.04368955/2
#[1] 0.02184478

# maybe the growth is calculated from the age of the previous year (pre-disturbance)?
stand1Dist$age
#[1] 53  1  2
# growth at age 53
# growth_increments[52:57,]
#       id age   swmerch      swfol   swother hwmerch hwfol hwother
# [1,]  1  51 0.4686883 0.04388448 0.1235044       0     0       0
# [2,]  1  52 0.4579469 0.04264688 0.1198167       0     0       0
# [3,]  1  53 0.4470938 0.04141701 0.1161637       0     0       0
# [4,]  1  54 0.4361543 0.04019670 0.1125506       0     0       0
# [5,]  1  55 0.4251526 0.03898764 0.1089822       0     0       0
# [6,]  1  56 0.4141119 0.03779138 0.1054628       0     0       0

# growth at year dist matches the growth at age 53
# SoftwoodMerch SoftwoodFoliage SoftwoodOther 
# 0.4470938      0.04141701     0.1161637

### ANOTHER WEIRD THING ###
#the sequence of c transfers should be: growth1, domturnover, bioturnover,
#overmature, growth2 , domDecay, slow decay, slow mixing 
# there seems to be ONLY growth applied after the disturbance 
# we expected the growth- biomturnover to be  happening
#no domturnover and no overmature decline, as expected
#(allProcesses$OvermatureDecline[[1]] is an identity matrix), no domDecay, no
#slowDecay, and no slow mixing growt1-bioturnover+growth2

# these are the values for the turnover
# allProcesses$BioTurnover$`5`
#       row col  value
# [1,]   1   1 1.0000
# [2,]   2   2 1.0000
# [3,]   3   3 1.0000
# [4,]   4   4 1.0000
# [5,]   5   5 1.0000
# [6,]   6   6 1.0000
#       ...
#       [27,]   2  19 0.0060
#       [28,]   3  12 0.0500
#       [29,]   4  20 0.0075
#       [30,]   4  14 0.0225
#       [31,]   5  14 0.0100
#       [32,]   5  15 0.0100
#       [33,]   6  12 0.3205
#       [34,]   6  13 0.3205
#       [35,]   7  21 0.0060
#       [36,]   8  12 0.9500
#       [37,]   9  22 0.0075
#       [38,]   9  14 0.0225
#       [39,]  10  14 0.0100
#       [40,]  10  15 0.0100
#       [41,]  11  12 0.3205
#       [42,]  11  13 0.3205
# growth - bioturnover
merchOut <- 0.4470938-0.0060
folOut <- 0.04141701-0.0500
otherOut <- 0.1161637-0.0075
stand1DistYr <- stand1Dist[simYrs==2050,]


# allProcesses$DomTurnover$`5`
# [27,]  19  19 0.968
# [28,]  19  16 0.032
# [29,]  20  20 0.900
# [30,]  20  14 0.100
# [31,]  21  21 0.968
# [32,]  21  16 0.032
# [33,]  22  22 0.900
# [34,]  22  14 0.100


# 2.check disturbances in the spinup