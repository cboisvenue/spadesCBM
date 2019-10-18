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

## END DISTURBANCE SWMERCH = 0 -----------------------------------------

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

################# MATCH ########################
# growth at year dist matches the growth at age 53
# SoftwoodMerch SoftwoodFoliage SoftwoodOther 
# 0.4470938      0.04141701     0.1161637

### ANOTHER WEIRD THING ###
#the sequence of c transfers should be: growth1, domturnover, bioturnover,
#overmature, growth2 , domDecay, slow decay, slow mixing there seems to be ONLY
#growth applied after the disturbance we expected the growth- biomturnover to be
#happening? BUT it seems that turnover is a calculation not a
#substraction...maybe... no domturnover and no overmature decline, as expected
#(allProcesses$OvermatureDecline[[1]] is an identity matrix), no domDecay, no
#slowDecay, and no slow mixing growt1-bioturnover+growth2

# these are the values for the turnover these do not change
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

# this is what we expected....
# growth - bioturnover
merchOut <- 0.4470938-0.0060
folOut <- 0.04141701-0.0500
otherOut <- 0.1161637-0.0075
stand1DistYr <- stand1Dist[simYrs==2050,]
# SWMERCH SWFOL SWOTher summary: growth is taken from the age=53 (prior to
# disturbance) and no other transfers happen in those three
# pools. Is that tru for the previous year? YES-------------------------------------------------

# check ROOTS swCoarseRoots and FineRoots pools------------------------------------------
# given the value of swmerch swFol and swOther, can I calculate the roots growth
# pools, SoftwoodCoarseRoots	SoftwoodFineRoots
# from G:\RES_Work\Work\SpaDES\spadesCBM\Matrices.xlsx and from
# G:\RES_Work\Work\SpaDES\spadesCBM\workingOutNPP.xlsx

# these are the parameters, they are all the same for all spatial units

rootp <- spadesCBMout$cbmData@rootParameters[1,2:7]
# this function gives the root biomass, not the root carbon, given the 
calcRoots <- function(Msw = 0 ,Fsw = 0 ,Osw = 0 ,Mhw = 0 ,Fhw = 0 ,Ohw = 0 ){
  rb_hw_a <- 1.576
  rb_sw_a <- 0.222
  rb_hw_b <- 0.615
  frp_a <- 0.0720
  frp_b <- 0.354
  frp_c <- -0.06021195 # same as -1/16.608
  Tsw <- rb_sw_a*(Msw+Fsw+Osw)*2
  Thw <- rb_hw_a*((Mhw+Fhw+Ohw)*2)^rb_hw_b
  Pfine <- frp_a+(frp_b*exp(frp_c*(Thw+Tsw)))
  
  FineRoothw <- Thw*Pfine
  FineRootsw <- Tsw*Pfine
  CoarseRoothw <- Thw*(1-Pfine)
  CoarseRootsw <- Tsw*(1-Pfine)
  rootPools <- as.numeric(c(CoarseRootsw,FineRootsw,CoarseRoothw,FineRoothw))
  
  return(rootPools)
}
## note: the Msw, Fsw and Osw needs to be this years' not the previous like the growth
std1RootsDistYr <- calcRoots(Msw = 0.4792917, Fsw = 0.04512786,Osw = 0.1272211)
stand1DistYr[,c(8,9,13,14)]==std1RootsDistYr
# they are not equal
# SoftwoodCoarseRoots SoftwoodFineRoots HardwoodCoarseRoots HardwoodFineRoots
# [1,]               FALSE             FALSE                TRUE              TRUE
# but their sums are
sum(std1RootsDistYr/2)
#[1] 0.1342377
sum(stand1DistYr[,c(8,9,13,14)])
#[1] 0.1342377
# proportions are different
std1RootsDistYr/2/0.1342377
#[1] 0.5796767 0.4203236 0.0000000 0.0000000
stand1DistYr[,c(8,9,13,14)]/0.1342377
# SoftwoodCoarseRoots SoftwoodFineRoots HardwoodCoarseRoots HardwoodFineRoots
# 1:            0.927686        0.07231426            
########### NOT SURE HOW PROPORTIONS VARY...EQUATION IN CPP (lines 551-552)
########### match the equations in my function##################

# apply turnover?  allProcesses$BioTurnover$`5` is below
# 5 is swCoarse, 6 swFine, 14, belowgroundFastSoil, 15, MediumSoil. Does that
# match the parameters in the cbmData? are these already multiplied by the root
# ag/bg spilt? and by the ag fast.branch split?
#       [31,]   5  14 0.0100
#       [32,]   5  15 0.0100
#       [33,]   6  12 0.3205
#       [34,]   6  13 0.3205

spadesCBMout$cbmData@turnoverRates[spadesCBMout$cbmData@turnoverRates[,1]==5,]
# EcoBoundaryID    SoftwoodFoliageFallRate    HardwoodFoliageFallRate 
# 5.000                      0.050                      0.950 
# StemAnnualTurnoverRate SoftwoodBranchTurnoverRate HardwoodBranchTurnoverRate 
# 0.006                      0.030                      0.030 
# CoarseRootAGSplit         CoarseRootTurnProp            FineRootAGSplit 
# 0.500                      0.020                      0.500 
# FineRootTurnProp     OtherToBranchSnagSplit     BranchSnagTurnoverRate 
# 0.641                      0.250                      0.100 
# StemSnagTurnoverRate 
# 0.032 
# These turnover parameters all match
turnRoots <- c(0.02,0.641,0,0)
std1RootsDistYr-(std1RootsDistYr*turnRoots)

# non dist?
rootsNoDist <- calcRoots(0.5813704,0.06521656,0.1817496)
### NO ###

# END ROOTS------------------------------------------------------------


## Products?----------------------------------------
stand1Prod <- stand1Dist$Products[2]-stand1Dist$Products[1]
# 0.8 of merch goes to product
stand1Prod
(stand1Dist[1,5]*0.8)
# yes that is as expected---------------------------

## Can I folow growth with no disturbance?-------------------------------------
stand1g <-  rcbmPools[standindex==1 & simYrs %in% 2015:2020,]
# growth_increments[19:24,]
#     id age   swmerch      swfol   swother hwmerch hwfol hwother
# [1,]  1  18 0.5480835 0.06792647 0.1987011       0     0       0
# [2,]  1  19 0.5598350 0.06850838 0.2004115       0     0       0
# [3,]  1  20 0.5703324 0.06894299 0.2016636       0     0       0
# [4,]  1  21 0.5796093 0.06923852 0.2024833       0     0       0
# [5,]  1  22 0.5876994 0.06940285 0.2028951       0     0       0
# [6,]  1  23 0.5946368 0.06944349 0.2029229       0     0       0
gvect <- c(0.5598350,0.06850838,0.2004115)
groot <- calcRoots(Msw = stand1g[stand1g$age==20,5],Fsw = stand1g[stand1g$age==20,6],Osw = stand1g[stand1g$age==20,7])
## HERE THERE seems to be something wrong with the roots calc...
# divide the root calculation by two since it gives us biomass and this is carbon
stand1.20 <-c((stand1g[stand1g$age==19,5:7]+gvect),groot/2)
stand1.20
stand1g[stand1g$age==20,5:9]

# turnover does not take carbon away from the growth, it is a function of how
# much there is in the growth pools

# did not check dom values yet

# AboveGroundVFSoil---------------------------------------------------------
# Following the transfers in d214
# gets put to 0 by this disturbance (% transfer add to 1)
# In annual growth cycle, this pools receives from foliage and fine roots
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
#       [27,]   2  19 0.0060 from merch to stemSnags
#       [28,]   3  12 0.0500 from foliage to AGVF
#       [29,]   4  20 0.0075 from other to branchSbag
#       [30,]   4  14 0.0225 from other to AGfast
#       [31,]   5  14 0.0100 from coarse to AGfast
#       [32,]   5  15 0.0100 from coarse to BGfast
#       [33,]   6  12 0.3205 from fine roots to AGVF
#       [34,]   6  13 0.3205 from fine roots to BGVF
#       [35,]   7  21 0.0060
#       [36,]   8  12 0.9500 from HW foliage to AGVF
#       [37,]   9  22 0.0075
#       [38,]   9  14 0.0225 from other to AGfast
#       [39,]  10  14 0.0100 from coarse to AGfast
#       [40,]  10  15 0.0100 from coarse to BGfast
#       [41,]  11  12 0.3205 from HW fine roots to AGVF
#       [42,]  11  13 0.3205 from HW fine roots to BGVF

# inputs post disturbance should be: swFoliage*proportionTransfered +
# swFroots*proportionTransfered
vfCalc <- stand1Dist[2,6]*0.0500+stand1Dist[2,9]*0.3205
## note: this seems to give biomass and needs to be divided by two
# close enough??
(vfCalc/2) - stand1Dist[2,15]

# Non dist year
vfNoDist <- #0.017991723
  # in from bio turn
  (stand1Dist[3,6]*0.0500+stand1Dist[3,9]*0.3205)/2 +#0.01395552
  # what stays from the decay
  0.919722348*stand1Dist[2,15] #0.002470445
# END AboveGroupVFsoil-------------------------------------------------

# BelowGroundVeryFastSoil------------------------------------------
# 100% stays in itself in disturbance stand1Dist[1,16] + 0.33 foliage + 0.8 fine
# roots + 0.8 AGVFsoil PLUS the turnover? MINUS decay?

# this is Domturnover
# [26,]  26  26 1.000
# [27,]  19  19 0.968 from stemSnag to stemSnag
# [28,]  19  16 0.032 from stemSnags to medSoil
# [29,]  20  20 0.900
# [30,]  20  14 0.100 from branchSnag to AGfast
# [31,]  21  21 0.968 from stemSnag to stemSnag
# [32,]  21  16 0.032 from stemSnags to medSoil
# [33,]  22  22 0.900
# [34,]  22  14 0.100  from branchSnag to AGfast

# this is dom decay
# [27,]  12  12 0.919722348
# [28,]  12  17 0.014851366 from AGVFsoil to AGSlow
# [29,]  12  23 0.065426286 from AGVFsoil to CO2
# [30,]  13  13 0.826312482 from BGVFsoil to BGVFsoil
# [31,]  13  18 0.029526881 from BGVFsoil to BelowGroundSlowSoil
# [32,]  13  23 0.144160637 from BGVFsoil to CO2
# [33,]  14  14 0.950151682 from  AGfast to AGfast
# [34,]  14  17 0.008474215 from AGFfast to AGSlow
# [35,]  14  23 0.041374103 from AGFfast to CO2
# [36,]  15  15 0.950151682 from BGfast to BGfast
# [37,]  15  18 0.008474215
# [38,]  15  23 0.041374103 from BGfast to CO2
# [39,]  16  16 0.987008174 from medSoil to medSoil
# [40,]  16  17 0.002208611 from medSoil to AGSlow
# [41,]  16  23 0.010783216 from medSoil to CO2
# [42,]  19  19 0.993504087
# [43,]  19  17 0.001104305 from stemSnag to AGSlow
# [44,]  19  23 0.005391608 from stemSnag to CO2
# [45,]  20  20 0.975075841 from branchSnag to branchSnag
# [46,]  20  17 0.004237107 from branchSnag to AGSlow
# [47,]  20  23 0.020687051 from branchSnag to CO2
# [48,]  21  21 0.993504087
# [49,]  21  17 0.001104305 from stemSnag to AGSlow
# [50,]  21  23 0.005391608 from stemSnag to CO2
# [51,]  22  22 0.975075841
# [52,]  22  17 0.004237107 from branchSnag to AGSlow
# [53,]  22  23 0.020687051 from branchSnag to CO2

# postDist value 8.037873
bgvfCalc <- 
  # dist says 100% stays in
  stand1Dist[1,16]+#2.44021
  # this is the "in" from the disturbance part prev yr would make sense
  (0.33*stand1Dist[1,6]+0.8*stand1Dist[1,9]+0.8*stand1Dist[1,15])/2+ #3.668852
  # this is "in" from turnover # pre or post? 0.9354949 or 0.01327415? pre makes sense/2
  (0.3205*stand1Dist[2,9])/2 + #0.2643128
  # decay on the previous yr amount (1.312812)? or on this year to date (4.451575)?
  stand1Dist[1,16]*0.826312482 #1.973156
### POST DISTURBANCE DOES NOT WORK BUT THE VALUES ARE MUCH CLOSER IF I DO NOT ADD THE TURNOVER

# try a non dist year
# ALL happens in the current year
# turnover (from fine roots), no DOM turnover
  bgvfNoDist <- 
    # turnover (from fine roots), no DOM turnover
    (0.3205*stand1Dist[3,9])/2+
  # stays in from Dom decay
    stand1Dist[2,16]*0.826312482 ###this is what stays try what goes out####
## Got this one
  
## NOPE-------------------------------------------------------------

## AboveGroundFastSoil----------------------------------------------

  # with disturbance this is the simulated 7.974675
agfCalc <- 
    # dist: receives 0.33 from other + 0.33 from coarse + 0.8*AGfast (itself) + 
    # 0.8 BGfast + 0.8 branchSnag
    (0.33*stand1Dist[1,7]+0.33*stand1Dist[1,8]+0.8*stand1Dist[1,17]+
       0.8*stand1Dist[1,18]+0.8*stand1Dist[1,23])/2+ #4.195478
    
    # bioturnover "in"
    (0.0225**stand1Dist[2,7]+0.0100*stand1Dist[2,8])/2+ #0.03128336
    # DomTurnover
    (0.1*stand1Dist[2,23])/2 + #0.02101527
    # stays in from DomDecay
    0.950151682*stand1Dist[1,17] #3.228098
# calc-simulated
  agfCalc-stand1Dist[2,17]
  
    
  # no dist 7.581640
  agfNoDist <- 
    # THis would be the logical way but that does not work
    # bioturnover "in"
    (0.0225**stand1Dist[3,7]+0.0100*stand1Dist[3,8])/2+ #0.2514229
    # DomTurnover "in"
    (0.1*stand1Dist[3,23])/2 +
    # DomDecay
    0.950151682*stand1Dist[2,17]
  ## But this is closer MAKES NO SENSE
  agfNoDist*0.950151682

  agfNoDist-stand1Dist[3,17]
## AboveGroudFastSoil NO MATCH--------------------------------------
  
### BelowGroundFastSoil 0.0006341294-------------------------------------------------------
  bgfCalc <- 
    # receives NOTHING from disturbance...
    # THis would be the logical way but that does not work
    # # bioturnover "in"
    # (0.0100*stand1Dist[2,8])/2+#0.0006673981
    # # NO DomTurnover
    # # DomDecay
    # 0.950151682*stand1Dist[1,18]
    # THIS works:
    (0.0100*stand1Dist[2,8])/2 * 0.950151682
  
bgfCalc -   stand1Dist[2,18]

bgfNoDist <-   #0.0017448799   
  # bioturnover "in"
  (0.0100*stand1Dist[3,8])/2+
  # NO DomTurnover
  # DomDecay
  0.950151682*stand1Dist[2,18]
  
bgfNoDist-  stand1Dist[3,18]
# ok
#### BelowGroundFastSoil-------------------------------------------------------

### MediumSoil--------------------------
medSoil <- #3.2758072
  # in disturbance prop that stays in
  (0.899999976*stand1Dist[1,19]+0.029999999*stand1Dist[1,5]+0.899999976158*stand1Dist[1,22])/2 + #1.659463 
  # No bioturnover
  # Dom turnover  from stemSnags to medSoil
  (0.032*stand1Dist[2,22])/2 + #2.285656e-05
  # dom decay
  0.987008174*stand1Dist[1,19] # this could be0.899999976*stand1Dist[1,18] *prop
  
medSoil-stand1Dist[2,19]

medSoilNoDist <-   #3.2332936
  # Dom turnover  from stemSnags to medSoil
  (0.032*stand1Dist[2,22])/2 +
  # dom decay
  0.987008174*stand1Dist[2,19] #

medSoilNoDist - stand1Dist[3,19]
### MediumSoil--------------------------

## AGslow ------------------------------------------
# this is slow decay
# spadesCBMout$allProcesses$SlowDecay$`26`
# [27,]  17  17 0.996607987 from AGslow to AGslow
# [28,]  17  23 0.003392013 from AGslow to CO2
# [29,]  18  18 0.996700000
# [30,]  18  23 0.003300000 from BGslow to CO2

# spadesCBMout$allProcesses$SlowMixing$`1`
# [27,]  17  18 0.006 from AGslow to BGslow
# [28,]  17  17 0.994 from AGslow to AGslow

agSlow <- #0.07776605 
  # goes to 0 and gets nothing from disturbance
  # no bioturnover, no dom turnover
  # Dom decay
  ((0.014851366*stand1Dist[1,15]+0.008474215*stand1Dist[1,17]+0.002208611*stand1Dist[1,19]+
  0.001104305*stand1Dist[1,22] + 0.004237107*stand1Dist[1,23])/2)*  #0.06450022
  #SlowDecay - this much would stays but there is nothing left # SlowMixing - this is how much stays
  0.994
  
agSlow-stand1Dist[2,20]

agSlowNoDist <- #0.15148939
  (0.014851366*stand1Dist[2,15]+0.008474215*stand1Dist[2,17]+0.002208611*stand1Dist[2,19]+
       0.001104305*stand1Dist[2,22] + 0.004237107*stand1Dist[2,23])/2+ #0.03742877
     #SlowDecay - this is how much stays # SlowMixing - this is how much stays
     stand1Dist[2,20]*0.996607987 + stand1Dist[2,20]*0.994

agSlowNoDist-stand1Dist[3,20]
## END AGslow ------------------------------------------

## BGslow ------------------------------------------------------------------------
bgSlow <- #92.82063
  # dist gets ALL of itself and ALL of agSlow
  stand1Dist[1,21]+stand1Dist[1,20]* ##already above
  0.996700000
bgSlow-stand1Dist[2,21]

bgSlowNoDist <- #92.75221
  # slow decay
  stand1Dist[2,21]*0.996700000 +
  # slow mixing
  stand1Dist[2,20]*0.006
  
bgSlowNoDist-stand1Dist[3,21]
## END BGslow --------------------------------------------------------------------

#stemSnag-------------------------------------
stemSnag <- #0.001428535
  #dist goes to 0
  # bio turnover from merch
  ((0.0060*stand1Dist[2,5])/2) * #0.001437875
  # dom turnover of itself (but there is nothing left) and prop that stays in decay
  0.993504087

stemSnag-stand1Dist[2,22]

stemSnagNoDist <- #0.004535156
  # bio turnover from merch
  (((0.0060*stand1Dist[3,5])/2) + #0.001437875
  # dom turnover of itself and prop that stays in from decay
  (stand1Dist[2,22]*0.968))+ #0.001382822
  stand1Dist[2,22]*0.993504087 #0.001419255
# END stemSnag-------------------------------------

# branchSnag----------------------------------------------------
branchSnag <- #0.0004651883
  # dist goes to 0, nothing goes in
  # bio turnover from other
  # 0.900 stays in from dom turnover but there is nothing left to stay
  # this much stays from the decay
  (0.0075*stand1Dist[2,7])/2 * 0.975075841
branchSnag-  stand1Dist[2,23]

branchSnagNoDist <- #0.0015379963
  # bio turnover from other
  (0.0075*stand1Dist[3,7])/2 +#0.0006815609
  # 0.900 stays in from dom turnover
  stand1Dist[2,23]*0.900 + #0.0004186694
  # stays from decay
  stand1Dist[2,23]*0.975075841
# END branchSnag----------------------------------------------------

# CO2--------------------------------------------------------------------------
# the absolute does not matter...the delta matters
d214[sinkName=="CO2",]
# disturbance_matrix_id source_pool_id              sourceName sink_pool_id sinkName
# 1:                   214              1           SoftwoodMerch           22      CO2
# 2:                   214             10       HardwoodFineRoots           22      CO2
# 3:                   214             11 AboveGroundVeryFastSoil           22      CO2
# 4:                   214             13     AboveGroundFastSoil           22      CO2
# 5:                   214             14     BelowGroundFastSoil           22      CO2
# 6:                   214             15              MediumSoil           22      CO2
# 7:                   214             18        SoftwoodStemSnag           22      CO2
# 8:                   214             19      SoftwoodBranchSnag           22      CO2
# 9:                   214              2         SoftwoodFoliage           22      CO2
# 10:                   214             20        HardwoodStemSnag           22      CO2
# 11:                   214             21      HardwoodBranchSnag           22      CO2
# 12:                   214              3           SoftwoodOther           22      CO2
# 13:                   214              4     SoftwoodCoarseRoots           22      CO2
# 14:                   214              5       SoftwoodFineRoots           22      CO2
# 15:                   214              6           HardwoodMerch           22      CO2
# 16:                   214              7         HardwoodFoliage           22      CO2
# 17:                   214              8           HardwoodOther           22      CO2
# 18:                   214              9     HardwoodCoarseRoots           22      CO2
# proportion
# 1:      0.153
# 2:      0.180
# 3:      0.180
# 4:      0.180
# 5:      0.180
# 6:      0.090
# 7:      0.090
# 8:      0.180
# 9:      0.603
# 10:      0.090
# 11:      0.180
# 12:      0.603
# 13:      0.603
# 14:      0.180
# 15:      0.153
# 16:      0.603
# 17:      0.603
# 18:      0.603
CO2 <- #22793.59 difference 22774.79-22793.59 <- -18.8
  # dist from merch, fineroots, agvfSoil, agfSoil, bgfSoil, medSoil,stemSnag, branchSnag, foliage, other
  stand1Dist[1,5]*0.153 +
  stand1Dist[1,9]*0.180 +
  stand1Dist[1,15]*0.180 +
  stand1Dist[1,17]*0.180 +
  stand1Dist[1,18]*0.180 +
  stand1Dist[1,19]*0.090 +
  stand1Dist[1,22]*0.090 +
  stand1Dist[1,23]*0.180 +
  stand1Dist[1,6]*0.603 +
  stand1Dist[1,7]*0.603 +
  stand1Dist[1,8]*0.603 +
  #16.7111
  
  # dom decay AGVF, BGVF, AGF, BGF, medSoil, 
  0.065426286*stand1Dist[2,15] +
  0.144160637*stand1Dist[2,16] +
  0.041374103*stand1Dist[2,17] +
  0.041374103*stand1Dist[2,18] +
  0.010783216*stand1Dist[2,19] +
  0.005391608*stand1Dist[2,22] +
  0.020687051*stand1Dist[2,23] + #0.9580484
  
  # slow decay from AG and GBSlow
  0.003392013 * stand1Dist[2,20] +
  0.003300000 * stand1Dist[2,21] #0.3066058

stand1Dist$CO2[2]-stand1Dist$CO2[1]-CO2
  
CO2NoDist <- #22795.43 22795.43-22793.59 <- 1.84
  # dom decay AGVF, BGVF, AGF, BGF, medSoil, 
  0.065426286*stand1Dist[2,15] +
  0.144160637*stand1Dist[2,16] +
  0.041374103*stand1Dist[2,17] +
  0.041374103*stand1Dist[2,18] +
  0.010783216*stand1Dist[2,19] +
  0.005391608*stand1Dist[2,22] +
  0.020687051*stand1Dist[2,23] + #0.9580484
  
  # slow decay from AG and GBSlow
  0.003392013 * stand1Dist[2,20] +
  0.003300000 * stand1Dist[2,21] #0.3066058
# END CO2------------------------------------------------------------

# CH4--------------------------------------------------------------------------
# the absolute does not matter...the delta matters
d214[sinkName=="CH4",]
# disturbance_matrix_id source_pool_id              sourceName sink_pool_id sinkName proportion
# 1:                   214              1           SoftwoodMerch           23      CH4     0.0017
# 2:                   214             10       HardwoodFineRoots           23      CH4     0.0020
# 3:                   214             11 AboveGroundVeryFastSoil           23      CH4     0.0020
# 4:                   214             13     AboveGroundFastSoil           23      CH4     0.0020
# 5:                   214             14     BelowGroundFastSoil           23      CH4     0.0020
# 6:                   214             15              MediumSoil           23      CH4     0.0010
# 7:                   214             18        SoftwoodStemSnag           23      CH4     0.0010
# 8:                   214             19      SoftwoodBranchSnag           23      CH4     0.0020
# 9:                   214              2         SoftwoodFoliage           23      CH4     0.0067
# 10:                   214             20        HardwoodStemSnag           23      CH4     0.0010
# 11:                   214             21      HardwoodBranchSnag           23      CH4     0.0020
# 12:                   214              3           SoftwoodOther           23      CH4     0.0067
# 13:                   214              4     SoftwoodCoarseRoots           23      CH4     0.0067
# 14:                   214              5       SoftwoodFineRoots           23      CH4     0.0020
# 15:                   214              6           HardwoodMerch           23      CH4     0.0017
# 16:                   214              7         HardwoodFoliage           23      CH4     0.0067
# 17:                   214              8           HardwoodOther           23      CH4     0.0067
# 18:                   214              9     HardwoodCoarseRoots           23      CH4     0.0067

CH4 <- # same pools as CO2:
  stand1Dist[1,5]*0.0017 +
  stand1Dist[1,9]*0.0020 +
  stand1Dist[1,15]*0.0020 +
  stand1Dist[1,17]*0.0020 +
  stand1Dist[1,18]*0.0020 +
  stand1Dist[1,19]*0.0010 +
  stand1Dist[1,22]*0.0010 +
  stand1Dist[1,23]*0.0020 +
  stand1Dist[1,6]*0.0067 +
  stand1Dist[1,7]*0.0067+
  stand1Dist[1,8]*0.0067  #0.1856789
CH4+stand1Dist$CH4[1]
# END CH4 ----------------------------------------------------------------------
d214[sinkName=="CO",]
# disturbance_matrix_id source_pool_id              sourceName sink_pool_id sinkName proportion
# 1:                   214              1           SoftwoodMerch           24       CO     0.0153
# 2:                   214             10       HardwoodFineRoots           24       CO     0.0180
# 3:                   214             11 AboveGroundVeryFastSoil           24       CO     0.0180
# 4:                   214             13     AboveGroundFastSoil           24       CO     0.0180
# 5:                   214             14     BelowGroundFastSoil           24       CO     0.0180
# 6:                   214             15              MediumSoil           24       CO     0.0090
# 7:                   214             18        SoftwoodStemSnag           24       CO     0.0090
# 8:                   214             19      SoftwoodBranchSnag           24       CO     0.0180
# 9:                   214              2         SoftwoodFoliage           24       CO     0.0603
# 10:                   214             20        HardwoodStemSnag           24       CO     0.0090
# 11:                   214             21      HardwoodBranchSnag           24       CO     0.0180
# 12:                   214              3           SoftwoodOther           24       CO     0.0603
# 13:                   214              4     SoftwoodCoarseRoots           24       CO     0.0603
# 14:                   214              5       SoftwoodFineRoots           24       CO     0.0180
# 15:                   214              6           HardwoodMerch           24       CO     0.0153
# 16:                   214              7         HardwoodFoliage           24       CO     0.0603
# 17:                   214              8           HardwoodOther           24       CO     0.0603
# 18:                   214              9     HardwoodCoarseRoots           24       CO     0.0603
CO <- # same pools as CO2:
  stand1Dist[1,5]*0.0153 +
  stand1Dist[1,9]*0.0180 +
  stand1Dist[1,15]*0.0180 +
  stand1Dist[1,17]*0.0180 +
  stand1Dist[1,18]*0.0180 +
  stand1Dist[1,19]*0.0090 +
  stand1Dist[1,22]*0.0090 +
  stand1Dist[1,23]*0.0180 +
  stand1Dist[1,6]*0.0603 +
  stand1Dist[1,7]*0.0603+
  stand1Dist[1,8]*0.0603  #0.1856789
CO+stand1Dist$CO[1]
# END CO---------------------------------------------------------------------------
