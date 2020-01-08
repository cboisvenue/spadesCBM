#check that the mySpuDmid is ok 
# CBoisvenue Sept. 25, 2019 
#$mySpuDmids is built by the user. It links the DMIDs that are default in Canada
#for CBM runs with the disturbances in your simulations. There are 426 DMIDs
#described here spadesCBMout$cbmData@disturbanceMatrix

# sim$mySpuDmids is build in the SpadesCBMinputs module
# lines 310-343
# each user will have to ensure this matches expectations
# there are functions that help to build mySpuDmids in spadesCBMextraFunctions
# load these functions

# disturbances are in a raster with values from 0 to 5, with 0 being not
# disturbed, and 1:5 defined in DisturbanceLoopup.csv used in the 2016 publications
# 1 is Wildfire (Fire)
# 2 is Clearcut harvesting with salvage (Harvesting)
# 3 is Deforestation â€” Transportation â€” Salvage, uprooting and burn (Road)
# 4 Generic 20% mortality (Lcondition)
# 5	Generic 20% mortality (Unclass)

# these are all the rasters for a small test area
library(raster)
library(magrittr)
myBigList <- grep(pattern = "*.grd", 
                  x = list.files("data/forIan/SK_data/CBM_GIS/disturbance_testArea/", full.names = TRUE),
                  value = TRUE)
myBigList <- lapply(myBigList, raster::raster)
myBigStack <- raster::stack(myBigList)
freqTables <- raster::freq(myBigStack)


# run a spadesCBM simulation
spadesCBMout$mySpuDmids
# spatial_unit_id disturbance_matrix_id events
# 6056               27                   378      1
# 6057               28                   371      1
# 3886               27                   409      2
# 3894               28                   409      2
# 300                27                    26      3
# 308                28                    26      3
# 2590               27                    91      4
# 2598               28                    91      4
# 25901              27                    91      5
# 25981              28                    91      5

d378 <- seeDist(378)
d371 <- seeDist(371)
d409 <- seeDist(409)
d26 <- seeDist(26)
d91 <- seeDist(91)

names(d378)
names(d371)
names(d409)
names(d26)
names(d91)
# > names(d378)
# [1] "Wildfire for Saskatchewan - Boreal Shield West (NIR2011)"
# > names(d371)
# [1] "Wildfire for Saskatchewan - Boreal Plains (NIR2011)"
# > names(d409)
# [1] "clearcut with 94% utilization rate and 50% salvage of snags"
# > names(d26)
# [1] "Deforestation Matrix #1 (Stand Replacing).   Traditionally used for all ecozones across Canada."
# > names(d91)
# [1] "generic 20% mortality"
