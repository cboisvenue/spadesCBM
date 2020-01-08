# GHG checking and decisions------------------
# there is carbon in the C02, CH4 and CO pools at the end of spinup
# these should be at 0 in 1990 for us to be able to track the GHG contributions
# 1) change the CO2 columns to 0 post spinup
# There should be a lower contributions with no disturbances
# 2) check that the A- disturbances happen 
#   B- non-disturbed and disturbed pixel groups differ - this will require tracking disturbances...
# CBoisvenue September 20, 2019
#--------------------------------------------

# spinup results  
spinup <- spadesCBMout$spinupResult
summary(spinup)
dim(spinup)
spinup[,23:25] <- 0
# this was changed in the spinup event (init) of spadesCBMcore

