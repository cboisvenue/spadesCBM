#---------------------------------
# List of Tasks spadesCBM
# This script is where we work on our code
# the cleaned-up version goes into either spadesCBMextraFunctions.r or the modules themselves
#
# RULES:
# all these ecept task #7 need to be done on the master branch
# Ian and Celine coordinate, we pick a task, make a branch. The the task is
# complete we do a pull-request. We don't work on the same task and we tell the
# other person which one we are trying to solve.
#
# CBoisvenue IEddy
# February 1, 2019
#---------------------------------

# List---------------------------------
# These are the tasked to be addressed

#1.make sure that the function plotCarbonRaster matches the pixels to the right
#pixel group.
#We had suspected that it does not use the sim$pixelKeep but sticks
#to the original pixelGroupID 
#person: Ian Eddy

#2.use the LandR::generatePixelGroups and the LandR::updateCohort()function to
#create the PixelGroups. 
#presently in the pixel groups are create for the spinup, and again in the
#annual cycle to account for the yearly disturbances. There are two problems:
#right now the values of the yearly carbon are WRONG because pixel groups are
#all "reBuilt" for the righ pixel groups to be affected by the disturbances. The
#sim$pools if also "rebuilt" because the length has to match all the other
#vectors that go into the C++ functions...but it is rebuilt blank! It needs the
#carbon pools coming out of the Spinup() as a starting point...but the pixel
#group don't match, they were rebuilt. I think the using the LandR fncts can help fix this.
#person: either

#3.create pre-calculated values for mapping: total carbon, live carbon, dead
#carbon, above ground carbon, below ground carbon, NPP. 
#NPP may be tricky as it is NPP=increment + turnover
#What this is really is sum(GrossGrowthAG)+sum(GrossGrowthBG)
#GrossGrowthAG = increment going to Merch, Otherwood, foliage
#GrossGrowthBG = increment going to Fine Root Coarse Root
# the merch foliage and other are in the spadesCBMout$growth_increments, but the
# roots...might have to calculate them in an other way?
#Netgrowth would be = NPP- litterfall
#litterfall = sum(biomassToSoil)
#NEP=NPP-sum(DOMpool decomp to atmosphere)
#person: Celine to create values

#4.integrate the values in 3  as defaults in the mapping function, plotCarbonRasters
#person: Ian (but wait until Celine done)

#4.histogram/bar plot of total carbon etc.
#person: Ian 

#5.spu locator needs to be functional for users. For now, make it a separate function.
#person: Ian 

#6.make the user input clearer: clean-up spadesCBMinputs.r...
#person: either/both

#7.run for all of SK, not just the test area. Locate the large rasters and try just 2 years.
#person: Celine

#8.unit tests on master branch (create separate branch for tests)
#person: Ian

#9.using options() to fix the pckgs issues (Require vs require) this is problem
#loading RSQLite and CBMVolumeToBiomass
#person: either

#10.make sure/check that the spinup debug event in the spadesCBMcore works. 
#person: Celine


# Parked tasks: - fix all the warnings so that caching works reliably. 
#- make a separaterate growth and yield module - make a separate volume to biomass
# module with the Boudewyn parameters for transparency to replace
# CBMVolumeToBiomass. 
#- generic 20% disturbance is currently used for the
# disturbance Lcondition and Unclassified. The disturbance matrix reduces the
# biomass by 20% but then continues to growth on the same growth curve as
# previously assigned. This is not really a good representation of a partial
# disturbance. We could either have "20% disturbance growth curve" or "disturbed
# 20% of the pixels"? or? Just saying... 
#- see if it is worth writing the whole
# thing in R (as opposed to Rcpp) or if it will be too slow or too different
# from the original model. My hunch is that we need to keep it in Rcpp, but that
# we can slip-up the Rcpp code be able to better understand and manipulate the
# inputs and outputs. 
#- add transition curves note: sim$gcHash is a hashed
# matrix of the increments from the processing of the growth curve (lines
# 113-133 in spadesCBMinputs.r). This should not be affected by disturbances. It
# would be affected if we added transition growth curves, i.e., when a pixel
# changes growth curve after being disturbed. That is for later.
#- disturbance rasters are currently read-in every year. This might slow down 
# things but would make it easier if the rasters are provided by other modules 
# down the line. If not all rasters can be read-in as a stack and looked-up each
# year.
#
# Notes:
# - in sim$allProcesses (list of 9), only three, the growth (Growth1 and Growth2) and the OvermatureDecline are calculated by the Rcpp function ComputeGrowthAndDeclineMatrices2() and are the lenght of the unique(PixelGroupID).
# - NOTE: yield components (yieldComponentsSK.csv) cannot change format until we can play with the Rcpp code
#--------------------------------------

