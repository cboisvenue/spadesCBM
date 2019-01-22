#---------------------------------
# List of Tasks spadesCBM
# This script is where we work on our code
# the cleaned-up version goes into either spadesCBMextraFunctions.r or the modules themselves
#
# CBoisvenue IEddy
# January 22, 2019
#---------------------------------

# List---------------------------------
# These are the tasked addressed in this script.
#1.make sure that the function plotCarbonRaster matches the pixels to the right pixel group
#2.use the LandR::generatePixelGroups function to create the PixelGroups. Note: the first time this happens there is no "events" column.
#3.create pre-calculated values for mapping: total carbon, live carbon, dead carbon, above ground carbon, below ground carbon, NPP. Celine
#4.histogram/bar plot of total carbon etc.
#5.spu locator needs to be functional for users. For now, as a separate function.
#6.make the user input clearer: clean-up spadesCBMinputs.r...
#7.run for all of SK, not just the test area. Locate the large rasters and try just 2 years.
#8.unit tests on master branch (create separate branch for tests)
#9.using options() to fix the pckgs issues (Require vs require) this is problem loading RSQLite and CBMVolumeToBiomass
#10.make sure/check that the spinup debug event in the spadesCBMcore works. Celine

# These are not, but need to keep this list
# Parked tasks:
# - fix all the warnings so that caching works. expected inputs etc in spadesCBMinputs and spadesCBMcore.
# - make a separaterate growth and yield module
# - make a separate volume to biomass module with the Boudewyn parameters.
# - generic 20% disturbance is currently used for the disturbance Lcondition and Unclassified. The disturbance matrix reduces the biomass by 20% but then continues to growth on the same growth curve as previously assigned. This is not really a good representation of a partial disturbance. We could either have "20% disturbance growth curve" or "disturbed 20% of the pixels"? or? Just saying...
# - is if it is worth writing the whole thing in R (as opposed to Rcpp) or if it will be too slow or too different from the original model. My hunch is that we need to keep it in Rcpp, but that we can slip-up the Rcpp code be able to better understand and manipulate the inputs and outputs.
# - add transition curves note: sim$gcHash is a hashed matrix of the increments from the processing of the growth curve (lines 113-133 in spadesCBMinputs.r). This should not be affected by disturbances. It would be affected if we added transition growth curves, i.e., when a pixel changes growth curve after being disturbed. That is for later.
# - disturbance rasters are currently read-in every year. This might slow down things but would make it easier if the rasters are provided by other modules down the line. If not all rasters can be read-in as a stack and looked-up each year.
#
# Notes:
# - in sim$allProcesses (list of 9), only three, the growth (Growth1 and Growth2) and the OvermatureDecline are calculated by the Rcpp function ComputeGrowthAndDeclineMatrices2() and are the lenght of the unique(PixelGroupID).
# - NOTE: yield components (yieldComponentsSK.csv) cannot change format until we can play with the Rcpp code
#--------------------------------------

# who works on what?----
# either/both: 2, 6 
# Celine: 3, 7, 10
# Ian: 1, 4, 5, 8, 9
#-----------------------