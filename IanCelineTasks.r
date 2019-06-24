#---------------------------------
# List of Tasks spadesCBM
#
# RULES: all these tasks need to be re-inserted into the master branch when
# solved Ian and Celine coordinate, we pick a task, make a branch. The the task
# is complete we do a pull-request or a merge. We try not to work on the same
# task and we tell the other person which one we are trying to solve.
#
# CBoisvenue IEddy
# May 1, 2019
#---------------------------------

# List---------------------------------
# To do

#1.Need to add the option of plotting NPP, DOM and Biomass (above + below) using
#the plotCarbonRasters() function. The NPP() function in spadesCBMextraFunctions
#can be used to calculate NPP.

#4.integrate the values in 3 (NPP, DOM, and Live Biomass) as defaults in the
#mapping function, plotCarbonRasters
#person: Ian (but wait until Celine done)

#6.make the user input clearer: clean-up spadesCBMinputs.r...
#person: either/both 

#8.unit tests on master branch (create separate branch for tests)
#person: Ian


# Tasks completed-----------------------------------------------------------------------------------------
# 1.THIS PART IS DONE--------------------
#plotCarbonRasters() and lives in/spadesCBM/exploringCode/spadesCBMextrafunctions
#b) change the master so that the years that we need to plot can be defined by
#the user and saved in the cPoolsPixelYear.csv or somewhere we can access them.
#The plots are not saved? If I change the plotting interval, I see the plots flash by but can't go back to them...
# does not work if I change the pools (tried SoftwoodMerch), still defaults to Total C and ANPP
# we have NPP not NPP...ANPP
#--------------------------------------

#2. Task completed. Use the LandR::generatePixelGroups and the LandR::updateCohort()function to
#create the PixelGroups. G:\RES_Work\Work\SpaDES\spadesCBM\task2Notes.txt
# used generatePixelGroups but not updateCohort...too many LandR-specific things to consider.

#3.Task completed. Create pre-calculated values for mapping: NPP. All details are in task3NPPetc.r

#4.Taks completed. histogram/bar plot of total carbon etc.
#person: Ian 

#5.Task completed. spu locator needs to be functional for users. For now, make it a separate function.
#person: Ian 

#9.FIXED. library(RSQLite) and library(CBMVolumeToBiomass), still need to be loaded
#using options() to fix the pckgs issues (Require vs require) this is problem
#loading RSQLite and CBMVolumeToBiomass person: either

# END task completed-----------------------------------------------------------------------------------------


# Tasks postponed-------------------------------------------------------------------------------------------

#7.POSTPONED. run for all of SK, not just the test area. Locate the large rasters and try just 2 years.
#person: Celine

#10.POSTPONED. make sure/check that the spinup debug event in the spadesCBMcore works. 
#person: Celine

#11.POSTPONED. Rehomogenation of pixelGroups: We don't seem to be getting rid of pixels
#groups...this may be because the test area is too small to have homogenizing
#disturbances...keep an eye on this.

#12.POSTPONED. decayRates: matrix 48X12 - holds the decay rates for all the dom pools (11
#of them) per spatial unit. Where is this created? How does it link to the
#“decay_parameter” table in cbmData?

#13.POSTPONED. rewrite the CBMVolumeToBiomass library for more transparency. carbonCurve:
#this is the result from using all the growth curves and “translating” them into
#biomass compartments using SMorken’s CBMVolumeToBiomass library

#14.POSTPONED. fix all the warnings so that caching works reliably. 

#15.POSTPONED. make a separate growth and yield module.

#16.POSTPONED. generic 20% disturbance is currently used for the disturbance Lcondition and
#Unclassified. The disturbance matrix reduces the biomass by 20% but then
#continues to growth on the same growth curve as previously assigned. This is
#not really a good representation of a partial disturbance. We could either have
#"20% disturbance growth curve" or "disturbed 20% of the pixels"? or? Just
#saying...

#17.Waiting for Consultant report. see if it is worth writing the whole thing in R (as opposed to Rcpp) or if
#it will be too slow or too different from the original model. My hunch is that
#we need to keep it in Rcpp, but that we can slip-up the Rcpp code be able to
#better understand and manipulate the inputs and outputs.
#person: Iain Duncan.

#18. Later. add transition curves note: sim$gcHash is a hashed
# matrix of the increments from the processing of the growth curve (lines
# 113-133 in spadesCBMinputs.r). This should not be affected by disturbances. It
# would be affected if we added transition growth curves, i.e., when a pixel
# changes growth curve after being disturbed. That is for later, when we link to LandR-biomass

#19.Maybe. disturbance rasters are currently read-in every year. This might slow down 
# things but would make it easier if the rasters are provided by other modules 
# down the line. If not all rasters can be read-in as a stack and looked-up each
# year.

#20. Maybe. the bar plotting uses pixelKeep, but we need to move away from this for memory purposes. 
# instead the values should be calculated for time(sim) and added to a plotting object
# using rbind 
#
# Notes:
# - in sim$allProcesses (list of 9), only three, the growth (Growth1 and
# Growth2) and the OvermatureDecline are calculated by the Rcpp function
# ComputeGrowthAndDeclineMatrices2() and are the lenght of the
# unique(PixelGroupID).
# - yield components (yieldComponentsSK.csv) cannot change format until we can
# play with the Rcpp code
#sim$pools created in the soadesCBMinputs b/c it is needed in the Spinup(cpp)

#--------------------------------------

