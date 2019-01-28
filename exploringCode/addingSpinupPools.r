#--------------------------------------------------
# CBoisvenue
# Janurary 25, 2019
#There is an error in the sim$pool and consequently in the sim$cbmPools with store the results
#The problem in sequence:
# - sim$pools is create as an empty matrix (spadesCBMinputs.r)
# - passed to spadesCBMcore.r spinup event where it it "filled up" 759X26
# - 1st year of disturbance requires that disturbed pixels create a seperate
#PixelGroupID, so level3DT recreated
# - because all teh inputs to the C++ functions need to be teh same lenght, 
#sim$ was recreated empty - THIS IS WRONG
# - this results in all the sim$cbmPools that are accumulated to make the 
#output cPoolYear.csv only accumulate the increments and has all the turnovers wrong.
