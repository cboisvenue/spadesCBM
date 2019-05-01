# Task 3
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
#person: Celine

# May 1, 2019
# CBoisvenue

# working with the spadesCBMout object for now

grow_inc <- spadesCBMout$growth_increments
