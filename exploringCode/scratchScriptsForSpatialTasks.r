# Figuring out the TASKS in spatialInfor.rmd
# this file is to be ignored
# CBoisvenue June 12, 2018


### 6- Last pass disturbance  
#The user has to identify the last disturbance that happened to the stand/pixel that is 
#at the age in the age raster (in 1). For example, this could be a fire or a clearcut. 
#In each spatial unit across Canada, there are a number of defined matrices for clearcut 
#and fire. So spatial unit needs to be associated with a pixel - as in 5, this can be 
#done via the gcID assignment (in 4) since they have spatial units associated with them. 
#Once the spatial unit per pixels is known, the table 
#`spadesCBMout@.envir$cbmData@disturbanceMatrixAssociation` can provide the 
#disturbance_type_id and the disturbance_matrix_id that are possible for the spatial unit. 
#The cbm_default.db table disturbance_type gives the name and a description of the 
#disturbance (using the disturbance_type_id) from which the user can select..  

#***TASKS***  
#  1. write script to facilitate the choice of last pass disturbance.


##Step 1: determine which spu you are in


## For us, take them from the growth information
# ***NEW TASKS***could we provide spu rasters? adminXeco with the same numbering as the table in 

# Read in the gc files AND read-in the cbm_defaults.db to create the spu (to line 44)

gcComponent <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldComponentRCBM.csv"))
gcIn <- as.matrix(read.csv("C:/Celine/GitHub/spadesCBM/spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv"))

## here trying to match the spatial_unit_id in gcIn with the spatial_unit in cbm_defaults

# ***run readInSQLiteData.r which creates the data.frame spu (combines all ids with 
# province and ecozone names)***

# figure out what spatial_unit these gc are in with the matching of 
# gcIn$spatial_unit_id and cbmTables$spatial_unit

gcSpu <- spu[which(spu$spu_id %in% unique(gcIn[,1])),]

##Got the spus.
#     spu_id admin_boundary_id     province eco_boundary_id            ecozone
# 8      26                 9 Saskatchewan               5  Taiga Shield West
# 11     27                 9 Saskatchewan               6 Boreal Shield West
# 22     28                 9 Saskatchewan               9      Boreal Plains
# 25     29                 9 Saskatchewan              10  Subhumid Prairies
# 48     30                 9 Saskatchewan              18  Semiarid Prairies