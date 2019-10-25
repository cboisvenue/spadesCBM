### spuDist()-------------------------------------------------------------------
#This function identifies the ID number (CBM-CFS3 legacy) that are possible in
#the specific spatial unit you are in. You give is spatial units you are
#targetting (mySpu) and it give you the disturbance matrix id that are
#possible/default in that specific spu and a descriptive name of that disturbance matrix
#it creates a data.frame of length number of disturbances, with three columns: spatial_unit_is, 
#disturbance_matrix_id, and a desciption of the disturbance.

## figure out which spu you are in 
#Note: can we have a canada-wide spu map and they locate themselves on the map?
#this needs to be done before simulations are run so the user can provide this
#info (location info) for the simulations - Ian is working on this.

# could be with the rasters
library(raster)
spuRaster <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
spatial_unit_id <- getValues(spuRaster) #28 27
# or with the growth curves
gcIn <- as.matrix(read.csv(file.path(getwd(),"spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv")))

mySpu <- unique(spatial_unit_id)
# or 
mySpu <- unique(gcIn[,1])

# the function has the defaults from the Sk managed forest example. These can my
# changed by feeing in other spu
spuDist <- function(mySpu = c(27,28),dbPath = file.path(getwd(),"data","cbm_defaults","cbm_defaults.db")){
  
  library(RSQLite)
  
  sqlite.driver <- dbDriver("SQLite")
  
  cbmDefaults <- dbConnect(sqlite.driver,
                           dbname = dbPath)
  
  alltables = dbListTables(cbmDefaults)
  cbmTables <- list()
  
  for(i in 1:length(alltables)){
    cbmTables[[i]] <- dbReadTable(cbmDefaults,alltables[i])
  }
  # match mySpu with the disturbance_matrix_association table
  dmid <- unique(cbmTables[[7]][which(cbmTables[[7]][,1] %in% mySpu),c(1,3)])
  
  # add the descriptive names
  spuDist <- cbind(dmid,cbmTables[[6]][dmid$disturbance_matrix_id,3])
  return(spuDist)
}

###END spuDist------------------------------------------------------------------

###calcTurnoverRates ------------------------------------------------------------------
# matching the turnover rates to the spatial unit

calcTurnoverRates <- function(turnoverRates, spatialUnitIds, spatialUnits) {
  turnoverRates <- as.data.table(turnoverRates)
  SPU <- as.data.table(spatialUnitIds)
  SPU <- SPU[SpatialUnitID %in% unique(spatialUnitIds)]
  SPU <- merge(SPU, turnoverRates, by = "EcoBoundaryID", all.y = FALSE)
  SPU <- SPU[SpatialUnitID %in% unique(spatialUnits),]
  return(SPU)
}
### END calcTurnoverRates ------------------------------------------------------------------

# calculate c transfer for diturbances and annual processes post disturbance-----------------------------
# mismatch in c transfers when disturbanc happens in cpp processing so bypassing it
# c transfer functions: one for the disturbance (so small decimals erros in
# matrices are corrected), and one for the annual processes
cTransferDist <- function(standIn=preD,transProp=distProp){
  standIn <- as.data.table(cbind(standIn,row=c(1:length(standIn))))
  names(standIn) <- c("V1","row")
  transProp <- as.data.table(transProp)
  rowJoin <- standIn[transProp, on="row"][,fluxOut:=(V1*value)]
  
  # calculate carbon out and in
  outC <- rowJoin[,.(outC = sum(fluxOut)), by=row]
  inC <-  rowJoin[,.(inC = sum(fluxOut)), by=col]
  names(inC) <- c("row","inC")
  fluxes <- merge(outC,inC,by="row", all=TRUE)
  # no NAs allowed
  fluxes$inC[which(is.na(fluxes$inC))] <- 0
  
  standOut <- standIn[fluxes,on="row"][,.(calcDist = V1-outC+inC),by="row"]
  
  # these two lines are "fixes for smal decimal differences that should not be there
  # pools can't go negative
  standOut[calcDist<0,"calcDist"] <- 0
  # if it does not transfer to itself it has to end-up empty
  rowsTofix <- transProp[row==col,.N,by="row"]
  standOut[!(row %in% rowsTofix$row),"calcDist"] <- 0
  
  return(standOut)
}
## not doing the "fixes" in this one for the small decimal errors in the dist matrices
cTransfer <- function(standIn=preD,transProp=distProp){
  standIn <- as.data.table(cbind(standIn,row=c(1:length(standIn))))
  names(standIn) <- c("V1","row")
  transProp <- as.data.table(transProp)
  rowJoin <- standIn[transProp, on="row"][,fluxOut:=(V1*value)]
  
  # calculate carbon out and in
  outC <- rowJoin[,.(outC = sum(fluxOut)), by=row]
  inC <-  rowJoin[,.(inC = sum(fluxOut)), by=col]
  names(inC) <- c("row","inC")
  fluxes <- merge(outC,inC,by="row", all=TRUE)
  # no NAs allowed
  fluxes$inC[which(is.na(fluxes$inC))] <- 0
  
  standOut <- standIn[fluxes,on="row"][,.(calcDist = V1-outC+inC),by="row"]
  
  return(standOut)
}
# END calculate c transfer for diturbances and annual processes post disturbance-----------------------------

