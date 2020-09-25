
readSqlFile <- function(filePath) {
  fileconn<-file(filePath,"r")
  sqlString<-readLines(fileconn)
  sqlString<-paste(sqlString,collapse=" ")
  gsub("\t","", sqlString)
  close(fileconn)
  return( sqlString )
}

query <- function(dbPath, sql){
  con = dbConnect(dbDriver("SQLite"), dbPath)
  table <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(table)
}

getTable <- function(filename, dbPath, sqlDir) {
  
  con = dbConnect(dbDriver("SQLite"), dbPath)
  filePath <- file.path(sqlDir, filename)
  table <- query(dbPath, readSqlFile(filePath))
  dbDisconnect(con)
  return(table)
}


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
# END calculate c transfer for disturbances and annual processes post disturbance-----------------------------

matrixHash <- function(x){
  keys = unique(x[,1])
  e <- new.env(hash = TRUE, size=length(keys), parent = emptyenv())
  apply(as.matrix(keys), 1, function(key) {
    assign(toString(key), x[x[,1]==key,2:ncol(x)], envir = e)
  });
  return(e)
}

#step 1 functions
# this class stores all CBM-CFS3 parameters in matrices
setClass("dataset", where = envir(sim), slots=list(
  turnoverRates="matrix",
  rootParameters="matrix",
  decayParameters="matrix",
  spinupParameters="matrix",
  classifierValues="matrix",
  climate="matrix",
  spatialUnitIds="matrix",
  slowAGtoBGTransferRate="matrix",
  biomassToCarbonRate="matrix",
  ecoIndices="matrix",
  spuIndices="matrix",
  stumpParameters="matrix",
  overmatureDeclineParameters="matrix",
  disturbanceMatrix="matrix",
  disturbanceMatrixAssociation="matrix",
  disturbanceMatrixValues="matrix",
  disturbanceMatrixIndices="matrix",
  disturbanceEvents="matrix",
  landclasses="matrix",
  pools="matrix",
  domPools="matrix"))

readSqlFile <- function(filePath) {
  fileconn<-file(filePath,"r")
  sqlString<-readLines(fileconn)
  sqlString<-paste(sqlString,collapse=" ")
  gsub("\t","", sqlString)
  close(fileconn)
  return( sqlString )
}

query <- function(dbPath, sql){
  con = dbConnect(dbDriver("SQLite"), dbPath)
  table <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(table)
}

getTable <- function(filename, dbPath, sqlDir) {
  
  con = dbConnect(dbDriver("SQLite"), dbPath)
  filePath <- file.path(sqlDir, filename)
  table <- query(dbPath, readSqlFile(filePath))
  dbDisconnect(con)
  return(table)
}

#step 2 functions
getIdentityCoordinateMatrix<-function(size){
  return(cbind(1:size, 1:size, rep(1.0,size)))
}

#' calculates the decay rate based on mean annual temperature 
#' and other parameters
#' 
#' @param meanAnnualTemp scalar temperature in deg Celcius
#' @param baseDecayRate scalar base decay rate constant
#' @param q10 the scalar q10 value
#' @param tref the reference temperature
#' @param max the maximum allowed decay rate
#' @return the scalar rate of decay
#'
#' this is the ecological theory for decomposing being used in CBM
#' 
decayRate <- function(meanAnnualTemp, baseDecayRate, q10, tref, max) {
  min(baseDecayRate * exp((meanAnnualTemp-tref) * log(q10) * 0.1), max)
}

#' returns a vector of decay rates where the indices of the vector
#' are the dom pools of CBM
#' 
#' @param meanAnnualTemp scalar temperature in deg Celcius
#' @param decayParameters table of decay parameters for calculating 
#' the temperature dependant decay rate
#' @return the vector of decay rates
getDecayRates <- function(meanAnnualTemp, decayParameters, domPools) {
  decayRateOfNum <- function(decayParameter){
    decayRate(meanAnnualTemp,
              decayParameter["OrganicMatterDecayRate"],
              decayParameter["Q10"],
              decayParameter["ReferenceTemp"],
              decayParameter["MaxDecayRate"])
  }
  result <- apply(decayParameters, 1, decayRateOfNum)
  names(result) <- domPools[,"name"]
  return(result)
}

spatialUnitDecayRates <- function(climate, decayparameters, domPools){
  decayRates <- t(apply(as.matrix(climate[,"MeanAnnualTemperature"]), 1, 
                        function(t) getDecayRates(t, decayparameters, domPools)))
  decayRates <- cbind(climate[,"SpatialUnitID"], decayRates)
  colnames(decayRates)[1] <- "SpatialUnitID"
  return (decayRates)
}

#' calculates a portion of the dom decay matrix (here only to reduce repetitive code)
#' 
#' @param mat the datatable 
#' @param decayRates vector of annual decay rates by dom pool
#' @param propToAtmosphere vector of the proportions of decay emitted to the atmosphere as CO2 by this process
#' @param src the integer code for the dom pool being decayed
#' @param dst the integer code for the dom pool receving non-emitted decayed matter
#' @param emission the integer code for the CO2 pool
#' @return A modified copy of the input \code{mat}
domDecayMatrixItem <- function(mat, decayRates, propToAtmosphere, src, dst, emission) {
  offset<-HardwoodFineRoots
  mat <- rbind(mat, c(src, src, 1-decayRates[src-offset]))
  mat <- rbind(mat, c(src, dst, decayRates[src-offset] * (1-propToAtmosphere[src-offset])))
  mat <- rbind(mat, c(src, emission, decayRates[src-offset] * propToAtmosphere[src-offset]))
  return( mat)
}

#' compute a single dom decay matrix based on the specified table of decay rates
#' 
#' @param decayRates vector of decay rates (each element represents a dom pool)
#' @param decayparameters table of cbm decay parameters 
domDecayMatrix <- function(decayRates, decayParameters, PoolCount) {
  
  
  mat <- getIdentityCoordinateMatrix(PoolCount)
  propToAtmosphere <- decayParameters[,"PropToAtmosphere"]
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, AboveGroundVeryFastSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, BelowGroundVeryFastSoil, BelowGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, AboveGroundFastSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, BelowGroundFastSoil, BelowGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, MediumSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, SoftwoodStemSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, SoftwoodBranchSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, HardwoodStemSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, HardwoodBranchSnag, AboveGroundSlowSoil, CO2)
  
  return (mat)
}

#' compute all dom decay matrices in coordinate matrix format.  
#' The first column in the specified decayRates parameter acts as the key to
#' each matrix
#' 
#' @param decayRates matrix of decay rates column 1 is the key for the values 
#' in columns 1:n and columns 1:n are the dom pool specific decay rates
#' @param decayparameters table of cbm decay parameters
computeDomDecayMatrices <- function(decayRates, decayParameters, PoolCount){
  
  matrices <- NULL
  for(x in 1:nrow(decayRates)){
    mat <- domDecayMatrix(decayRates[x,-1], decayParameters, PoolCount)
    mat <- cbind(decayRates[x,1], mat)
    matrices <- rbind(matrices, mat)
  }
  
  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}


slowDecayMatrix <- function(decayRates, decayParameters, PoolCount) {
  offset <- HardwoodFineRoots
  mat <- getIdentityCoordinateMatrix(PoolCount)
  propToAtmosphere <- decayParameters[,"PropToAtmosphere"]
  mat <- rbind(mat, c(AboveGroundSlowSoil, AboveGroundSlowSoil, 1-decayRates[AboveGroundSlowSoil-offset]))
  mat <- rbind(mat, c(AboveGroundSlowSoil, CO2, decayRates[AboveGroundSlowSoil-offset] * propToAtmosphere[AboveGroundSlowSoil-offset]))
  mat <- rbind(mat, c(BelowGroundSlowSoil, BelowGroundSlowSoil, 1-decayRates[BelowGroundSlowSoil-offset]))
  mat <- rbind(mat, c(BelowGroundSlowSoil, CO2, decayRates[BelowGroundSlowSoil-offset]* propToAtmosphere[AboveGroundSlowSoil-offset]))
  return (mat)
}

computeSlowDecayMatrices <- function(decayRates, decayParameters, PoolCount){
  matrices <- NULL
  for(x in 1:nrow(decayRates)){
    mat <- slowDecayMatrix(decayRates[x,-1], decayParameters, PoolCount)
    mat <- cbind(decayRates[x,1], mat)
    matrices <- rbind(matrices, mat)
  }
  
  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}

computeSlowMixingMatrix <- function(slowMixingRate, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)
  mat <- rbind(mat, c(AboveGroundSlowSoil, BelowGroundSlowSoil, slowMixingRate))
  mat <- rbind(mat, c(AboveGroundSlowSoil, AboveGroundSlowSoil, 1 - slowMixingRate))
  mat <- cbind(rep(1,nrow(mat)),mat)
  colnames(mat) <- c("id", "row", "col", "value")
  return(mat)
}

domTurnOverMatrix <- function(turnoverParam, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)
  
  mat <- rbind(mat, c(SoftwoodStemSnag, SoftwoodStemSnag, 1-turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodStemSnag, MediumSoil, turnoverParam["StemSnagTurnoverRate"]))
  
  mat <- rbind(mat, c(SoftwoodBranchSnag, SoftwoodBranchSnag, 1-turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodBranchSnag, AboveGroundFastSoil, turnoverParam["BranchSnagTurnoverRate"]))
  
  mat <- rbind(mat, c(HardwoodStemSnag, HardwoodStemSnag, 1-turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodStemSnag, MediumSoil, turnoverParam["StemSnagTurnoverRate"]))
  
  mat <- rbind(mat, c(HardwoodBranchSnag, HardwoodBranchSnag, 1-turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodBranchSnag, AboveGroundFastSoil, turnoverParam["BranchSnagTurnoverRate"]))
  
  return (mat)
}
computeDomTurnoverMatrices <- function(turnoverParameters, PoolCount){
  matrices <- NULL
  for(x in 1:nrow(turnoverParameters)){
    mat <- domTurnOverMatrix(turnoverParameters[x,], PoolCount)
    mat <- cbind(turnoverParameters[x,1], mat)
    matrices <- rbind(matrices, mat)
  }
  
  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}
biomassTurnoverMatrix <- function(turnoverParam, PoolCount) {
  
  mat <- getIdentityCoordinateMatrix(PoolCount)
  mat <- rbind(mat, c(SoftwoodMerch, SoftwoodMerch, 
                      1-turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodMerch, SoftwoodStemSnag,
                      turnoverParam["StemAnnualTurnoverRate"]))  
  mat <- rbind(mat, c(SoftwoodFoliage, SoftwoodFoliage, 
                      1-turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(SoftwoodFoliage, AboveGroundVeryFastSoil,
                      turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(SoftwoodOther, SoftwoodOther,
                      1-turnoverParam["SoftwoodBranchTurnoverRate"]))  
  mat <- rbind(mat, c(SoftwoodOther, SoftwoodBranchSnag,
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodOther, AboveGroundFastSoil,
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, SoftwoodCoarseRoots,
                      1-turnoverParam["CoarseRootTurnProp"]))  
  mat <- rbind(mat, c(SoftwoodCoarseRoots, AboveGroundFastSoil,
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, BelowGroundFastSoil,
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, SoftwoodFineRoots,
                      1-turnoverParam["FineRootTurnProp"]))  
  mat <- rbind(mat, c(SoftwoodFineRoots, AboveGroundVeryFastSoil,
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, BelowGroundVeryFastSoil,
                      (1 - turnoverParam["FineRootAGSplit"]) * turnoverParam["FineRootTurnProp"]))
  
  mat <- rbind(mat, c(HardwoodMerch, HardwoodMerch, 
                      1-turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodMerch, HardwoodStemSnag,
                      turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodFoliage, HardwoodFoliage, 
                      1-turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(HardwoodFoliage, AboveGroundVeryFastSoil,
                      turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(HardwoodOther, HardwoodOther,
                      1-turnoverParam["HardwoodBranchTurnoverRate"]))  
  mat <- rbind(mat, c(HardwoodOther, HardwoodBranchSnag,
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodOther, AboveGroundFastSoil,
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, HardwoodCoarseRoots,
                      1-turnoverParam["CoarseRootTurnProp"]))  
  mat <- rbind(mat, c(HardwoodCoarseRoots, AboveGroundFastSoil,
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, BelowGroundFastSoil,
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, HardwoodFineRoots,
                      1-turnoverParam["FineRootTurnProp"]))  
  mat <- rbind(mat, c(HardwoodFineRoots, AboveGroundVeryFastSoil,
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, BelowGroundVeryFastSoil,
                      (1 - turnoverParam["FineRootAGSplit"]) * turnoverParam["FineRootTurnProp"]))
  return (mat)
}
computeBioTurnoverMatrices <- function(turnoverParameters, PoolCount){
  matrices <- NULL
  for(x in 1:nrow(turnoverParameters)){
    mat <- biomassTurnoverMatrix(turnoverParameters[x,], PoolCount)
    mat <- cbind(turnoverParameters[x,1], mat)
    matrices <- rbind(matrices, mat)
  }
  
  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}

loadDisturbanceMatrixIds<-function(disturbanceMatrixValues, dbPools){
  ids<-unique(disturbanceMatrixValues[,"disturbance_matrix_id"])
  
  # matches the pool def id by name for safety
  getPoolDefId <- function(dbPoolID){
    dbPoolName <- dbPools[as.numeric(dbPools[,"id"])==dbPoolID, "name"]
    get(dbPoolName)
  }
  
  #fill in the neutral transfers not covered by the matrix data
  #ie. the matrix will not withdraw from any of the following pools
  neutrals <- NULL
  neutrals <- rbind(neutrals, c(Input, Input, 1))
  neutrals <- rbind(neutrals, c(CO2, CO2, 1))
  neutrals <- rbind(neutrals, c(CH4, CH4, 1))
  neutrals <- rbind(neutrals, c(CO, CO, 1))
  neutrals <- rbind(neutrals, c(Products, Products, 1))
  
  loadMatrix <- function(dmid){
    dbmat<-disturbanceMatrixValues[disturbanceMatrixValues[,"disturbance_matrix_id"]==dmid,]
    mat <- matrix(0, ncol=3, nrow=nrow(dbmat))
    
    for(i in 1:nrow(dbmat)){
      mat[i,]=c(getPoolDefId(dbmat[i,"source_pool_id"]),
                getPoolDefId(dbmat[i,"sink_pool_id"]),
                dbmat[i,"proportion"])
    }
    
    mat<-rbind(mat, neutrals)
    
    return(mat)
  }
  ##?? not sure is this is needed in the sim$ since it is not openly used anywhere else
  allMatrices <- NULL
  # return the matrix ids of the loaded matrices
  for(x in 1:length(ids)){
    dm <- loadMatrix(ids[x])
    dm <- cbind(rep(ids[x], nrow(dm)),dm)
    allMatrices <- rbind(allMatrices, dm)
  }
  colnames(allMatrices) <- c("id", "row", "col", "value")
  return(allMatrices)
  
}
# from spadesCBMinputsFunctions.r---------------------
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

#----------when you need other modules to be run before---------------
suggestModules <- function(sim, suggestedModules) {
  io <- inputObjects(sim, currentModule(sim))
  objectNamesExpected <- io$objectName
  available <- objectNamesExpected %in% ls(sim)
  if (any(!available)) stop("The inputObjects for spadesCBMcore are not all available:",
                            "These are missing:", paste(objectNamesExpected[!available], collapse = ", "),
                            ". \n\nHave you run ", 
                            suggestedModules,
                            "?")
}