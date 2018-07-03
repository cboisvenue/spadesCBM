# functions to be used in the carb1 module
# CBoisvenue January 18
# with Eliot


# Added these after (copied from spadesCBMdefaultFunctions.r)
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

hash <- function(x) {
  e <- new.env(hash = TRUE, size = nrow(x),
               parent = emptyenv());
  apply(x, 1, function(col) {
    assign(toString(col[1]), col[2:length(col)], envir = e)
  });
  
  return(e)
}

# used in spadeCBMInputes (in g&y reading) AND in spadesCBMdefaults (in creating sim$processes)
matrixHash <- function(x){
  keys = unique(x[,1])
  e <- new.env(hash = TRUE, size=length(keys), parent = emptyenv())
  apply(as.matrix(keys), 1, function(key) {
    assign(toString(key), x[x[,1]==key,2:ncol(x)], envir = e)
  });
  return(e)
}

processGrowthCurve <- function(gcid,growthCurves,growthCurveComponents,sim) {
  
  matchingRows <- t(as.matrix(growthCurves[growthCurves[,"growth_curve_id"]==gcid,]))
  swSpeciesCode = 0
  hwSpeciesCode = 0
  swAgeVolumePairs = as.matrix(0)
  hwAgeVolumePairs = as.matrix(0)
  for(row in 1:nrow(matchingRows)){
    if(matchingRows[row,"forest_type_id"]==1) {
      swSpeciesCode <- matchingRows[row, "species_id"]
      swAgeVolumePairs <- growthCurveComponents[growthCurveComponents[,"GrowthCurveComponentID"] == matchingRows[row,"growth_curve_component_id"], c("Age","MerchVolume")]
    } else {
      hwSpeciesCode <- matchingRows[row, "species_id"]
      hwAgeVolumePairs <- growthCurveComponents[growthCurveComponents[,"GrowthCurveComponentID"] == matchingRows[row,"growth_curve_component_id"], c("Age","MerchVolume")]
    }
  }
  carbonCurve <- VolumeToBiomassConvert(dbpath = sim$dbPath, 
                                        spatial_unit_id = matchingRows[1,"spatial_unit_id"],
                                        sw_species_code = swSpeciesCode, 
                                        swAgeVolume = swAgeVolumePairs, 
                                        hw_species_code = hwSpeciesCode, 
                                        hwAgeVolume = hwAgeVolumePairs)
  return (carbonCurve)
}

