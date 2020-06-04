# functions to be used in the spadesCBMinputs module
# CBoisvenue 



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


### celine's disturbance magic
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
  # this could be done with sim$cbmData@disturbanceMatrixAssociation and would
  # not require RSQLite
  dmid <- unique(cbmTables[[7]][which(cbmTables[[7]][,1] %in% mySpu),c(1,3)])
  
  # add the descriptive names
  spuDist <- cbind(dmid,cbmTables[[6]][dmid$disturbance_matrix_id,3])
  return(spuDist)
}

