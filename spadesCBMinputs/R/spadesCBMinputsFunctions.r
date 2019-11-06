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

## FUNCTIONS to process growth curves into growth increments--------------------------------------------
# eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
# the gross merchantable volume/ha. Parameters a and b are in table3
b_m <- function(paramTable1, vol){
  b_m <- unique(paramTable1$a) * vol ^ unique(paramTable1$b)
  return(b_m)
}
# eq2 is for non-merch sized trees.
nmfac <- function(table4,eq1){
  nmFac <- unique(table4$k) + (unique(table4$a) * eq1 ^ unique(table4$b))
  b_nm <- nmFac * eq1
  b_n <- b_nm - eq1
  return(cbind(b_n,b_nm))
}
# eq3 is for the saplings and it needs b_nm from the previous eq2
sapfac <- function(table5, eq2){
  sapFac <- table5$k + (table5$a * eq2[,2] ^ table5$b)
  b_snm <- sapFac * eq2[,2]
  b_s <- b_snm - eq2[,2]
  return(b_s)
}
# calculate the 4 proportions that should be returned
# will eventually add species, ecozone
# vol = gross merchantable volume per ha
# lvol = natural logarithm of (vol+5)
biomProp <- function(table6,vol){
  lvol <- log(vol+5)
  a <- c(7:9)
  b <- c(10:12)
  c <- c(13:15)
  pstem <- 1 / ( 1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
                   exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
                   exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  
  pbark <- exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  
  pbranches <- exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  
  pfol <- exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol) /
    (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
       exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
       exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
  propVect <- cbind(pstem,pbark,pbranches,pfol)   
  return(propVect)
}

convertM3biom <- function(meta,gCvalues,spsMatch,ecozones,params3, params4, params5,params6){
  oneCurve <- gCvalues[GrowthCurveComponentID==meta$growth_curve_component_id,]
  spec <- spsMatch[speciesName==meta$species,]$canfi_species
  ez <- ecozones[SpatialUnitID==meta$spatial_unit_id,]$EcoBoundaryID
  gen <- spsMatch[speciesName==meta$species,]$genus
  
  params3 <- params3[canfi_species== spec & ecozone == ez,] 
  params4 <- params4[canfi_species== spec & ecozone == ez,]
  # table 5 is different than the others
  params5 <- params5[genus == gen & ecozone == ez,]
  params6 <- params6[species == spec & eco == ez,]
  
  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  eq1 <- b_m(params3, oneCurve$MerchVolume)
  # eq2 returns a two colum matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm, the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merche-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  eq2 <- nmfac(params4, eq1 = eq1)
  #some NAs where it was 0s. Leave these in place
  # eq3 is for biomass of the saplings, the smallest of the nonmerch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  eq3 <- sapfac(params5, eq2 = eq2)
  #eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees
  merch <- eq1+eq2[,1] + eq3
  merch[which(is.nan(merch))] <- NA
  # calculate the 4 proportions that should be returned: proportion for
  # stemwood, prop for bark, prop for branches, and prop for foliage.
  pVect <- biomProp(table6 = params6, vol = oneCurve$MerchVolume)  
  # translating this into biomass values for the carbon pools
  totMerch <- merch/pVect[,1]
  bark <- totMerch*pVect[,2]
  branch <- totMerch*pVect[,3]
  fol <- totMerch*pVect[,4]
  other <- branch+bark
  biomCumulative <- as.matrix(cbind(totMerch,fol,other))
  return(biomCumulative)
  
}
# END process growth curve functions-------------------------------------------------------------  


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
  dmid <- unique(cbmTables[[7]][which(cbmTables[[7]][,1] %in% mySpu),c(1,3)])
  
  # add the descriptive names
  spuDist <- cbind(dmid,cbmTables[[6]][dmid$disturbance_matrix_id,3])
  return(spuDist)
}

