spatialPlot <- function(pixelkeep, cbmPools, poolsToPlot, years, masterRaster) {
  colnames(cbmPools)[1:3] <- c("simYear", "pixelGroup", "age")
  if ("totalCarbon" %in% poolsToPlot) {
    totalCarbon <- apply(cbmPools[,5:25], 1, 'sum')
    cbmPools <- cbind(cbmPools, totalCarbon)
  }
  
  if (any(!poolsToPlot %in% colnames(cbmPools))) {
    stop("The carbon pool you specified is not contained in the pool definitions")
  }
  
  cbmPools <- as.data.table( cbmPools) 
  #Build rasters for every year and pool
  carbonStacks <- vector(mode = "list", length = length(poolsToPlot))
  names(carbonStacks) <- poolsToPlot
  
  for (pool in poolsToPlot) {
    carbonStacks[[pool]] <- lapply(years, FUN = function(x, poolsDT = cbmPools, 
                                                         var = pool, 
                                                         pixelKeep =  pixelkeep) {
      poolsDT <- poolsDT[order(pixelGroup)] %>% #order by stand index
        .[simYear == x, .(pixelGroup, "var" =  get(pool))] #subset by year
      #subset  pixelKeep
      colsToKeep <- c("pixelIndex", paste0("pixelGroup", x))
      
      pixelKeep <- pixelKeep[, colsToKeep, with = FALSE] %>%
        setnames(., c("pixelIndex", "pixelGroup")) #with=FALSE tells data.table colsToKeep isn't a column name    
      pixelKeep <- pixelKeep[poolsDT, on = c("pixelGroup")] %>% #join with pixelKeep
        .[order(pixelIndex)] #order by rowOrder for raster prep
      
      masterRaster[masterRaster == 0] <- NA #Species has zeroes instead of NA. Revisit if masterRaster changes
      masterRaster[!is.na(masterRaster)] <- pixelKeep$var
      
      names(masterRaster) <- x #name will begin with x if no character assigned
      return(masterRaster)
    })
    names(carbonStacks[[pool]]) <- years
  }
  temp <- unlist(carbonStacks)
  quickPlot::Plot(temp, new = TRUE)
}

barPlot <- function(cbmPools, masterRaster, pixelKeep) {
  colnames(cbmPools)[1:3] <- c("simYear", "pixelGroup", "age")
  #Need to first average values per ha
  cbmPools <- as.data.table(cbmPools)
  cbmPools$pixelGroup <- as.character(cbmPools$pixelGroup)
  cbmPools$simYear <- as.character(cbmPools$simYear)
  soilCarbon <- cbmPools[, .(soilCarbon = sum(get(colnames(cbmPools)[15:21])), 
          livingCarbon = sum(get(colnames(cbmPools)[5:14]))), by = .(pixelGroup, simYear)]
  
  pixTable <- pixelKeep[, !"pixelIndex", with = FALSE]
  freqTable <- as.data.table(lapply(pixTable, tabulate, nbins = max(pixTable)))
  #Not sure this will always work
  newNames <- as.character(c(0, unique(soilCarbon$simYear)))
  colnames(freqTable) <- newNames
  weights <- freqTable/nrow(pixelKeep)
  
  weights$pixelGroup <- row.names(weights)
  weights <- data.table::melt.data.table(weights, id.vars = 'pixelGroup', value.name = "weight", variable.name = "simYear")
 
  #Need to make this a character
  weights$simYear <- as.character(weights$simYear)
  setkey(weights, pixelGroup, simYear)
  setkey(soilCarbon, pixelGroup, simYear)
  outTable <- weights[soilCarbon]
  outTable <- outTable[, .(soilCarbon = sum(soilCarbon * weight), treeCarbon = sum(weight * livingCarbon)), by = simYear]
  outTable <- data.table::melt.data.table(outTable, id.vars = 'simYear', 
                                          measure.vars = c("soilCarbon", "treeCarbon"), 
                                          variable.name = 'pool', 
                                          value.name = "carbon")
  outTable$simYear <- as.numeric(outTable$simYear)
  outTable$carbon <- as.numeric(outTable$carbon)
  totalCarbon <- ggplot(data = outTable, aes(x = simYear, y = carbon)) +
    geom_area(aes(fill = pool)) + 
    scale_fill_discrete(name = "carbon pool",
                        labels = c("AG", "BG")) + 
    labs(x = "Year", y = "C (Mg/ha)")
      
    
  quickPlot::Plot(totalCarbon, new = TRUE, title = "mean C per pixel")

  #plot Units must be multiplied by 10000/prod(res(masterRaster)) to get tonnes/ha 
  
}

aNPPPlot <- function(spatialDT, changeInNPP, masterRaster){
  t <- spatialDT[, .(pixelIndex, pixelGroup)]
  temp <- t[changeInNPP, on = "pixelGroup"]
  setkey(temp, pixelIndex)
  masterRaster[!masterRaster == 0] <- temp$totalNPP
  names(masterRaster) <- "total aNPP"
  quickPlot::Plot(masterRaster, new = TRUE, title = "total ANPP")
  
}