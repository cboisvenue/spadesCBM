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
  }
  names(carbonStacks) <- NULL
  temp <- unlist(carbonStacks)
  quickPlot::Plot(temp, new = TRUE, title = paste0("Total carbon in ", years))
}