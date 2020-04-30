spatialPlot <- function(pixelkeep, cbmPools, poolsToPlot, years, masterRaster) {

  colnames(cbmPools)[c(1,3,4)] <- c("simYear", "pixelGroup", "age")
  if ("totalCarbon" %in% poolsToPlot) {
    totalCarbon <- apply(cbmPools[,SoftwoodMerch:HardwoodBranchSnag], 1, 'sum')
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
      # until here it is ok...then in 1993 - an extra line gets added from the merge below
      #Keep <- poolsDT[pixelKeep, on = c("pixelGroup")]
      pixelKeep <- merge(pixelKeep,poolsDT,by="pixelGroup", all.x=TRUE) %>% #join with pixelKeep
         .[order(pixelIndex)] #order by rowOrder for raster prep
      #pixelKeep <- pixelKeep[poolsDT, on = c("pixelGroup")] %>% #join with pixelKeep
      pixels <- getValues(masterRaster)
      
      ### Problem here -  three seems to be a change to the masterRaster made by this line below
      
      ##RIA modification:
      plotMaster <- raster(masterRaster)
      plotMaster[] <- 0
      plotMaster[pixelKeep$pixelIndex] <- pixelKeep$var
      # masterRaster[masterRaster == 0] <- NA #Species has zeroes instead of NA. Revisit if masterRaster changes
      # masterRaster[!is.na(masterRaster)] <- pixelKeep$var
      
      #name will begin with x if no character assigned
      return(plotMaster)
    })

  }
  
  names(carbonStacks) <- paste0(poolsToPlot)
  
  temp <- unlist(carbonStacks)
  quickPlot::Plot(temp, addTo = "temp", title = paste0(poolsToPlot, " in ", years))
}

areaPlot <- function(cbmPools, masterRaster) {
  #This needs to change to belowground living, aboveground living, soil, snag
  colnames(cbmPools)[c(1,3,4)] <- c("simYear", "pixelGroup", "age")
  #Need to first average values per ha
  cbmPools <- as.data.table(cbmPools)
  cbmPools$pixelGroup <- as.character(cbmPools$pixelGroup)
  #pixelNo <- sum(cbmPools$pixelCount/length(unique(cbmPools$simYear))) #Get pixel Sum
  cbmPools$simYear <- as.character(cbmPools$simYear)
  productEmissions <- cbmPools[, .(products = sum(Products*(prod(res(masterRaster))/10000)*pixelCount),
                             emissionsCH4 = sum(CH4*(prod(res(masterRaster))/10000)*pixelCount),
                             emissionsCO = sum(CO*(prod(res(masterRaster))/10000)*pixelCount), 
                             emissionsCO2 = sum(CO2*(prod(res(masterRaster))/10000)*pixelCount)), 
                         by = .(pixelGroup, simYear)]
  

  outTable <- productEmissions[, .(emissions = sum(emissionsCH4,emissionsCO,emissionsCO2), 
                           products = sum(products)), 
                           by = simYear]
  
  # #turn average pixel/ha into sum of pixels / pixel-area/ha
  # outTable <- outTable[, .(emissions = emissions * pixelNo * prod(res(masterRaster))/10000,
  #                          products = products * pixelNo * prod(res(masterRaster))/10000),
  #                      by = simYear]
  outTable <- data.table::melt.data.table(outTable, id.vars = 'simYear', 
                                          measure.vars = c("emissions", "products"), 
                                          variable.name = 'pool', 
                                          value.name = "carbon")
  outTable$simYear <- as.numeric(outTable$simYear)
  outTable$carbon <- as.numeric(outTable$carbon)
  areaPlots <- ggplot(data = outTable, aes(x = simYear, y = carbon)) +
    geom_col(aes(fill = pool)) + 
    scale_fill_discrete(name = "carbon pool") +
    labs(x = "Year", y = "Mg C") + 
    theme_bw()
      
  quickPlot::Plot(areaPlots, addTo = 'areaPlots', title = "Total - all simulated pixels")

  #plot Units must be multiplied by 10000/prod(res(masterRaster)) to get tonnes/ha 
  
}

NPPPlot <- function(spatialDT, changeInNPP, masterRaster, time){
  #NPP will not have the disturbed pixelGroups in spatialDT
  changeInNPP <- changeInNPP[simYear == time,]
  t <- spatialDT[, .(pixelIndex, pixelGroup)]
  temp <- merge(t, changeInNPP, on = "pixelGroup", all.x = TRUE)
  setkey(temp, pixelIndex)
  #pixelCount[which(is.na(pixelCount$N)),"N"] <- 0
  temp1 <- temp[which(!is.na(temp$simYear)),.(pixelIndex,NPP)]
  temp1[order(pixelIndex)]
  #masterRaster[!masterRaster == 0] <- temp$NPP
  plotMaster <- raster(masterRaster)
  plotMaster[] <- 0
  plotMaster[temp1$pixelIndex] <- temp1$NPP
  quickPlot::Plot(plotMaster, new = TRUE, title = paste0("NPP in ", time))
}

barPlot <- function(cbmPools, masterRaster) {
  #This needs to change to belowground living, aboveground living, soil, snag
  colnames(cbmPools)[c(1,3,4)] <- c("simYear", "pixelGroup", "age")
  #Need to first average values per ha
  cbmPools <- as.data.table(cbmPools)
  cbmPools$pixelGroup <- as.character(cbmPools$pixelGroup)
  pixelNo <- sum(cbmPools$pixelCount/length(unique(cbmPools$simYear))) #Get pixel Sum
    cbmPools$simYear <- as.character(cbmPools$simYear)
  soilCarbon <- cbmPools[, .(DOM = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                                       AboveGroundFastSoil, BelowGroundFastSoil,
                                       AboveGroundSlowSoil, BelowGroundSlowSoil),
                             AGB = sum(SoftwoodMerch, SoftwoodFoliage, SoftwoodOther,
                                       HardwoodMerch, HardwoodFoliage, HardwoodOther),
                             BGB = sum(SoftwoodCoarseRoots, SoftwoodFineRoots, 
                                       HardwoodCoarseRoots, HardwoodFineRoots),
                             weight = pixelCount/pixelNo),
                         by = .(pixelGroup, simYear)]
  
  
  outTable <- soilCarbon[, .(DOM = sum(DOM * weight), 
                           AGB = sum(AGB * weight),
                           BGB = sum(BGB * weight)), 
                       by = simYear]
  
  outTable <- data.table::melt.data.table(outTable, id.vars = 'simYear', 
                                          measure.vars = c("DOM", "AGB", "BGB"), 
                                          variable.name = 'pool', 
                                          value.name = "carbon")
  outTable$simYear <- as.numeric(outTable$simYear)
  outTable$carbon <- as.numeric(outTable$carbon)
  barPlots <- ggplot(data = outTable, aes(x = simYear, y = carbon, fill = pool)) +
    geom_col(position = "fill") + 
    scale_fill_discrete(name = "carbon pool") +
    labs(x = "Year", y = "proportion") + 
    theme_bw()
  
  quickPlot::Plot(barPlots, addTo = 'barPlots', title = "proportion of C in DOM/AGB/BGB")
}
