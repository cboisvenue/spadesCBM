# quick check of volume curves
# quick check of increment translation
# CBoisvenue November 15, 2019
#-------------------------------------------

# volumes coming in
checkVolIn <- function(simOut = spadesCBMout){
volInc <- fread(simOut$gcurveComponentsFileName)
library(ggplot2)
volCurves <- ggplot(data=volInc, aes(x=Age,y=MerchVolume,group=GrowthCurveComponentID, colour=GrowthCurveComponentID)) +
  geom_line()
return(volCurves)
}

checkVol <- checkVolIn(simOut = spadesCBMout)

# metadata for the growth curves coming in
# this will work until the gcMeta order of columns changes
gcMeta <- function(simOut = spadesCBMout){
gcMeta <- fread(simOut$gcurveFileName)
gcMeta <- unique(gcMeta[,c(2,3,4,5,6,9,10)])
}
metaCheck <- gcMeta()


# growth increments check
m3ToVolCheckPlots <- function(sim=spadesCBMout){
  gInc <- as.data.table(sim$growth_increments)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim,]
  gc <- melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:8)
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop) {
    ggplot(data=gc[id == idLoop], 
           aes(x=age,y=value,group=variable,colour=variable)) + geom_line()
  })
  
  return(plots)
}

incPlots <- m3ToVolCheckPlots()


# just looking at the increments:
m3ToBiomIncOnlyPlots <- function(inc=increments){
  gInc <- as.data.table(inc)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim,]
  gc <- melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:8)
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop) {
    ggplot(data=gc[id == idLoop], 
           aes(x=age,y=value,group=variable,colour=variable)) + geom_line()
  })
  
  return(plots)
}
#
