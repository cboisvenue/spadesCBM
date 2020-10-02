m3ToBiomIncOnlyPlots <- function(inc=increments){
  gInc <- as.data.table(inc)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim,]
  gc <- data.table::melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:dim(gInc)[2])
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop) {
    ggplot(data=gc[id == idLoop], 
           aes(x=age,y=value,group=variable,colour=variable)) + geom_line()
  })
  names(plots) <- paste0("id",names(plots))
  return(plots)
}

