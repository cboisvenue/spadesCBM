# Plot growth matrices
library(SpaDES)
for(j in 1:5) {
  dev(j+1, width=12, height=12);
  par(mfrow=c(5,5))
  hashes <- ls(gcHash)
  ord <- order(as.numeric(hashes))
  for(gcid in na.omit(ls(gcHash)[ord][1:25 + 25*(j-1)])) {
    # If all the values in column 2 of matrix are 0 (or sum to something close to zero),
    #  use second set of columns, which are 3 columns over 
    ord2 <- order(as.numeric(names(as.list(gcHash[[gcid]]))))
    vals <- do.call(rbind, as.list(gcHash[[gcid]])[ord2])
    vals <- cbind(age=1:NROW(vals), vals)
    addTo <- if(sum(vals[,2])<0.001) 1 else 0
    woodtype <- c("Softwood", "Hardwood")
    
    plot(xlab="", ylab="", type="l",main=paste(gcid, woodtype[addTo+1]),
         vals[,1], vals[,2+addTo*3]);
    lines(vals[,1], vals[,3+addTo*3], col="blue"); 
    lines(vals[,1], vals[,4+addTo*3], col="red")
  }
}


# Plot loaded growth curves - check curves as the come in
## need to run the RunSK_new.R code until line 420 to get the growthCurveComponents

growthIn <- as.data.frame(growthCurveComponents)

library(ggplot2)
a <- ggplot(data=growthIn,aes(x=Age,y=MerchVolume,group=GrowthCurveComponentID,colour=GrowthCurveComponentID)) +geom_line()

# growth increment as they come out of the Volume to Biomass function

growthInc <- as.data.table(growth_increments)

biomInc <- growthInc[,.(V1,V2,V9 = rowSums(growthInc[,V3:V8]))]

b <- ggplot(data=biomInc, aes(x=V2,y=V9,group=V1,colour=V1)) +geom_line()

# conclusion, there are things wrong with these...but not as wrong as it first seemed. 
# Celine will proceed with building these from straight biomass equations to see if it is better
