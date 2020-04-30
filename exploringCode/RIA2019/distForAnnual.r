# Disturbances after dist through annual process in the C++ functions
distForAnnual <- dcast(toAddDist,pixelGroup~row, value.var="calcDist")


names(distForAnnual) <- c("pixelGroup",as.character(poolsToRows$variable))
distForAnnual[order(pixelGroup),]
countDist <- pixelCount[ pixelGroup %in% c((maxPixelGroup+1):max(pixelCount$pixelGroup)),N]
ages <- unique(distPixels[,ages, by=newGroup])
ages[order(newGroup),]
toAdd[order(pixelGroup),]
distForAnnProcesses <- cbind(toAdd[,.(ages,spatial_unit_id,growth_curve_component_id,growth_curve_id,ecozones)],
                  distForAnnual)


# Changing the vectors and matrices that need to be changed to process this year's growth
distPoolsIn <- as.matrix(distForAnnProcesses[,Input:Products])
# disturbances are processed below, outside the Rcpp functions
eventDMIDs <- rep(0,dim(distForAnnProcesses)[1])##--##c( ...dim(pixelGroupForAnnual)[1] - length(DMIDS)),DMIDS)
# ecoToSpu <- as.data.frame(sim$cbmData@spatialUnitIds[,c(1,3)])
# names(ecoToSpu) <- c("spatial_unit_id","ecozones")
# ecozones <- merge(pixelGroupForAnnual,ecoToSpu)
# ecozones <- ecozones[order(pixelGroup),]
sim$ecozones <- distForAnnProcesses$ecozones
sim$ages <- distForAnnProcesses[,ages]
sim$nStands <- length(sim$ages)
sim$gcids <- distForAnnProcesses[,growth_curve_component_id]
sim$spatialUnits <- distForAnnProcesses[,spatial_unit_id]

sim$opMatrixCBM <- cbind(
  rep(0,sim$nStands), #disturbance
  1:sim$nStands, #growth 1
  sim$ecozones, #domturnover
  sim$ecozones, #bioturnover
  1:sim$nStands, #overmature
  1:sim$nStands, #growth 2
  sim$spatialUnits, #domDecay
  sim$spatialUnits, #slow decay
  rep(1, sim$nStands) #slow mixing
)
colnames(sim$opMatrixCBM) <- c("disturbance", "growth 1", "domturnover",
                               "bioturnover", "overmature", "growth 2",
                               "domDecay", "slow decay", "slow mixing")

sim$opMatrixCBM[,"disturbance"]<-eventDMIDs

sim$allProcesses <- list(
  Disturbance=sim$processes$disturbanceMatrices,
  Growth1=NULL,
  DomTurnover=sim$processes$domTurnover,
  BioTurnover=sim$processes$bioTurnover,
  OvermatureDecline=NULL,
  Growth2=NULL,
  DomDecay=sim$processes$domDecayMatrices,
  SlowDecay=sim$processes$slowDecayMatrices,
  SlowMixing=sim$processes$slowMixingMatrix
)  


# ! ----- EDIT BELOW ----- ! #
# 
# compute the growth increments


growthAndDecline <- ComputeGrowthAndDeclineMatrices2(
  growthIncrements = sim$gcHash,
  ages = sim$ages,
  gcids = sim$gcids,
  pools = distPoolsIn,
  rootParameters = as.data.frame(t(sim$cbmData@rootParameters[1,])),
  turnoverParams = as.data.frame(t(sim$cbmData@turnoverRates[1,])),
  biomassToCarbonRate = as.numeric(sim$cbmData@biomassToCarbonRate),
  swMult = 0.5, hwMult = 0.5)

sim$allProcesses$Growth1=growthAndDecline$Growth
sim$allProcesses$Growth2=growthAndDecline$Growth
sim$allProcesses$OvermatureDecline=growthAndDecline$OvermatureDecline
rm(growthAndDecline)
# this has to be the same length as the DT going in for processing
#sim$opMatrixCBM[,"disturbance"]<-eventDMIDS

distPoolsOut <- StepPools(pools=distPoolsIn, 
                       opMatrix = sim$opMatrixCBM, 
                       flowMatrices = sim$allProcesses)
