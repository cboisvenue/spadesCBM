### calcTurnoverRates ------------------------------------------------------------------
# matching the turnover rates to the spatial unit

calcTurnoverRates <- function(turnoverRates, spatialUnitIds, spatialUnits) {
  turnoverRates <- as.data.table(turnoverRates)
  SPU <- as.data.table(spatialUnitIds)
  SPU <- SPU[SpatialUnitID %in% unique(spatialUnitIds)]
  SPU <- merge(SPU, turnoverRates, by = "EcoBoundaryID", all.y = FALSE)
  SPU <- SPU[SpatialUnitID %in% unique(spatialUnits), ]
  return(SPU)
}
### END calcTurnoverRates ------------------------------------------------------------------

# calculate c transfer for disturbances and annual processes post disturbance-----------------------------
# mismatch in c transfers when disturbance happens in cpp processing so bypassing it
# c transfer functions: one for the disturbance (so small decimals errors in
# matrices are corrected), and one for the annual processes
cTransferDist <- function(standIn = preD, transProp = distProp) {
  standIn <- as.data.table(cbind(standIn, row = c(1:length(standIn))))
  names(standIn) <- c("V1", "row")
  transProp <- as.data.table(transProp)
  rowJoin <- standIn[transProp, on = "row"][, fluxOut := (V1 * value)]

  # calculate carbon out and in
  outC <- rowJoin[, .(outC = sum(fluxOut)), by = row]
  inC <- rowJoin[, .(inC = sum(fluxOut)), by = col]
  names(inC) <- c("row", "inC")
  fluxes <- merge(outC, inC, by = "row", all = TRUE)
  # no NAs allowed
  fluxes$inC[which(is.na(fluxes$inC))] <- 0

  standOut <- standIn[fluxes, on = "row"][, .(calcDist = V1 - outC + inC), by = "row"]

  # these two lines are "fixes for smal decimal differences that should not be there
  # pools can't go negative
  standOut[calcDist < 0, "calcDist"] <- 0
  # if it does not transfer to itself it has to end-up empty
  rowsTofix <- transProp[row == col, .N, by = "row"]
  standOut[!(row %in% rowsTofix$row), "calcDist"] <- 0

  return(standOut)
}
## not doing the "fixes" in this one for the small decimal errors in the dist matrices
cTransfer <- function(standIn = preD, transProp = distProp) {
  standIn <- as.data.table(cbind(standIn, row = c(1:length(standIn))))
  names(standIn) <- c("V1", "row")
  transProp <- as.data.table(transProp)
  rowJoin <- standIn[transProp, on = "row"][, fluxOut := (V1 * value)]

  # calculate carbon out and in
  outC <- rowJoin[, .(outC = sum(fluxOut)), by = row]
  inC <- rowJoin[, .(inC = sum(fluxOut)), by = col]
  names(inC) <- c("row", "inC")
  fluxes <- merge(outC, inC, by = "row", all = TRUE)
  # no NAs allowed
  fluxes$inC[which(is.na(fluxes$inC))] <- 0

  standOut <- standIn[fluxes, on = "row"][, .(calcDist = V1 - outC + inC), by = "row"]

  return(standOut)
}
# END calculate c transfer for diturbances and annual processes post disturbance-----------------------------

# copies from spadesCBMdefault functions ------------------------------------
matrixHash <- function(x) {
  keys <- unique(x[, 1])
  e <- new.env(hash = TRUE, size = length(keys), parent = emptyenv())
  apply(as.matrix(keys), 1, function(key) {
    assign(toString(key), x[x[, 1] == key, 2:ncol(x)], envir = e)
  })
  return(e)
}
