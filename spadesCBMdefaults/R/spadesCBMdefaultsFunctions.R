#' calculates the decay rate based on mean annual temperature
#' and other parameters
#'
#' @param meanAnnualTemp scalar temperature in degrees Celsius
#' @param baseDecayRate scalar base decay rate constant
#' @param q10 the scalar q10 value
#' @param tref the reference temperature
#' @param max the maximum allowed decay rate
#' @return the scalar rate of decay
#'
#' this is the ecological theory for decomposing being used in CBM
#'
decayRate <- function(meanAnnualTemp, baseDecayRate, q10, tref, max) {
  min(baseDecayRate * exp((meanAnnualTemp - tref) * log(q10) * 0.1), max)
}

#' returns a vector of decay rates where the indices of the vector
#' are the dom pools of CBM
#'
#' @param meanAnnualTemp scalar temperature in deg Celcius
#' @param decayParameters table of decay parameters for calculating
#' the temperature dependant decay rate
#' @return the vector of decay rates
getDecayRates <- function(meanAnnualTemp, decayParameters, domPools) {
  decayRateOfNum <- function(decayParameter) {
    decayRate(
      meanAnnualTemp,
      decayParameter["OrganicMatterDecayRate"],
      decayParameter["Q10"],
      decayParameter["ReferenceTemp"],
      decayParameter["MaxDecayRate"]
    )
  }
  result <- apply(decayParameters, 1, decayRateOfNum)
  names(result) <- domPools[, "name"]
  return(result)
}

spatialUnitDecayRates <- function(climate, decayparameters, domPools) {
  decayRates <- t(apply(
    as.matrix(climate[, "MeanAnnualTemperature"]), 1,
    function(t) getDecayRates(t, decayparameters, domPools)
  ))
  decayRates <- cbind(climate[, "SpatialUnitID"], decayRates)
  colnames(decayRates)[1] <- "SpatialUnitID"
  return(decayRates)
}

#' calculates a portion of the dom decay matrix
#'
#' @param mat the datatable
#' @param decayRates vector of annual decay rates by dom pool
#' @param propToAtmosphere vector of the proportions of decay emitted to the atmosphere as CO2 by this process
#' @param src the integer code for the dom pool being decayed
#' @param dst the integer code for the dom pool receving non-emitted decayed matter
#' @param emission the integer code for the CO2 pool
#' @return A modified copy of the input \code{mat}
domDecayMatrixItem <- function(mat, decayRates, propToAtmosphere, src, dst, emission) {
  offset <- HardwoodFineRoots
  mat <- rbind(mat, c(src, src, 1 - decayRates[src - offset]))
  mat <- rbind(mat, c(src, dst, decayRates[src - offset] * (1 - propToAtmosphere[src - offset])))
  mat <- rbind(mat, c(src, emission, decayRates[src - offset] * propToAtmosphere[src - offset]))
  return(mat)
}

#' compute a single dom decay matrix based on the specified table of decay rates
#'
#' @param decayRates vector of decay rates (each element represents a dom pool)
#' @param decayparameters table of cbm decay parameters
domDecayMatrix <- function(decayRates, decayParameters, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)
  propToAtmosphere <- decayParameters[, "PropToAtmosphere"]
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, AboveGroundVeryFastSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, BelowGroundVeryFastSoil, BelowGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, AboveGroundFastSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, BelowGroundFastSoil, BelowGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, MediumSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, SoftwoodStemSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, SoftwoodBranchSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, HardwoodStemSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, HardwoodBranchSnag, AboveGroundSlowSoil, CO2)

  return(mat)
}

#' compute all dom decay matrices in coordinate matrix format.
#' The first column in the specified decayRates parameter acts as the key to
#' each matrix
#'
#' @param decayRates matrix of decay rates column 1 is the key for the values
#' in columns 1:n and columns 1:n are the dom pool specific decay rates
#' @param decayparameters table of cbm decay parameters
computeDomDecayMatrices <- function(decayRates, decayParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(decayRates)) {
    mat <- domDecayMatrix(decayRates[x, -1], decayParameters, PoolCount)
    mat <- cbind(decayRates[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}

slowDecayMatrix <- function(decayRates, decayParameters, PoolCount) {
  offset <- HardwoodFineRoots
  mat <- getIdentityCoordinateMatrix(PoolCount)
  propToAtmosphere <- decayParameters[, "PropToAtmosphere"]
  mat <- rbind(mat, c(AboveGroundSlowSoil, AboveGroundSlowSoil, 1 - decayRates[AboveGroundSlowSoil - offset]))
  mat <- rbind(mat, c(AboveGroundSlowSoil, CO2, decayRates[AboveGroundSlowSoil - offset] * propToAtmosphere[AboveGroundSlowSoil - offset]))
  mat <- rbind(mat, c(BelowGroundSlowSoil, BelowGroundSlowSoil, 1 - decayRates[BelowGroundSlowSoil - offset]))
  mat <- rbind(mat, c(BelowGroundSlowSoil, CO2, decayRates[BelowGroundSlowSoil - offset] * propToAtmosphere[AboveGroundSlowSoil - offset]))
  return(mat)
}

computeSlowDecayMatrices <- function(decayRates, decayParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(decayRates)) {
    mat <- slowDecayMatrix(decayRates[x, -1], decayParameters, PoolCount)
    mat <- cbind(decayRates[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}

computeSlowMixingMatrix <- function(slowMixingRate, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)
  mat <- rbind(mat, c(AboveGroundSlowSoil, BelowGroundSlowSoil, slowMixingRate))
  mat <- rbind(mat, c(AboveGroundSlowSoil, AboveGroundSlowSoil, 1 - slowMixingRate))
  mat <- cbind(rep(1, nrow(mat)), mat)
  colnames(mat) <- c("id", "row", "col", "value")
  return(mat)
}

domTurnOverMatrix <- function(turnoverParam, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)

  mat <- rbind(mat, c(SoftwoodStemSnag, SoftwoodStemSnag, 1 - turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodStemSnag, MediumSoil, turnoverParam["StemSnagTurnoverRate"]))

  mat <- rbind(mat, c(SoftwoodBranchSnag, SoftwoodBranchSnag, 1 - turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodBranchSnag, AboveGroundFastSoil, turnoverParam["BranchSnagTurnoverRate"]))

  mat <- rbind(mat, c(HardwoodStemSnag, HardwoodStemSnag, 1 - turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodStemSnag, MediumSoil, turnoverParam["StemSnagTurnoverRate"]))

  mat <- rbind(mat, c(HardwoodBranchSnag, HardwoodBranchSnag, 1 - turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodBranchSnag, AboveGroundFastSoil, turnoverParam["BranchSnagTurnoverRate"]))

  return(mat)
}

computeDomTurnoverMatrices <- function(turnoverParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(turnoverParameters)) {
    mat <- domTurnOverMatrix(turnoverParameters[x, ], PoolCount)
    mat <- cbind(turnoverParameters[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}

biomassTurnoverMatrix <- function(turnoverParam, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)

  mat <- rbind(mat, c(SoftwoodMerch, SoftwoodMerch, 1 - turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodMerch, SoftwoodStemSnag, turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodFoliage, SoftwoodFoliage, 1 - turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(SoftwoodFoliage, AboveGroundVeryFastSoil, turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(SoftwoodOther, SoftwoodOther, 1 - turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodOther, SoftwoodBranchSnag,
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodOther, AboveGroundFastSoil,
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, SoftwoodCoarseRoots, 1 - turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, AboveGroundFastSoil,
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, BelowGroundFastSoil,
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, SoftwoodFineRoots, 1 - turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, AboveGroundVeryFastSoil,
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, BelowGroundVeryFastSoil,
                      (1 - turnoverParam["FineRootAGSplit"]) * turnoverParam["FineRootTurnProp"]))

  mat <- rbind(mat, c(HardwoodMerch, HardwoodMerch, 1 - turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodMerch, HardwoodStemSnag, turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodFoliage, HardwoodFoliage, 1 - turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(HardwoodFoliage, AboveGroundVeryFastSoil, turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(HardwoodOther, HardwoodOther, 1 - turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodOther, HardwoodBranchSnag,
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodOther, AboveGroundFastSoil,
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, HardwoodCoarseRoots, 1 - turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, AboveGroundFastSoil,
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, BelowGroundFastSoil,
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, HardwoodFineRoots, 1 - turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, AboveGroundVeryFastSoil,
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, BelowGroundFastSoil,
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, AboveGroundVeryFastSoil,
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, BelowGroundVeryFastSoil,
                      (1 - turnoverParam["FineRootAGSplit"]) * turnoverParam["FineRootTurnProp"]))
  return(mat)
}

computeBioTurnoverMatrices <- function(turnoverParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(turnoverParameters)) {
    mat <- biomassTurnoverMatrix(turnoverParameters[x, ], PoolCount)
    mat <- cbind(turnoverParameters[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")
  return(matrices)
}

loadDisturbanceMatrixIds <- function(disturbanceMatrixValues, dbPools) {
  ids <- unique(disturbanceMatrixValues[, "disturbance_matrix_id"])

  # matches the pool def id by name for safety
  getPoolDefId <- function(dbPoolID) {
    dbPoolName <- dbPools[as.numeric(dbPools[, "id"]) == dbPoolID, "name"]
    get(dbPoolName)
  }

  # fill in the neutral transfers not covered by the matrix data
  # ie. the matrix will not withdraw from any of the following pools
  neutrals <- NULL
  neutrals <- rbind(neutrals, c(Input, Input, 1))
  neutrals <- rbind(neutrals, c(CO2, CO2, 1))
  neutrals <- rbind(neutrals, c(CH4, CH4, 1))
  neutrals <- rbind(neutrals, c(CO, CO, 1))
  neutrals <- rbind(neutrals, c(Products, Products, 1))

  loadMatrix <- function(dmid) {
    dbmat <- disturbanceMatrixValues[disturbanceMatrixValues[, "disturbance_matrix_id"] == dmid, ]
    mat <- matrix(0, ncol = 3, nrow = nrow(dbmat))

    for (i in 1:nrow(dbmat)) {
      mat[i, ] <- c(
        getPoolDefId(dbmat[i, "source_pool_id"]),
        getPoolDefId(dbmat[i, "sink_pool_id"]),
        dbmat[i, "proportion"]
      )
    }

    mat <- rbind(mat, neutrals)

    return(mat)
  }
  ## ?? not sure is this is needed in the sim$ since it is not openly used anywhere else
  allMatrices <- NULL
  # return the matrix ids of the loaded matrices
  for (x in 1:length(ids)) {
    dm <- loadMatrix(ids[x])
    dm <- cbind(rep(ids[x], nrow(dm)), dm)
    allMatrices <- rbind(allMatrices, dm)
  }
  colnames(allMatrices) <- c("id", "row", "col", "value")
  return(allMatrices)
}
