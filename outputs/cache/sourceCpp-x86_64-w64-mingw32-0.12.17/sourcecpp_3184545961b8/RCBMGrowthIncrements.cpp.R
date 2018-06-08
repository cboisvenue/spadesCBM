`.sourceCpp_1_DLLInfo` <- dyn.load('C:/Celine/GitHub/spadesCBM/outputs/cache/sourceCpp-x86_64-w64-mingw32-0.12.17/sourcecpp_3184545961b8/sourceCpp_2.dll')

StepPoolsRef <- Rcpp:::sourceCppFunction(function(pools, opMatrix, flowMatrices) {}, TRUE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_StepPoolsRef')
StepPools <- Rcpp:::sourceCppFunction(function(pools, opMatrix, flowMatrices) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_StepPools')
ComputeOvermatureDecline <- Rcpp:::sourceCppFunction(function(growthIncrements, turnoverParams) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_ComputeOvermatureDecline')
ComputeGrowthIncrements <- Rcpp:::sourceCppFunction(function(growthIncrements, ages, gcids, pools, rootParameters, biomassToCarbonRate, swMult = 1.0, hwMult = 1.0) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_ComputeGrowthIncrements')
ComputeGrowthAndDeclineMatrices <- Rcpp:::sourceCppFunction(function(growthIncrements, decline) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_ComputeGrowthAndDeclineMatrices')
ComputeGrowthAndDeclineMatrices2 <- Rcpp:::sourceCppFunction(function(growthIncrements, ages, gcids, pools, rootParameters, turnoverParams, biomassToCarbonRate, swMult = 1.0, hwMult = 1.0) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_ComputeGrowthAndDeclineMatrices2')
Spinup <- Rcpp:::sourceCppFunction(function(pools, opMatrix, constantProcesses, growthIncrements, ages, gcids, historicdmids, lastPassdmids, delays, minRotations, maxRotations, returnIntervals, rootParameters, turnoverParams, biomassToCarbonRate, debug = FALSE) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_Spinup')

rm(`.sourceCpp_1_DLLInfo`)
