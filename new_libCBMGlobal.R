# start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

## PFC work-around
## this is a work-around for working from PFC...R cannot connect to URL

#options("download.file.method" = "wininet")

while (!require("SpaDES.project")) {
  install.packages("SpaDES.project", repos = "https://predictiveecology.r-universe.dev")
  require(SpaDES.project)
}


out <- SpaDES.project::setupProject(
  params = list(
    CBM_defaults = list(
      .useCache = TRUE
    ),
    CBM_dataPrep_SK = list(
      .useCache = TRUE
    ),
    CBM_vol2biomass = list(
      .useCache = TRUE
    ),
    CBM_core = list(
      #.useCache = "init", #c(".inputObjects", "init")
      .plotInterval = 1,
      .plotInitialTime = 1990,
      poolsToPlot = c("totalCarbon"),
      spinupDebug = FALSE ## TODO: temporary
    )
  ),
  paths = list(projectPath = "C:/Celine/github/spadesCBM",
               modulePath = "modules"),
  packages = c("googledrive"),
  require =
    c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
      "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)",
      "PredictiveEcology/CBMutils@development"),
  modules = c("PredictiveEcology/CBM_defaults@main",
              "PredictiveEcology/CBM_dataPrep_SK@development",
              "PredictiveEcology/CBM_vol2biomass@CBM_vol2biomass_SK",
              "PredictiveEcology/CBM_core@main"
  ),
  times = list(start = 1990.00, end = 1993.00)

)


# times <- list(start = 1990.00, end = 1993.00)
# parameters <- list(
#   CBM_defaults = list(
#     .useCache = TRUE
#   ),
#   CBM_dataPrep_SK = list(
#     .useCache = TRUE
#   ),
#   CBM_vol2biomass = list(
#     .useCache = TRUE
#   ),
#   CBM_core = list(
#     #.useCache = "init", #c(".inputObjects", "init")
#     .plotInterval = 1,
#     .plotInitialTime = 1990,
#     poolsToPlot = c("totalCarbon"),
#     spinupDebug = FALSE ## TODO: temporary
#   )
# )

out$objects <- list(
  dbPath = file.path(out$paths$modulePath, "CBM_defaults", "data","cbm_defaults", "cbm_defaults.db"),
  sqlDir = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults")
)
#out$debug = 1 is the default which is like TRUE
out$loadOrder <- unlist(out$modules)


spadesCBMrunsSK <- do.call(simInitAndSpades, out)

#   simInitAndSpades(times = out$times,
#                                     params = parameters,
#                                     modules = out$modules,
#                                     objects = objects,
#                                     #paths = paths, ## not needed when using
#                                     #setPaths() above
#                                     loadOrder = unlist(out$modules),
#                                     debug = TRUE
# )


