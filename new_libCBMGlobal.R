# start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

while (!require("SpaDES.project")) {
  install.packages("SpaDES.project", repos = "https://predictiveecology.r-universe.dev")
  require(SpaDES.project)
}
out <- SpaDES.project::setupProject(paths = list(projectPath = "C:/Celine/github/spadesCBM",
                                          modulePath = "modules"),
                             packages = c("googledrive"),
                             require =
                               c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
                                 "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
                             modules = c("PredictiveEcology/CBM_defaults@main",
                                         "PredictiveEcology/CBM_dataPrep_SK@development",
                                         "PredictiveEcology/CBM_vol2biomass@main",
                                         "PredictiveEcology/CBM_core@main"
                             )
)


times <- list(start = 1990.00, end = 1993.00)
parameters <- list(
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
)

objects <- list(
  dbPath = file.path(out$paths$inputPath, "cbm_defaults", "cbm_defaults.db"),
  sqlDir = file.path(out$paths$inputPath, "cbm_defaults")
)

# quickPlot::dev.useRSGD(FALSE)
# dev()
# clearPlot()

spadesCBMrunsSK <- simInitAndSpades(times = times,
                                    params = parameters,
                                    modules = out$modules,
                                    objects = objects,
                                    #paths = paths, ## not needed when using
                                    #setPaths() above
                                    loadOrder = unlist(out$modules),
                                    debug = TRUE
)
