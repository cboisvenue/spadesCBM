# start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

## PFC work-around
## this is a work-around for working from PFC...R cannot connect to URL
##This current set up sets options to wininet on any NRCan computer at PFC
if (.Platform$OS.type == "windows") {
  ## based on <https://stackoverflow.com/a/14357701/1380598>
  ip <- system("ipconfig", intern = TRUE)
  ip <- ip[grep("IPv4", ip)]
  ip <- gsub(".*? ([[:digit:]])", "\\1", ip)

  if (any(grepl("^132[.]156[.]", ip))) {
    options("download.file.method" = "wininet")
  }
}
#options("download.file.method" = "wininet")

if (all(tryCatch(packageVersion("SpaDES.project") < "0.0.7.9023", error = function(e) TRUE),
        tryCatch(packageVersion("Require") < "0.2.6.9010", error = function(e) TRUE))) {
  install.packages(c("Require", "SpaDES.project"),
                   repos = c("https://predictiveecology.r-universe.dev",
                             getOption("repos")))
}



out <- SpaDES.project::setupProject(
  name = "spadesCBM",
  paths = list(projectPath =
                 switch(user(),
                        cboisven = "C:/Celine/github/spadesCBM",
                        "~/GitHub/spadesCBM"),
               modulePath = "modules"
               ),
  options = list(
    repos = c(PE = "https://predictiveecology.r-universe.dev/", ## latest PredictievEcology packages
              SF = "https://r-spatial.r-universe.dev/",         ## latest sf and other spatial packages
              CRAN = "https://cloud.r-project.org"),
    reproducible.destinationPath = "inputs", ## TODO: SpaDES.project#24
    reproducible.useTerra = TRUE,
    reproducible.rasterRead = "terra::rast"
  ),
  params = list(
    CBM_defaults = list(
      .useCache = TRUE
    ),
    CBM_dataPrep_SK = list(
      .useCache = TRUE
    ),
    CBM_vol2biomass = list(
      outputFigurePath = switch(user(),
                                achubaty =
                                file.path("outputs", "figures", "CBM_vol2biomass"),
                                NA), ## NA means use default: './modules/CBM_vol2biomass/figures/'
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
  packages = "pkgload",
  require =
    c("googledrive",
      "PredictiveEcology/reproducible@dev (HEAD)", #development (>= 1.2.16.9017)",
      "PredictiveEcology/SpaDES.core@development (>= 1.1.1)",
      "PredictiveEcology/CBMutils@development (HEAD)"),
  modules = c("PredictiveEcology/CBM_defaults@main",
              "PredictiveEcology/CBM_dataPrep_SK@development",
              "PredictiveEcology/CBM_vol2biomass@CBM_vol2biomass_SK",
              "PredictiveEcology/CBM_core@main"
  ),
  times = list(start = 1990.00, end = 1993.00),
  setLinuxBinaryRepo = FALSE, ## TODO: interferes with other package installation
  updateRprofile  = FALSE ## TODO: verify what it is doing
)


out$objects <- list(
  dbPath = file.path(out$paths$modulePath, "CBM_defaults", "data","cbm_defaults", "cbm_defaults.db"),
  sqlDir = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults")
)
#out$debug = 1 is the default which is like TRUE but not quite
out$loadOrder <- unlist(out$modules)

# This line is when is to locally load the CBMutils package, to debug or modify
# CBMutils functions
# pkgload::load_all("C:\\Celine\\github\\CBMutils")

##TODO: deal with plotting issues. Plotting takes longer than the simulation,
## and if the window is not big enough, the simulation will crash. If you open a
## window (as the lines below do) the sim will run.
# quickPlot::dev.useRSGD(FALSE)
# dev()
# clearPlot()

# creating a data.frame to save $cbmPools and $NPP at specific time steps.
out$outputs <- as.data.frame(expand.grid(objectName = c("cbmPools", "NPP"),
                                         saveTime = sort(c(out$times$start,
                                                           out$times$start +
                                                             c(1:(out$times$end - out$times$start))
                                                           ))))
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


