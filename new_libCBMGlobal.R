# start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

## PFC work-around
## this is a work-around for working from PFC...R cannot connect to URL

#need to compare current ipaddress to "132.156" "at PFC"
#options("download.file.method" = "wininet")

while (!require("SpaDES.project")) {
  install.packages("SpaDES.project", repos = "https://predictiveecology.r-universe.dev")
  require(SpaDES.project)
}

##TODO Celine will check IP address first few numbers when at PFC next week.
##This current set up sets options to wininet on any NRCan computer at PFC or
##not.
##TODO Alex, whow do I check the IP of a computer and check if it stars by
##132.blabla
## WORKAROUND: setupProject overwriting .gitignore (SpaDES.project#23)
# file.copy(".gitignore", ".gitignore.orig")
# if (file.exists(".gitignore")) {
#   unlink(".gitignore")
# }

out <- SpaDES.project::setupProject(
  name = "spadesCBM",
  paths = list(projectPath =
                 switch(user(),
                        cboisven = "C:/Celine/github/spadesCBM",
                        "~/GitHub/spadesCBM"),
               modulePath = "modules"),
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
      "PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
      "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)",
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

# pkgload::load_all("C:\\Celine\\github\\CBMutils")

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


