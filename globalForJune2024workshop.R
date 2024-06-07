## Global script to run spadesCBM-Cpp for SpaDES training class June 2024. The
## input data matches the Boisvenue et al 2016 spatial CBM runs and are for the
## managed forests of SK.

# CBoisvenue
# June 6, 2024

##NOTES: efforts are currently underway to replace C++ functions that spadesCBM
##relies on with Python-based libcbm a prototype developed by the Carbon
##Accounting Team. The spadesCBM run here is what has been used for
##SpaDES-related carbon work to date.

getOrUpdatePkg <- function(p, minVer, repo) {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    if (missing(repo)) repo = c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("SpaDES.project", repo = c("predictiveecology.r-universe.dev", getOption("repos")))

repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
install.packages(c("reproducible", "SpaDES.core","Require"), repos = repos)

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
    reproducible.destinationPath = "inputs",
    reproducible.useTerra = TRUE,
    reproducible.rasterRead = "terra::rast",
    ## These are for speed
    reproducible.useMemoise = TRUE,
    # Require.offlineMode = TRUE,
    spades.moduleCodeChecks = FALSE
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
      .plotInitialTime = 1990,
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
      "PredictiveEcology/reproducible",
      "PredictiveEcology/SpaDES.core",
      "PredictiveEcology/CBMutils"),
  modules = c("PredictiveEcology/CBM_defaults@main",
              "PredictiveEcology/CBM_dataPrep_SK@main",
              "PredictiveEcology/CBM_vol2biomass@main",
              "PredictiveEcology/CBM_core@main"
  ),
  times = list(start = 1990.00, end = 1993.00),
  setLinuxBinaryRepo = FALSE, ## TODO: interferes with other package installation
  updateRprofile  = FALSE ## TODO: verify what it is doing
)

out$objects <- list(
  dbPath = file.path(out$paths$modulePath, "CBM_defaults", "data","cbm_defaults",
                     "cbm_defaults.db"),
  sqlDir = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults")
)

out$loadOrder <- unlist(out$modules)


# creating a data.frame to save $cbmPools and $NPP at specific time steps.
out$outputs <- as.data.frame(expand.grid(objectName = c("cbmPools", "NPP"),
                                         saveTime = sort(c(out$times$start,
                                                           out$times$start +
                                                             c(1:(out$times$end - out$times$start))
                                         ))))
spadesCBMrunsSK <- do.call(SpaDES.core::simInitAndSpades, out)



