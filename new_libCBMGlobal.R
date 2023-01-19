# start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

##TODO not working
#Require::Require("PredictiveEcology/SpaDES.project@development")
#
# SpaDES.project::setupProject(paths = list(projectPath = "~/spadesCBM"),
#                              require =
#                                c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
#                                  "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
#                              modules = c("PredictiveEcology/CBM_defaults@main",
#                                          "PredictiveEcology/CBM_vol2biomass@main",
#                                          "PredictiveEcology/CBM_dataPrep_SK@main",
#                                          "PredictiveEcology/CBM_core@main"
#                                          )
#
# )

##This is my alternative
pkgDir <- file.path(tools::R_user_dir(basename(getwd()), "data"), "packages",
                    version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)

# had to go back to using this??
options("download.file.method" = "wininet")

if (!require("remotes")) {
  install.packages("remotes")
}

remotes::install_github("PredictiveEcology/Require@development")
library(Require)
# Require::Require("PredictiveEcology/SpaDES.project@transition")
# library(SpaDES.project)
# SpaDES.project::setupProject(paths = list(projectPath = "~/spadesCBM"),
#                              require =
#                                c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
#                                  "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
#                              modules = c("PredictiveEcology/CBM_defaults@main",
#                                          "PredictiveEcology/CBM_vol2biomass@main",
#                                          "PredictiveEcology/CBM_dataPrep_SK@main",
#                                          "PredictiveEcology/CBM_core@main"
#                                          )
#
# )

Require(c("PredictiveEcology/SpaDES.project@transition", "SpaDES", "reproducible",
          "SpaDES.core (>=1.1.0)", "SpaDES.tools (>= 1.0.0)",
          "googledrive", 'RCurl', 'XML', "PredictiveEcology/CBMutils@development",
          "data.table", "devtools"), # comes up with an error so added "devtools'
        require = c("SpaDES.core", "devtools", "magrittr"), # call `require` only on this package (same as `library`)
        verbose = 1)
#there is a problem with CBMutils
#Require("devtools")
#load_all("C:/Celine/github/CBMutils")

Require("magrittr") # this is needed to use "%>%" below
Require("SpaDES.core")
## straigh copy over from old global
cacheDir <- reproducible::checkPath("cache", create = TRUE)
moduleDir <- reproducible::checkPath("modules")
inputDir <- reproducible::checkPath("inputs", create = TRUE)
outputDir <- reproducible::checkPath("outputs", create = TRUE)
scratchDir <- file.path(tempdir(), "scratch", "CBM") |> reproducible::checkPath(create = TRUE)

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

modules <- list("CBM_defaults", "CBM_dataPrep_SK", "CBM_vol2biomass", "CBM_core")
objects <- list(
  dbPath = file.path(inputDir, "cbm_defaults", "cbm_defaults.db"),
  sqlDir = file.path(inputDir, "cbm_defaults")
)

setPaths(
  cachePath = cacheDir,
  modulePath = moduleDir,
  inputPath = inputDir,
  outputPath = outputDir,
  rasterPath = scratchDir
)

quickPlot::dev.useRSGD(FALSE)
dev()
clearPlot()

spadesCBMrunsSK <- simInitAndSpades(times = times,
                                    params = parameters,
                                    modules = modules,
                                    objects = objects,
                                    #paths = paths, ## not needed when using
                                    #setPaths() above
                                    loadOrder = unlist(modules),
                                    debug = TRUE
)
