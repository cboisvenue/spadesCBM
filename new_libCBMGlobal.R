## start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

# project basic setup -------------------------------------------------------------------------

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT
if (file.exists("spadesCBM.Renviron")) readRenviron("spadesCBM.Renviron") ## database credentials

.debug <- if (exists(".debug")) .debug else FALSE ## used to debug C++ code
.nodename <- Sys.info()[["nodename"]] ## current computer name; used to configure machine-specific settings
.user <- Sys.info()[["user"]] ## current computer username; used to configure user-specific settings

## define project directory - this code expects it is being run from this location
## **do not change the paths defined here**
## if you need to add a machine- or user-specific path, please do so _conditionally_
prjDir <- switch(.user, ## SpaDES.project::user() gets used below, but it's not available yet b/c not loaded
                 cboisven = "C:/Celine/github/spadesCBM",
                 "~/GitHub/spadesCBM") ## GitHub desktop client put cloned repos in ~/GitHub by default

## ensure script being run from the project directory -- in case user isn't using the Rstudio project
stopifnot(identical(normalizePath(prjDir), normalizePath(getwd())))

## use project-specific location for packages to avoid conflicts with other projects
## this is the default SpaDES.project will use (but it's not loaded yet)
pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                    version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

# R options (package options will be set in next section) -------------------------------------

cranRepos <- c(PE = "https://predictiveecology.r-universe.dev/", ## latest PredictiveEcology packages
               SF = "https://r-spatial.r-universe.dev/",         ## latest sf and other spatial packages
               CRAN = "https://cloud.r-project.org")

options(
  Ncpus = min(parallel::detectCores() / 2, 16L), ## default number of CPU cores to use, e.g. for pkg install
  repos = cranRepos
)

## work-around for working from PFC...R cannot connect to certain urls
## TODO: improve conditional by only using wininet if *at* PFC, not just on a PFC machine
##       e.g., use external IP address or machine names
if ((.Platform$OS.type == "windows") && grepl("[L|W]-VIC", .nodename)) {
  # options("download.file.method" = "wininet") ## TODO: not needed unless actually at PFC
}

## define specific package versions here that will be used below ------------------------------

needPkgs <- list(
  reproducible =  "PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
  SpaDES.core = "PredictiveEcology/SpaDES.core@development (>= 1.1.1)"
)

## workarounds for package installation etc. for SpaDES.project -------------------------------

if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## this is for macOS only, in order to install sf from source using homebrew
## and it's only needed to install latest development version of sf until v1.0.10 gets to CRAN
if (Sys.info()[["sysname"]] == "Darwin") {
  ## install sf + rgdal following <https://github.com/r-spatial/sf#macos>
  if (!require("sf", quietly = TRUE)) {
    # install.packages("sf", type = "source", configure.args = "--with-proj-lib=/usr/local/lib/") ## (intel)
    install.packages("sf", type = "source", configure.args = "--with-proj-lib=/opt/homebrew/lib/") ## (arm; M1/M2)
  }
  if (!require("rgdal", quietly = TRUE)) {
    # install.packages("rgdal", type = "source",
    #                  configure.args = c("--with-proj-lib=/usr/local/lib/",
    #                                     "--with-proj-include=/usr/local/include/")) ## (intel)
    # install.packages("rgdal", type = "source",
    #                  configure.args = c("--with-proj-lib=/usr/local/lib/",
    #                                     "--with-proj-include=/opt/homebrew/include/")) ## (arm; M1/M2)
    install.package("rgdal", type = "binary")
  }
}

## TODO: unresolved Require issues
# if (!require("Require", quietly = TRUE)) {
#   ## will install latest development version from PE r-universe
#   install.packages("Require", lib = pkgDir)
#   require("Require", lib.loc = pkgDir)
# }
#
# if (!require("SpaDES.core", quietly = TRUE)) {
#   Require::Install(needPkgs$SpaDES.core)
# }

## END WORKAROUNDS

## project setup using SpaDES.project --------------------------------------------------------------

if (!require("SpaDES.project", quietly = TRUE)) {
  install.packages("SpaDES.project", repos = "https://predictiveecology.r-universe.dev")
  require(SpaDES.project)
}

options(
  Require.updateRprofile = FALSE ## used by SpaDES.project
)

out <- SpaDES.project::setupProject(
  name = basename(prjDir), ## "spadesCBM"
  paths = list(projectPath = prjDir, ## set above because we need user info before packages installed
               packagePath = pkgDir, ## set above because we need user info before packages installed
               modulePath = "modules",
               inputPath = "inputs",
               outputPath = "outputs"),
  options = list(
    # repos = cranRepos, ## set above b/c needed for package installation prior to SpaDES.project
    reproducible.destinationPath = if (.user == "achubaty") "inputs" else NULL, ## TODO: SpaDES.project#24
    reproducible.rasterRead = "terra::rast",
    reproducible.useTerra = TRUE
  ),
  params = list(
    CBM_defaults = list(
      .useCache = TRUE
    ),
    CBM_dataPrep_SK = list(
      .useCache = TRUE
    ),
    CBM_vol2biomass = list(
      outputFigurePath = switch(.user,
                                achubaty = file.path("outputs", "figures", "CBM_vol2biomass"),
                                NA), ## NA means use default: './modules/CBM_vol2biomass/figures/'
      .useCache = TRUE
    ),
    CBM_core = list(
      #.useCache = "init", #c(".inputObjects", "init")
      .plotInterval = 1,
      .plotInitialTime = times$start,
      poolsToPlot = c("totalCarbon"),
      spinupDebug = FALSE ## TODO: temporary
    )
  ),
  packages = c(
    #"PredictiveEcology/CBMutils@development (>= 0.0.7.9009)", ## this should be pulled in via modules already
    "googledrive", "pkgload",
    #"PredictiveEcology/Require@development (>= 0.2.6)", ## not needed b/c it's a dependencies of SpaDES.project and installed already
    "rgdal", "sf" ## TODO: why is Require trying to install binary versions?? not using RSPM!
    #needPkgs$SpaDES.core ## not needed b/c it's a dependencies of SpaDES.project and installed already
  ),
  require = c(
    needPkgs$reproducible,
    needPkgs$SpaDES.core
  ),
  modules = c(
    "PredictiveEcology/CBM_defaults@main",
    "PredictiveEcology/CBM_dataPrep_SK@development",
    "PredictiveEcology/CBM_vol2biomass@CBM_vol2biomass_SK",
    "PredictiveEcology/CBM_core@main"
  ),
  times = list(start = 1990.00, end = 1993.00),
  setLinuxBinaryRepo = FALSE, ## TODO: interferes with other package installation
  updateRprofile  = FALSE ## TODO: verify what it is doing
)

## default: use the copy in the modules' `data/` directories, unless user overrides destinationPath above
if (is.null(getOption("reproducible.destinationPath"))) {
  out$objects <- list(
    dbPath = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults", "cbm_defaults.db"),
    sqlDir = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults")
  )
}

# out$debug = 1 is the default which is like TRUE but not quite
out$loadOrder <- unlist(out$modules)

## simulation setup --------------------------------------------------------------------------------

if (.user == "cboisven") {
  # pkgload::load_all("C:/Celine/github/CBMutils")
} else if (.user == "achubaty" && isTRUE(.debug)) {
  pkgload::load_all("~/GitHub/PredictiveEcology/CBMutils")
}

spadesCBMrunsSK <- do.call(simInitAndSpades, out)
