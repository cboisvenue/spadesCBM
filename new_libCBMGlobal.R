## start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

# project basic setup -------------------------------------------------------------------------

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT
if (file.exists("spadesCBM.Renviron")) readRenviron("spadesCBM.Renviron") ## database credentials

.ncores <- min(parallel::detectCores() / 2, 32L) ## default number of CPU cores to use, e.g. for pkg install
.nodename <- Sys.info()[["nodename"]] ## current computer name; used to configure machine-specific settings
.user <- Sys.info()[["user"]] ## current computer username; used to configure user-specific settings

## define project directory - this code expects it is being run from this location
## **do not change the paths defined here**
## if you need to add a machine- or user-specific path, please do so _conditionally_
prjDir <- switch(.user,
                 cboisven = "C:/Celine/github/spadesCBM",
                 "~/GitHub/spadesCBM")

## ensure script being run from the project directory
stopifnot(identical(normalizePath(prjDir), normalizePath(getwd())))

options(
  Ncpus = .ncores,
  repos = c(PE = "https://predictiveecology.r-universe.dev/", ## latest PredictievEcology packages
            SF = "https://r-spatial.r-universe.dev/",         ## latest sf and other spatial packages
            CRAN = "https://cloud.r-project.org"),
  Require.RPackageCache = "default" ## will use default package cache directory: `RequirePkgCacheDir()`
)

## work-around for working from PFC...R cannot connect to certain urls
## TODO: improve conditional by only using wininet if *at* PFC, not just on a PFC machine
if ((.Platform$OS.type == "windows") && grepl("[L|W]-VIC", .nodename)) {
  options("download.file.method" = "wininet")
}

# install and load packages -------------------------------------------------------------------

## use project-specific location for packages to avoid conflicts with other projects
pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                    version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

## package installation only; do not load module packages until after install
if (!"remotes" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  install.packages("remotes")
}

if (!"Require" %in% rownames(installed.packages(lib.loc = .libPaths()[1])) ||
    packageVersion("Require", lib.loc = .libPaths()[1]) < "0.2.6") {
  remotes::install_github("PredictiveEcology/Require@development")
}

library(Require)

# setLinuxBinaryRepo() ## setup binary package installation for linux users; currently interferes with remotes installation

## installs development branch of SpaDES.core and SpaDES.project from https://predictiveecology.r-universe.dev
pkgsToInstall <- c("googledrive", "SpaDES.core", "SpaDES.project")
#Install(pkgsToInstall), upgrade = FALSE, standAlone = TRUE) ## TODO: fails with spatial pkgs + quickPlot
if (!all(pkgsToInstall %in% rownames(installed.packages(lib.loc = .libPaths()[1])))) {
  install.packages(pkgsToInstall)
}

if (.user == "cboisven") {
  ## TODO CBMutils does not seem to load - I am connected to the development branch of CBMutils
  devtools::load_all("C:/Celine/github/CBMutils")
} else {
  ## TODO: Require fails to install
  # Install("PredictiveEcology/CBMutils@development", dependencies = TRUE, standAlone = TRUE, upgrade = FALSE)
  remotes::install_github("PredictiveEcology/CBMutils@development", repos = "https://cloud.r-project.org", upgrade = FALSE)
}

## project setup using SpaDES.project --------------------------------------------------------------

## WORKAROUND: setupProject overwriting .gitignore (SpaDES.project#23)
# file.copy(".gitignore", ".gitignore.orig")
if (file.exists(".gitignore")) {
  unlink(".gitignore")
}
out <- SpaDES.project::setupProject(
  paths = list(projectPath = prjDir,
               packagePath = pkgDir,
               modulePath = "modules",
               inputPath = "inputs",
               outputPath = "outputs"),
  packages = c(pkgsToInstall, "CBMutils"), ## TODO: Require fails to install packages
  require = c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
              "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
  modules = c("PredictiveEcology/CBM_defaults@main",
              "PredictiveEcology/CBM_dataPrep_SK@development",
              "PredictiveEcology/CBM_vol2biomass@CBM_vol2biomass_SK",
              "PredictiveEcology/CBM_core@main"),
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
      .plotInitialTime = 1990,
      poolsToPlot = c("totalCarbon"),
      spinupDebug = FALSE ## TODO: temporary
    )
  ),
  options = list(
    reproducible.destinationPath = "inputs", ## TODO: SpaDES.project#24
    reproducible.useTerra = TRUE
  ),
  times = list(start = 1990.00, end = 1993.00),
  setLinuxBinaryRepo = FALSE, ## TODO: interferes with other package installation
  updateRprofile  = FALSE ## TODO: verify what it is doing
)
if (!file.exists(".gitignore")) {
  file.copy(".gitignore.orig", ".gitignore")
}

if (.user == "cboiven") {
  out$objects <- list(
    dbPath = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults", "cbm_defaults.db"),
    sqlDir = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults")
  )
}

out$loadOrder <- unlist(out$modules)

## simulation setup --------------------------------------------------------------------------------

spadesCBMrunsSK <- do.call(simInitAndSpades, out)
