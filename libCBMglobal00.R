# start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

## PFC work-around
## this is a work-around for working from PFC...R cannot connect to URL
##This current set up sets options to wininet on any NRCan computer at PFC Not
##sure if this is needed anymore. See notes
##G:\RES_Work\Work\LandRCBM\libCBMtransition\SSLissuePFC.docx
# if (.Platform$OS.type == "windows") {
#   ## based on <https://stackoverflow.com/a/14357701/1380598>
#   ip <- system("ipconfig", intern = TRUE)
#   ip <- ip[grep("IPv4", ip)]
#   ip <- gsub(".*? ([[:digit:]])", "\\1", ip)
#
#   if (any(grepl("^132[.]156[.]", ip))) {
#     #options("download.file.method" = "wininet")
#     Sys.setenv(REQUESTS_CA_BUNDLE = normalizePath("~/NRCAN-RootCA.crt"))
#   }
# }
#options("download.file.method" = "wininet")
#options(repos = "https://cloud.r-project.org")

##TODO work being done on SpaDES.project, this will need to be changed

## This is not working
# install.packages("Require")#, lib = 'C:/Users/cboisven/AppData/Local/R/win-library/4.3')
#
# Require::Require("PredictiveEcology/SpaDES.project@transition")

# ## this is me trying
# library(remotes)
# remotes::install_github("PredictiveEcology/SpaDES.project@transition")
# #remotes::install_github("PredictiveEcology/reproducible@development")
#
# ## below is what I used to use...
# # if (all(tryCatch(packageVersion("SpaDES.project") < "0.0.7.9023", error = function(e) TRUE),
# #         tryCatch(packageVersion("Require") < "0.3.0", error = function(e) TRUE))) {
# #   install.packages(c("Require", "SpaDES.project"),
# #                    repos = c("https://predictiveecology.r-universe.dev",
# #                              getOption("repos")))
# # }

getOrUpdatePkg <- function(p, minVer, repo) {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    if (missing(repo)) repo = c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

# getOrUpdatePkg("Require", "0.3.1.14")

getOrUpdatePkg("SpaDES.project", "0.0.8.9026")

library(SpaDES.project)

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
      "PredictiveEcology/reproducible@development (>= 2.0.8.9001)", ##fixed a problem with caching dataPrep
      "PredictiveEcology/SpaDES.core@development",
      "PredictiveEcology/CBMutils@development"),
  modules = c("PredictiveEcology/CBM_defaults",
              "PredictiveEcology/CBM_dataPrep_SK",
              "PredictiveEcology/CBM_vol2biomass",
              "PredictiveEcology/CBM_core"
  ),
  times = list(start = 1990.00, end = 1993.00),
  setLinuxBinaryRepo = FALSE, ## TODO: interferes with other package installation
  updateRprofile  = FALSE ## TODO: verify what it is doing
)

##TODO: At the end of each annual event, sim$cbmPools is replaced by updated
## pools where all the carbon transactions are done for that year and pools
## updated.However, because annual inter variability is useful for carbon
## dynamics, the default behaviour in this example is for to write yearly
## results to sim$$outputPath via the out$outputs below. This may become really
## big for large of fine-pixelled areas. Need to create a user-controlled
## parameter for "# of years saved". Note that the second to last sim$cbmPools
## needs to be saved if we want to be able to use the
## CBMutils::checkTransactions() function. Maybe make that saving a default?


out$objects <- list(
  dbPath = file.path(out$paths$modulePath, "CBM_defaults", "data","cbm_defaults",
                     "cbm_defaults.db"),
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
#dev()

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


