# start of work to go from C++ to libcbm4
# CBoisvenue
# newGlobal

while (!require("SpaDES.project")) {
  install.packages("SpaDES.project", repos = "https://predictiveecology.r-universe.dev")
  require(SpaDES.project)
}

##TODO current set-up creates input/inputs and input/outputs folder...need to
##figure out which we keep and make sure the "extras" are not created. The
##Spades.project call creates the ones without the "s" at the end but thorughout
##the code the ones with the "s" are used...but this might be only in the
##specific module folders. Modules .Rmd use "outputs" but no code seems to use it.
## Inputs with an "s" seems to be only used here in CBM_core (lines 305-317):
# spinup <- function(sim) {
#   io <- inputObjects(sim, currentModule(sim))
#   objectNamesExpected <- io$objectName
#   available <- objectNamesExpected %in% ls(sim)
#   if (any(!available)) {
#     stop(
#       "The inputObjects for CBM_core are not all available:",
#       "These are missing:", paste(objectNamesExpected[!available], collapse = ", "),
#       ". \n\nHave you run ",
#       paste0("spadesCBM", c("defaults", "inputs", "m3ToBiomass", "userDist"), collapse = ", "),
#       "?"
#     )
#   }
## Need to see if we can change that to "input".

out <- SpaDES.project::setupProject(paths = list(projectPath = "C:/Celine/github/spadesCBM",
                                          modulePath = "modules"),
                             packages = c("googledrive", "devtools"),
                             require =
                               c("PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
                                 "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"),
                             modules = c("PredictiveEcology/CBM_defaults@main",
                                         "PredictiveEcology/CBM_dataPrep_SK@development",
                                         "PredictiveEcology/CBM_vol2biomass@CBM_vol2biomass_SK",
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
  dbPath = file.path(out$paths$modulePath, "CBM_defaults", "data","cbm_defaults", "cbm_defaults.db"),
  sqlDir = file.path(out$paths$modulePath, "CBM_defaults", "data", "cbm_defaults")
)

# quickPlot::dev.useRSGD(FALSE)
# dev()
# clearPlot()
##TODO SpaDES.core is in the "require = " part of the setupProject call above.
##Why is it not loading?
library(SpaDES.core)
##TODO CBMutils does not seem to load
# ::gcidsCreate
# Error: 'gcidsCreate' is not an exported object from 'namespace:CBMutils'
# work around until Alex can fix it, putting this in global (note in
# CBM_DataPrep_SK.R init event)
# cumPoolsCreate wasn't loading either
# just change all the reqdPkgs = list("PredictiveEcology/CBMutils@development")
##TODO change this once the package is stable.
# gcidsCreate <- function(...) {
#   do.call(paste, c(list(...)[[1]], sep= "_"))
# }
##TODO added "devtools" to the setupProject packages =
library("devtools")
devtools::load_all("C:/Celine/github/CBMutils")

spadesCBMrunsSK <- simInitAndSpades(times = times,
                                    params = parameters,
                                    modules = out$modules,
                                    objects = objects,
                                    #paths = paths, ## not needed when using
                                    #setPaths() above
                                    loadOrder = unlist(out$modules),
                                    debug = TRUE
)


## scrap

install.packages("reproducible", repos = "https://predictiveecology.r-universe.dev")

library(sf)
library(reproducible)
library(terra)

options("reproducible.useTerra" = TRUE)

dataPath <- tempdir()
masterRaster <- Cache(
  prepInputs,
  url = "https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW",
  fun = "terra::rast",
  destinationPath = dataPath
)
masterRaster[masterRaster == 0] <- NA

canadaSpu <- prepInputs(
  targetFile = "spUnit_Locator.shp",
  url = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed",
  destinationPath = dataPath,
  alsoExtract = "similar"
)

spuShp <- postProcess(
  canadaSpu,
  rasterToMatch = masterRaster,
  #targetCRS = terra::crs(masterRaster),
  useCache = FALSE, filename2 = NULL
) %>%
  st_collection_extract("POLYGON")

spuRaster <- terra::rasterize(
  terra::vect(spuShp),
  terra::rast(masterRaster),
  field = "spu_id") |> raster::raster() #### <----- if you want it to be a `Raster` object, add this pipe at end
