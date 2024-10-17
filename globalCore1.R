## PFC work-around
## this is a work-around for working from PFC...somtimes R cannot connect to URL
##This current set up sets options to wininet on any NRCan computer at PFC Not
##sure if this is needed anymore. See notes
##options(download.file.method="curl", download.file.extra="--ssl-revoke-best-effort")
##OLD WAY - will enventually delete
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


######## Trials at setting up the Python environment in reticulate without
######## confusing where it looks for libcbm package
#install.packages("reticulate")
library(reticulate)
reticulate::use_virtualenv(virtualenv = "r-reticulate")
reticulate::py_install("libcbm", envname = "r-reticulate")
#reticulate::import("sys")$executable
## the start of this path has to match where the libcbm is loaded


repos <- unique(c("predictiveecology.r-universe.dev", getOption("repos")))
install.packages(c("Require", "SpaDES.project"), repos = repos)


# start in 1998 because there are known disturbances in the study area
times <- list(start = 1998, end = 2000)

out <- SpaDES.project::setupProject(
  name = "spadesCBM",
  ##TODO need to figure out how to connect a new modules and new repo. Test is
  ##we can get rid of inputsForScott.
  paths = list(modulePath = "modules",
               inputScott = "inputsForScott",
               inputPath = "inputs"), #this will be replaced with updates CBM_dataPrep_SK and CBM_defaults

  options = list(
    repos = c(repos = repos),
      # PE = "https://predictiveecology.r-universe.dev/", ## latest PredictievEcology packages
      #         SF = "https://r-spatial.r-universe.dev/",         ## latest sf and other spatial packages
      #         CRAN = "https://cloud.r-project.org"),
    reproducible.destinationPath = "inputs", ## TODO: SpaDES.project#24
    ## These are for speed
    reproducible.useMemoise = TRUE,
    # Require.offlineMode = TRUE,
    spades.moduleCodeChecks = FALSE
  ),
  modules =  c("PredictiveEcology/CBM_defaults@python",
               "PredictiveEcology/CBM_dataPrep_SK@python",
               "PredictiveEcology/CBM_vol2biomass@libcbm",
               "PredictiveEcology/CBM_core@python"),##TODO not linked yet!
  times = times,
  require = c("SpaDES.core",
              "PredictiveEcology/libcbmr", "data.table"),
  # params = list(
  #   CBM_core = list(
  #     .useCache = c(".inputObjects", "init")
  #     )),

  #### begin manually passed inputs ##########################################

  spatialDT = {
    dt <- readRDS(file.path(paths$inputScott, "spatialDT.rds"))
    ##Transition: getting rid of the double gcids columns and naming one column
    ##gcids
    data.table::setnames(dt,"growth_curve_component_id", "gcids")
    dt[, growth_curve_id := NULL]
    dt
  },


  delays = rep(0, length(unique(spatialDT$pixelGroup))),

  userDist = data.table(distName = c("wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality"),
                        rasterID = c(1L, 2L, 4L, 3L, 5L),
                        wholeStand = c(1L, 1L, 1L, 0L, 0L)),

##Need to keep this master raster here. It defines the smaller study area. We
##will not need it when we run all of the managed forests of SK as the study
##area will be defined by a masterRaster that we get via a URL.
  masterRaster = {
    extent = reproducible::.unwrap(structure(list(xmin = -687696, xmax = -681036,
                                                  ymin = 711955, ymax = 716183), class = "PackedSpatExtent"))
    masterRaster <- terra::rast(extent, res = 30)
    terra::crs(masterRaster) <- "PROJCRS[\"Lambert_Conformal_Conic_2SP\",\n    BASEGEOGCRS[\"GCS_GRS_1980_IUGG_1980\",\n        DATUM[\"D_unknown\",\n            ELLIPSOID[\"GRS80\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-95,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",77,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
    masterRaster[] <- rep(1, terra::ncell(masterRaster))
    mr <- reproducible::prepInputs(targetFile = file.path(paths$inputScott, "ldSp_TestArea.tif"),
                                   destinationPath = ".",
                                   to = masterRaster,
                                   method = "near")
    mr[mr[] == 0] <- NA
    mr
  },
##Need to keep the disturbance rasters here. We will not need it when we run all
##of the managed forests of SK as the disturbance rasters will be defined by
##rasters that we get via a URL.
  disturbanceRasters = {
    rasts <- terra::rast(file.path(paths$inputScott, paste0("SaskDist_", times$start:times$end, ".grd")))
    names(rasts) <- times$start:times$end
    rasts <- reproducible::postProcessTo(rasts, cropTo = masterRaster, projectTo = masterRaster,
                              maskTo = masterRaster, method = "near")
  },

  Restart = getOption("SpaDES.project.Restart", FALSE),

  outputs = as.data.frame(expand.grid(objectName = c("cbmPools", "NPP"),
                                      saveTime = sort(c(times$start,
                                                        times$start +
                                                          c(1:(times$end - times$start))
                                      )))),
  updateRprofile = TRUE# ,

)

out$loadOrder <- unlist(out$modules)

# Run
simPython <- do.call(SpaDES.core::simInitAndSpades, out)

## easy way to check results:
# # Read all years from disk
# savedOutputs <- outputs(simCoreAlone)[, "file"]
#
# NPPfiles <- grep("NPP", value = TRUE, savedOutputs)
# NPP <- rbindlist(lapply(NPPfiles, readRDS))
#
# cbmPoolsFiles <- grep("cbmPools", value = TRUE, savedOutputs)
# allPools <- rbindlist(lapply(cbmPoolsFiles, readRDS))

