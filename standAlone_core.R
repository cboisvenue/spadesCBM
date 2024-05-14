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

# start in 1998 because there are known disturbances in the study area

# NOTE: This is using a temporary directory for package installation, so if
#  the R session is restarted, then the packages will be reinstalled (40 seconds after each restart)
# install.packages("remotes")
library(remotes)
remotes::install_github("PredictiveEcology/SpaDES.project@transition")

times <- list(start = 1998, end = 2000)

out <- SpaDES.project::setupProject(
  name = "spadesCBM",
  paths = list(modulePath = "modules",
               inputPath = "inputsForScott"),
               #packagePath = file.path(tempdir(), "testsSpadesCBM7")),
  options = list(
    repos = c(PE = "https://predictiveecology.r-universe.dev/", ## latest PredictievEcology packages
              SF = "https://r-spatial.r-universe.dev/",         ## latest sf and other spatial packages
              CRAN = "https://cloud.r-project.org"),
    reproducible.destinationPath = "inputs", ## TODO: SpaDES.project#24
    ## These are for speed
    reproducible.useMemoise = TRUE,
    # Require.offlineMode = TRUE,
    spades.moduleCodeChecks = FALSE
  ),
  modules =  "PredictiveEcology/CBM_core@main",
  times = times,
  require = "PredictiveEcology/SpaDES.core@development",
  processes = readRDS(file.path(paths$inputPath, "processes.rds")),

  # these two files are specific to the study area used here
  gcHash = readRDS(file.path(paths$inputPath, "gcHash.rds")),
  spatialDT = readRDS(file.path(paths$inputPath, "spatialDT.rds")),

  # provide values for CBM_core --> these are all in `expectsInput` metadata
  pooldef = c("Input", "SoftwoodMerch", "SoftwoodFoliage", "SoftwoodOther",
               "SoftwoodCoarseRoots", "SoftwoodFineRoots", "HardwoodMerch",
               "HardwoodFoliage", "HardwoodOther", "HardwoodCoarseRoots",
               "HardwoodFineRoots", "AboveGroundVeryFastSoil",
               "BelowGroundVeryFastSoil", "AboveGroundFastSoil",
               "BelowGroundFastSoil", "MediumSoil", "AboveGroundSlowSoil",
               "BelowGroundSlowSoil", "SoftwoodStemSnag",
               "SoftwoodBranchSnag", "HardwoodStemSnag", "HardwoodBranchSnag",
               "CO2", "CH4", "CO", "Products"),

  PoolCount = length(pooldef),
  ages = c(100, 100, 100, 100, 101, 101, 101, 102, 102, 109, 109, 11,
            110, 12, 12, 128, 129, 13, 13, 130, 14, 79, 81, 81, 82, 88, 89,
            89, 9, 90, 90, 91, 91, 92, 92, 93, 93, 94, 99, 99, 99),
  nStands = length(ages),
  pools = {
    pls <- matrix(ncol = PoolCount, nrow = nStands, data = 0)
    colnames(pls) <- pooldef
    pls[, "Input"] <- 1
    pls
  },

  realAges = ages,

  gcids = structure(c(1L, 2L, 3L, 5L, 1L, 2L, 5L, 1L, 2L, 1L, 2L, 3L, 2L,
                       1L, 3L, 2L, 2L, 1L, 3L, 2L, 1L, 4L, 1L, 2L, 2L, 1L, 1L, 2L, 3L,
                       1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 5L),
                     levels = c("49", "50", "52", "58", "61"), class = "factor"),

  ecozones = rep(9, nStands),
  spatialUnits = rep(28, nStands),
  historicDMIDs = rep(371, nStands),
  lastPassDMIDS = historicDMIDs,
  delays = rep(0, nStands),
  minRotations = rep(10, nStands),
  maxRotations = rep(30, nStands),
  returnIntervals = list(return_interval = rep(75, nStands)),

  dPath = paths$inputPath,
  disturbanceRasters = {
    rasts <- terra::rast(file.path(dPath, paste0("SaskDist_", times$start:times$end, ".grd")))
    names(rasts) <- times$start:times$end
    rasts
    },

  userDist = data.table(distName = c("wildfire", "clearcut", "deforestation", "20% mortality", "20% mortality"),
                         rasterID = c(1L, 2L, 4L, 3L, 5L),
                         wholeStand = c(1L, 1L, 1L, 0L, 0L)),
  sqlDir = file.path(paths$modulePath, "CBM_defaults", "data", "cbm_defaults"),
  dbPath = file.path(sqlDir, "cbm_defaults.db"),

  level3DT = {
    df <- data.table(ages, spatialUnits, gcids, gcids,
                         ecozones, pixelGroup = seq(nStands), gcids)
    colnames(df) <- c("ages", "spatial_unit_id", "growth_curve_component_id",
                          "growth_curve_id", "ecozones", "pixelGroup", "gcids")
    df
  },

  curveID = "growth_curve_component_id",

  dmPerSpu = data.table(
    rasterID = c(1, 2, 4, 3, 5),
    spatial_unit_id = c(28),
    disturbance_matrix_id = c(371, 409, 26, 91, 91)),
  mySpuDmids = userDist[dmPerSpu, on = "rasterID"],

  masterRaster = {
    extent = reproducible::.unwrap(structure(list(xmin = -687696, xmax = -681036,
                                                   ymin = 711955, ymax = 716183), class = "PackedSpatExtent"))
    masterRaster <- terra::rast(extent, res = 30)
    terra::crs(masterRaster) <- "PROJCRS[\"Lambert_Conformal_Conic_2SP\",\n    BASEGEOGCRS[\"GCS_GRS_1980_IUGG_1980\",\n        DATUM[\"D_unknown\",\n            ELLIPSOID[\"GRS80\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-95,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",77,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
    masterRaster[] <- rep(1, terra::ncell(masterRaster))
    mr <- reproducible::prepInputs(targetFile = file.path(paths$inputPath, "ldSp_TestArea.tif"),
                                   destinationPath = ".",
                     to = masterRaster,
                     method = "near")
    mr[mr[] == 0] <- NA
    mr
  },
  outputs = as.data.frame(expand.grid(objectName = c("cbmPools", "NPP"),
                                      saveTime = sort(c(times$start,
                                                        times$start +
                                                          c(1:(times$end - times$start))
                                      ))))

  )

## if you don't have CBMutils, you can get it here "PredictiveEcology/CBMutils"
library(CBMutils)
out$cbmData = readRDS(file.path(out$paths$inputPath, "cbmData.rds"))

library(data.table)
# Run
simCoreAlone <- do.call(simInitAndSpades, out)

# Read all years from disk
savedOutputs <- outputs(simCoreAlone)[, "file"]

NPPfiles <- grep("NPP", value = TRUE, savedOutputs)
NPP <- rbindlist(lapply(NPPfiles, readRDS))

cbmPoolsFiles <- grep("cbmPools", value = TRUE, savedOutputs)
allPools <- rbindlist(lapply(cbmPoolsFiles, readRDS))

