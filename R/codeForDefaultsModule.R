### these are  bits of code that will help us figure out CBM_defaults
### CBoisvenue May 14, 2024

## we can read libcbm_py's default values from this URL
### https://github.com/cat-cfs/libcbm_py/tree/master/libcbm/resources/cbm_defaults_db
### I copied it here b/c I can't get the URL to work at this moment
###

dbPath <- "C:/Celine/github/spadesCBM/defaultDB/cbm_defaults_v1.2.8340.362.db"

library(RSQLite)
devtools::load_all("../CBMutils")
library(CBMutils)

archiveIndex <- dbConnect(dbDriver("SQLite"), dbPath)
dbListTables(archiveIndex)
# [1] "admin_boundary"                       "admin_boundary_tr"
# [3] "afforestation_initial_pool"           "afforestation_pre_type"
# [5] "afforestation_pre_type_tr"            "biomass_to_carbon_rate"
# [7] "composite_flux_indicator"             "composite_flux_indicator_category"
# [9] "composite_flux_indicator_category_tr" "composite_flux_indicator_tr"
# [11] "composite_flux_indicator_value"       "decay_parameter"
# [13] "disturbance_matrix"                   "disturbance_matrix_association"
# [15] "disturbance_matrix_tr"                "disturbance_matrix_value"
# [17] "disturbance_type"                     "disturbance_type_tr"
# [19] "dom_pool"                             "eco_boundary"
# [21] "eco_boundary_tr"                      "flux_indicator"
# [23] "flux_indicator_sink"                  "flux_indicator_source"
# [25] "flux_process"                         "forest_type"
# [27] "forest_type_tr"                       "genus"
# [29] "genus_tr"                             "growth_multiplier_series"
# [31] "growth_multiplier_value"              "land_class"
# [33] "land_class_tr"                        "land_type"
# [35] "locale"                               "pool"
# [37] "pool_tr"                              "random_return_interval"
# [39] "root_parameter"                       "slow_mixing_rate"
# [41] "spatial_unit"                         "species"
# [43] "species_tr"                           "spinup_parameter"
# [45] "stump_parameter"                      "turnover_parameter"
# [47] "vol_to_bio_factor"                    "vol_to_bio_forest_type"
# [49] "vol_to_bio_genus"                     "vol_to_bio_species"

## all the information for the disturbance matrices can be extracted like this:
matrices1 <- dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix")
matrices2 <- dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix_association")
matrices3 <- dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix_tr")
matrices4 <- dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix_value")
matrices5 <- dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type")
matrices6 <- dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type_tr")

### matrices2, matrices3, and matrices4 are needed for us.
### libcbmr::cbm_exn_get_default_parameters() does not currently give us the
### names and that is important because the user may provide the disturbance
### names or they may come from another product and need to be matched with the
### CBM DMDIDs

### will need the species names - they are in spinup_input$parameters$species
### but this is a panda, need to figure out how to search those in R. We need to
### be able to do a species match
species <- dbGetQuery(archiveIndex, "SELECT * FROM species")
species_tr <- dbGetQuery(archiveIndex, "SELECT * FROM species_tr")

### what is this? probably for Juha's uncertainty analyses...
random_return_interval <- dbGetQuery(archiveIndex, "SELECT * FROM random_return_interval")

### looking for the fire return intervals
spinupSQL <- dbGetQuery(archiveIndex, "SELECT * FROM spinup_parameter")
