### these are  bits of code that will help us figure out CBM_defaults
### CBoisvenue May 14, 2024

## we can read libcbm_py's default values from this URL
### https://github.com/cat-cfs/libcbm_py/tree/master/libcbm/resources/cbm_defaults_db
### I copied it here b/c I can't get the URL to work at this moment
###

dbPath <- "C:/Celine/github/spadesCBM/defaultDB/cbm_defaults_v1.2.8340.362.db"

### default sql queries used in the NIR are here:
###https://github.com/cat-cfs/libcbm_py/blob/main/libcbm/resources/cbm_defaults_queries/disturbance_type_ref.sql

###Conversation with Scott Morken about the stability of the SQLight------------
# [Yesterday 14:32] Boisvenue, Céline Hi Scott, quick question: do you have the
# intention of changing the use of the SQLight for default parameters in libcbm?
# I have everything pointing here
# https://github.com/cat-cfs/libcbm_py/tree/master/libcbm/resources/cbm_defaults_db
# I just want to make sure I am not building something on an approach that will
# be replaced. libcbm_py/libcbm/resources/cbm_defaults_db at main ·
# cat-cfs/libcbm_py Carbon budget model library based on CBM-CFS3. Contribute to
# cat-cfs/libcbm_py development by creating an account on GitHub.
#
# [Yesterday 14:38] Morken, Scott I would say that database is fully stable
# other than maintenance like parameter changes or bugfixes for anything
# CBM-CFS3 related that happens in CBM4, and I think we will need to support
# CBM3 modelling one way or another for a while yet due to things moving slower
# on the reporting side.  It's stable for that use case for sure.   There is a
# version stamp (the 1.2.8340.362 part on the filename)  on the database file
# itself that matches the Operational Scale CBM CFS3 version that it originated
# from.  So as changes slowly happen that database may evolve, but you can refer
# back to the older versions too.  If I update it, I will add new versions along
# side that one
#
#
#
# [Yesterday 14:39] Morken, Scott
# The database schema should be even more stable, and I hope we dont need to
# make changes to that at all.
#
# [Yesterday 14:42] Morken, Scott now that I am thinking about this though, we
# also have separate NFCMARS versions of these databases, since that evolves
# independantly... the Operational Scale versions of this parameter database is
# downstream from this, meaning we usually absorb changes that come from NFCMARS
# into the public release (things like new disturbance types/disturbance
# matrices)  I really tried hard to make this a single versioned parameter
# database so we could collectively share and work with a single version, but
# there was too much inertia
#
# [08:43] Boisvenue, Céline Thanks Scott. I think I understand the situation.
# For me it is what libcbm uses that I am interested. I am less interested in
# the CBM3 maintenance since that is the more operational side and I am trying
# to move the research side forward. So, I guess my question really is: if I
# point to that file (link above), will I always be using the updated version
# (if updates are made) that you (or anybody using libcbm) are using in libcbm
# calls?
#
# [09:40] Morken, Scott that's not how I currently have it structured.  I was
# thinking of adding new files in that dir as updates are made.  So for example
# if I update to the a more recent toolbox version there would be 2 files in
# there: cbm_defaults_v1.2.8340.362.db and cbm_defaults_v1.2.8923.387.db.
#
# The way to always get the latest path though is like this through a function
# call.  This will return the path to a local copy of the database that's stored
# within the installed libcbm package
#
#
# libcbm_resources <- reticulate::import("libcbm.resources")
# cbm_defaults_path <- libcbm_resources$get_cbm_defaults_path()

#
# [09:46] Boisvenue, Céline
# Ok - this works for me. It is part of the continuous workflow that we are
# trying to implement.

#
###This means that we have to load Python modules for our CBM_default module. BUT NO

# [14:35] Boisvenue, Céline
# Hi Scott: wouldn't just pointing to the URL automatically give me the latest
# version? I am trying to avoid spreading too many Python calls in the modules,
# they are slow to load, and users may just want to see the parameters and not
# run the whole model (this was the purpose of one of our modules CBM_defaults -
# it would be nice if they wouldn't need Python for this). It is super fast in R
# just to load the SQLight, and I cache it. I would like (and this may not be
# possible) to limit the calls to Python functions to one module (CBM_core).
# Here is what I was thinking: in my CBM_vol2biomass, I directly pick-up the
# parameters from the NFIS sites so I always have the updated ones. If they
# don't change, I have to do that once and after I use a cached version. This is
# what I would like to do in my CBM_default, just get the database at the URL
# and cache. Check for changes everything you rerun, if no changes use the
# cached version. Let me know if this is not clear or if you have questions.
#
# [14:41] Morken, Scott
# yes this makes sense, I think maybe even the best thing would be to make a
# permanent github repo that is the "authoritative" copy of this database and
# have both your work and libcbm point at that.  For now it should be safe to
# just point at the current URL (I have no imminent plans to change this, and if
# I did want to change it I could let you know?)   For the hypothetical case of
# a change, like if I had to make a change in a few months, If you had to change
# the URL in your script at some point in the future would it really break
# things?  If  I do make a change like that I'll make a more permanent
# solution..
#
# [14:44] Boisvenue, Céline
# Hi Scott, this works for me. No, it would not break anything, I would just
# need to update the URL. Thank you for this! Have a good weekend.
#
# [14:44] Morken, Scott
# OK I'll for sure keep this in mind for the next time any change comes up with
# that database
#  like 1
#

library(RSQLite)
#devtools::load_all("../CBMutils")
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

## need this:##NEW SECRET KNOWLEDGE: disturbance_type_ref_en_CA.csv. Scott will eventually
##put this table somewhere where we can access it via URL.

### matrices2, matrices3, and matrices4 are needed for us.
### libcbmr::cbm_exn_get_default_parameters() does not currently give us the
### names and that is important because the user may provide the disturbance
### names or they may come from another product and need to be matched with the
### CBM DMDIDs

disturbance_type_ref <- as.data.table(dbGetQuery(archiveIndex,
                                   "SELECT disturbance_type.id as disturbance_type_id,
disturbance_type_tr.name as disturbance_type_name,
disturbance_type_tr.description as disturbance_type_description
from disturbance_type
inner join disturbance_type_tr on disturbance_type_tr.disturbance_type_id == disturbance_type.id
inner join locale on disturbance_type_tr.locale_id = locale.id"))

###CELINE NOTES: until we have the workflow (and objects) cleaned up for
###spadesCBM-Python, I create this here and saved into inputsForScott
# I am needing the DMIDs to make sure all is working, but in the Python based
# sims, the match to the DMIDs will be internal and based on disturbance_type_id.


## this is the sql script to get the
# select
# disturbance_type.id as disturbance_type_id,
# disturbance_type_tr.name as disturbance_type_name,
# disturbance_type_tr.description as disturbance_type_description
# from disturbance_type
# inner join disturbance_type_tr on disturbance_type_tr.disturbance_type_id == disturbance_type.id
# inner join locale on disturbance_type_tr.locale_id = locale.id
# where locale.code = ?


### will need the species names - they are in spinup_input$parameters$species
### but this is a panda, need to figure out how to search those in R. We need to
### be able to do a species match
species <- dbGetQuery(archiveIndex, "SELECT * FROM species")
species_tr <- dbGetQuery(archiveIndex, "SELECT * FROM species_tr")

### what is this? probably for Juha's uncertainty analyses...
random_return_interval <- dbGetQuery(archiveIndex, "SELECT * FROM random_return_interval")

### looking for the fire return intervals
spinupSQL <- dbGetQuery(archiveIndex, "SELECT * FROM spinup_parameter")

### looking at forest types to see if that can be used for sw_hw
forestType1 <- dbGetQuery(archiveIndex, "SELECT * FROM forest_type")
forestType2 <- dbGetQuery(archiveIndex, "SELECT * FROM forest_type_tr")
