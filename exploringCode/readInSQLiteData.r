#------------------------------------------------------
# Need to know all that the C:\Celine\GitHub\spadesCBM\data\cbm_defaults\cbm_defaults.db
# contains
# April 5, 2019
# CBoisvenue
#------------------------------------------------------

library(RSQLite)


inputDir <- file.path(getwd(),"data/12_Spades_run/") 
dbPath = file.path(inputDir,"cbm_defaults","cbm_defaults.db")

sqlite.driver <- dbDriver("SQLite")

cbmDefaults <- dbConnect(sqlite.driver,
                dbname = dbPath)
dbDriver()
alltables = dbListTables(cbmDefaults)
cbmTables <- list()


for(i in 1:length(alltables)){
  cbmTables[[i]] <- dbReadTable(cbmDefaults,alltables[i])
}

names(cbmTables) <- alltables

# make table to check/look-up annoying spatial_unit_id admin and eco boundaries

adminNames <- cbmTables$admin_boundary
names(adminNames) <- c("admin_boundary_id","stump_parameter_id","name")
ecoNames <- cbmTables$eco_boundary
names(ecoNames) <- c("eco_boundary_id","stump_parameter_id","name")

spuNames <- merge(adminNames,cbmTables$spatial_unit,by="admin_boundary_id")
names(spuNames) <- c("admin_boundary_id","stump_parameter_id", "province","id",
                     "eco_boundary_id","root_parameter_id", "climate_time_series_id","spinup_parameter_id")
spuNames1 <- merge(ecoNames,spuNames,by="eco_boundary_id")
names(spuNames1) <- c("eco_boundary_id","stump_parameter_id.x","ecozone","admin_boundary_id","stump_parameter_id.y", 
                      "province","spu_id","root_parameter_id", "climate_time_series_id","spinup_parameter_id")
spu <- spuNames1[,c(7,4,6,1,3,5,8,9,10)]
spu <- spu[order(spu$spu_id),]

# make table to be able to check species_id, forest_id, and genus_id

sps <- cbmTables$species
names(sps) <- c("species_id","species","forest_type_id","genus_id")

forests <- cbmTables$forest_type
names(forests) <- c("forest_type_id","forest")
sps <- merge(sps,forests,by="forest_type_id")

genus <- cbmTables$genus
names(genus) <- c("genus_id","genus")
sps <- merge(sps,genus,by="genus_id")
sps <- sps[,c(3,4,2,5,1,6)]

