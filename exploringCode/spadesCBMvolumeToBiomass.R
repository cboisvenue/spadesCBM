#---------------------------------------------
#Boudewyn st al parameters for conversion from m3/ha to biomass in
#the three main carbon pools that make-up the $growth_increments used to move
#spadesCBM forward in growth from year to year
# https://nfi.nfis.org/en/biomass_models

table3 <- read.csv("data/appendix2_table3.csv")#)file.path(paths(spadesCBMout)$inputPath,"
table4 <- read.csv("data/appendix2_table4.csv")
table5 <- read.csv("data/appendix2_table5.csv")
table6 <- read.csv("data/appendix2_table6_v2.csv")

### needs
library(data.table)

# make a function that identifies 
    
  # identify species, genus, and ecozone
  # read-in meta data
  gcMetaData <- fread("data/spadesGCurvesSK.csv")#sim$gcurveFileName
  gcMetaData <- unique(gcMetaData[,.(growth_curve_component_id,spatial_unit_id,forest_type_id,species,rasterSps)])
  # ecozones: gcMetaData contains all the ecozones in SK. Our study area does not.
  ecoToSpu <- as.data.table(spadesCBMout$cbmData@spatialUnitIds)#sim$cbmData@spatialUnitIds[,c(1,3)]
  ecozones <- ecoToSpu[which(ecoToSpu$SpatialUnitID %in% unique(spadesCBMout$level3DT$spatial_unit_id)),]
  gcMetaData <- gcMetaData[spatial_unit_id %in% ecozones$SpatialUnitID,]
  
  # our study region does not have all the species in the gcMetaData
  gcMetaData <- gcMetaData[rasterSps %in% spadesCBMout$level3DT$rasterSps,]
  
  # read-in m3/ha values
  gCvalues <- fread("data/yieldComponentSK.csv") # sim$gcurveComponentsFileName
  gCvalues <- gCvalues[GrowthCurveComponentID %in% unique(gcMetaData$growth_curve_component_id),]
#### THIS WILL GO ON LINE 119 of spadesCBMinputs.R ### now only 20 curves to translate ##  

# identify jurisdiction
# matching jurisdiction to CBM-legacy numbering with Boudewyn jurisdiction params
  # choices are: 
  # table3$juris_id and table4$juris_id and table6$jur
  # AB BC MB NB NF NS NT NU ON PE QC SK YK
  # table5$juris_id
  # AB BC NB NF NT
cbmAdmin <- c(10,11,8,5,1,2,3,13,14,7,4,6,9,12)
paramJur <- c("AB","BC","MB","NB","NF","NF","NS" ,"NT" ,"NU" ,"ON" ,"PE", "QC", "SK", "YK")
adminMatch <- as.data.table(cbind(cbmAdmin,paramJur))
jurisdiction <- as.character(adminMatch[which(cbmAdmin %in% unique(ecoToSpu[SpatialUnitID %in% unique(gcMetaData$spatial_unit_id),2])),2])

# read-in species match with canfi_species code and genus
spsMatch <- fread("data/spsMatchNameRasterGfileBiomParams.csv")
# Match gcID$species to spsMatch$speciesName, then sktable3-4 have
# $canfi_species, sktable5 $genus, sktable6 has $species which is equilvalent
# to $canfi_species

# make smaller tables for ease
### change names to more generic #####
sktable3 <- as.data.table(table3[table3$juris_id==jurisdiction,])
sktable4 <- as.data.table(table4[table4$juris_id==jurisdiction,])
# table5 is weird since they did not have enough data for SK. I am selecting AB
# instead. Another catch is that they don't have the same species match. I
# manually check and ABIES is genus 3 (used below)
#### PUT error messge if the specified jurisdiction is not found #### GIVE CHOICES
sktable5 <- as.data.table(table5[table5$juris_id=="AB",])
sktable6 <- as.data.table(table6[table6$jur==jurisdiction,])
  
  ## until here this is non species specific
  # could loop through each species...? ? 
  # and the ecozones?
  fullSpecies <- unique(gcMetaData$species)
  swInc <- NULL
  hwInc <- NULL
  #growth_increments <- NULL
  #colnames(growth_increments)<- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
  
  
  for(i in 1:length(fullSpecies)){
    speciesMeta <- gcMetaData[species==fullSpecies[i],]
    for(j in 1:length(unique(speciesMeta$growth_curve_component_id))){
          meta <- speciesMeta[j,]
          id <- gCvalues$GrowthCurveComponentID[which(gCvalues$GrowthCurveComponentID == meta$growth_curve_component_id)][-1]
          age <- gCvalues[GrowthCurveComponentID==meta$growth_curve_component_id,Age][-1]
          cumBiom <- as.matrix(convertM3biom(meta = meta,gCvalues = gCvalues))
          inc <- diff(cumBiom)
          if(meta$forest_type_id==1){
            incs  <- cbind(id,age,inc,rep(0,length(age)),rep(0,length(age)),rep(0,length(age)))
            swInc <- rbind(swInc,incs)
            #FYI:
            # cbmTables$forest_type
            # id           name
            # 1  1       Softwood
            # 2  2      Mixedwood
            # 3  3       Hardwood
            # 4  9 Not Applicable
            } else if(meta$forest_type_id==3){incs <- cbind(id,age,rep(0,length(age)),rep(0,length(age)),rep(0,length(age)),inc)
              hwInc <- rbind(hwInc,incs)}
      }
  }
  
# increments calculated but matrix needs to be made to look like $growth_increments  
colnames(swInc) <- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
colnames(hwInc) <- c("id", "age", "swmerch","swfol","swother","hwmerch","hwfol","hwother")
increments <- as.data.table(rbind(swInc,hwInc)) %>% .[order(id),]
increments[is.na(increments)] <- 0
growth_increments <- as.matrix(increments)

## the following functions are now defined in the package:
# b_m()
# nmfac()
# sapfac()
# biomProp()
# convertM3biom()


