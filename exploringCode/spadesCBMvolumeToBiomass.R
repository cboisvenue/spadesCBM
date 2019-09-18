#---------------------------------------------
#Boudewyn st al parameters for conversion from m3/ha to biomass in
#the three main carbon pools that make-up the $growth_increments used to move
#spadesCBM forward in growth from year to year
# https://nfi.nfis.org/en/biomass_models

table3 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table3.csv"))
table4 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table4.csv"))
table5 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table5.csv"))
table6 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table6_v2.csv"))

### needs
library(data.table)

# make a function that identifies 
    
  # identify species, genus, and ecozone
  # read-in meta data
  gcMetaData <- fread("C:/Celine/GitHub/spadesCBM/data/spadesGCurvesSK.csv")#sim$gcurveFileName
  gcMetaData <- unique(gcMetaData[,.(growth_curve_component_id,spatial_unit_id,forest_type_id,species,rasterSps)])
  # ecozones: gcMetaData contains all the ecozones in SK. Our study area does not.
  ecoToSpu <- as.data.table(spadesCBMout$cbmData@spatialUnitIds)#sim$cbmData@spatialUnitIds[,c(1,3)]
  ecozones <- ecoToSpu[which(ecoToSpu$SpatialUnitID %in% unique(spadesCBMout$level3DT$spatial_unit_id)),]
  gcMetaData <- gcMetaData[spatial_unit_id %in% ecozones$SpatialUnitID,]
  
  # our study region does not have all the species in the gcMetaData
  gcMetaData <- gcMetaData[rasterSps %in% spadesCBMout$level3DT$rasterSps,]
  
  # read-in m3/ha values
  gCvalues <- fread("C:/Celine/GitHub/spadesCBM/data/yieldComponentSK.csv") # sim$gcurveComponentsFileName
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
spsMatch <- fread("C:/Celine/GitHub/spadesCBM/data/spsMatchNameRasterGfileBiomParams.csv")
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
  
## FUNCTIONS HERE
  # eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  b_m <- function(paramTable1, vol){
    b_m <- unique(paramTable1$a) * vol ^ unique(paramTable1$b)
    return(b_m)
  }
  # eq2 is for non-merch sized trees.
  nmfac <- function(table4,eq1){
    nmFac <- unique(table4$k) + (unique(table4$a) * eq1 ^ unique(table4$b))
    b_nm <- nmFac * eq1
    b_n <- b_nm - eq1
    return(cbind(b_n,b_nm))
  }
  # eq3 is for the saplings and it needs b_nm from the previous eq2
  sapfac <- function(table5, eq2){
    sapFac <- table5$k + (table5$a * eq2[,2] ^ table5$b)
    b_snm <- sapFac * eq2[,2]
    b_s <- b_snm - eq2[,2]
    return(b_s)
  }
  # calculate the 4 proportions that should be returned
  # will eventually add species, ecozone
  # vol = gross merchantable volume per ha
  # lvol = natural logarithm of (vol+5)
  biomProp <- function(table6,vol){
    lvol <- log(vol+5)
    a <- c(7:9)
    b <- c(10:12)
    c <- c(13:15)
    pstem <- 1 / ( 1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
                     exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
                     exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
    
    pbark <- exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) /
      (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
         exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
         exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
    
    pbranches <- exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) /
      (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
         exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
         exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
    
    pfol <- exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol) /
      (1 + exp(table6[, a1] + table6[, a2] * vol + table6[, a3] * lvol) +
         exp(table6[, b1] + table6[, b2] * vol + table6[, b3] * lvol) +
         exp(table6[, c1] + table6[, c2] * vol + table6[, c3] * lvol))
    propVect <- cbind(pstem,pbark,pbranches,pfol)    
  }
   
convertM3biom <- function(meta,gCvalues){
  oneCurve <- gCvalues[GrowthCurveComponentID==meta$growth_curve_component_id,]
  spec <- spsMatch[speciesName==meta$species,]$canfi_species
  ez <- ecozones[SpatialUnitID==meta$spatial_unit_id,]$EcoBoundaryID
  gen <- spsMatch[speciesName==meta$species,]$genus
  
  params3 <- sktable3[canfi_species== spec & ecozone == ez,] 
  params4 <- sktable4[canfi_species== spec & ecozone == ez,]
  # table 5 is different than the others
  params5 <- sktable5[genus == gen & ecozone == ez,]
  params6 <- sktable6[species == spec & eco == ez,]
  
  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  eq1 <- b_m(params3, oneCurve$MerchVolume)
  # eq2 returns a two colum matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm, the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merche-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  eq2 <- nmfac(params4, eq1 = eq1)
  #some NAs where it was 0s. Leave these in place
  # eq3 is for biomass of the saplings, the smallest of the nonmerch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  eq3 <- sapfac(params5, eq2 = eq2)
  #eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees
  merch <- eq1+eq2[,1] + eq3
  merch[which(is.nan(merch))] <- NA
  # calculate the 4 proportions that should be returned: proportion for
  # stemwood, prop for bark, prop for branches, and prop for foliage.
  pVect <- biomProp(table6 = params6, vol = oneCurve$MerchVolume)  
  # translating this into biomass values for the carbon pools
  totMerch <- merch/pVect[,1]
  bark <- totMerch*pVect[,2]
  branch <- totMerch*pVect[,3]
  fol <- totMerch*pVect[,4]
  other <- branch+bark
  biomCumulative <- as.matrix(cbind(totMerch,fol,other))
  return(biomCumulative)

}
  
  
