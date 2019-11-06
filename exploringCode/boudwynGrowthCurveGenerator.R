growth.inc <- spadesCBMout$growth_increments

table3 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table3.csv"))
sktable3 <- table3[table3$jur=="SK",]
table4 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table4.csv"))
sktable4 <- table4[table4$jur=="SK",]
# table5 is weird since they did not have enough data for SK. I am selecting AB
# instead. Another catch is that they don't have the same species match. I
# manually check and ABIES is genus 3 (used below)
table5 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table5.csv"))
sktable5 <- table5[table5$juris_id=="AB",]
table6 <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table6_v2.csv"))
sktable6 <- table6[table6$jur=="SK",]
# read-in parameters done---------------------------

#what are the spus in SK? and what ecoboundaries do we have?-------------
spu <- as.data.frame(spadesCBMout$cbmData@spatialUnitIds)
spu[spu$AdminBoundaryID==9&spu$EcoBoundaryID %in% c(6,9),]
# end spu and eco--------------------------------------------------------

# what species do we have?--------------------------------------------------------
gcID <- read.csv(spadesCBMout$gcurveFileName)#file.path(getwd(),"data/spadesGCurvesSK.csv"))
gcSpsName <- unique(gcID$species)
# this is cbm-species number
gcSpsID <- unique(gcID$species_id)
## PROBLEM: Sps
# now need to associate the growth_curve_component_id to the species
# there are 10 curves, 
# unique growth_curve_component_id
SpsIDMatch <- vector(length=10)
for(i in 1: length(gcSpsName)){
  SpsIDMatch[i] <- gcID[gcID$species==gcSpsName[i],4]%>% .[1]
}
species <- as.character(gcSpsName)
gcSpsMatch <- as.data.table(cbind(species,gcSpsName,SpsIDMatch))

# what are the species code for the parameters?
paramSps <- read.csv(file.path(paths(spadesCBMout)$inputPath,"appendix2_table7.csv"))
paramSps <- paramSps[paramSps$prov=="SK"&paramSps$ecozone %in% c(6,9),]

####Start function ####

#####
species <- c(302,1203,101,203,1201,1303,105)
#POPU is 1201, 1203. 
#Balsam Fir is 302,
#PICE Mar is 101
#betu pap is 1303
#pinu ban is 203
#Picea gla is 105
gComp <- fread(spadesCBMout$gcurveComponentsFileName)

# try one volume at year 100
# vol100 <- gComp14[,3]

sktable3 <- data.table(sktable3)
sktable4 <- data.table(sktable4)
sktable6 <- data.table(sktable6)
sktable5 <- as.data.table(sktable5)

possibleGenus <- unique(paramSps$genus)
sktable5Inputs <- sktable5[ecozone == 9, (.N), .(canfi_genus, genus)]
sktable3Inputs <- sktable3[ecozone == 9, (.N), .(species, genus)]
#Make sure they are all the same
possibleFullSpecies <- unique(gcSpsMatch)
# gComp <- read.csv(spadesCBMout$gcurveComponentsFileName)
#Try for Picae gla

writeGC <- function(gen, spec, ez, fullSpecies, gComp, speciesMatch){
  #Get species subset
  gCompSpec <- gComp[GrowthCurveComponentID == speciesMatch[species == fullSpecies]$SpsIDMatch]
  vol100 <- gCompSpec[, 3]
  names(speciesMatch) <- c("speciesName", "rasterSps","SpsIDMatch")
  spsMatch <- cbind(speciesMatch, spec)
  
  params3 <- sktable3[species== spec & ecozone == ez,][1] # these first-row subsets incase two rows returned
  params4 <- sktable4[species== spec & ecozone == ez,][1]
  # table 5 is different than the others
  params5 <- sktable5[genus == gen & ecozone == ez,][1]
  params6 <- sktable6[species == spec & ecozone == ez,][1]
  # eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  b_m <- function(paramTable1, vol){
    b_m <- paramTable1$a * vol ^ paramTable1$b
    return(b_m)
  }
  eq1 <- b_m(params3, vol100$MerchVolume)
  
  # eq2 is for non-merch sized trees.
  browser()
  nmfac <- function(table4,eq1){
    browser()
    nmFac <- table4$k + (table4$a * eq1 ^ table4$b)
    b_nm <- nmFac * eq1
    b_n <- b_nm - eq1
    return(cbind(b_n,b_nm))
  }
  eq2 <- nmfac(params4, eq1 = eq1)
  #some NAs where it was 0s
  eq2[is.na(eq2)] <- 0
  
  # eq3 is for the saplings and it needs b_nm from the previous eq2
  sapfac <- function(table5, eq2){
    sapFac <- table5$k + (table5$a * eq2[,2] ^ table5$b)
    b_snm <- sapFac * eq2[,2]
    b_s <- b_snm - eq2[,2]
    return(b_s)
  }
  eq3 <- sapfac(params5, eq2 = eq2)
  eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3
  merch <- eq1+eq2[,1] + eq3

  # calculate the 4 proportions  should be returned
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
  pVect <- biomProp(table6 = params6, vol = vol100$MerchVolume)  
  
  totbiom <- merch/pVect[,1]
  bark <- totbiom*pVect[,2]
  branch <- totbiom*pVect[,3]
  fol <- totbiom*pVect[,4]
  other <- branch+bark
  
  # End of one age for Balsam fir at age 100, biomass seem reasonable-----------------------
  # then black spruce
  # Jack pine
  gcCompID <- gCompSpec$GrowthCurveComponentID[1]
  
  id <- rep(gcCompID, length(merch))
  curve <- cbind(id,merch,fol,other)
  names(curve) <- c("id", "merch", "fol", "other")
  # merch, fol, other looks fine once translated for Balsam fir--------------------------------

  curve <- as.matrix(curve)
  growth_increments <- data.frame(diff(curve))
  growth_increments$id <- gcCompID
  
  return(growth_increments)
  
}

piceaGlaucaGC <- writeGC(gen = "PICE", spec = 'GLA', ez = 9, fullSpecies = 'White spruce', 
                         gComp = gComp, speciesMatch = gcSpsMatch)
# write.csv(piceaGlaucaGC, file.path(inputPath(spadesCBMout), "whiteSpruceInc.csv"))
pinusBanksianaGC <- writeGC(gen = "PINU", spec = "BAN", ez = 9, fullSpecies = 'Jack pine',
                            gComp = gComp, speciesMatch = gcSpsMatch)
# write.csv(pinusBanksianaGC, file.path(inputPath(spadesCBMout), "jackPineInc.csv"))
betulaPapyriferaGC <- writeGC(gen = "BETU", spec = "PAP", ez = 9, fullSpecies = 'White birch', 
                              gComp = gComp, speciesMatch = gcSpsMatch)
# write.csv(betulaPapyriferaGC, file.path(inputPath(spadesCBMout), "whiteBirchInc.csv"))
abiesBalsamiferaGC <- writeGC(gen = "ABIE", spec = "BAL", ez = 9, fullSpecies = "Balsam fir", 
                              gComp = gComp, speciesMatch = gcSpsMatch)
# write.csv(abiesBalsamiferaGC, file.path(inputPath(spadesCBMout), "balsamFirInc.csv"))



bSpruceInc <- read.csv(file.path(paths(spadesCBMout)$inputPath,"blackSpruceInc.csv"))

BSid <- c(8,9,29,30,50,51,71,72,92,93)

growth.inc <- spadesCBMout$growth_increments
for(i in 1:length(BSid)){
  growth.inc[growth.inc[,1] == BSid[i],3] <- bSpruceInc[,2]
  growth.inc[growth.inc[,1] == BSid[i],4] <- bSpruceInc[,3]
  growth.inc[growth.inc[,1] == BSid[i],5] <- bSpruceInc[,4]
}




