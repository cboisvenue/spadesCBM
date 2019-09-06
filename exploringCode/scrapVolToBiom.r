params3 <- sktable3[sktable3$canfi_species==1201&sktable3$eco==6,]
params4 <- sktable4[sktable4$canfi_species==1201&sktable4$eco==6,]
# table 5 is different than the others
params5 <- sktable5[sktable5$canfi_genu==9&sktable5$ecozone==6,]
params6 <- sktable6[sktable6$species==1201&sktable6$eco==6,]

# try one volume at year 100
vol100 <- gComp14[,3]

# eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
# the gross merchantable volume/ha. Parameters a and b are in table3
b_m <- function(paramTable1,vol){
  b_m <- paramTable1$a*vol^paramTable1$b
  return(b_m)
}
eq1 <- b_m(params3,vol100)

# eq2 is for non-merch sized trees.
nmfac <- function(table4,eq1){
  nmFac <- table4$k+table4$a*eq1^table4$b
  b_nm <- nmFac*eq1
  b_n <- b_nm-eq1
  return(cbind(b_n,b_nm))
}
eq2 <- nmfac(params4,eq1 = eq1)
# Boudewyn parameters
# read-in all parameters---------------------------
# model parameters are specific to the jurisdiction, ecozone and lead tree species
# can be updated here: https://nfi.nfis.org/en/biomass_models
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


# now Jack PIne 
params3 <- sktable3[sktable3$canfi_species==1201&sktable3$eco==6,]
params4 <- sktable4[sktable4$canfi_species==1201&sktable4$eco==6,]
# table 5 is different than the others
params5 <- sktable5[sktable5$canfi_genu==9&sktable5$ecozone==6,]
params6 <- sktable6[sktable6$species==1201&sktable6$eco==6,]

# try one volume at year 100
vol100 <- gComp14[,3]

# eq1 gives the total stem wood biomass in metric tonnes/ha, when you give it
# the gross merchantable volume/ha. Parameters a and b are in table3
b_m <- function(paramTable1,vol){
  b_m <- paramTable1$a*vol^paramTable1$b
  return(b_m)
}
eq1 <- b_m(params3,vol100)

# eq2 is for non-merch sized trees.
nmfac <- function(table4,eq1){
  nmFac <- table4$k+table4$a*eq1^table4$b
  b_nm <- nmFac*eq1
  b_n <- b_nm-eq1
  return(cbind(b_n,b_nm))
}
eq2 <- nmfac(params4,eq1 = eq1)
#some NAs where it was 0s
#some NAs where it was 0s
eq2[which(is.na(eq2))] <- 0

# eq3 is for the saplings and it needs b_nm from the previous eq2
sapfac <- function(table5,eq2){
  sapFac <- table5$k+table5$a*eq2[,2]^table5$b
  b_snm <- sapFac*eq2[,2]
  b_s <- b_snm-eq2[,2]
  return(b_s)
}
eq3 <- sapfac(params5,eq2 = eq2)
eq3[which(is.na(eq3))] <- 0

# middle box flowchart3
merch <- eq1+eq2[,1]+eq3

# calculate the 4 proportions  should be returned
# will eventually add species, ecozone
# vol = gross merchantable volume per ha
# lvol = natural logarithm of (vol+5)
biomProp <- function(table6,vol){
  lvol <- log(vol+5)
  a <- c(5:7)
  b <- c(8:10)
  c <- c(11:13)
  pstem <- 1/(1+exp(table6[,a[1]]+table6[,a[2]]*vol+table6[,a[3]]*lvol)+
                exp(table6[,b[1]]+table6[,b[2]]*vol+table6[,b[3]]*lvol)+
                exp(table6[,c[1]]+table6[,c[2]]*vol+table6[,c[3]]*lvol))
  pbark <- exp(table6[,a[1]]+table6[,a[2]]*vol+table6[,a[3]]*lvol)/
    (1+exp(table6[,a[1]]+table6[,a[2]]*vol+table6[,a[3]]*lvol)+
       exp(table6[,b[1]]+table6[,b[2]]*vol+table6[,b[3]]*lvol)+
       exp(table6[,c[1]]+table6[,c[2]]*vol+table6[,c[3]]*lvol))
  pbranches <- exp(table6[,b[1]]+table6[,b[2]]*vol+table6[,b[3]]*lvol)/
    (1+exp(table6[,a[1]]+table6[,a[2]]*vol+table6[,a[3]]*lvol)+
       exp(table6[,b[1]]+table6[,b[2]]*vol+table6[,b[3]]*lvol)+
       exp(table6[,c[1]]+table6[,c[2]]*vol+table6[,c[3]]*lvol))
  pfol <- exp(table6[,c[1]]+table6[,c[2]]*vol+table6[,c[3]]*lvol)/
    (1+exp(table6[,a[1]]+table6[,a[2]]*vol+table6[,a[3]]*lvol)+
       exp(table6[,b[1]]+table6[,b[2]]*vol+table6[,b[3]]*lvol)+
       exp(table6[,c[1]]+table6[,c[2]]*vol+table6[,c[3]]*lvol))
  propVect <- cbind(pstem,pbark,pbranches,pfol)    
}
pVect <- biomProp(table6=params6,vol = vol100)  

totbiom <- merch/pVect[,1]
bark <- totbiom*pVect[,2]
branch <- totbiom*pVect[,3]
fol <- totbiom*pVect[,4]
other <- branch+bark
