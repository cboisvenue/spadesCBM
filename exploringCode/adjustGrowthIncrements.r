# trying to figure out what to do with post-Boudewyn negative growth values...

gcMeta <- fread(spadesCBMout$gcurveFileName)
gcMeta <- unique(gcMeta[,c(2,3,4,5,6,9,10)])
## there are 50...

gInc <- as.data.table(spadesCBMout$growth_increments)
idSim <- unique(gInc$id)
# but only 16 used in the simulation

# for each of the 16 curves check all three biomPools
# like this
gc <- melt(gc52[,2:5],id.vars = "age")
all3 <- ggplot(data=gc, aes(x=age,y=value,group=variable,colour=variable)) + geom_line()

# or plot all merch?
swMerchList <- list()

gcSim <- gInc[id %in% idSim,]
gc <- melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:8)
gc[value < 0 & age<80, value := 0]
# plots <- gcSim[, ggplot(data=.SD, aes(x=age,y=value,group=variable,colour=variable)) + geom_line(), 
#                by = "id"]

names(idSim) <- idSim
plots <- lapply(idSim, function(idLoop) {
  ggplot(data=gc[id == idLoop], 
                aes(x=age,y=value,group=variable,colour=variable)) + geom_line()
})
  
# problems with White Birch id 37 and 58, and problems with BS id 49 and 50 in
# spu 28(the first set worked id 28 is G productivity in spu27, and id 29 is M
# prod in spu 27)

############### DECISIONS ###############################
## AFTER CHECKING THAT THE PARAMS WERE RIGTH IN THE BOUDEWYN CALCULATIONS
# BS curves 49 and 50 replaced with 28 and 29 (in that order)
# White birch id 37 and 58 replace with TA id 34
### THIS WILL BE HARD CODE IN spadesCBMinputs.r
#########################################################

## HARD CODED FIXES to insert into spadesCBMinputs.r #######################

increments <- as.data.table(spadesCBMout$growth_increments)
increments[id==49,3:8] <- increments[id==28,3:8]
increments[id==50,3:8] <- increments[id==29,3:8]
increments[id==37,3:8] <- increments[id==34,3:8]
increments[id==58,3:8] <- increments[id==34,3:8]

## check### MAKE A FUNCTION WITH THIS###
m3ToVolCheckPlots <- function(sim=spadesCBMout){
  gInc <- as.data.table(sim$growth_increments)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim,]
  gc <- melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:8)
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop) {
    ggplot(data=gc[id == idLoop], 
           aes(x=age,y=value,group=variable,colour=variable)) + geom_line()
  })
  
  return(plots)
}
# plots <- gcSim[, ggplot(data=.SD, aes(x=age,y=value,group=variable,colour=variable)) + geom_line(), 
#                by = "id"]




## LEG WORK DOWN HERE#######################################
#load the functions to process the growth cureves from spadesCBMinputsFunctions.r
# Chek White Birch parameters

############################################# WHite Birch check ######
## danger hard coded## need to change this to read URL or cache these.
table3 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table3.csv")#)file.path(paths(sim)$inputPath,"appendix2_table3.csv"
table4 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table4.csv")
table5 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table5.csv")
table6 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table6_v2.csv")

# identify jurisdiction matching CBM-legacy numbering with Boudewyn
# jurisdiction params----------------------------------------------
# choices are: 
# table3$juris_id and table4$juris_id and table6$jur
# AB BC MB NB NF NS NT NU ON PE QC SK YK
# table5$juris_id
# AB BC NB NF NT
cbmAdmin <- c(10,11,8,5,1,2,3,13,14,7,4,6,9,12)## danger hard coded##
paramJur <- c("AB","BC","MB","NB","NF","NF","NS" ,"NT" ,"NU" ,"ON" ,"PE", "QC", "SK", "YK")
adminMatch <- as.data.table(cbind(cbmAdmin,paramJur))
ecoToSpu <- as.data.table(ecoToSpu)

jurisdiction <- "SK"
table3 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table3.csv")#)file.path(paths(sim)$inputPath,"appendix2_table3.csv"
table4 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table4.csv")
table5 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table5.csv")
table6 <- read.csv("C:/Celine/GitHub/spadesCBM/data/appendix2_table6_v2.csv")
sktable3 <- as.data.table(table3[table3$juris_id==jurisdiction,])
sktable4 <- as.data.table(table4[table4$juris_id==jurisdiction,])
# table5 is weird since they did not have enough data for SK. I am selecting AB
# instead. Another catch is that they don't have the same species match. I
# manually check and ABIES is genus 3 (used below)
#### PUT error message if the specified jurisdiction is not found #### GIVE CHOICES
sktable5 <- as.data.table(table5[table5$juris_id=="AB",])
sktable6 <- as.data.table(table6[table6$jur==jurisdiction,])
# END jurisdiction-----------------------------------------------

# read-in species match with canfi_species code and genus to get rigth
# Boudewyn params---------------------------------------------------
## danger this is hard coded ## Species match will have to be checked by user
spsMatch <- fread("C:/Celine/GitHub/spadesCBM/data/spsMatchNameRasterGfileBiomParams.csv")#file.path(paths(sim)$inputPath,"spsMatchNameRasterGfileBiomParams.csv"
# Match gcID$species to spsMatch$speciesName, then sktable3-4 have
# $canfi_species, sktable5 $genus, sktable6 has $species which is equilvalent
# to $canfi_species

fullSpecies <- unique(gcMeta$species)
swInc <- NULL
hwInc <- NULL


#for(i in 1:length(fullSpecies)){
i=6 # white birch
i=3 # BS
  speciesMeta <- gcMeta[species==fullSpecies[i],]
  
  ## only need id 37 and 58 for White birch
  speciesMeta <- speciesMeta[2:3,]
  # BS 49 and 50
  speciesMeta <- speciesMeta[5:6,]
  
  #for(j in 1:2)){ 
  j=1
    meta <- speciesMeta[j,]
    id <- 49#growthCurveComponents$GrowthCurveComponentID[which(growthCurveComponents$GrowthCurveComponentID == meta$growth_curve_component_id)][-1]
    age <- volInc[GrowthCurveComponentID==id,Age][-1]
      #growthCurveComponents[GrowthCurveComponentID==meta$growth_curve_component_id,Age][-1]
    ecozones <- ecoToSpu[which(ecoToSpu$SpatialUnitID %in% c(27,28)),]
    cumBiom <- as.matrix(convertM3biom(meta = meta,gCvalues = volInc,spsMatch=spsMatch, 
                                       ecozones = ecozones,params3=sktable3, params4=sktable4, 
                                       params5=sktable5,params6=sktable6))
    # going from tonnes of biomass/ha to tonnes of carbon/ha here
    cumBiom <- cumBiom*0.5
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
 # }
#}
