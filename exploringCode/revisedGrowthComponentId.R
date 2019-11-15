# Growth Component revised 
# The growth component file (ther one that has the
# m3/ha over time) provided for the spadesCBM runs had 105 unique
# growth_curve_component_id. In reality there are 10 curves. This script is
# intended to reduce the 105 back into 10 (tasks 1). 
# Those 10 will then be "feed into" the new volumeToBiomass function (task 2)

# task 1------------------------------------------------------------------------
# checking that the metadata for the growth curve components can be modified
# down to 10 without loosing information

# metadata
gcID <- fread(spadesCBMout$gcurveFileName)
# remove references to curve id (105 of them)
gcID[,c("V1","growth_curve_component_id","growth_curve_id"):=NULL]
unique(gcID$spatial_unit_id)

ecoToSpu <- as.data.table(spadesCBMout$cbmData@spatialUnitIds[,c(1,3)])
skEcoSpu <- ecoToSpu[which(SpatialUnitID %in% unique(gcID$spatial_unit_id)),]
# 5 ecozones

length(unique(gcID$species))
length(unique(gcID$species_id))
#[1] 7
unique(gcID$forest_type_id)
# 1 and 3 (Softwood and Hardwood)
unique(gcID$Productivity)
unique(gcID$prodClass)

# if Boudewyn parameters are different for each of the ecozones in SK, we need the 105 curves
# 5 eco X 3 prod X 7 species...except wedon't have 3 prod levels for each species...
# black spruce has two
# trembling aspen has two
# white spruce has two
spsOneProd <- c("Balsam fir","Balsam poplar","Jack pine","White birch")
gcID[which(species %in% spsOneProd), c("Productivity","prodClass"):= list(1,"G")]
spsTwoProd <- c("Black spruce","Trembling aspen","White spruce")
gcID[which(species %in% spsTwoProd), c("Productivity","prodClass"):= list(rep(as.integer(c(2,2,3)),15),
                                                           rep(c("M","M","G"),15))]
# there are only 50 unique combinations.
newGcID <- unique(gcID)

# reduce the growth components now...
gComp <- fread(spadesCBMout$gcurveComponentsFileName)
removeIDs <- c(2,3,23,24,44,45,65,66,86,87,
               5,6,26,27,47,48,68,69,89,90,
               9,30,51,72,93,
               11,12,32,33,53,54,74,75,95,96,
               15,36,57,78,99,
               17,18,38,39,59,60,80,81,101,102,
               21,42,63,84,105)

## These are the only growth curve components that should exist.               
newGcomp <- gComp[which(!(GrowthCurveComponentID %in% removeIDs)),]   

#this is the new vector for growth_curve_component, and growth_curve_id
growth_curve_component_id2 <- c(1,
                                1,
                                1,
                                22,
                                22,
                                22,
                                43,
                                43,
                                43,
                                64,
                                64,
                                64,
                                85,
                                85,
                                85,
                                4,
                                4,
                                4,
                                25,
                                25,
                                25,
                                46,
                                46,
                                46,
                                67,
                                67,
                                67,
                                88,
                                88,
                                88,
                                7,
                                28,
                                49,
                                70,
                                91,
                                8,
                                8,
                                29,
                                29,
                                50,
                                50,
                                71,
                                71,
                                92,
                                92,
                                10,
                                10,
                                10,
                                31,
                                31,
                                31,
                                52,
                                52,
                                52,
                                73,
                                73,
                                73,
                                94,
                                94,
                                94,
                                13,
                                34,
                                55,
                                76,
                                97,
                                14,
                                14,
                                35,
                                35,
                                56,
                                56,
                                77,
                                77,
                                98,
                                98,
                                16,
                                16,
                                16,
                                37,
                                37,
                                37,
                                58,
                                58,
                                58,
                                79,
                                79,
                                79,
                                100,
                                100,
                                100,
                                19,
                                40,
                                61,
                                82,
                                103,
                                20,
                                20,
                                41,
                                41,
                                62,
                                62,
                                83,
                                83,
                                104,
                                104
)

# re-read the gcID, add new vector, save all files
gcID <- fread(spadesCBMout$gcurveFileName)
gcID[,c("V1","growth_curve_component_id","growth_curve_id"):= list(NULL,growth_curve_component_id2,growth_curve_component_id2)]

write.csv(gcID,spadesCBMout$gcurveFileName)
write.csv(newGcomp,spadesCBMout$gcurveComponentsFileName)
