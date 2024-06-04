#Comparing the spinup results from Python and from Cpp

## browser() set at the botton of the spinup event in CBM_core@python
result1 <- cbm_vars$pools
result1 <- data.table(cbm_vars$pools)
spinupPython <- result1[, pixelGroup:= spatial_inv_gc_merge$pixelGroup]


## Cpp
spinupCpp <-  data.table(simCoreAlone$spinupResult)
spinupCpp[, pixelGroup := simCoreAlone$level3DT$pixelGroup]

spinupCpp[, merch := (SoftwoodMerch + HardwoodMerch)]
spinupCpp[, foliage := (SoftwoodFoliage + HardwoodFoliage)]
spinupCpp[, other := (SoftwoodOther + HardwoodOther)]
spinupCpp[, coarseRoots := (SoftwoodCoarseRoots + HardwoodCoarseRoots)]
spinupCpp[, fineRoots := (SoftwoodFineRoots + HardwoodFineRoots)]
spinupCpp[, stemSnag := (SoftwoodStemSnag + HardwoodStemSnag)]
spinupCpp[, branchSnag := (SoftwoodBranchSnag + HardwoodBranchSnag)]

colnames(spinupPython)
# [1] "Input"                   "Merch"                   "Foliage"
# [4] "Other"                   "CoarseRoots"             "FineRoots"
# [7] "AboveGroundVeryFastSoil" "BelowGroundVeryFastSoil" "AboveGroundFastSoil"
# [10] "BelowGroundFastSoil"     "MediumSoil"              "AboveGroundSlowSoil"
# [13] "BelowGroundSlowSoil"     "StemSnag"                "BranchSnag"
# [16] "CO2"                     "CH4"                     "CO"
# [19] "NO2"                     "Products"                "pixelGoup"

colnames(spinupCpp)
[1] "Input"
[13]
[28] "merch"                   "foliage"                 "other"
[31] "coarseRoots"             "fineRoots"

outCols <- c("SoftwoodMerch", "SoftwoodFoliage", "SoftwoodOther",
             "SoftwoodCoarseRoots", "SoftwoodFineRoots", "HardwoodMerch",
             "HardwoodFoliage", "HardwoodOther", "HardwoodCoarseRoots",
             "HardwoodFineRoots",
             "SoftwoodStemSnag",
             "SoftwoodBranchSnag",
             "HardwoodStemSnag",
             "HardwoodBranchSnag"
             )
spinupCpp[ , (outCols) := NULL]

ordCols <- c(
  "Input",
  "pixelGroup",
  "merch",
  "foliage",
  "other",
  "coarseRoots",
  "fineRoots",
  "stemSnag",
  "branchSnag",
  "AboveGroundVeryFastSoil",
  "BelowGroundVeryFastSoil",
  "AboveGroundFastSoil",
  "BelowGroundFastSoil",
  "MediumSoil",
  "AboveGroundSlowSoil",
  "BelowGroundSlowSoil",
  "CO2",
  "CH4",
  "CO",
  "Products"
)
setcolorder(spinupCpp, ordCols)
setkey(spinupCpp,pixelGroup)
setkey(spinupPython,pixelGroup)

bothSpinup <- merge(spinupCpp, spinupPython)
ordCols2 <- c(
  "Input.x", "Input.y",
  "pixelGroup",
  "merch", "Merch",
  "foliage", "Foliage",
  "other", "Other",
  "coarseRoots", "CoarseRoots",
  "fineRoots", "FineRoots",
  "stemSnag", "StemSnag",
  "branchSnag", "BranchSnag",
  "AboveGroundVeryFastSoil.x","AboveGroundVeryFastSoil.y",
  "BelowGroundVeryFastSoil.x",  "BelowGroundVeryFastSoil.y",
  "AboveGroundFastSoil.x","AboveGroundFastSoil.y",
  "BelowGroundFastSoil.x", "BelowGroundFastSoil.y",
  "MediumSoil.x", "MediumSoil.y",
  "AboveGroundSlowSoil.x", "AboveGroundSlowSoil.y",
  "BelowGroundSlowSoil.x", "BelowGroundSlowSoil.y",
  "CO2.x", "CO2.y",
  "CH4.x", "CH4.y",
  "CO.x", "CO.y",
  "Products.x", "Products.y"
)

setcolorder(bothSpinup, ordCols2)

## checking that the disturbance matrices are ther same:
distMatCheck<- data.table(reticulate::py_to_r(libcbm_default_model_config$disturbance_matrix_association))
distMatCheck[(spatial_unit_id==28 & disturbance_type_id == 1), ]
#    spatial_unit_id disturbance_type_id  sw_hw disturbance_matrix_id
#              <num>               <num> <char>                 <num>
# 1:              28                   1     sw                   371
# 2:              28                   1     hw                   851

## just compare the sw stands since the simCoreAlone only took DMID 371
spatial_inv_gc_merge[is_sw != TRUE,]
# ages spatial_unit_id growth_curve_component_id growth_curve_id ecozones pixelGroup
# <num>           <num>                    <fctr>          <fctr>    <num>      <int>
#   1:    79              28                        58              58        9         22
# gcids  is_sw
# <fctr> <lgcl>
#   1:     58  FALSE

bothSpinup <- bothSpinup[pixelGroup != 22,]
write.csv(bothSpinup, "compareSpinup.csv")
