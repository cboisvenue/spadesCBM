###################
# figuring out what the recliner raters are
# July 2018
# CBoisvenue
# 

library(raster)
library(SpaDES.tools)
dist1991r <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/disturbance/1991.tif"))
dev()
Plot(dist1991r)
dist1991all <- getValues(dist1991r)
dist1991 <- !is.na(dist1991all)
