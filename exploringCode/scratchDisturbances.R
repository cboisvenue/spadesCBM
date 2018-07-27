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
table(dist1991all)

# 1612  1630  1693  1737  1743  1744  1760  1761  1762  1774  1950  1957  1975  1997  1998 
# 6     7    18    22    31    10    17    12     7     9   350    97   274   108   140 
# 2007  2010 10276 10292 10307 10308 10350 13645 13646 17917 17921 
# 33    37     5    11    10     8    19    17     9    13    12 

#pick another
dist1995r <- raster(file.path(getwd(),"data/forIan/SK_data/CBM_GIS/disturbance/1995.tif"))
dev()
Plot(dist1995r)
dist1995all <- getValues(dist1995r)
table(dist1995all)
