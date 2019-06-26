# cheking out the DMIDs used in our simulations

# these are the disturbance matrix ids used
dists <- unique(spadesCBMout$mySpuDmids$disturbance_matrix_id)

# use the functions in extraFunctions
listDists <- seeDist(dists)
str(listDists)

### Checking the matrices--------------------------------------------------------------------------
# fire in the Boreal Shield West------------------------
fireBS <- listDists[[1]]







# fire in teh Boreal Plains
firBP <- listDists[[2]]

# ccut
ccut <-  listDists[[3]]

# defor
defor <-  listDists[[4]]

#mortality 20%
mort20 <-  listDists[[5]]




# frequency of disturbances:
#Get frequency table from disturbance rasters
myBigList <- grep(pattern = "*.tif", 
                  x = list.files("data/forIan/SK_data/CBM_GIS/disturbance_testArea/", full.names = TRUE),
                  value = TRUE)
myBigList <- lapply(myBigList, raster::raster)
myBigStack <- raster::stack(myBigList)
freqTables <- raster::freq(myBigStack)