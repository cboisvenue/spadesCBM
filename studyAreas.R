library(magrittr)
library(sf)
library(raster)
library(reproducible)

studyAreaLarge <- prepInputs(url = 'https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/view?usp=sharing',
                             destinationPath = "data",
                             overwrite = TRUE,
                             useCache = 'overwrite',
                             FUN = 'sf::st_read') %>%
  sf::st_as_sf(.)
studyAreaLarge <- studyAreaLarge[studyAreaLarge$TSA_NUMBER %in% c('08', '16', '24', '40', '41'),]

if (length(unique(sf::st_geometry_type(studyAreaLarge))) > 1)  ## convert sfc to sf if needed
  sf::st_geometry(studyAreaLarge) <- sf::st_collection_extract(x = sf::st_geometry(studyAreaLarge), type = "POLYGON")

studyAreaLarge <- sf::st_buffer(studyAreaLarge, 0) %>%
  sf::as_Spatial(.) %>%
  raster::aggregate(.) %>%
  sf::st_as_sf(.)
studyAreaLarge$studyArea <- "5TSA"
studyAreaLarge <- sf::as_Spatial(studyAreaLarge)
plot(studyAreaLarge)
