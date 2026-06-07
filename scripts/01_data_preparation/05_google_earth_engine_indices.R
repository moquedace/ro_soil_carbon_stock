pkg <- c("caret", "rgdal", "beepr", "sf", "fasterize", "stringr", "geobr",
         "readxl", "dplyr", "tmap", "tmaptools", "terra", "parallelly", "gbm",
         "parallel", "doParallel", "DescTools", "Cubist", "kknn", "kernlab",
         "tidyr", "RColorBrewer", "mpspline2")

sapply(pkg, require, character.only = T)

rm(list = ls())  # clean memory
gc()

setwd("C:/R/ro_soil_carbon")


ro_bf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000)


ro_bf_wgs_sf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>%
  st_transform(crs = "EPSG:4326")

ro_bf_wgs <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>%
  st_transform(crs = "EPSG:4326") %>%
  vect()


ro <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  vect()



dem_bf <- rast("./covariaveis/original/dem/mosaico/dem.tif")

lre <- list.files(path = "./covariaveis/original/engine",
                  pattern = ".tif$", full.names = T)





evi <- vrt(lre[1:2])
msavi <- vrt(lre[3:4])
nbr <- vrt(lre[5:6])
nbr2 <- vrt(lre[7:8])
ndvi <- vrt(lre[9:10])
nmdi <- vrt(lre[11:12])
savi <- vrt(lre[13:14])


lrem <- c(evi, msavi, nbr, nbr2, ndvi, nmdi, savi) %>%
  terra::crop(ro_bf_wgs) %>%
  terra::mask(ro_bf_wgs)


plot(lrem[[5]])
summary(lrem)

names(lrem) <- c("evi", "msavi", "nbr", "nbr2", "ndvi", "nmdi", "savi")



lrem <- project(lrem, y = "ESRI:102015", method = "cubicspline")

plot(lrem[[5]])
summary(lrem)


for (i in seq_along(names(lrem))) {

  print(names(lrem)[i])

  writeRaster(lrem[[i]], gdal = c("COMPRESS=LZW"), overwrite = T,
              filename = paste0("./covariaveis/bf/", names(lrem)[i], ".tif"))

}



rst <- list.files(path = "./covariaveis/bf",
                  pattern = ".tif$", full.names = T) %>%
  grep(pattern = "84", value = T) %>%
  rast()

dem <- rast("./covariaveis/prontas/present/dem.tif")

rst_mask <- rst %>%
  terra::crop(ro) %>%
  terra::mask(ro)


for (i in seq_along(names(rst_mask))) {

  print(names(rst_mask)[i])

  writeRaster(rst_mask[[i]], gdal = c("COMPRESS=LZW"), overwrite = T,
              filename = paste0("./covariaveis/prontas/present/",
                                names(rst_mask)[i], ".tif"))

}
