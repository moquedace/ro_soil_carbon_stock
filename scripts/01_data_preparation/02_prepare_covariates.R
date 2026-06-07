

pkg <- c("rgdal", "beepr", "sf", "fasterize", "stringr", "geobr", "readxl",
         "dplyr", "tmap", "tmaptools", "terra")

sapply(pkg, require, character.only = T)


setwd("C:/R/ro_soil_carbon")

rm(list = ls())  # clean memory
gc()


ro_bf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>% vect()


ro_bf_wgs <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>%
  st_transform(crs = "EPSG:4326") %>% vect()



# worldclim ---------------------------------------------------------------



clim <- list.files(path = "./covariaveis/original/worldclim/present",
                   pattern = ".tif$", full.names = T) %>%
  rast()


n_clim <- list.files(path = "./covariaveis/original/worldclim/present",
                     pattern = ".tif$", full.names = F) %>%
  str_split("_", simplify = T, n = 3) %>%
  .[,3] %>%
  str_remove(".tif")







clim_cr_msk <- clim %>%
  terra::crop(ro_bf_wgs) %>%
  terra::mask(ro_bf_wgs) %>%
  terra::project(y = "ESRI:102015", method = "cubicspline")




plot(clim_cr_msk[[1]])


names(clim_cr_msk) <- n_clim



r_base <- rast("./covariaveis/original/dem/mosaico/dem.tif")

clim_resample <- resample(clim_cr_msk, r_base, method = "cubicspline")


plot(clim_resample[[1]])
plot(clim_cr_msk[[1]])
plot(r_base)
summary(clim_cr_msk)
summary(clim_resample)



for (i in seq_along(names(clim_resample))[44:55]) {

  terra::writeRaster(clim_resample[[i]],
                     filename = paste0("./covariaveis/bf/",
                                       names(clim_resample)[i], ".tif"),
                     overwrite = T, gdal = c("COMPRESS=LZW"))

}




# geology -----------------------------------------------------------------


geology <- rast("./covariaveis/original/geologia/geology.tif") %>%
  `names<-`("geology")


r_base <- rast("./covariaveis/original/dem/mosaico/dem.tif")


geology_resample <- resample(geology, r_base, method = "near")


plot(geology)
plot(geology_resample)


terra::writeRaster(geology_resample,
                   filename = paste0("./covariaveis/bf/",
                                     "geology", ".tif"),
                   overwrite = T, gdal = c("COMPRESS=LZW"))




# soil -----------------------------------------------------------------


soil <- rast("./covariaveis/original/solo/soil.tif") %>%
  `names<-`("soil")


r_base <- rast("./covariaveis/original/dem/mosaico/dem.tif")


soil_resample <- resample(soil, r_base, method = "near")


plot(soil)
plot(soil_resample)




terra::writeRaster(soil_resample,
                   filename = paste0("./covariaveis/bf/",
                                     "soil", ".tif"),
                   overwrite = T, gdal = c("COMPRESS=LZW"))
