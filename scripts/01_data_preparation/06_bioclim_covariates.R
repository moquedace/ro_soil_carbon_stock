pkg <- c("dplyr", "caret", "randomForest", "e1071", "ggplot2", "doParallel",
         "tidyr", "stringr", "parallelly", "quantregForest", "parallel",
         "terra", "data.table", "sf", "geobr")

sapply(pkg, require, character.only = T)

rm(list = ls())  # clean memory
gc()


setwd("D:/Usuario/cassio/R/ro_soil_carbon")


source("D:/Usuario/cassio/R/outros/scripts_aleatorios/s_bioclim.R")






ro_bf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000)


ro_bf_vect <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>% vect()


ro_bf_wgs <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>%
  st_transform(crs = "EPSG:4326")



ro_bf_wgs_vect <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>%
  st_transform(crs = "EPSG:4326") %>% vect()


ro <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015")


ro_vect <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>% vect()






ldir <- list.dirs("./covariaveis/original/worldclim",
                  full.names = T, recursive = F) %>%
  sort(decreasing = T)

n_ldir <- list.dirs("./covariaveis/original/worldclim",
                  full.names = F, recursive = F) %>%
  sort(decreasing = T)


i = 1

for (i in seq_along(ldir)) {

  t1 <- Sys.time()


  print(paste(n_ldir[i], t1))
  clim <- list.files(path = ldir[i],
                     pattern = ".tif$", full.names = T) %>%
    rast()







  lr_cm <- clim %>%
    terra::crop(ro_bf_wgs_vect) %>%
    terra::mask(mask = ro_bf_wgs_vect) %>%
    terra::project(y = "ESRI:102015", method = "cubicspline")


  plot(lr_cm[[2]])
  n_clim <- names(lr_cm) %>%
    str_split("_", simplify = T, n = 3) %>%
    .[,3]




  bioc <- biovar_terra(prec = terra::subset(lr_cm, 1:12),
                       tmax = terra::subset(lr_cm, 13:24),
                       tmin = terra::subset(lr_cm, 25:36))


  names(lr_cm) <- n_clim

  bioc <- resample(bioc, lr_cm, method = "cubicspline")






  world_clim <- c(lr_cm, bioc)







  r_base <- rast("./covariaveis/original/dem/mosaico/dem.tif")

  clim_resample <- resample(world_clim, r_base, method = "cubicspline")


  writeRaster(clim_resample,
              "./clim_resample.tif",
              gdal = c("COMPRESS=LZW"), overwrite = T)







  clim_resample <- rast("./clim_resample.tif")


  tmpFiles(current = T, orphan = F, old = F, remove = T)



  clim_resample <- clim_resample %>%
    terra::crop(ro_vect)




  writeRaster(clim_resample,
              "./clim_resample.tif",
              gdal = c("COMPRESS=LZW"), overwrite = T)


  clim_resample <- rast("./clim_resample.tif")



  tmpFiles(current = T, orphan = F, old = F, remove = T)









  clim_resample <- clim_resample %>%
    terra::mask(ro_vect)




  writeRaster(clim_resample,
              "./clim_resample.tif",
              gdal = c("COMPRESS=LZW"), overwrite = T)


  clim_resample <- rast("./clim_resample.tif")



  tmpFiles(current = T, orphan = F, old = F, remove = T)




  r_base <- rast("./covariaveis/prontas/present/dem.tif")

  if (!compareGeom(clim_resample, r_base, stopOnError = F)){

    clim_resample <- resample(clim_resample, r_base, method = "cubicspline")

  }




  if (!file.exists(paste0("./covariaveis/prontas/", n_ldir[i]))) {
    dir.create(paste0("./covariaveis/prontas/", n_ldir[i]))
  }




  for (n in seq_along(names(clim_resample))) {

    t2 <- Sys.time()


    writeRaster(clim_resample[[n]],
                paste0("./covariaveis/prontas/", n_ldir[i], "/",
                names(clim_resample)[n], ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)

    print(paste(names(clim_resample)[n],  round(Sys.time() - t2, 2),
                units(Sys.time() - t2)))


  }

 unlink("D:/Usuario/cassio/R/ro_soil_carbon/clim_resample.tif")

  print(paste(n_ldir[i],  round(Sys.time() - t1, 2),
              units(Sys.time() - t1)))

}










bio <- biovar_terra(prec = prec, tmin = tmin, tmax = tmax)




plot(bio[[12]])
