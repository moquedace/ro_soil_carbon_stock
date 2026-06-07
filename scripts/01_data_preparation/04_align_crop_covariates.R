pkg <- c("geobr", "dplyr", "sf", "terra", "beepr", "beepr", "mdsFuncs",
         "tmap", "tmaptools")

sapply(pkg, require, character.only = T)

rm(list = ls())



setwd("C:/R/ro_soil_carbon")


ro <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  vect()


ro_sf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015")


# aling morfo -------------------------------------------------------------


dem <- rast("./covariaveis/original/dem/mosaico/dem.tif")







morfo <- list.files(path = "./covariaveis/original/dem/morfo", pattern = ".tif$",
                    full.names = T) %>%
  rast()


morfo_res <- resample(morfo, dem, method = "near")



for (i in seq_along(names(morfo_res))) {

  print(names(morfo_res)[i])
  plot(morfo_res[[i]])

  writeRaster(morfo_res[[i]],
              filename = paste0("./covariaveis/original/dem/morfo_res/",
                                names(morfo_res)[i], ".tif"),
              overwrite = T, gdal = c("COMPRESS=LZW"))

}

beep(2)



# crop mask ---------------------------------------------------------------



pred_bf <- list.files(path = "./covariaveis/bf", pattern = ".tif$",
                      full.names = T) %>%
  c(list.files(path = "./covariaveis/original/dem/morfo_res", pattern = ".tif$",
               full.names = T)) %>%
  .[58:116] %>%
  rast()



tmpFiles(current = T, orphan = F, old = F, remove = T)

pred_mask <- pred_bf %>%
  crop(ro) %>%
  mask(ro)




for (i in seq_along(names(pred_mask))) {

  print(names(pred_mask)[i])

  writeRaster(pred_mask[[i]],
              filename = paste0("./covariaveis/prontas/present/",
                                names(pred_mask)[i], ".tif"),
              overwrite = T, gdal = c("COMPRESS=LZW"))

}


# teste

rst <- list.files(path = "./covariaveis/prontas/present", pattern = ".tif$",
                  full.names = T) %>%
  rast()




for (i in seq_along(names(rst))) {

  plot(rst[[i]])
  plot(ro, add = T)

}





# check factor ------------------------------------------------------------

dd <- mdsFuncs::unique_value_raster(rst[[1]])

write.csv2(dd, file = "./sheet/unique_values_pred2.csv", row.names = F)

dd <- list()
for (i in seq_along(names(rst))) {

  dd[[i]] <- mdsFuncs::unique_value_raster(rst[[i]])
  print(dd[[i]])

}







# reclas ------------------------------------------------------------------


curvature_classification
landforms_tpi_based
surface_specific_points
terrain_surface_classification_iwahashi
hill_idx
valley_idx



terra::subset(rst, "basin") %>%
  plot(type = "classes")



rcl <- matrix(c(-Inf, 0.99, 0,
                0.9, +Inf, 1), ncol = 3, byrow = T)



test <- terra::subset(rst, "slope_idx") %>%
  terra::classify(rcl)



terra::plot(test)




writeRaster(test,
            filename = paste0("./covariaveis/prontas/present/",
                              names(test), ".tif"),
            overwrite = T, gdal = c("COMPRESS=LZW"))
