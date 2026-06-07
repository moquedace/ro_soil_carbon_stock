pkg <- c("dplyr", "sf", "terra", "beepr", "tmap", "tmaptools", "exactextractr",
         "RColorBrewer")

sapply(pkg, require, character.only = T)

rm(list = ls())


setwd("C:/R/ro_soil_carbon")




ld <- list.dirs(path = "./results_ocs/rf", full.names = T, recursive = F)

ld_n <- list.dirs(path = "./results_ocs/rf", full.names = F, recursive = F)





for (i in seq_along(ld)) {


  rst_126 <- list.files(path = paste0(ld[i], "/raster"), pattern = ".tif$",
                        full.names = T, recursive = T) %>%
    grep(pattern = "_126/", value = T) %>%
    rast() %>%
    app("mean", cores = 15)


  plot(rst_126)



  rst_585 <- list.files(path = paste0(ld[i], "/raster"), pattern = ".tif$",
                        full.names = T, recursive = T) %>%
    grep(pattern = "_585/", value = T) %>%
    rast() %>%
    app("mean", cores = 15)


  plot(rst_585)





  if (!dir.exists(paste0(ld[i], "/raster_means"))) {
    dir.create(paste0(ld[i], "/raster_means"))
  }






  terra::writeRaster(rst_126,
                     filename = paste0(
                       ld[i], "/raster_means/", ld_n[i], "_2081_2100_126.tif"),
                     gdal = c("COMPRESS=LZW"), overwrite = T)



  terra::writeRaster(rst_585,
                     filename = paste0(
                       ld[i], "/raster_means/", ld_n[i], "_2081_2100_585.tif"),
                     gdal = c("COMPRESS=LZW"), overwrite = T)



}





lp <- list.files("results_ocs/rf", pattern = ".tif$", recursive = T,
                 full.names = T) %>%
  grep(pattern = "present", value = T) %>%
  rast() %>%
  app("sum", cores = 15) * 10


  rast("results_ocs/rf/OCS_0_5/raster/present/OCS_0_5_1.tif")

lf <- list.files("results_ocs/rf", pattern = ".tif$", recursive = T,
                 full.names = T) %>%
  grep(pattern = "raster_means", value = T) %>%
  grep(pattern = "585.", value = T) %>%
  rast() %>%
  app("sum", cores = 15) * 10




ll <- lf-lp

sum(terra::values(lp), na.rm = T)
sum(terra::values(lf), na.rm = T) - sum(terra::values(lp), na.rm = T)


plot(ll)

tm_shape(ll) +
  tm_raster(midpoint = 0,
            palette = colorRampPalette(brewer.pal(11, "Spectral"))(21),
            legend.reverse = T,
            style = "fixed", breaks = c(-Inf, seq(-20, 0,
                                                  length.out = 10),
                                        seq(0, 20, length.out = 10), +Inf))
