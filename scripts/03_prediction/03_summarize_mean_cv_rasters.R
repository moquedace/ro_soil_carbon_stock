



pkg <- c("dplyr", "ggplot2", "stringr", "tidyr", "RColorBrewer", "data.table",
         "terra")


sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("D:/Usuario/cassio/R/ro_soil_carbon/")






# mean and cv -------------------------------------------------------------


lp <- list.dirs(path = "./results_ocs_100/rf",
                full.names = T, recursive = F)


lpn <- list.dirs(path = "./results_ocs_100/rf",
                 full.names = F, recursive = F)




lper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = T,
                  recursive = F)

nlper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = F,
                   recursive = F)





r_base <- rast("./covariaveis/prontas/present/dem.tif")




for (n in seq_along(nlper)) {




  for (i in seq_along(lp)) {

    t <- Sys.time()


    rstm <- list.files(path = paste0(lp[i], "/raster/", nlper[n]),
                       pattern = ".tif$", full.names = T) %>%
      rast() %>%
      app("mean", cores = 11) %>%
      `names<-`(paste0(lpn[i], "_mean"))

    plot(rstm, main = names(rstm))




    rstsd <- list.files(path = paste0(lp[i], "/raster/", nlper[n]),
                        pattern = ".tif$", full.names = T) %>%
      rast() %>%
      app("sd", cores = 11) %>%
      `names<-`(paste0(lpn[i], "_sd"))



    rstcv <- rstsd / rstm * 100

    names(rstcv) <- paste0(lpn[i], "_cv")

    plot(rstcv, main = names(rstcv))






    if (!dir.exists(paste0(lp[i], "/raster_summary"))) {
      dir.create(paste0(lp[i], "/raster_summary"))
    }


    if (!dir.exists(paste0(lp[i], "/raster_summary/", nlper[n]))) {
      dir.create(paste0(lp[i], "/raster_summary/",  nlper[n]))
    }







    if (!compareGeom(rstm, r_base, stopOnError = F)){

      rstm <- resample(rstm, r_base, method = "cubicspline")


      writeRaster(rstm,
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstm), ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)

    } else {

      writeRaster(rstm,
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstm), ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)


    }











    if (!compareGeom(rstcv, r_base, stopOnError = F)){

      rstm <- resample(rstm, r_base, method = "cubicspline")


      writeRaster(rstcv,
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstcv), ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)

    } else {

      writeRaster(rstcv,
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstcv), ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)


    }



    tmpFiles(current = T, orphan = F, old = F, remove = T)

    print(Sys.time() - t)

  }




}









lr_m <- list.files(path = "./results_ocs_100/rf/OCS_0_5/raster/present",
                   pattern = ".tif$", full.names = T) %>%
  rast() %>%
  app("mean", cores = 11)




lr_sd <- list.files(path = "./results_ocs_100/rf/OCS_0_5/raster/present",
                    pattern = ".tif$", full.names = T) %>%
  rast() %>%
  app("sd", cores = 11)



cv <- lr_sd / lr_m * 100




plot(lr_m)

plot(lr_sd)

plot(cv)






# calc 0-100 --------------------------------------------------------------



lp <- list.dirs(path = "./results_ocs_100/rf",
                full.names = T, recursive = F)


lpn <- list.dirs(path = "./results_ocs_100/rf",
                 full.names = F, recursive = F)




lper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = T,
                  recursive = F)

nlper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = F,
                   recursive = F)





r_base <- rast("./covariaveis/prontas/present/dem.tif")




for (n in seq_along(nlper)) {


  t <- Sys.time()


  rstm <- list.files(path = paste0("./results_ocs_100/rf"),
                     pattern = ".tif$", full.names = T, recursive = T) %>%
    grep(pattern = nlper[n], value = T) %>%
    grep(pattern = "mean", value = T) %>%
    rast() %>%
    app("sum", cores = 11) %>%
    `names<-`("OCS_0_100_mean")

  plot(rstm, main = names(rstm))




  rstcv <- list.files(path = paste0("./results_ocs_100/rf"),
                      pattern = ".tif$", full.names = T, recursive = T) %>%
    grep(pattern = nlper[n], value = T) %>%
    grep(pattern = "cv", value = T) %>%
    rast() %>%
    app("mean", cores = 11) %>%
    `names<-`("OCS_0_100_cv")



  plot(rstcv, main = names(rstcv))






  if (!dir.exists("./results_ocs_100/rf/OCS_0_100")) {
    dir.create("./results_ocs_100/rf/OCS_0_100")
  }




  if (!dir.exists("./results_ocs_100/rf/OCS_0_100/raster_summary")) {
    dir.create("./results_ocs_100/rf/OCS_0_100/raster_summary")
  }


  if (!dir.exists(paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/", nlper[n]))) {
    dir.create(paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",  nlper[n]))
  }







  if (!compareGeom(rstm, r_base, stopOnError = F)) {

    rstm <- resample(rstm, r_base, method = "cubicspline")


    writeRaster(rstm,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstm), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)

  } else {

    writeRaster(rstm,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstm), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)


  }











  if (!compareGeom(rstcv, r_base, stopOnError = F)) {

    rstm <- resample(rstm, r_base, method = "cubicspline")


    writeRaster(rstcv,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstcv), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)

  } else {

    writeRaster(rstcv,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstcv), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)


  }



  #  tmpFiles(current = T, orphan = F, old = F, remove = T)

  print(Sys.time() - t)

}












# quantile -------------------------------------------------------------


lp <- list.dirs(path = "./results_ocs_100/rf",
                full.names = T, recursive = F)[2:6]


lpn <- list.dirs(path = "./results_ocs_100/rf",
                 full.names = F, recursive = F)[2:6]




lper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = T,
                  recursive = F)

nlper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = F,
                   recursive = F)





r_base <- rast("./covariaveis/prontas/present/dem.tif")


n = 1
i = 2



source("D:/Usuario/cassio/R/outros/scripts_aleatorios/s_tiles.R")






for (n in seq_along(nlper)) {


  for (i in seq_along(lp)[2:5]) {

    t <- Sys.time()


    rstm <- list.files(path = paste0(lp[i], "/raster/", nlper[n]),
                       pattern = ".tif$", full.names = T) %>%
      rast()



    if (!dir.exists(paste0(lp[i], "/tiles"))) {
      dir.create(paste0(lp[i], "/tiles"))
    }


    if (!dir.exists(paste0(lp[i], "/tiles/", nlper[n]))) {
      dir.create(paste0(lp[i], "/tiles/",  nlper[n]))
    }

   tiles <-  rst_tile(rst = rstm, nc = 2, nr = 10, coord = NULL,
             exte = NULL, nl = NULL,
             file = paste0(lp[i], "/tiles/", nlper[n], "/",
                           "tile_.tif"))

    tmpFiles(current = T, orphan = F, old = F, remove = T)



    if (!dir.exists(paste0(lp[i], "/tiles_q"))) {
      dir.create(paste0(lp[i], "/tiles_q"))
    }


    if (!dir.exists(paste0(lp[i], "/tiles_q/", nlper[n]))) {
      dir.create(paste0(lp[i], "/tiles_q/",  nlper[n]))
    }


    for (m in seq_along(tiles)) {



      tile_q <- terra::quantile(rast(tiles[m]), probs = c(0.05, 0.95))




      writeRaster(tile_q,
                  filename = paste0(lp[i], "/tiles_q/",  nlper[n],
                                    "/quant_", m, ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)

      tmpFiles(current = T, orphan = F, old = F, remove = T)



    }


    unlink(tiles)




    lrq <- list.files(path = paste0(lp[i], "/tiles_q/",  nlper[n]),
                      pattern = ".tif$", full.names = T) %>%
      vrt() %>%
      `names<-`(c(paste0(lpn[i], "_q05"), paste0(lpn[i], "_q95")))









    if (!compareGeom(lrq, r_base, stopOnError = F)) {

      lrq <- resample(lrq, r_base, method = "cubicspline")

    }








    writeRaster(lrq[[1]],
                filename = paste0(lp[i], "/raster_summary/",  nlper[n],
                                  "/", names(lrq)[1], ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)



    writeRaster(lrq[[2]],
                filename = paste0(lp[i], "/raster_summary/",  nlper[n],
                                  "/", names(lrq)[2], ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)



    unlink(list.files(path = paste0(lp[i], "/tiles_q/",  nlper[n]),
                      pattern = ".tif$", full.names = T))



    plot(lrq)






    tmpFiles(current = T, orphan = F, old = F, remove = T)



    print(Sys.time() - t)

  }

}



































# calc 0-100 --------------------------------------------------------------



lp <- list.dirs(path = "./results_ocs_100/rf",
                full.names = T, recursive = F)[2:6]


lpn <- list.dirs(path = "./results_ocs_100/rf",
                 full.names = F, recursive = F)[2:6]




lper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = T,
                  recursive = F)

nlper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = F,
                   recursive = F)





r_base <- rast("./covariaveis/prontas/present/dem.tif")




for (n in seq_along(nlper)) {


  t <- Sys.time()


  rstm <- list.files(path = paste0("./results_ocs_100/rf"),
                     pattern = ".tif$", full.names = T, recursive = T) %>%
    grep(pattern = nlper[n], value = T) %>%
    grep(pattern = "q05", value = T) %>%
    rast() %>%
    app("sum", cores = 11) %>%
    `names<-`("OCS_0_100_q05")

  plot(rstm, main = names(rstm))





  rstm95 <- list.files(path = paste0("./results_ocs_100/rf"),
                     pattern = ".tif$", full.names = T, recursive = T) %>%
    grep(pattern = nlper[n], value = T) %>%
    grep(pattern = "q95", value = T) %>%
    rast() %>%
    app("sum", cores = 11) %>%
    `names<-`("OCS_0_100_q95")

  plot(rstm95, main = names(rstm))








  if (!dir.exists("./results_ocs_100/rf/OCS_0_100")) {
    dir.create("./results_ocs_100/rf/OCS_0_100")
  }




  if (!dir.exists("./results_ocs_100/rf/OCS_0_100/raster_summary")) {
    dir.create("./results_ocs_100/rf/OCS_0_100/raster_summary")
  }


  if (!dir.exists(paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/", nlper[n]))) {
    dir.create(paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",  nlper[n]))
  }







  if (!compareGeom(rstm, r_base, stopOnError = F)) {

    rstm <- resample(rstm, r_base, method = "cubicspline")


    writeRaster(rstm,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstm), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)

  } else {

    writeRaster(rstm,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstm), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)


  }











  if (!compareGeom(rstm95, r_base, stopOnError = F)) {

    rstm95 <- resample(rstm95, r_base, method = "cubicspline")


    writeRaster(rstm95,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstm95), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)

  } else {

    writeRaster(rstm95,
                filename = paste0("./results_ocs_100/rf/OCS_0_100/raster_summary/",
                                  nlper[n], "/",
                                  names(rstm95), ".tif"),
                gdal = c("COMPRESS=LZW"), overwrite = T)


  }



  #  tmpFiles(current = T, orphan = F, old = F, remove = T)

  print(Sys.time() - t)

}
