



pkg <- c("dplyr", "ggplot2", "stringr", "tidyr", "RColorBrewer", "data.table",
         "terra")


sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("D:/Usuario/cassio/R/ro_soil_carbon/")






# mean and cv -------------------------------------------------------------


lp <- list.dirs(path = "./results_ocs_100/rf",
                full.names = T, recursive = F)[-1]


lpn <- list.dirs(path = "./results_ocs_100/rf",
                 full.names = F, recursive = F)[-1]




lper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = T,
                  recursive = F)

nlper <- list.dirs(path = "./results_ocs_100/rf/OCS_0_5/raster", full.names = F,
                   recursive = F)





r_base <- rast("./covariaveis/prontas/present/dem.tif")





qquant <- terra::quantile








qquant <- new("MethodDefinition", .Data = function (x, ...)
{
  .local <- function (x, probs = c(0.05, 0.95), na.rm = TRUE,
                      filename = "", ...)
  {
    opt <- spatOptions(filename, ...)
    x@ptr <- x@ptr$quantile(probs, na.rm[1], opt)
    messages(x, "quantile")
  }
  .local(x, ...)
}, target = new("signature", .Data = "SpatRaster", names = "x",
                package = "terra"), defined = new("signature", .Data = "SpatRaster",
                                                  names = "x", package = "terra"), generic = "quantile")







for (n in seq_along(nlper)) {




  for (i in seq_along(lp)) {

    t <- Sys.time()


    rstq <- list.files(path = paste0(lp[i], "/raster/", nlper[n]),
                       pattern = ".tif$", full.names = T) %>%
      rast() %>%
      app(qquant, cores = 11)
      `names<-`(c(paste0(lpn[i], "_q05"), paste0(lpn[i], "_q95")))

    plot(rstq, main = names(rstq))







    if (!dir.exists(paste0(lp[i], "/raster_summary"))) {
      dir.create(paste0(lp[i], "/raster_summary"))
    }


    if (!dir.exists(paste0(lp[i], "/raster_summary/", nlper[n]))) {
      dir.create(paste0(lp[i], "/raster_summary/",  nlper[n]))
    }







    if (!compareGeom(rstq, r_base, stopOnError = F)){

      rstq <- resample(rstq, r_base, method = "cubicspline")


      writeRaster(rstq[[1]],
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstq)[1], ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)

      writeRaster(rstq[[2]],
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstq)[2], ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)

    } else {

      writeRaster(rstq[1],
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstq)[1], ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)


      writeRaster(rstq[2],
                  filename = paste0(lp[i], "/raster_summary/", nlper[n], "/",
                                    names(rstq)[2], ".tif"),
                  gdal = c("COMPRESS=LZW"), overwrite = T)




    }






    tmpFiles(current = T, orphan = F, old = F, remove = T)

    print(Sys.time() - t)

  }




}








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
