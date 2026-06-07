pkg <- c("dplyr", "caret", "randomForest", "e1071", "ggplot2", "doParallel",
         "tidyr", "stringr", "parallelly", "quantregForest", "parallel",
         "terra", "data.table")

sapply(pkg, require, character.only = T)

rm(list = ls())  # clean memory
gc()









# folder
# path_raiz <- "/storage1/dados/es101563/ro_soil_carbon/"
# path_results <- "/storage1/dados/es101563/ro_soil_carbon/results_ocs/"

path_raiz <- "E:/bkp_cassio/R/ro_soil_carbon/"
path_results <- "E:/bkp_cassio/R/ro_soil_carbon/results_ocs_100/"


setwd(path_raiz)



lmodels <- list.dirs(path_results, recursive = F, full.names = T)

nlmodels <- list.dirs(path_results, recursive = F, full.names = F)




lp <- list.dirs(paste0(path_raiz, "covariaveis/prontas"),
                full.names = T, recursive = F) %>% sort(decreasing = T)
ln <- list.dirs(paste0(path_raiz, "covariaveis/prontas"),
                full.names = F, recursive = F) %>% sort(decreasing = T)




zz = 5
hh = 2
nn = 1
m = 57
yy = 3

for (zz in seq_along(lmodels)) {

  limg <- list.files(path = paste0(lmodels[zz]), pattern = ".RData$",
                     full.names = T, recursive = T)


  for(hh in seq_along(limg)) {

    load(limg[hh])

    # folder
    path_raiz <- "E:/bkp_cassio/R/ro_soil_carbon/"
    path_results <- "E:/bkp_cassio/R/ro_soil_carbon/results_ocs_100/"


    # path_raiz <- "/storage1/dados/es101563/ro_soil_carbon/"
    # path_results <- "/storage1/dados/es101563/ro_soil_carbon/results_ocs/"

    tvar <- Sys.time()
    for (nn in seq_along(lp)[1]) {

      stk_cova <- list.files(lp[nn], full.names = T) %>% rast()



      if (!file.exists(paste0(path_raiz, "tiles/", ln[nn]))) {

        dir.create(paste0(path_raiz, "tiles/", ln[nn]))

        r_tile <- rast(ncols = 10, nrows = 10, nlyrs = nlyr(stk_cova),
                       crs = "ESRI:102015", extent = ext(stk_cova))

        stk_tile <- terra::makeTiles(stk_cova, y = r_tile,
                                     filename = paste0(
                                       path_raiz, "tiles/", ln[nn], "/tile_.tif"),
                                     gdal = c("COMPRESS=LZW"), overwrite = T)
      } else {

        stk_tile <- list.files(path = paste0(path_raiz, "tiles/", ln[nn]),
                               full.names = T, pattern = ".tif$")
      }


      for (m in seq_along(lmodel)[57:100]) {

        tmodel <- Sys.time()

        for (yy in seq_along(stk_tile)) {

          t_tile_frag <- Sys.time()
          rst_tiles_frag <- rast(stk_tile[yy]) %>%
            subset(lrfepred[[m]])

          df_stk_pp <- terra::as.data.frame(rst_tiles_frag, xy = T,
                                            na.rm = T)



          if (grepl(x = paste(lrfepred[[m]], collapse = " "),
                    pattern = paste(varfact, collapse = "|"))) {

            df_stk_pp <- df_stk_pp %>%
              mutate_at(.vars = vars(contains(varfact)), as.factor)

          }




          if (nrow(df_stk_pp) != 0) {

            pred_frag <- predict(lmodel[[m]], df_stk_pp[, -c(1, 2)]) %>%
              cbind(df_stk_pp[, c(1, 2)], .)

            if (yy == 1) {

              raster_pred <- pred_frag

            } else {
              if (exists("raster_pred")) {

                raster_pred <- rbind(raster_pred, pred_frag)

              } else {

                raster_pred <- pred_frag

              }

              gc()
              tile_frag <- paste(ln[nn], var, "model", nlmodels[zz], m, "tile",
                                 yy, "faltando",
                                 length(stk_tile) - yy,
                                 round(Sys.time() - t_tile_frag, 2),
                                 units(Sys.time() - t_tile_frag))
              write.table(x = tile_frag, file = paste0(
                path_raiz, "tile_frag_", ln[nn], ".txt"),
                col.names = F, row.names = F)
              print(tile_frag)

            }
          }
        }


        names(raster_pred)[3] <- var

        raster_pred <- raster_pred %>%
          terra::rast(type = "xyz", crs = "ESRI:102015")

        plot(raster_pred)


        if (!file.exists(paste0(path_results, nlmodels[zz], "/",
                                var, "/raster"))) {

          dir.create(paste0(path_results, nlmodels[zz], "/",
                            var, "/raster"))

        }




        if (!file.exists(paste0(path_results, nlmodels[zz], "/",
                                var, "/raster/", ln[nn]))) {

          dir.create(paste0(path_results, nlmodels[zz], "/",
                            var, "/raster/", ln[nn]))

        }



        r_base <- rast(paste0(path_raiz, "covariaveis/prontas/present/dem.tif"))

        if (!compareGeom(raster_pred, r_base, stopOnError = F)){

          raster_pred <- resample(raster_pred, r_base, method = "cubicspline")


          terra::writeRaster(raster_pred, overwrite = T,
                             gdal = c("COMPRESS=LZW"),
                             filename = paste0(path_results, nlmodels[zz], "/",
                                               var, "/raster/", ln[nn], "/",
                                               names(raster_pred), "_", m,
                                               ".tif"))

        } else {

          terra::writeRaster(raster_pred, overwrite = T,
                             gdal = c("COMPRESS=LZW"),
                             filename = paste0(path_results, nlmodels[zz], "/",
                                               var, "/raster/", ln[nn], "/",
                                               names(raster_pred), "_", m,
                                               ".tif"))

        }




        s_tmodel <- paste(ln[nn], var, "model", nlmodels[zz], m, round(Sys.time() - tmodel, 2),
                          units(Sys.time() - tmodel))
        print(s_tmodel)
        write.table(x = s_tmodel, file = paste0(
          path_raiz, "s_tmodel_", ln[nn], ".txt"),
          col.names = F, row.names = F)

        plot(raster_pred)
        rm(raster_pred)
        tmpFiles(current = T, orphan = F, old = F, remove = T)


      }

    }

    s_tvar <- paste(ln[nn], var, "model", nlmodels[zz], "full", round(Sys.time() - tvar, 2),
                    units(Sys.time() - tvar))
    print(s_tvar)
    write.table(x = s_tvar, file = paste0(
      path_raiz, "s_tvar_", ln[nn], ".txt"),
      col.names = F, row.names = F)


  }
}
