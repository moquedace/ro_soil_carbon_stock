


pkg <- c("dplyr", "ggplot2", "stringr", "tidyr", "terra", "data.table")


sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("E:/bkp_cassio/R/ro_soil_carbon")


cd <- 1:100

i = 1
lr <- list()

tf <- Sys.time()
for (i in seq_along(cd)) {


  t1 <- Sys.time()


  lr[[i]] <-  list.files(path = "./results_ocs_100/rf",
                         pattern = paste0("_", cd[i], ".tif$"),
                         full.names = T,
                         recursive = T) %>%
    rast() %>%
    app("sum", cores = 15)

  plot(lr[[i]])



  df <- data.frame(raster = paste0("OCS_0_100_", i),
                   stock_mg = sum(values(lr[[i]], na.rm = T), na.rm = T) *
                     (prod(res(lr[[i]])) / 10000)) %>%
    mutate(stock_tg = stock_mg / 1000000)


  if (i == 1) {

    dfg <- df

  } else {

    dfg <- rbind(dfg, df)

  }



  print(paste("raster", i, round(Sys.time() - t1, 2), units(Sys.time() - t1)))
  gc()

}

paste("time full", round(Sys.time() - tf, 2), units(Sys.time() - tf))

write.csv2(dfg, file = "./sheet/stock_sum_0_100_runs_100.csv",
           row.names = F)


df_summary <- dfg %>%
  summarise_at(.vars = vars(stock_mg, stock_tg),
               .funs = list("mean" = mean, "sd" = sd))





for (i in seq_along(lr)) {

  t1 <- Sys.time()

  rst <- lr[[i]] %>%
    `names<-`(paste0("OCS_0_100_", i))


  writeRaster(rst, gdal = "COMPRESS=LZW", overwrite = T,
              filename = paste0("./results_ocs_100/rf/OCS_0_100/raster/",
                                names(rst), ".tif"))

  print(paste("write raster", i, round(Sys.time() - t1, 2), units(Sys.time() - t1)))
  gc()

}
