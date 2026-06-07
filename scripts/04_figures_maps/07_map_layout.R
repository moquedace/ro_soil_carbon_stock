pkg <- c("tmap", "tmaptools", "dplyr", "terra", "geobr", "sf",
         "RColorBrewer", "grDevices", "stringr")


sapply(pkg, require, character.only = T)


rm(list = ls())


setwd("C:/R/ro_soil_carbon")



ro <- read_state(code_state = "RO", year = 2018) %>%
  st_transform("ESRI:102015")



lr <- list.files(path = "./results_ocs/rf", recursive = T, pattern = ".tif$",
                 full.names = T) %>%
  grep(pattern = "present", value = T) %>%
  rast() %>%
  app("sum", cores = 15)






i =1






map[[i]] <- tm_shape(lr[[i]], raster.downsample = T) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_raster(style = "fixed",
            breaks = c(-Inf, seq(5, 30, length.out = 10), +Inf),
            legend.hist = F,
           palette = if(grepl("sum", names(lr)[i])){
              colorRampPalette(brewer.pal(11, "Spectral"))(11)
            } else {
              rev(colorRampPalette(brewer.pal(11, "Spectral"))(11))
            },
            title = names(lr)[i], legend.reverse = T) +
  tm_legend(legend.format = if(grepl("OCS", names(lr)[i])){
    list(text.separator = "-",
         decimal.mark = ".",
         digits = 1,
         big.mark = ",")
  } else {
    list(text.separator = "-",
         decimal.mark = ".",
         digits = 0,
         big.mark = ",")
  },
  legend.position = c("left", "bottom"),
  legend.stack = "vertical",
  legend.just = "center",
  legend.outside = F,
  legend.text.size = 2.8,
  legend.title.size = 3.2,
  legend.frame = T) +
  tm_layout(inner.margins = c(0.01, 0.05, 0.01, 0.05),
            frame.lwd = 2) +
  # tm_compass(type = "rose", position = c("right", "top"), size = 3,
  #            text.size = 1.5) +
  # tm_scale_bar(position = c("left", "top"), text.size = 1) +
  tm_shape(ro) +
  tm_borders(lwd = 2, col = "black")





tmap_save(tm = map[[i]],
          filename = paste0("./", names(lr)[i], ".jpg"),
          dpi = 600, width = 19, units = "in",
          outer.margins = c(0.001, 0.001, 0.001, 0.001))



















map <- list()
for (i in seq_along(und)) {

  map[[i]] <- tm_shape(lr[[i]], raster.downsample = T) +
    tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                  labels.size = 2.7, labels.inside.frame = T, col = "black",
                  labels.col = "black") +
    tm_raster(style = "fisher", legend.hist = F,
              n = 13,
              palette = if(grepl("OCS", names(lr)[i])){
                rev(colorRampPalette(brewer.pal(11, "Spectral"))(13))
              } else {
                colorRampPalette(brewer.pal(11, "Spectral"))(13)
              },
              title = names(lr)[i], legend.reverse = T) +
    tm_legend(legend.format = if(grepl("OCS", names(lr)[i])){
      list(text.separator = "-",
           decimal.mark = ".",
           digits = 1,
           big.mark = ",")
    } else {
      list(text.separator = "-",
           decimal.mark = ".",
           digits = 0,
           big.mark = ",")
    },
    legend.position = c("left", "bottom"),
    legend.stack = "vertical",
    legend.just = "center",
    legend.outside = F,
    legend.text.size = 2.8,
    legend.title.size = 3.2,
    legend.frame = T) +
    tm_layout(inner.margins = c(0.01, 0.05, 0.01, 0.05),
              frame.lwd = 2) +
    # tm_compass(type = "rose", position = c("right", "top"), size = 3,
    #            text.size = 1.5) +
    # tm_scale_bar(position = c("left", "top"), text.size = 1) +
    tm_shape(ro) +
    tm_borders(lwd = 2, col = "black")


  tmap_save(tm = map[[i]],
            filename = paste0("C:/R/pedometrics_brazil_2021/cassio/",
                              names(stk_0_100[[i]]), ".jpg"),
            dpi = 600, width = 19, units = "in",
            outer.margins = c(0.001, 0.001, 0.001, 0.001))



}

map[[i]] <- tm_shape(stk_0_100[[i]], raster.downsample = T) +
  tm_graticules(n.y = 5, n.x = 3, lines = F, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_raster(style = "fisher", legend.hist = F,
            n = 13,
            palette = if(grepl("%", und[i])){
              rev(colorRampPalette(brewer.pal(11, "Spectral"))(13))
            } else {
              colorRampPalette(brewer.pal(11, "Spectral"))(13)
            },
            title = und[i], legend.reverse = T) +
  tm_legend(legend.format = if(grepl("%", und[i])){
    list(text.separator = "-",
         decimal.mark = ".",
         digits = 1,
         big.mark = ",")
  } else {
    list(text.separator = "-",
         decimal.mark = ".",
         digits = 0,
         big.mark = ",")
  },
  legend.position = c("left", "bottom"),
  legend.stack = "vertical",
  legend.just = "center",
  legend.outside = F,
  legend.text.size = 2.8,
  legend.title.size = 3.2,
  legend.frame = T) +
  tm_layout(inner.margins = c(0.01, 0.05, 0.01, 0.05),
            frame.lwd = 2) +
  # tm_compass(type = "rose", position = c("right", "top"), size = 3,
  #            text.size = 1.5) +
  tm_scale_bar(position = c("right", "top"), text.size = 3) +
  tm_shape(ro) +
  tm_borders(lwd = 2, col = "black")


tmap_save(tm = map[[i]],
          filename = paste0("C:/R/pedometrics_brazil_2021/cassio/",
                            "escala", ".jpg"),
          dpi = 300, width = 19, units = "in",
          outer.margins = c(0.001, 0.001, 0.001, 0.001))



n_tif <- str_split(lf, "_", simplify = T)[,6] %>% unique() %>%
  gsub(".tif", "", .) %>%
  rep(each = 2)


map_ar <- list()
for (i in seq(1, 24, 3)) {

  map_ar[[i]] <- tmap_arrange(map[[i]], map[[i+1]], map[[i+2]],
                              nrow = 1)


  if(i == 22){

    map_ar <- map_ar[-which(sapply(map_ar, is.null))]

    for (l in seq_along(n_tif)) {

      tmap_save(tm = map_ar[[l]],
                filename = paste0("figuras/", n_tif[l], "_", l,
                                  ".jpg"),
                dpi = 1200, width = 19, height = 5.9375, units = "in",
                outer.margins = c(0.001, 0.001, 0.001, 0.001))

    }
  }
}
