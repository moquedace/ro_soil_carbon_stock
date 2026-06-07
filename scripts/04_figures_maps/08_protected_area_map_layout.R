pkg <- c("geobr", "sf", "terra", "dplyr", "stringr", "tmap", "tmaptools",
         "readxl", "ggplot2",  "RColorBrewer", "ggridges", "scales", "tidyr")

sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("E:/bkp_cassio/R/ro_soil_carbon")

options(scipen = 999)




ro <- read_state(code_state = "RO") %>%
  st_transform("ESRI:102015")




uc <- read_conservation_units(showProgress = F) %>%
  st_transform("ESRI:102015") %>%
  st_intersection(ro) %>%
  dplyr::select("category") %>%
  mutate(category = recode(category,
                           `Reserva Extrativista` = "Extractive reserve",
                           `Reserva Particular do Patrimônio Natural` = "PNHR",
                           `Floresta` = "Public forest",
                           `Parque` = "Park",
                           `Reserva Biológica` = "Biological reserve",
                           `Monumento Natural` = "Natural monument",
                           `Área de Proteção Ambiental` = "EPA",
                           `Estação Ecológica` = "Ecological station"))



ti <- read_indigenous_land(showProgress = F) %>%
  st_transform("ESRI:102015") %>%
  st_intersection(ro) %>%
  st_combine() %>% st_union() %>% st_as_sf() %>%
  mutate(category = "Indigenous land") %>%
  rename("geom" = x)



tm_shape(uc) +
  tm_polygons(col = "category")


tm_shape(ti) +
  tm_polygons(col = "category")








map_uc <- tm_shape(ro) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_polygons(col = "grey95") +
  tm_shape(uc) +
  tm_polygons(col = "category", palette = "Pastel1", border.col = "black") +

  tm_legend(legend.show = F,
            legend.format =
              list(text.separator = "-",
                   decimal.mark = ".",
                   digits = 0,
                   big.mark = ","),
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
  tm_borders(lwd = 2, col = "black") ; map_uc


tmap_save(tm = map_uc,
          filename = paste0("./fig/",
                            "map_uc", ".jpg"),
          dpi = 600, width = 19, units = "in",
          outer.margins = c(0.001, 0.001, 0.001, 0.001))






leg_uc <- tm_shape(ro) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_polygons(col = "grey95") +
  tm_shape(uc) +
  tm_polygons(col = "category", palette = "Pastel1", border.col = "black") +

  tm_legend(legend.only = T,
            legend.format =
              list(text.separator = "-",
                   decimal.mark = ".",
                   digits = 0,
                   big.mark = ","),
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
  tm_borders(lwd = 2, col = "black") ; leg_uc


tmap_save(tm = leg_uc,
          filename = paste0("./fig/",
                            "leg_uc", ".jpg"),
          dpi = 600, width = 19, units = "in",
          outer.margins = c(0.001, 0.001, 0.001, 0.001))



















map_ti <- tm_shape(ro) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_polygons(col = "grey95") +
  tm_shape(ti) +
  tm_polygons(col = "category", palette = "darkgreen", border.col = "transparent") +

  tm_legend(legend.show = F,
            legend.format =
              list(text.separator = "-",
                   decimal.mark = ".",
                   digits = 0,
                   big.mark = ","),
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
  tm_borders(lwd = 2, col = "black") ; map_ti


tmap_save(tm = map_ti,
          filename = paste0("./fig/",
                            "map_ti", ".jpg"),
          dpi = 600, width = 19, units = "in",
          outer.margins = c(0.001, 0.001, 0.001, 0.001))






leg_ti <- tm_shape(ro) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_polygons(col = "grey95") +
  tm_shape(ti) +
  tm_polygons(col = "category", palette = "darkgreen", border.col = "transparent") +


  tm_legend(legend.only = T,
            legend.format =
              list(text.separator = "-",
                   decimal.mark = ".",
                   digits = 0,
                   big.mark = ","),
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
  tm_borders(lwd = 2, col = "black") ; leg_ti


tmap_save(tm = leg_ti,
          filename = paste0("./fig/",
                            "leg_ti", ".jpg"),
          dpi = 600, width = 19, units = "in",
          outer.margins = c(0.001, 0.001, 0.001, 0.001))
