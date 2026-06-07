pkg <- c("tmap", "tmaptools", "dplyr", "terra", "geobr", "sf", "readxl",
         "RColorBrewer", "stringr", "tidyr", "ggplot2", "ggpubr", "caret")


sapply(pkg, require, character.only = T)


rm(list = ls())
setwd("E:/bkp_cassio/R/ro_soil_carbon")

options(scipen = 999)





# prep data ---------------------------------------------------------------


ro <- read_state(code_state = "RO", showProgress = F) %>%
  st_transform("ESRI:102015")


r_base <- rast("./covariaveis/prontas/present/dem.tif")



sgrid <- rast("./comparacao/soilgrids/soilDepth_stock_0_30.tif") %>%
  project(y = "ESRI:102015", method = "near") %>%
  resample(r_base) %>%
  crop(ro) %>%
  mask(ro)



c(sgrid, r_base)


plot(sgrid)

names(sgrid) <- "OCS_0_30_mean_soilgrids"

# writeRaster(sgrid,
#             filename = paste0("./comparacao/soilgrids/",
#                               names(sgrid), ".tif"),
#             overwrite = T, gdal = c("COMPRESS=LZW"))





lucas <- list.files(path = "./comparacao/lucas",
                    pattern = ".tif$", full.names = T) %>%
  rast() %>%
  app("sum", cores = 11)



crs(lucas) <- "ESRI:102015"


lucas <- lucas %>%
  resample(r_base) %>%
  crop(ro) %>%
  mask(ro)


c(lucas, r_base)


plot(lucas)

names(lucas) <- "OCS_0_30_mean_lucas"



lucas <- lucas*10

plot(lucas)




writeRaster(lucas,
            filename = paste0("./comparacao/lucas/",
                              names(lucas), ".tif"),
            overwrite = T, gdal = c("COMPRESS=LZW"))



gustavo <- rast("./comparacao/gustavo/Brazil.SOCstock.0-30cm.t.ha.tif") %>%
  project(y = "ESRI:102015", method = "near") %>%
  resample(r_base) %>%
  crop(ro) %>%
  mask(ro)


c(gustavo, r_base)


plot(gustavo)

names(gustavo) <- "OCS_0_30_mean_gustavo"





writeRaster(gustavo,
            filename = paste0("./comparacao/gustavo/",
                              names(gustavo), ".tif"),
            overwrite = T, gdal = c("COMPRESS=LZW"))





cassio <- list.files(path = "./results_ocs_100/rf", pattern = ".tif$",
                     full.names = T, recursive = T) %>%
  grep(pattern = "raster_summary", value = T) %>%
  grep(pattern = "mean", value = T) %>%
  grep(pattern = "0_5|5_15|15_30", value = T) %>%
  rast() %>% app("sum", cores = 11)








c(cassio, r_base)


plot(cassio)

names(cassio) <- "OCS_0_30_mean_cassio"





writeRaster(cassio,
            filename = paste0("./comparacao/cassio/",
                              names(cassio), ".tif"),
            overwrite = T, gdal = c("COMPRESS=LZW"))















lrc <- list.files(path = "./comparacao", pattern = ".tif$",
                  full.names = T, recursive = T) %>%
  grep(pattern = "OCS", value = T) %>%
  rast()






plot(lrc)





df <- as.data.frame(lrc, na.rm = T) %>% as_tibble() %>%
  mutate(across(everything(.), ~ . * (prod(res(lrc)) / 10000),
                .names = "stk_{col}"))



grp <- df %>%
  dplyr::select(5:8) %>%
  summarise_all(sum)

# RMSE MAE R2 -------------------------------------------------------------

lrc <- list.files(path = "./comparacao", pattern = ".tif$",
                  full.names = T, recursive = T) %>%
  grep(pattern = "OCS_", value = T) %>%
  rast()


spline_sf <- st_read("./shp/spline_ocs.shp") %>%
  drop_na(c(OCS_0_5, OCS_5_15, OCS_15_30)) %>%
  mutate(OCS_0_30 = OCS_0_5 + OCS_5_15 + OCS_15_30) %>%
  dplyr::select(OCS_0_30)

plot(st_geometry(spline_sf))



spline_vect <- st_read("./shp/spline_ocs.shp") %>%
  drop_na(c(OCS_0_5, OCS_5_15, OCS_15_30)) %>%
  mutate(OCS_0_30 = OCS_0_5 + OCS_5_15 + OCS_15_30) %>%
  dplyr::select(OCS_0_30) %>% vect()

plot(spline_vect)


df_p_o <- terra::extract(lrc, spline_vect, ID = F) %>%
  cbind(as.data.frame(spline_sf) %>% select(OCS_0_30), .)



pr <- rbind(postResample(obs = df_p_o$OCS_0_30,
                         pred = df_p_o$OCS_0_30_mean_cassio),
            postResample(obs = df_p_o$OCS_0_30,
                         pred = df_p_o$OCS_0_30_mean_gustavo),
            postResample(obs = df_p_o$OCS_0_30,
                         pred = df_p_o$OCS_0_30_mean_lucas),
            postResample(obs = df_p_o$OCS_0_30,
                         pred = df_p_o$OCS_0_30_mean_soilgrids)) %>%
  data.frame(models = c("cassio", "gustavo", "lucas", "soilgrids")) %>%
  select(-Rsquared)






df_p_o_gg <- df_p_o %>%
  gather(key = "models", value = "predict", -c(OCS_0_30)) %>%
  rename(observed = "OCS_0_30") %>%
  mutate(models = recode(models   ,
                         OCS_0_30_mean_lucas = "Gomes et al. (2019)",
                         OCS_0_30_mean_soilgrids = "SoilGrids (Poggio et al., 2021)",
                         OCS_0_30_mean_gustavo = "Vasques et al. (2017)",
                         OCS_0_30_mean_cassio = "Predicted"),
         models = factor(as.factor(models),
                         levels = c("Predicted", "Vasques et al. (2017)",
                                    "Gomes et al. (2019)",
                                    "SoilGrids (Poggio et al., 2021)")))



df_p_o_box <- df_p_o %>%
  rename(observed = "OCS_0_30") %>%
  gather(key = "models", value = "value") %>%
  mutate(models = recode(models,
                         OCS_0_30_mean_lucas = "Gomes et al. (2019)",
                         OCS_0_30_mean_soilgrids = "SoilGrids (Poggio et al., 2021)",
                         OCS_0_30_mean_gustavo = "Vasques et al. (2017)",
                         OCS_0_30_mean_cassio = "Predicted",
                         observed = "Observed"),
         models = factor(as.factor(models),
                         levels = c("Observed", "Predicted",
                                    "Vasques et al. (2017)",
                                    "Gomes et al. (2019)",
                                    "SoilGrids (Poggio et al., 2021)")))




# fig sep ------------------------------------------------------

comparacao <- ggplot(df_p_o_gg, aes(x = predict, y = observed)) +
  geom_point(aes(size = 0.1), alpha = 0.25) +
 # geom_smooth(method = "lm") +
  scale_size(range = c(0.01, 1)) +
  scale_x_continuous(n.breaks = 10, expand = c(0, 0)) +
  scale_y_continuous(n.breaks = 10, expand = c(0, 0)) +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 1) +
  scale_color_brewer(palette = "Spectral") +
  theme_classic() +
  labs(x = expression(Predicted~SOC~stock~(Mg~ha^-1)),
       y = expression(Observed~SOC~stock~(Mg~ha^-1)), col = NULL) +
  guides(color = guide_legend(override.aes = list(size = 3)),
         size = "none") +
  theme(legend.position = c(0.095, 0.905),
        legend.direction = "vertical",
        legend.margin = margin(0, 2, 8, 2),
        legend.background = element_rect(colour = "black"),
        axis.text = element_text(size = 22, colour = "black"),
        axis.title = element_text(size = 24, colour = "black"),
        strip.text = element_text(size = 26, colour = "black"),
        legend.text = element_text(size = 20, colour = "black"),
        #  legend.title = element_text(size = 19, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_line(linetype = 1),
        panel.border = element_rect(colour = "black",
                                    fill = "transparent")) +
  facet_wrap(~ models, scales = "free_x") ; comparacao


ggsave(comparacao, filename = "./fig/comparacao_pontos.png", dpi = 1200,
       width = 19, height = 10.69, units = "in")


# box + barras ------------------------------------------------------------



pr_geral <- pr %>% mutate(models = recode(models,
                                          lucas = "Gomes et al. (2019)",
                                          soilgrids = "SoilGrids (Poggio et al., 2021)",
                                          gustavo = "Vasques et al. (2017)",
                                          cassio = "Predicted"),
                          models = factor(as.factor(models),
                                          levels = c("Predicted",
                                                     "Gomes et al. (2019)",
                                                     "SoilGrids (Poggio et al., 2021)",
                                                     "Vasques et al. (2017)"))) %>%
  gather(key = "metric", value = "value", -models)


comparacao_bar <- ggplot(pr_geral, aes(x = metric, y = value, fill = models)) +
  geom_col(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = rev(soilpalettes::soil_palette("redox2", 5))[-1]) +
  labs(y = NULL, x = NULL, fill = NULL) +
  scale_y_continuous(n.breaks = 7, expand = c(0, 0, 0.05, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        legend.direction = "vertical",
        axis.text = element_text(size = 25, colour = "black"),
        axis.title = element_text(size = 25, colour = "black"),
        legend.title = element_text(size = 24, colour = "black"),
        legend.text = element_text(size = 24, colour = "black"),
        strip.text = element_text(size = 25, colour = "black", face = "bold"),
        axis.ticks = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.major.y = element_line(linetype = 1),
        panel.border = element_rect(colour = "black", fill = "transparent")) ; comparacao_bar


comparacao_box <- ggplot(df_p_o_box, aes(y = value, x = models,
                                         fill = models, col = models)) +
  geom_violin(alpha = 0.6, position = position_dodge()) +
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.3,
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = rev(soilpalettes::soil_palette("redox2", 5))) +
  scale_color_manual(values = rev(soilpalettes::soil_palette("redox2", 5))) +
  labs(y =  expression(SOC~stock~(Mg~ha^-1)), x = NULL, fill = NULL,
       col = NULL) +
  # scale_fill_brewer(palette = "Spectral") +
  # scale_color_brewer(palette = "Spectral") +
  coord_cartesian(ylim = c(min(df_p_o_gg$observed) - 6,
                           max(df_p_o_gg$observed) + 2)) +
  scale_y_continuous(n.breaks = 7, expand = c(0, 0)) +
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 18)) +
  theme_classic() +
  theme(legend.position = c(0.7, 0.9),
        legend.direction = "horizontal",
        legend.title = element_text(size = 22, colour = "black"),
        legend.text = element_text(size = 22, colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 25, colour = "black"),
        axis.title = element_text(size = 25, colour = "black"),
        strip.text = element_text(size = 25, colour = "black", face = "bold"),
        axis.ticks = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.major.y = element_line(linetype = 1),
        panel.border = element_rect(colour = "black", fill = "transparent")) +
  stat_summary(fun = mean, geom = "point", aes(group = models),
               position = position_dodge(.75),
               color = "black", size = 2, shape = 15) ; comparacao_box

legend <- get_legend(comparacao_box)


comparacao_box <- comparacao_box + theme(legend.position = "none")

blankPlot <- ggplot() + geom_blank(aes(1, 1)) +
  cowplot::theme_nothing()


com_arr <- ggarrange(comparacao_box, comparacao_bar,
                     common.legend = T, widths = c(0.6, 0.4),
                     labels = c("a)", "b)"), label.y = 1,
                     label.x = c(0.1, 0.05),
                     font.label = list(size = 30, color = "black",
                                       face = "plain", family = NULL)) ; com_arr


#com_arr <- ggarrange(comparacao_box, comparacao_bar, common.legend = T)

ggsave(com_arr, filename = "./fig/comparacao_box_barras.png", dpi = 1200,
       width = 19, height = 10.69, units = "in")





# mapas -------------------------------------------------------------------




sgrid <- rast("./comparacao/soilgrids/OCS_0_30_mean_soilgrids.tif")


plot(sgrid)

names(sgrid) <- "OCS_0_30_mean_soilgrids"


lucas <- rast("./comparacao/lucas/OCS_0_30_mean_lucas.tif")


plot(lucas)

names(lucas) <- "OCS_0_30_mean_lucas"




gustavo <- rast("./comparacao/gustavo/OCS_0_30_mean_gustavo.tif")


plot(gustavo)

names(gustavo) <- "OCS_0_30_mean_gustavo"






cassio <-  rast("./comparacao/cassio/OCS_0_30_mean_cassio.tif")


plot(cassio)





lro <- list(cassio, gustavo, lucas, sgrid)






ro <- read_state(code_state = "RO", year = 2020) %>%
  st_transform("ESRI:102015")






i=3

for (i in seq_along(lro)) {

  map <- tm_shape(lro[[i]], raster.downsample = F) +
    tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                  labels.size = 2.7, labels.inside.frame = T, col = "black",
                  labels.col = "black") +
    tm_raster(style = "fixed", legend.hist = F,
              breaks = c(-Inf, seq(41, 224.69, length.out = 13), +Inf),
              palette = colorRampPalette(brewer.pal(11, "Spectral"))(14),
              title = expression((Mg~ha^-1)), legend.reverse = T) +
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
    tm_borders(lwd = 2, col = "black")


  tmap_save(tm = map,
            filename = paste0("./fig/",
                              names(lro[[i]]), ".jpg"),
            dpi = 600, width = 19, units = "in",
            outer.margins = c(0.001, 0.001, 0.001, 0.001))


  gc()
}


# legenda -----------------------------------------------------------------

map <- tm_shape(lro[[1]], raster.downsample = F) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_raster(style = "fixed", legend.hist = F,
            breaks = c(-Inf, seq(41, 224.69, length.out = 13), +Inf),
            palette = colorRampPalette(brewer.pal(11, "Spectral"))(14),
            title = expression((Mg~ha^-1)), legend.reverse = T) +
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
  tm_borders(lwd = 2, col = "black")


tmap_save(tm = map,
          filename = paste0("./fig/",
                            names(lro[[1]]), "_leg.jpg"),
          dpi = 600, width = 19, units = "in",
          outer.margins = c(0.001, 0.001, 0.001, 0.001))

# diferen -----------------------------------------------------------------

ro <- read_state(code_state = "RO", showProgress = F) %>%
  st_transform("ESRI:102015")


lrc <- list.files(path = "./comparacao", pattern = ".tif$",
                  full.names = T, recursive = T) %>%
  grep(pattern = "OCS", value = T) %>%
  rast()





dif_map <- c(lrc[[2]] - lrc[[1]], lrc[[3]] - lrc[[1]], lrc[[4]] - lrc[[1]]) %>%
  `names<-`(c("OCS_0_30_dif_gustavo", "OCS_0_30_dif_lucas", "OCS_0_30_dif_soilgrids"))



plot(dif_map)

dif_agrg <- terra::aggregate(dif_map, fact = 10)


maxi <- max(values(dif_agrg), na.rm = T)
mini <- min(values(dif_agrg), na.rm = T)


map <- list()

map[[1]] <- tm_shape(dif_agrg[[1]], raster.downsample = T) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                # labels.size = 2.7,
                labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_raster(style = "fixed", legend.hist = F, midpoint = NA,
            breaks = c(seq(min(values(dif_agrg[[1]]), na.rm = T) + 0.25 * min(values(dif_agrg[[1]]), na.rm = T),
                           0,
                           length.out = 7),
                       seq(0, max(values(dif_agrg[[1]]), na.rm = T) - 0.25 * max(values(dif_agrg[[1]]), na.rm = T),
                           length.out = 7)),
            palette = colorRampPalette(brewer.pal(11, "Spectral"))(13),
            title = expression((Mg~ha^-1)), legend.reverse = T) +
  tm_legend(legend.format =
              list(text.separator = "-",
                   decimal.mark = ".",
                   digits = 0,
                   big.mark = ","),
            legend.position = c("left", "bottom"),
            legend.stack = "vertical",
            legend.just = "center",
            legend.outside = F,
            #  legend.text.size = 2.8,
            #  legend.title.size = 3.2,
            legend.frame = T) +
  tm_layout(inner.margins = c(0.01, 0.05, 0.01, 0.05),
            frame.lwd = 2) +
  # tm_compass(type = "rose", position = c("right", "top"), size = 3,
  #            text.size = 1.5) +
  # tm_scale_bar(position = c("left", "top"), text.size = 1) +
  tm_shape(ro) +
  tm_borders(lwd = 2, col = "black") ; map[[1]]


map[[2]] <- tm_shape(dif_agrg[[2]], raster.downsample = T) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                #labels.size = 2.7,
                labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_raster(style = "fisher", legend.hist = F,
            n = 13, midpoint = NA,
            palette = colorRampPalette(brewer.pal(11, "Spectral"))(13),
            title = expression((Mg~ha^-1)), legend.reverse = T) +
  tm_legend(legend.format =
              list(text.separator = "-",
                   decimal.mark = ".",
                   digits = 0,
                   big.mark = ","),
            legend.position = c("left", "bottom"),
            legend.stack = "vertical",
            legend.just = "center",
            legend.outside = F,
            #  legend.text.size = 2.8,
            #  legend.title.size = 3.2,
            legend.frame = T) +
  tm_layout(inner.margins = c(0.01, 0.05, 0.01, 0.05),
            frame.lwd = 2) +
  # tm_compass(type = "rose", position = c("right", "top"), size = 3,
  #            text.size = 1.5) +
  # tm_scale_bar(position = c("left", "top"), text.size = 1) +
  tm_shape(ro) +
  tm_borders(lwd = 2, col = "black")




map[[3]] <- tm_shape(dif_agrg[[3]], raster.downsample = T) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                # labels.size = 2.7,
                labels.inside.frame = T, col = "black",
                labels.col = "black") +
  tm_raster(style = "fisher", legend.hist = F,
            n = 13, midpoint = NA,
            palette = colorRampPalette(brewer.pal(11, "Spectral"))(13),
            title = expression((Mg~ha^-1)), legend.reverse = T) +
  tm_legend(legend.format =
              list(text.separator = "-",
                   decimal.mark = ".",
                   digits = 0,
                   big.mark = ","),
            legend.position = c("left", "bottom"),
            legend.stack = "vertical",
            legend.just = "center",
            legend.outside = F,
            #  legend.text.size = 2.8,
            #  legend.title.size = 3.2,
            legend.frame = T) +
  tm_layout(inner.margins = c(0.01, 0.05, 0.01, 0.05),
            frame.lwd = 2) +
  # tm_compass(type = "rose", position = c("right", "top"), size = 3,
  #            text.size = 1.5) +
  # tm_scale_bar(position = c("left", "top"), text.size = 1) +
  tm_shape(ro) +
  tm_borders(lwd = 2, col = "black")





tm_arrr <- tmap::tmap_arrange(map[[1]], map[[2]], map[[3]], nrow = 1)





tmap_save(tm = tm_arrr,
          filename = paste0("./fig/tm_arrr.jpg"),
          dpi = 600, width = 19, units = "in", height = 4.73,
          outer.margins = c(0.001, 0.001, 0.001, 0.001))
