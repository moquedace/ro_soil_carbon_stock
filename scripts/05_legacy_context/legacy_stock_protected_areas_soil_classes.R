pkg <- c("geobr", "sf", "raster", "dplyr", "stringr", "tmap", "tmaptools",
         "readxl", "ggplot2", "fasterRaster", "RColorBrewer", "ggridges",
         "progress", "scales")

sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("C:/R/mestrado/soil_carbon/")


options(scipen = 999)
ro <- read_state(code_state = "RO", year = 2018, showProgress = F) %>%
  st_transform("ESRI:102015")


uc <- read_conservation_units(showProgress = F) %>%
  st_transform("ESRI:102015") %>%
  st_intersection(ro)

ti <- read_indigenous_land(showProgress = F) %>%
  st_transform("ESRI:102015") %>%
  st_intersection(ro)

lr <- list.files(path = "./modelo/rf/200m/s_conv/mapas/presente/quartis_medias",
                 pattern = ".tif$", full.names = T) %>%
  as.data.frame() %>%
  filter(str_detect(., "media.tif")) %>%
  pull() %>%
  .[c(1, 2, 5, 3, 4, 6)]



s_uc <- st_intersection(filter(uc,
                               category == "Reserva Particular do Patrim�nio Natural"),
                        solo)


glimpse(uc)


proff <- c("0-100 cm", "0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm")

prof_ord <- c("0-100", "0-5", "5-15", "15-30", "30-60", "60-100")
prof_ord2 <- c("0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm")
# STOCK PROFUNDIDADE ------------------------------------------------------


stock_geral <- read_excel("./planilhas/stock_prof.xlsx") %>%
  mutate(DEPTH = recode(DEPTH,
                        "0-5" = "0-5 cm",
                        "5-15" = "5-15 cm",
                        "15-30" = "15-30 cm",
                        "30-60" = "30-60 cm",
                        "60-100" = "60-100 cm"),
         DEPTH = factor(DEPTH, levels = prof_ord2))


stock_med <- stock_geral %>%
  group_by(DEPTH = DEPTH) %>%
  summarise(media = mean(STOCK),
            assimetria = e1071::skewness(STOCK),
            curtose = moments::kurtosis(STOCK),
            dv = sd(STOCK))


stock_geral_p <- ggplot(stock_geral, aes(x = STOCK, y = DEPTH,
                                         fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = 4, scale = 3) +
  scale_x_continuous(n.breaks = 8, position = "top") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = alpha(colorRampPalette(brewer.pal(11,
                                                               "Spectral"))(4),
                                   0.75),
                    labels = c("0-25%", "25-50%", "50-75%", "75-100%")) +
  labs(x = expression(Carbon~stock~(Mg~ha^{-1})), y = NULL,
       fill = "Quartiles:") +
  #facet_wrap(~ DEPTH) +
  theme_classic() +
  theme(legend.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        legend.title = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 24, color = "black"),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 24, color = "black"),
        strip.background = element_blank(),
        strip.placement = "output") +
  guides(alpha = FALSE) ; stock_geral_p


ggsave(stock_geral_p, filename = "./figuras/stock_geral_p2.jpg", dpi = 1200,
       width = 19, height = 10.69, units = "in")










stk_geral <- data.frame(prof = proff, stock = NA, area = NA)

# ESTOUE GERAL
for (i in seq_along(lr)) {

  stock <- raster(lr[i])


  area <- raster::zonal(stock, stock, fun = "count") *
    (prod(res(stock))) / 10000




  if (i == 1){
    stk_geral[i, "stock"] <- sum(values(stock), na.rm = T) *
      (prod(res(stock)) / 10000)

    stk_geral[i, "area"] <- sum(area[, 2])
  }

  if (i == 2){
    stk_geral[i, "stock"] <- sum(values(stock), na.rm = T) *
      (prod(res(stock)) / 10000)

    stk_geral[i, "area"] <- sum(area[, 2])
  }

  if (i == 3){
    stk_geral[i, "stock"] <- sum(values(stock), na.rm = T) *
      (prod(res(stock)) / 10000)

    stk_geral[i, "area"] <- sum(area[, 2])
  }

  if (i == 4){
    stk_geral[i, "stock"] <- sum(values(stock), na.rm = T) *
      (prod(res(stock)) / 10000)

    stk_geral[i, "area"] <- sum(area[, 2])
  }

  if (i == 5){
    stk_geral[i, "stock"] <- sum(values(stock), na.rm = T) *
      (prod(res(stock)) / 10000)

    stk_geral[i, "area"] <- sum(area[, 2])
  }

  if (i == 6){
    stk_geral[i, "stock"] <- sum(values(stock), na.rm = T) *
      (prod(res(stock)) / 10000)

    stk_geral[i, "area"] <- sum(area[, 2])
  }


}

stk_geral$stock_mg_ha <- stk_geral$stock / stk_geral$area
stk_geral$stock_teragrama <- stk_geral$stock / 1000000

xlsx::write.xlsx(stk_geral, "./planilhas/stock total final.xlsx", row.names = F)
sum(filter(stk_geral, prof != "0-100 cm")$stock)








# UNIDADES DE CONSERVA��O -------------------------------------------------

cat_uc <- unique(uc$category)

stock_uc_cat <- data.frame(expand.grid(cat = cat_uc, prof = proff, stock = NA,
                                       area = NA))

for (i in seq_along(lr)) {

  stock <- raster(lr[i])

  for (m in seq_along(cat_uc)) {

    uc_stock <- stock %>%
      crop(filter(uc, category == cat_uc[m])) %>%
      mask(filter(uc, category == cat_uc[m]))


    area <- raster::zonal(uc_stock, uc_stock, fun = "count") *
      (prod(res(uc_stock))) / 10000




    if (i == 1){
      stock_uc_cat[m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_uc_cat[m, "area"] <- sum(area[, 2])
    }

    if (i == 2){
      stock_uc_cat[8 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_uc_cat[8 + m, "area"] <- sum(area[, 2])
    }

    if (i == 3){
      stock_uc_cat[16 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_uc_cat[16 + m, "area"] <- sum(area[, 2])
    }

    if (i == 4){
      stock_uc_cat[24 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_uc_cat[24 + m, "area"] <- sum(area[, 2])
    }

    if (i == 5){
      stock_uc_cat[32 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_uc_cat[32 + m, "area"] <- sum(area[, 2])
    }

    if (i == 6){
      stock_uc_cat[40 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_uc_cat[40 + m, "area"] <- sum(area[, 2])
    }


  }

}



# TERRA IND�GENA ----------------------------------------------------------

stock_ti <- vector()
for (i in seq_along(lr)) {

  stock <- raster(lr[i])


  uc_stock <- stock %>%
    crop(ti) %>%
    mask(ti)

  stock_ti[i] <- sum(values(uc_stock), na.rm = T) *
    (prod(res(stock)) / 10000)

  area <- raster::zonal(uc_stock, uc_stock, fun = "count") *
    (prod(res(uc_stock))) / 10000


  area <- sum(area[, 2])

}


ti_stock <- data.frame(cat = "Terra ind�gena", prof = proff, stock = stock_ti,
                       area = area)





# �REA ANTR�PICA ----------------------------------------------------------

ap <- st_union(uc, ti) %>%
  st_combine() %>%
  st_union(by_feature = T)


trop <- st_sym_difference(ro, ap)


stock_trop <- vector()
for (i in seq_along(lr)) {

  stock <- raster(lr[i])


  uc_stock <- stock %>%
    crop(trop) %>%
    mask(trop)

  stock_trop[i] <- sum(values(uc_stock), na.rm = T) *
    (prod(res(stock)) / 10000)

  area <- raster::zonal(uc_stock, uc_stock, fun = "count") *
    (prod(res(uc_stock))) / 10000


  area <- sum(area[, 2])

}


trop_stock <- data.frame(cat = "�rea antr�pica", prof = proff,
                         stock = stock_trop,  area = area)



uc_ind_ant <- rbind(stock_uc_cat, ti_stock, trop_stock)



uc_ind_ant$stock_mg_ha <- uc_ind_ant$stock / uc_ind_ant$area




write.csv2(uc_ind_ant,
           file = "./planilhas/stock area prot antrop final.csv",
           row.names = F)




# MapBiomas ---------------------------------------------------------------

mapbiomas <- st_read(dsn = "./shp/mapbiomas", layer = "map_ro_reclas_pol_bf") %>%
  st_transform("ESRI:102015") %>%
  st_make_valid() %>%
  st_intersection(ro)


cores_map <- read_excel("C:/R/outros/docs/cores_mapbiomas.xlsx")




map_2 <- left_join(mapbiomas, cores_map, by = "gridcode")


#st_write(map_2, dsn = "./shp/mapbiomas_classe.shp")


map_2 <- map_2 %>%
  arrange(clas_uso)

tm_shape(map_2) +
  tm_polygons(col = "clas_uso", border.col = "transparent",
              palette = unique(map_2$col)) +
  tm_shape(ro) +
  tm_borders(col = "black", lwd = 2)




cat_veg <- unique(map_2$clas_uso)

stock_veg_cat <- data.frame(expand.grid(cat = cat_veg, prof = prof, stock = NA,
                                        area = NA))

for (i in seq_along(lr)) {

  stock <- raster(lr[i])

  for (m in seq_along(cat_veg)) {

    map_stock <- stock %>%
      crop(filter(map_2, clas_uso == cat_veg[m])) %>%
      mask(filter(map_2, clas_uso == cat_veg[m]))


    area <- raster::zonal(map_stock, map_stock,
                          fun = "count") *
      (prod(res(map_stock))) / 10000




    if (i == 1){
      stock_veg_cat[m, "stock"] <- sum(values(map_stock), na.rm = T)
      stock_veg_cat[m, "area"] <- sum(area[, 2])
    }

    if (i == 2){
      stock_veg_cat[9 + m, "stock"] <- sum(values(map_stock), na.rm = T)
      stock_veg_cat[9 + m, "area"] <- sum(area[, 2])
    }

    if (i == 3){
      stock_veg_cat[18 + m, "stock"] <- sum(values(map_stock), na.rm = T)
      stock_veg_cat[18 + m, "area"] <- sum(area[, 2])
    }

    if (i == 4){
      stock_veg_cat[27 + m, "stock"] <- sum(values(map_stock), na.rm = T)
      stock_veg_cat[27 + m, "area"] <- sum(area[, 2])
    }

    if (i == 5){
      stock_veg_cat[36 + m, "stock"] <- sum(values(map_stock), na.rm = T)
      stock_veg_cat[36 + m, "area"] <- sum(area[, 2])
    }

    if (i == 6){
      stock_veg_cat[45 + m, "stock"] <- sum(values(map_stock), na.rm = T)
      stock_veg_cat[45 + m, "area"] <- sum(area[, 2])
    }


  }

}










# SOLOS -------------------------------------------------------------------
solo <- st_read(dsn = "./shp/solo/CLAS_SOLO_1NC.shp")



cat_solo <- as.character(na.omit(unique(solo$clss_1n)))



for (i in seq_along(lr)) {

  stock <- raster(lr[i])

  pb <- progress_bar$new(
    format = "calculando [:bar] (:percent) tempo decorrido: :elapsed  tempo restante: :eta",
    total = length(cat_solo), clear = F, width= 85)

  for (m in seq_along(cat_solo)) {

    pb$tick()
    Sys.sleep(1 / length(cat_solo))

    uc_stock <- stock %>%
      crop(filter(solo, clss_1n == cat_solo[m])) %>%
      mask(filter(solo, clss_1n == cat_solo[m]))


    area <- raster::zonal(uc_stock, uc_stock,
                          fun = "count") *
      (prod(res(uc_stock))) / 10000


    if (m == 1){

      v_stock <- values(uc_stock) %>%
        na.omit()

      stock_solo_cat <- data.frame(expand.grid(cat = cat_solo[m],
                                               prof = proff[i],
                                               stock = v_stock,
                                               area = sum(area[, 2])))

    }


    if (m != 1){

      v_stock <- values(uc_stock) %>%
        na.omit()

      stock_solo_cat_prof <- data.frame(expand.grid(cat = cat_solo[m],
                                                    prof = proff[i],
                                                    stock = v_stock,
                                                    area = sum(area[, 2])))

      stock_solo_cat <- rbind(stock_solo_cat, stock_solo_cat_prof)
    }


  }

  if (i == 1){

    stock_solo_cat_total <- stock_solo_cat

  }

  if (i != 1){

    stock_solo_cat_total_prof <- stock_solo_cat
    stock_solo_cat_total <- rbind(stock_solo_cat_total,
                                  stock_solo_cat_total_prof)

  }


}








stock_solo_cat <- data.frame(expand.grid(cat = cat_solo, prof = proff,
                                         stock = NA, area = NA))


for (i in seq_along(lr)) {

  stock <- raster(lr[i])

  pb <- progress_bar$new(
    format = "calculando [:bar] (:percent) tempo decorrido: :elapsed  tempo restante: :eta",
    total = length(cat_solo), clear = F, width= 85)

  for (m in seq_along(cat_solo)) {

    pb$tick()
    Sys.sleep(1 / length(cat_solo))

    uc_stock <- stock %>%
      crop(filter(solo, clss_1n == cat_solo[m])) %>%
      mask(filter(solo, clss_1n == cat_solo[m]))


    area <- raster::zonal(uc_stock, uc_stock,
                          fun = "count") *
      (prod(res(uc_stock))) / 10000




    if (i == 1){
      stock_solo_cat[m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_solo_cat[m, "area"] <- sum(area[, 2])
    }

    if (i == 2){
      stock_solo_cat[11 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_solo_cat[11 + m, "area"] <- sum(area[, 2])
    }

    if (i == 3){
      stock_solo_cat[22 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_solo_cat[22 + m, "area"] <- sum(area[, 2])
    }

    if (i == 4){
      stock_solo_cat[33 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_solo_cat[33 + m, "area"] <- sum(area[, 2])
    }

    if (i == 5){
      stock_solo_cat[44 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_solo_cat[44 + m, "area"] <- sum(area[, 2])
    }

    if (i == 6){
      stock_solo_cat[55 + m, "stock"] <- sum(values(uc_stock), na.rm = T) *
        (prod(res(stock)) / 10000)

      stock_solo_cat[55 + m, "area"] <- sum(area[, 2])
    }

  }

}

stock_solo_cat$stock_mg_ha <- stock_solo_cat$stock / stock_solo_cat$area








# TOTAL -------------------------------------------------------------------
stock_solo_cat$tipo <- "Solo"
uc_ind_ant$tipo <- "UC"
#total_uc$tipo <- "ANT"


stock_solo_cat <- stock_solo_cat %>%
  mutate(cat = str_to_sentence(cat))


uc_ind_ant <- uc_ind_ant %>%
  mutate(cat = recode(cat,
                      `Reserva Extrativista` = "Extractive reserve",
                      `Reserva Particular do Patrim�nio Natural` = "PNHR",
                      `Floresta` = "Public forest",
                      `Parque` = "Park",
                      `Reserva Biol�gica` = "Biological reserve",
                      `Monumento Natural` = "Natural monument",
                      `�rea de Prote��o Ambiental` = "EPA",
                      `Esta��o Ecol�gica` = "Ecological station",
                      `Terra ind�gena` = "Indigenous land",
                      `�rea antr�pica` = "Anthropic area"))

#str_to_sentence(stock_solo_cat$cat)

stock_estratificado <- rbind(stock_solo_cat, uc_ind_ant)


write.csv2(stock_estratificado, file = "./planilhas/stock estratificado_final.csv",
           row.names = F)







# Gr�ficos ----------------------------------------------------------------


stock_estratificado <- read.csv2("./planilhas/stock estratificado_final.csv") %>%
  mutate(prof = factor(as.factor(prof), levels = proff,
                       labels = prof_ord),
         cat = recode(cat,
                      `Anthropic area` = "Unprotected land"))



gg_solo <- ggplot(filter(stock_estratificado,
                         tipo %in% c("Solo")),
                  aes(x = stock, y = reorder(cat, stock), fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(8, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Tg)), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,
                                          accuracy = 1, big.mark = ""),
                     expand = c(0, 0, 0.004, 0)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_solo



gg_solo2 <- ggplot(filter(stock_estratificado,
                          tipo %in% c("Solo")),
                   aes(x = stock_mg_ha, y = reorder(cat, stock_mg_ha),
                       fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(8, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Mg~ha^{-1})), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(expand = c(0, 0, 0.07, 0)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_solo2



ggsolo_g <- ggpubr::ggarrange(gg_solo, gg_solo2, common.legend = T,
                              labels = c("a)", "b)"), vjust = 33,
                              font.label = list(size = 30))


ggsave(ggsolo_g, filename = "./figuras/ggsolo_g.jpg", dpi = 1200,
       width = 19, height = 10.69, units = "in")









gg_UC1 <- ggplot(filter(stock_estratificado,
                        tipo %in% c("UC")),
                 aes(x = stock, y = reorder(cat, stock), fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(8, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Tg)), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,
                                          accuracy = 1, big.mark = ""),
                     expand = c(0, 0, 0, 0)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_UC1



gg_UC2 <- ggplot(filter(stock_estratificado,
                        tipo %in% c("UC")),
                 aes(x = stock_mg_ha, y = reorder(cat, stock_mg_ha),
                     fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(8, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Mg~ha^{-1})), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(expand = c(0, 0, 0.09, 0)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_UC2



ggUC_g <- ggpubr::ggarrange(gg_UC1, gg_UC2, common.legend = T,
                            labels = c("a)", "b)"), vjust = 33,
                            font.label = list(size = 30))


ggsave(ggUC_g, filename = "./figuras/ggUC_g.jpg", dpi = 1200,
       width = 19, height = 10.69, units = "in")













gg_uso1 <- ggplot(filter(stock_estratificado,
                         tipo %in% c("Uso")),
                  aes(x = stock, y = reorder(cat, stock), fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(5, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Tg)), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,
                                          accuracy = 1)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_uso1



gg_uso2 <- ggplot(filter(stock_estratificado,
                         tipo %in% c("Uso")),
                  aes(x = stock_mg_ha, y = reorder(cat, stock_mg_ha),
                      fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(5, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Mg~ha^{-1})), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous() +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_uso2



gguso_g <- ggpubr::ggarrange(gg_uso1, gg_uso2, common.legend = T)


ggsave(gguso_g, filename = "./figuras/gguso_g.jpg", dpi = 1200,
       width = 19, height = 10.69, units = "in")







gg_ANT1 <- ggplot(filter(stock_estratificado,
                         tipo %in% c("ANT")),
                  aes(x = stock, y = reorder(cat, stock), fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(5, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Tg)), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,
                                          accuracy = 1)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_ANT1



gg_ANT2 <- ggplot(filter(stock_estratificado,
                         tipo %in% c("ANT")),
                  aes(x = stock_mg_ha, y = reorder(cat, stock_mg_ha),
                      fill = prof)) +
  geom_col(alpha = 0.5) +
  scale_fill_manual(values = c("black",
                               colorRampPalette(brewer.pal(5, "Set2"))(5))) +
  labs(x = expression(SOC~stock~(Mg~ha^{-1})), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous() +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 19, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.text = element_text(size = 24),
        plot.title = element_text(size = 20, hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 22, color = "black"),
        legend.title = element_text(size = 26, color = "black")) +
  facet_wrap(~ prof, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_ANT2



ggANT_g <- ggpubr::ggarrange(gg_ANT1, gg_ANT2, common.legend = T)


ggsave(ggANT_g, filename = "./figuras/ggANT_g.jpg", dpi = 1200,
       width = 19, height = 10.69, units = "in")










sum(filter(stock_solo_cat, prof != "0-100 cm")$stock)
sum(filter(total_uc, prof != "0-100 cm")$stock)
sum(filter(stk_geral, prof != "0-100 cm")$stock)

sum(filter(stock_solo_cat, prof != "0-100 cm")$stock) - sum(filter(total_uc, prof != "0-100 cm")$stock)




save.image("stock_veg_cat_box.RData")
