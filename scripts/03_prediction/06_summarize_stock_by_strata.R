pkg <- c("geobr", "sf", "terra", "dplyr", "stringr", "tmap", "tmaptools",
         "readxl", "ggplot2",  "RColorBrewer", "ggridges", "scales", "tidyr")

sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("D:/Usuario/cassio/R/ro_soil_carbon/")

options(scipen = 999)




# prep --------------------------------------------------------------------


source("D:/Usuario/cassio/R/outros/scripts_aleatorios/s_tiles.R")




lr <- list.files(path = "./results_ocs_100/rf", pattern = ".tif$",
                 full.names = T, recursive = T) %>%
  grep(pattern = "mean", value = T) %>%
  .[c(1, 2, 5, 3, 4, 6)] %>%
  rast()



# tiles <-  rst_tile(rst = lr, nc = 2, nr = 10, coord = NULL,
#                    exte = NULL, nl = NULL,
#                    file = "tiles/tile_.tif")
#




tiles <- list.files(path = "./tiles", pattern = ".tif$",
                    full.names = T)





# stock full --------------------------------------------------------------





for (i in seq_along(tiles)) {

  gc()
  df <- as.data.frame(rast(tiles[i]), na.rm = T) %>% as_tibble() %>%
    mutate(across(everything(.), ~ . * (prod(res(lr)) / 10000),
                  .names = "stk_{col}"))


  area <- cellSize(rast(tiles[i])[[1]], unit = "m") %>%
    values(na.rm = T) %>%
    sum()

  grp <- df %>%
    dplyr::select(7:12) %>%
    summarise_all(sum) %>%
    mutate(area = area)





  if (i == 1) {
    df_sum <- grp
  } else {
    df_sum <- rbind(df_sum, grp)
  }

}




df_stk_full <- df_sum %>%
  gather(key = "depth", value = "value", -area) %>%
  group_by(depth) %>%
  summarise(value = sum(value),
            area = sum(area)) %>%
  mutate(depth = recode(depth,
                        stk_OCS_0_100_mean = "0-100",
                        stk_OCS_0_5_mean = "0-5",
                        stk_OCS_5_15_mean = "5-15",
                        stk_OCS_15_30_mean = "15-30",
                        stk_OCS_30_60_mean = "30-60",
                        stk_OCS_60_100_mean = "60-100"),
         class = "full",
         categ = "full")


rm(df_sum)


# soil --------------------------------------------------------------------

soil <- st_read(dsn = "./shp/soil_clas.shp") %>%
  dplyr::select(clss_1n) %>%
  na.omit()



tm_shape(soil) +
  tm_fill(col = "clss_1n")



soil_clas <- unique(soil$clss_1n)


i = 1
n = 1
for (i in seq_along(soil_clas)) {


  vectf <- soil %>%
    filter(clss_1n == soil_clas[i]) %>%
    vect()


  par(mfrow = c(2, 1))
  plot(vectf)



  vectf <- soil %>%
    filter(clss_1n == soil_clas[i]) %>%
    st_make_valid() %>%
    vect()



  plot(vectf)




  par(mfrow = c(2, 2))




  for (n in seq_along(tiles)) {

    tst <- terra::intersect(y = terra::ext(rast(tiles[n])), x = vectf)

    if (!nrow(tst) == 0 & !ncol(tst) == 0) {

      rst <- rast(tiles[n]) %>% crop(vectf) %>% mask(vectf)

      plot(rst[[1]], main = soil_clas[i])
      plot(vectf, add = T)

      gc()
      df <- as.data.frame(rst, na.rm = T) %>% as_tibble() %>%
        mutate(across(everything(.), ~ . * (prod(res(lr)) / 10000),
                      .names = "stk_{col}"))


      area <- cellSize(rast(tiles[n])[[1]], unit = "m") %>%
        values(na.rm = T) %>%
        sum()


      grp <- df %>%
        dplyr::select(7:12) %>%
        summarise_all(sum) %>%
        mutate(area = area)



      if (!exists("df_sum")) {
        df_sum <- grp
      } else {
        df_sum <- rbind(df_sum, grp)
      }
      gc()

    }




  }

  if (i == 1) {

    df_stk_soil <- df_sum %>%
      gather(key = "depth", value = "value", -area) %>%
      group_by(depth) %>%
      summarise(value = sum(value),
                area = sum(area)) %>%
      mutate(depth = recode(depth,
                            stk_OCS_0_100_mean = "0-100",
                            stk_OCS_0_5_mean = "0-5",
                            stk_OCS_5_15_mean = "5-15",
                            stk_OCS_15_30_mean = "15-30",
                            stk_OCS_30_60_mean = "30-60",
                            stk_OCS_60_100_mean = "60-100"),
             categ = soil_clas[i],
             class = "soil")

  } else {

    p_soil <- df_sum %>%
      gather(key = "depth", value = "value", -area) %>%
      group_by(depth) %>%
      summarise(value = sum(value),
                area = sum(area)) %>%
      mutate(depth = recode(depth,
                            stk_OCS_0_100_mean = "0-100",
                            stk_OCS_0_5_mean = "0-5",
                            stk_OCS_5_15_mean = "5-15",
                            stk_OCS_15_30_mean = "15-30",
                            stk_OCS_30_60_mean = "30-60",
                            stk_OCS_60_100_mean = "60-100"),
             categ = soil_clas[i],
             class = "soil")

    df_stk_soil <- rbind(df_stk_soil, p_soil)

  }


  rm(df_sum)




}









# uc --------------------------------------------------------------------

ro <- read_state(code_state = "RO") %>%
  st_transform("ESRI:102015")




uc <- read_conservation_units(showProgress = F) %>%
  st_transform("ESRI:102015") %>%
  st_intersection(ro) %>%
  dplyr::select("category")

ti <- read_indigenous_land(showProgress = F) %>%
  st_transform("ESRI:102015") %>%
  st_intersection(ro) %>%
  st_combine() %>% st_union() %>% st_as_sf() %>%
  mutate(category = "Indigenous land") %>%
  rename("geom" = x)


ap <- rbind(uc, ti)





ap_clas <- unique(ap$category)


i = 1
n = 1
for (i in seq_along(ap_clas)) {


  vectf <- ap %>%
    filter(category == ap_clas[i]) %>%
    vect()


  par(mfrow = c(2, 1))
  plot(vectf)



  vectf <- ap %>%
    filter(category == ap_clas[i]) %>%
    st_make_valid() %>%
    vect()



  plot(vectf)




  par(mfrow = c(2, 2))




  for (n in seq_along(tiles)) {

    tst <- terra::intersect(y = terra::ext(rast(tiles[n])), x = vectf)

    if (!nrow(tst) == 0 & !ncol(tst) == 0) {

      rst <- rast(tiles[n]) %>% crop(vectf) %>% mask(vectf)

      plot(rst[[1]], main = ap_clas[i])
      plot(vectf, add = T)

      gc()
      df <- as.data.frame(rst, na.rm = T) %>% as_tibble() %>%
        mutate(across(everything(.), ~ . * (prod(res(lr)) / 10000),
                      .names = "stk_{col}"))


      area <- cellSize(rast(tiles[n])[[1]], unit = "m") %>%
        values(na.rm = T) %>%
        sum()


      grp <- df %>%
        dplyr::select(7:12) %>%
        summarise_all(sum) %>%
        mutate(area = area)



      if (!exists("df_sum")) {
        df_sum <- grp
      } else {
        df_sum <- rbind(df_sum, grp)
      }
      gc()

    }

  }

  if (i == 1) {

    df_stk_ap <- df_sum %>%
      gather(key = "depth", value = "value", -area) %>%
      group_by(depth) %>%
      summarise(value = sum(value),
                area = sum(area)) %>%
      mutate(depth = recode(depth,
                            stk_OCS_0_100_mean = "0-100",
                            stk_OCS_0_5_mean = "0-5",
                            stk_OCS_5_15_mean = "5-15",
                            stk_OCS_15_30_mean = "15-30",
                            stk_OCS_30_60_mean = "30-60",
                            stk_OCS_60_100_mean = "60-100"),
             categ = ap_clas[i],
             class = "protected area")

  } else {

    p_ap <- df_sum %>%
      gather(key = "depth", value = "value", -area) %>%
      group_by(depth) %>%
      summarise(value = sum(value),
                area = sum(area)) %>%
      mutate(depth = recode(depth,
                            stk_OCS_0_100_mean = "0-100",
                            stk_OCS_0_5_mean = "0-5",
                            stk_OCS_5_15_mean = "5-15",
                            stk_OCS_15_30_mean = "15-30",
                            stk_OCS_30_60_mean = "30-60",
                            stk_OCS_60_100_mean = "60-100"),
             categ = ap_clas[i],
             class = "protected area")

    df_stk_ap <- rbind(df_stk_ap, p_ap)

  }

  rm(df_sum)


}


# unprotected area --------------------------------------------------------

ap_u <- st_union(st_make_valid(uc), st_make_valid(ti)) %>%
  st_combine() %>%
  st_union(by_feature = T)


u_land <- st_sym_difference(ro, ap_u)

plot(st_geometry(u_land))






vectf <- u_land %>%
  vect()

dev.off()
par(mfrow = c(2, 2))

for (n in seq_along(tiles)) {

  tst <- terra::intersect(y = terra::ext(rast(tiles[n])), x = vectf)

  if (!nrow(tst) == 0 & !ncol(tst) == 0) {

    rst <- rast(tiles[n]) %>% crop(vectf) %>% mask(vectf)

    plot(rst[[1]])
    plot(vectf, add = T)

    gc()
    df <- as.data.frame(rst, na.rm = T) %>% as_tibble() %>%
      mutate(across(everything(.), ~ . * (prod(res(lr)) / 10000),
                    .names = "stk_{col}"))


    area <- cellSize(rast(tiles[n])[[1]], unit = "m") %>%
      values(na.rm = T) %>%
      sum()

    grp <- df %>%
      dplyr::select(7:12) %>%
      summarise_all(sum) %>%
      mutate(area = area)



    if (!exists("df_sum")) {
      df_sum <- grp
    } else {
      df_sum <- rbind(df_sum, grp)
    }
    gc()

  }

}



df_stk_uap <- df_sum %>%
  gather(key = "depth", value = "value", -area) %>%
  group_by(depth) %>%
  summarise(value = sum(value),
            area = sum(area)) %>%
  mutate(depth = recode(depth,
                        stk_OCS_0_100_mean = "0-100",
                        stk_OCS_0_5_mean = "0-5",
                        stk_OCS_5_15_mean = "5-15",
                        stk_OCS_15_30_mean = "15-30",
                        stk_OCS_30_60_mean = "30-60",
                        stk_OCS_60_100_mean = "60-100"),
         categ = "unprotected area",
         class = "protected area")
















# calc area ---------------------------------------------------------------


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}



area_ha <- ro %>%
  mutate(area = as.numeric(st_area(.) / 10000),
         class = "full",
         categ = "full") %>%
  as.data.frame() %>%
  select(categ, class, area)




area_ha <- area_ha %>% rbind(soil %>%
                               mutate(area = as.numeric(st_area(.) / 10000),
                                      categ = clss_1n,
                                      class = "soil") %>%
                               as.data.frame() %>%
                               select(class, categ, area) %>%
                               group_by(class, categ) %>%
                               summarise(area = sum(area))%>%
                               mutate(categ = tolower(categ),
                                      categ = firstup(categ)))


area_ha <- area_ha %>% rbind(ap %>%
                               mutate(area = as.numeric(st_area(.) / 10000),
                                      categ = category,
                                      class = "protected area") %>%
                               as.data.frame() %>%
                               select(class, categ, area) %>%
                               group_by(class, categ) %>%
                               summarise(area = sum(area)))



area_ha <- area_ha %>% rbind(u_land %>%
                               mutate(area = as.numeric(st_area(.) / 10000),
                                      categ = "unprotected area",
                                      class = "protected area") %>%
                               as.data.frame() %>%
                               select(class, categ, area) %>%
                               group_by(class, categ) %>%
                               summarise(area = sum(area)))







sum(filter(area_ha, class == "soil")$area)
sum(filter(area_ha, class == "protected area")$area)
sum(filter(area_ha, class == "full")$area)


# geral -------------------------------------------------------------------

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}





df_stk_soil_f <- df_stk_soil %>%
  mutate(categ = tolower(categ),
         categ = firstup(categ))





df_full <- rbind(df_stk_ap, df_stk_full, df_stk_soil_f, df_stk_uap)  %>%
  select(-area) %>%
  left_join(area_ha, by = c("class", "categ")) %>%
  mutate(categ = recode(categ,
                        `Reserva Extrativista` = "Extractive reserve",
                        `Reserva Particular do Patrimônio Natural` = "PNHR",
                        `Floresta` = "Public forest",
                        `Parque` = "Park",
                        `Reserva Biológica` = "Biological reserve",
                        `Monumento Natural` = "Natural monument",
                        `Área de Proteção Ambiental` = "EPA",
                        `Estação Ecológica` = "Ecological station",
                        `unprotected area` = "Unprotected area"))


df_full <- df_full %>%
  mutate(ocd = value / area) %>%
  rename(soc = value) %>%
  relocate(depth, class, categ, area, soc, ocd)




write.csv2(df_full, file = "./sheet/stock_full_strat.csv",  row.names = F)



# fig ---------------------------------------------------------------------

df_full <- read.csv2("./sheet/stock_full_strat.csv")



# soil --------------------------------------------------------------------


gg_solo <- ggplot(filter(df_full,
                         class %in% c("soil")),
                  aes(x = soc, y = reorder(categ, soc), fill = depth)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = soilpalettes::soil_palette("paleustalf", 6)) +
  labs(x = expression(SOC~stock~(Tg)), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,
                                          accuracy = 1, big.mark = ""),
                     expand = c(0, 0, 0.004, 0),
                     n.breaks = 4) +
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
        legend.title = element_text(size = 26, color = "black"),
        panel.spacing.x = unit(11, "mm")) +
  facet_wrap(~ depth, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_solo



gg_solo2 <- ggplot(filter(df_full,
                          class %in% c("soil")),
                   aes(x = ocd, y = reorder(categ, ocd),
                       fill = depth)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = soilpalettes::soil_palette("paleustalf", 6)) +
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
        legend.title = element_text(size = 26, color = "black"),
        panel.spacing.x = unit(6, "mm")) +
  facet_wrap(~ depth, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_solo2



ggsolo_g <- ggpubr::ggarrange(gg_solo, gg_solo2, common.legend = T,
                              labels = c("a)", "b)"), vjust = 33,
                              font.label = list(size = 30, face = "plain")) ; ggsolo_g


ggsave(ggsolo_g, filename = "./fig/ggsolo_g.png", dpi = 1200,
       width = 19, height = 10.69, units = "in")






# protected area ----------------------------------------------------------




gg_ap <- ggplot(filter(df_full,
                         class %in% c("protected area")),
                  aes(x = soc, y = reorder(categ, soc), fill = depth)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = soilpalettes::soil_palette("paleustalf", 6)) +
  labs(x = expression(SOC~stock~(Tg)), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,
                                          accuracy = 1, big.mark = ""),
                     expand = c(0, 0, 0.025, 0),
                     n.breaks = 4) +
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
        legend.title = element_text(size = 26, color = "black"),
        panel.spacing.x = unit(11, "mm")) +
  facet_wrap(~ depth, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_ap



gg_ap2 <- ggplot(filter(df_full,
                          class %in% c("protected area")),
                   aes(x = ocd, y = reorder(categ, ocd),
                       fill = depth)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = soilpalettes::soil_palette("paleustalf", 6)) +
  labs(x = expression(SOC~stock~(Mg~ha^{-1})), fill = "Depth (cm):",
       y = NULL) +
  scale_x_continuous(expand = c(0, 0, 0.025, 0),
                     n.breaks = 4) +
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
        legend.title = element_text(size = 26, color = "black"),
        panel.spacing.x = unit(6, "mm"),
        plot.margin = grid::unit(c(0, 0.2, 0, 0), "in")) +
  facet_wrap(~ depth, scales = "free_x") +
  guides(fill = guide_legend(nrow = 1)) ; gg_ap2



ggap_g <- ggpubr::ggarrange(gg_ap, gg_ap2, common.legend = T,
                              labels = c("a)", "b)"), vjust = 33,
                              font.label = list(size = 30, face = "plain")) ; ggap_g


ggsave(ggap_g, filename = "./fig/ggap_g.png", dpi = 1200,
       width = 19, height = 10.69, units = "in")
