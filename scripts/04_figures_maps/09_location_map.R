
pkg <- c("dplyr", "ggplot2", "stringr", "tidyr", "RColorBrewer", "grid",
         "agricolae", "readr","readxl", "geobr", "rstatix", "sf", "terra",
         "RStoolbox", "colorspace", "stringr", "fastshap", "tmap",
         "parallelly", "gbm", "parallel", "doParallel", "shapviz")


sapply(pkg, require, character.only = T)

rm(list = ls())


setwd("E:/bkp_cassio/R/ro_soil_carbon")




world <- rnaturalearth::ne_countries(scale = "medium", returnclass = c("sf"))


world$name %>% unique() %>% sort()




brazil <- world %>%
  filter(name_long == "Brazil")



south_america <- world %>%
  filter(continent == "South America")



amz <- read_amazon(year = 2012)




ro <- read_state(code_state = "RO", year = 2020) %>%
  st_transform("ESRI:102015")



pts <- st_read("./shp/spline_ocs.shp") %>%
  group_by(perfil) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_intersection(ro) %>%
  dplyr::select(perfil)



bbox_br <- st_bbox(brazil)



bbox_br[2] <- bbox_br[2] * 1.6
bbox_br[4] <- bbox_br[4] * 2.5

amz$text <- "Brazil"
s_america_map <- tm_shape(world, bbox = bbox_br) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 1.5, labels.inside.frame = F, col = "black",
                labels.col = "black", alpha = 0.15) +
  tm_polygons(col = "white", lwd = 2.5,
              fill = "grey90") +
  tm_layout(bg.color = "lightblue") +
  tm_scalebar(position = c("right", "bottom"),
              text.size = 1.1, width = 15) +
  tm_shape(brazil) +


  tm_fill(fill = "grey60", lwd = 2, col = "black") +
  tm_shape(amz) +
  tm_fill(fill = "grey30", lwd = 2, col = "black") +
  tm_shape(ro) +
  tm_fill(fill = "black", lwd = 2, col = "gold") ; s_america_map










soil <- st_read(dsn = "./shp/soil_clas.shp") %>%
  st_make_valid() %>%
  group_by(clss_1n) %>%
  summarise(geometry = st_union(geometry))

col <- read_excel("./sheet/cores_solos.xlsx")

soil <- soil %>%
  left_join(col, by = c("clss_1n" = "name")) %>%
  arrange(clss_1n)


map <- tm_shape(soil) +
  tm_graticules(n.y = 5, n.x = 3, lines = T, labels.rot = c(0, 90),
                labels.size = 2.7, labels.inside.frame = T, col = "black",
                labels.col = "black", alpha = 0.15) +
  tm_fill(
    fill = "clss_1n",
    fill.scale = tm_scale_categorical(
      values = soil$cor

    ),
    fill.legend = tm_legend(
      title = "Class", na.show = F,
      position = c(0.045, 0.6),
      bg.color = "white",
      title.size = 1.2,
      text.size = 1.2
    )
  ) +
  tm_layout(inner.margins = c(0.01, 0.05, 0.01, 0.05),
            frame.lwd = 2) +
  tm_add_legend(
    type = "symbols",
    title = "",
    shape = c(22, 22, 22, 22, 21),
    fill = c("grey90", "grey60", "grey30", "black", "grey"),
    col = c("white", "black", "black", "gold", "black"),
    labels =  c("International limits",
                "Brazil",
                "Legal Amazon",
                "Rondônia",
                "Soil profile"),
    border.lwd = 2,
    text.size = 1,
    size = 2,
    position = c(0.045, 0.98),
    bg.color = "white") +
  tm_scalebar(position = c(0.05, 0.13),
              text.size = 1.2, width = 15) +
  tm_compass(type ="rose",  north = 180, lwd = 0.1,
             cardinal.directions = "S", text.size = 1.3,
             position = c("right", "top")) +
  tm_shape(ro) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(pts) +
  tm_bubbles(fill = "grey", col = "black", size = 0.75, col_alpha = 0.4,
             fill_alpha = 0.35) ; map




png(filename = "./fig/map_location.png",
    width = 19, height = 10, units = "in", res = 1200)

pushViewport(
  viewport(
    layout = grid.layout(
      1, 2,
      widths  = grid::unit(c(0.4, 1), "null")
    )
  )
)
print(
  s_america_map,
  vp = viewport(layout.pos.col = 1)
)
print(
  map,
  vp = viewport(layout.pos.col = 2)
)


dev.off()
























print(
  hawaii_map,
  vp = viewport(
    x = 0.1, y = 0.07,
    height = us_states_hawaii_ratio / sum(c(us_states_alaska_ratio, 1))
  )
)
grid.lines(x = c(0, 1), y = c(0.58, 0.58), gp = gpar(lty = 2))
grid.lines(x = c(0, 0.2), y = c(0.33, 0), gp = gpar(lty = 2))

































print(map, vp = grid::viewport(0.8, 0.185, width = 0.2, height = 0.45))





tm_raster(col.scale = tm_scale(
  style = "cat",
  midpoint = NA,

  values = "-brewer.spectral",
  values.range = c(0,1)),
  col.legend = tm_legend(
    reverse = T,
    height = 40,
    text.size = 3,
    title.size = 3,
    title = "Predictor",
    position = tm_pos_auto_out()
  )
)
tm_layout(inner.margins = c(0.01, 0.05, 0, 0),
          label.format = list(text.separator = "to",
                              decimal.mark = ".",
                              digits = 1,
                              big.mark = ",")) +
  tm_shape(keller_lg) +
  tm_borders(lwd = 2, col = "black") ; map


tmap_save(tm = map,
          filename = paste0("./laser_drone_rema/fig/er_", names(rst_unico), ".jpg"),
          dpi = 600, width = 19, units = "in", height = 22,
          outer.margins = c(0.001, 0.001, 0.001, 0.001))
