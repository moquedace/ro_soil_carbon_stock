pkg <- c("caret", "rgdal", "beepr", "sf", "fasterize", "stringr", "geobr",
         "readxl", "dplyr", "tmap", "tmaptools", "terra", "parallelly", "gbm",
         "parallel", "doParallel", "DescTools", "Cubist", "kknn", "kernlab",
         "tidyr", "RColorBrewer", "mpspline2", "data.table")

sapply(pkg, require, character.only = T)

rm(list = ls())  # clean memory
gc()

setwd("C:/R/ro_soil_carbon")






varfact <- paste("geology", "drainage", "curvature_classification",
                 "landforms_tpi_based", "surface_specific_points",
                 "terrain_surface_classification_iwahashi", "soil", sep = "|")

lrf <- list.files(path = "./covariaveis/prontas/present", pattern = ".tif$",
                  full.names = T) %>%
 # grep(pattern = varfact, value = T) %>%
  rast()



vars_y_sf <- st_read("./shp/spline_ocs.shp")

vars_y_vect <- st_read("./shp/spline_ocs.shp") %>%
  na.omit() %>%
  vect()




# count terra -------------------------------------------------------------
count_points <- function(r, v) {

  rst_pol <- terra::as.polygons(r)


  int <- relate(rst_pol, v, "intersects")


  df <- int %>%
    as.data.frame() %>% rowwise() %>%
    summarise(sum_count = sum(across())) %>%
    mutate(!!sym(names(rst_pol)[1]) := pull(values(rst_pol))) %>%
    relocate(names(rst_pol)[1], sum_count)

  return(df)

}


df_p <- count_points(lrf[[5]], vars_y_vect)


# count sf -------------------------------------------------------------
count_points <- function(r, psf) {

  rst_pol <- terra::as.polygons(r) %>%
    st_as_sf()


  int <- st_intersection(x = rst_pol, y = psf)


  df <- int %>%
    group_by(soil) %>%
    count()

  return(df)

}

df_p_sf <- count_points(lrf[[5]], vars_y_sf)



# count all factor --------------------------------------------------------

ldf <- list()

for (i in seq_along(names(lrf))) {

  print(names(lrf)[i])
  t1 <- Sys.time()

  ldf[[i]] <- count_points(lrf[[i]], vars_y_vect)

  print(Sys.time() - t1)

}


# vizualization -----------------------------------------------------------


# save(ldf, file = "./lista_pontos_raster_fator.RData")




load("./lista_pontos_raster_fator.RData")


ldf[[1]] %>%
  gather(cova, clas_var, -sum_count) %>%
  relocate(cova, clas_var)


dfg <- ldf %>%
  lapply(gather, key = cova, value = clas_var, -sum_count) %>%
  lapply(relocate, cova, clas_var) %>%
  rbindlist() %>%
  arrange(clas_var, cova) %>%
  mutate(clas_var = as.factor(clas_var))


ggplot(dfg, aes(x = clas_var, y = sum_count)) +
  geom_col() +
  facet_wrap(~ cova, scales = "free")





# subset ------------------------------------------------------------------


count_points(terra::subset(lrf, "valley_idx"), vars_y_vect)
ldf
