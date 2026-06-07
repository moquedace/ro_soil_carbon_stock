
gc()

pkg <- c("rgdal","terra","dplyr","caret","doParallel", "ggplot2", "stringr",
         "sf", "tidyr")


sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("C:/R/ro_soil_carbon")




ln <- list.files(path = "./covariaveis/prontas/present", pattern = ".tif$",
                 full.names = F) %>%
  str_remove(".tif")


lr <- list.files(path = "./covariaveis/prontas/present", pattern = ".tif$",
                 full.names = T) %>%
  rast() %>% `names<-`(ln)


vars_y_sf <- st_read("./shp/spline_ocs.shp")

vars_y_vect <- st_read("./shp/spline_ocs.shp") %>%
  vect()




yx <- terra::extract(lr, vars_y_vect, xy = T) %>%
  cbind(vars_y_sf, .) %>%
  as.data.frame() %>%
  dplyr::select(-ID, -contains("geometry"), -x, -y, -perfil)


names(yx)



if (!file.exists("./extract_xy")) {
  dir.create("./extract_xy")
}

write.csv2(yx, file = paste0("./extract_xy/ocs_yx.csv"), row.names = F)
