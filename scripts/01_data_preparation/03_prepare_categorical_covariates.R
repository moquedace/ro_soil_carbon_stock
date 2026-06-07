

pkg <- c("rgdal", "rgdal", "raster", "plotKML", "sf", "fasterize",
         "geobr", "readxl", "dplyr", "tmap", "tmaptools", "terra")

sapply(pkg, require, character.only = T)


setwd("C:/R/ro_soil_carbon")

rm(list=ls())  # clean memory
gc()
# prepara��o dos dados ----------------------------------------------------

# soil clas ---------------------------------------------------------------


c_soil_sf <- st_read(dsn = "./covariaveis/original/solo", layer = "SOLOS_RO") %>%
  st_set_crs("EPSG:29190") %>%
  st_transform("ESRI:102015")


dados_clas <- read_excel("covariaveis/original/solo/CLASSIFICACAO_SOLOS_ZEERO_M.xlsx")


c_soil_sf <- left_join(c_soil_sf, dados_clas, by = c("IDR_ID" = "cat"))

tm_shape(c_soil_sf) +
  tm_polygons(col = "classe_gen", border.col = NULL) +
  tm_layout(legend.outside = T)

ro_sf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform("ESRI:102015")


ro_vec <- read_state(code_state = "RO", year = 2020) %>%
  st_transform("ESRI:102015") %>% vect()

c_soil_sf <- c_soil_sf %>%
  tidyr::replace_na(list("classe_gen" = "Outros"))



c_soil_sf$CLASSE_N <- unclass(as.factor(c_soil_sf$classe_gen))

st_write(c_soil_sf, dsn = "./shp/soil_clas.shp",
         append = F)

c_soil_vec <- vect(c_soil_sf)
r_gr <- rast("./covariaveis/original/dem/mosaico/dem.tif")



c_soil_r <- terra::rasterize(x = c_soil_vec, y = r_gr, field = "CLASSE_N")

plot(c_soil_r)



c_soil_r_m <- c_soil_r %>%
  terra::crop(ro_vec) %>%
  terra::mask(ro_vec)

plot(c_soil_r_m)

gc()
#dev.new()
#tm_shape(c_soil_r_m, raster.downsample = T) +
#  tm_raster(style = "cat", palette = "Set1") +
#  tm_layout(legend.outside = T)



res(c_soil_r_m)
c(c_soil_r_m, r_gr)
names(c_soil_r_m) <- "soil"

terra::writeRaster(c_soil_r_m, filename = "./covariaveis/original/solo/soil.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))





# geology -----------------------------------------------------------------



c_geo_sf <- st_read(dsn = "covariaveis/original/geologia", layer = "geologia_orig") %>%
  st_set_crs("EPSG:29190") %>%
  st_transform("ESRI:102015")


dados_geo <- read_excel("covariaveis/original/geologia/C�DIGO GEOLOGIA.xlsx")


c_geo_sf <- left_join(c_geo_sf, dados_geo, by = c("CODIGO" = "codigo"))

tm_shape(c_geo_sf) +
  tm_polygons(col = "gen", border.col = NULL) +
  tm_layout(legend.outside = T)

ro_sf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform("ESRI:102015")


ro_vec <- read_state(code_state = "RO", year = 2020) %>%
  st_transform("ESRI:102015") %>% vect()

c_geo_sf <- c_geo_sf %>%
  tidyr::replace_na(list("gen" = "Outros"))



c_geo_sf$CLASSE_N <- unclass(as.factor(c_geo_sf$gen))

st_write(c_geo_sf, dsn = "./shp/geology.shp", append = F)

c_geo_vec <- vect(c_geo_sf)
r_gr <- rast("./covariaveis/original/dem/mosaico/dem.tif")



c_geo_r <- terra::rasterize(x = c_geo_vec, y = r_gr, field = "CLASSE_N")

plot(c_geo_r)



c_geo_r_m <- c_geo_r %>%
  terra::crop(ro_vec) %>%
  terra::mask(ro_vec)

plot(c_geo_r_m)

gc()
# dev.new()
# tm_shape(c_geo_r_m, raster.downsample = T) +
#   tm_raster(style = "cat", palette = "Set1") +
#   tm_layout(legend.outside = T)



res(c_geo_r_m)
c(c_soil_r_m, r_gr)
names(c_geo_r_m) <- "geology"


terra::writeRaster(c_geo_r_m, filename = "./covariaveis/original/geologia/geology.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))
