pkg <- c("geobr", "dplyr", "sf", "terra", "beepr", "rgrass7", "rgdal",
         "raster", "beepr")

sapply(pkg, require, character.only = T)

rm(list = ls())



setwd("C:/R/ro_soil_carbon")


# shp RO e buffer ---------------------------------------------------------


ro_bf <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000)


ro_bf_wgs <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015") %>%
  st_buffer(dist = 10000) %>%
  st_transform(crs = "EPSG:4326")


ro <- read_state(code_state = "RO", year = 2020) %>%
  st_transform(crs = "ESRI:102015")


plot(st_geometry(ro_bf))
plot(st_geometry(ro), add = T)


st_write(ro_bf, dsn = "./shp/ro_bf.shp", append = F)
st_write(ro, dsn = "./shp/ro.shp", append = F)
st_write(ro_bf_wgs, dsn = "./shp/ro_bf_wgs.shp", append = F)



# mosaico reproject -------------------------------------------------------


r_vrt <- list.files(path = "./covariaveis/original/dem/nasadem", pattern = ".hgt$",
                    full.names = T) %>%
  vrt()

plot(r_vrt)



mosaico_rep <- terra::project(r_vrt, y = "ESRI:102015", method = "cubicspline")


plot(mosaico_rep)
plot(st_geometry(ro_bf), add = T)

ro_bf_vect <- vect(ro_bf)

mosaico_rep_crop <- mosaico_rep %>%
  terra::crop(ro_bf_vect) %>%
  terra::mask(ro_bf_vect)


plot(mosaico_rep_crop)
plot(st_geometry(ro_bf), add = T)

names(mosaico_rep_crop) <- "dem"
writeRaster(mosaico_rep_crop, "./covariaveis/original/dem/mosaico/dem.tif",
            gdal = c("COMPRESS=LZW"), overwrite = T)


# morphometric ------------------------------------------------------------


source("C:/R/outros/scripts_aleatorios/s_fmorpho.R")


ro_dem <- rast("./covariaveis/original/dem/mosaico/dem.tif")

plot(ro_dem)
# Using function
inicio <- Sys.time()

morfometricas_saga(dem = ro_dem,
                   outdir = "./covariaveis/original/dem/morfo/",
                   align_rasters = F,
                   sol_rad = T,
                   start_date = "01/01/2019",
                   end_date = "12/31/2019",
                   verbose = T,
                   parallel = T,
                   cores = 15)

print(Sys.time() - inicio)
beep(3)



r <- list.files(path = "./covariaveis/original/dem/morfo/",
                pattern = ".tif$", full.names = T) %>%
  rast()




for (i in seq_along(names(r))) {

  print(names(r)[i])



  plot(r[[i]])

  print(summary(r[[i]]))





}





# rgrass ------------------------------------------------------------------

dem <- rast("./covariaveis/original/dem/mosaico/dem.tif")

rname <- "./covariaveis/original/dem/mosaico/dem.tif"

source("C:/R/outros/scripts_aleatorios/grass_error.R")


loc <- initGRASS("C:/OSGeo4W/apps/grass/grass78", home = tempdir(),
                 mapset = "PERMANENT", override = T)

execGRASS("g.proj", flags = c("c"),
          parameters = list(proj4 = crs(dem, proj = T)))

execGRASS("r.in.gdal", flags = "o",
          parameters = list(input = rname, output = "dem"))

execGRASS("g.region",
          parameters = list(raster = "dem", res = as.character(res(dem)[1])))



# r.fill.dir --------------------------------------------------------------
execGRASS("r.fill.dir", flags = c("overwrite", "verbose"),
          parameters = list(input = "dem",
                            direction = "direction",
                            output = "depression"))

direction <- read_RAST(vname = "direction") %>%
  rast()

names(direction) <- "direction"
plot(direction)


terra::writeRaster(direction,
                   filename = "./covariaveis/bf/direction.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))



# r.watershed -------------------------------------------------------------
execGRASS("r.watershed", flags = c("overwrite", "verbose"),
          parameters = list(elevation = "dem",
                            basin = "basin",
                            accumulation = "accumulation",
                            threshold = 500,
                            drainage ="drainage"))

basin <- read_RAST(vname = "basin") %>%
  rast()
names(basin) <- "basin"
plot(basin)


accumulation <- read_RAST(vname = "accumulation") %>%
  rast()
names(accumulation) <- "accumulation"
plot(accumulation)


drainage <- read_RAST(vname = "drainage") %>%
  rast()
names(drainage) <- "drainage"
plot(drainage)



terra::writeRaster(basin,
                   filename = "./covariaveis/bf/basin.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))

terra::writeRaster(accumulation,
                   filename = "./covariaveis/bf/accumulation.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))

terra::writeRaster(drainage,
                   filename = "./covariaveis/bf/drainage.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))











# r.stream.extract --------------------------------------------------------
execGRASS("r.stream.extract", flags = c("overwrite", "verbose"),
          parameters = list(elevation = "dem",
                            accumulation = "accumulation",
                            threshold = 500,
                            stream_raster = "stream_raster",
                            stream_vector= "stream_vector",
                            direction = "direction"))

stream_raster <- read_RAST(vname = "stream_raster") %>%
  rast()
names(stream_raster) <- "stream_raster"
plot(stream_raster)


direction <- read_RAST(vname = "direction") %>%
  rast()
plot(direction)



# r.northerness.easterness ------------------------------------------------

execGRASS("r.northerness.easterness", flags = "verbose",
          parameters = list(elevation = "dem"))

northerness <- read_RAST(vname = "dem_northerness") %>%
  rast()
names(northerness) <- "northerness"
plot(northerness)


easterness <- read_RAST(vname = "dem_easterness") %>%
  rast()
names(easterness) <- "easterness"
plot(easterness)


dem_northerness_slope <- read_RAST(vname = "dem_northerness_slope") %>%
  rast()
names(dem_northerness_slope) <- "dem_northerness_slope"
plot(dem_northerness_slope)





terra::writeRaster(northerness,
                   filename = "./covariaveis/bf/northerness.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))



terra::writeRaster(easterness,
                   filename = "./covariaveis/bf/easterness.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))



terra::writeRaster(dem_northerness_slope,
                   filename = "./covariaveis/bf/dem_northerness_slope.tif",
                   overwrite = T, gdal = c("COMPRESS=LZW"))
