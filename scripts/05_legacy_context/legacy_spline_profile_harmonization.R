
### INITIALIZATION ####
rm(list=ls())  # clean memory
## load packages

library(plyr)
library(sp)
library(Hmisc)
library(lattice)
library(MASS)

library(rgdal)
library(devtools)
library(maptools)
library(base)
library(RSAGA)
library(e1071)
library(boot)
library(rgdal)
library(plotKML)
library(e1071)
library(gstat)
library(dplyr)
library(aqp)
library(GSIF)

library(sp)

gc()



setwd("C:/Usuario/Ganso/mde/dados")

rdSoil <- read.csv2("dados_base.csv")


dados_base = rdSoil


names(dados_base )
dados_base$PROF_I = as.character(dados_base$PROF_I)
dados_base$PROF_I = as.numeric(dados_base$PROF_I)

dados_base$PRO_F = as.character(dados_base$PRO_F)
dados_base$PRO_F = as.numeric(dados_base$PRO_F)

dados_base$ARGI = as.character(dados_base$ARGI)
dados_base$ARGI = as.numeric(dados_base$ARGI)

plot(dados_base$Longitude, dados_base$Latitude)

dados_base_d = dados_base %>% select("PROF_I","PRO_F", "ARGI", "Bulk","COT","Longitude", "Latitude")

dados_base_d1 = dados_base_d %>%  filter(!(is.na(dados_base_d$Latitude)))


coordinates(dados_base_d1)<- ~Longitude+Latitude
plot(dados_base_d1)
dados_base_d1@proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


CRS.new <- CRS("+proj=lcc +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs ")


dados_base_d1<- spTransform(dados_base_d1, CRS.new)
plot(dados_base_d1)

dados_base_final = cbind(dados_base_d1@data,dados_base_d1@coords)



dados_base_final_reg = dados_base_final %>% select("ARGI", "Bulk","COT")
dados_base_final_reg =na.omit(dados_base_final_reg )

rgm <- lm(formula = Bulk ~  ARGI + COT , data =dados_base_final_reg)
summary(rgm)


dados_base_final$BD_predicted = (rgm$coefficients[1] + rgm$coefficients[3]*(dados_base_final$COT) + rgm$coefficients[2]*(dados_base_final$ARGI))
dados_base_final = dados_base_final %>% filter(BD_predicted>0.78)
dados_base_final$Bulk =NULL
dados_base_final = na.omit(dados_base_final)




dados_base_finalcot = dados_base_final
dados_base_finalcot$id =paste(dados_base_finalcot$Longitude, "_", dados_base_finalcot$Latitude , sep="")





dados_base_finalcot <- dados_base_finalcot[order(dados_base_finalcot$id,dados_base_finalcot$PROF_I),]
depths(dados_base_finalcot) <- id ~ PROF_I + PRO_F


#----------------------------------------------------------------------0-5------------------------------------------



BD.spline_cot0_5 <-mpspline(dados_base_finalcot, var.name ="COT",
                            lam = 0.1, d = t(c(0,5)), vlow = 0,

                            vhigh = 1000, show.progress=TRUE)
site(dados_base_finalcot) <- ~ Longitude # E = longitute = x
site(dados_base_finalcot) <- ~ Latitude #  N  = latitude = y
dataframe_xy <- as.data.frame(dados_base_finalcot@site)
str(dataframe_xy)
#table(dataframe_xy$id)

BD.spline_cot0_5_s <- as.data.frame(BD.spline_cot0_5$var.std)
BD.spline_cot0_5_s$id <- (BD.spline_cot0_5$idcol)
str(BD.spline_cot0_5_s)
BD.spline_cot0_5_s <- join(BD.spline_cot0_5_s, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot0_5_s)[1] = "COT"



BD.spline_cot0_5 <-mpspline(dados_base_finalcot, var.name ="BD_predicted",
                            lam = 0.1, d = t(c(0,5)), vlow = 0,

                            vhigh = 1000, show.progress=TRUE)


BD.spline_cot0_5_d <- as.data.frame(BD.spline_cot0_5$var.std)
BD.spline_cot0_5_d$id <- (BD.spline_cot0_5$idcol)
str(BD.spline_cot0_5_d)
BD.spline_cot0_5_d <- join(BD.spline_cot0_5_d, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot0_5_d)[1] = "Dens"

BD.spline_cot0_5_uni = merge(BD.spline_cot0_5_s, BD.spline_cot0_5_d, by="id")

BD.spline_cot0_5_uni$texlat = BD.spline_cot0_5_uni$Latitude.x == BD.spline_cot0_5_uni$Latitude.y
BD.spline_cot0_5_uni$texlong = BD.spline_cot0_5_uni$Longitude.x == BD.spline_cot0_5_uni$Longitude.y
table(BD.spline_cot0_5_uni$texlat)
table(BD.spline_cot0_5_uni$texlong)

BD.spline_cot0_5_uni$texlong =NULL
BD.spline_cot0_5_uni$texlat =NULL
BD.spline_cot0_5_uni$Latitude.y =NULL
BD.spline_cot0_5_uni$Longitude.y =NULL

BD.spline_cot0_5_uni$`soil depth.y` =NULL
BD.spline_cot0_5_uni$stock = (BD.spline_cot0_5_uni$COT*BD.spline_cot0_5_uni$Dens*5)/10

BD.spline_cot0_5_uni$Dens=NULL
BD.spline_cot0_5_uni$id=NULL
BD.spline_cot0_5_uni$COT=NULL
BD.spline_cot0_5_uni$`soil depth.x`=NULL

names(BD.spline_cot0_5_uni)[1]="x"
names(BD.spline_cot0_5_uni)[2]="y"

coordinates(BD.spline_cot0_5_uni)<- ~x +y
graphics.off()
plot(BD.spline_cot0_5_uni)



writePointsShape(BD.spline_cot0_5_uni, "./shape_ok/stock_0_5.shp")


writeOGR(BD.spline_cot0_5_uni, dsn = "./shape_ok" , layer = "stock_0_5",driver="ESRI Shapefile")


#----------------------------------------------------------------------5-15------------------------------------------



BD.spline_cot5_15 <-mpspline(dados_base_finalcot, var.name ="COT",
                             lam = 0.1, d = t(c(5,15)), vlow = 0,

                             vhigh = 1000, show.progress=TRUE)
site(dados_base_finalcot) <- ~ Longitude # E = longitute = x
site(dados_base_finalcot) <- ~ Latitude #  N  = latitude = y
dataframe_xy <- as.data.frame(dados_base_finalcot@site)
str(dataframe_xy)
#table(dataframe_xy$id)

BD.spline_cot5_15_s <- as.data.frame(BD.spline_cot5_15$var.std)
BD.spline_cot5_15_s$id <- (BD.spline_cot5_15$idcol)
str(BD.spline_cot5_15_s)
BD.spline_cot5_15_s <- join(BD.spline_cot5_15_s, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot5_15_s)[1] = "COT"



BD.spline_cot5_15 <-mpspline(dados_base_finalcot, var.name ="BD_predicted",
                             lam = 0.1, d = t(c(5,15)), vlow = 0,

                             vhigh = 1000, show.progress=TRUE)


BD.spline_cot5_15_d <- as.data.frame(BD.spline_cot5_15$var.std)
BD.spline_cot5_15_d$id <- (BD.spline_cot5_15$idcol)
str(BD.spline_cot5_15_d)
BD.spline_cot5_15_d <- join(BD.spline_cot5_15_d, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot5_15_d)[1] = "Dens"

BD.spline_cot5_15_uni = merge(BD.spline_cot5_15_s, BD.spline_cot5_15_d, by="id")

BD.spline_cot5_15_uni$texlat = BD.spline_cot5_15_uni$Latitude.x == BD.spline_cot5_15_uni$Latitude.y
BD.spline_cot5_15_uni$texlong = BD.spline_cot5_15_uni$Longitude.x == BD.spline_cot5_15_uni$Longitude.y
table(BD.spline_cot5_15_uni$texlat)
table(BD.spline_cot5_15_uni$texlong)

BD.spline_cot5_15_uni$texlong =NULL
BD.spline_cot5_15_uni$texlat =NULL
BD.spline_cot5_15_uni$Latitude.y =NULL
BD.spline_cot5_15_uni$Longitude.y =NULL

BD.spline_cot5_15_uni$`soil depth.y` =NULL
BD.spline_cot5_15_uni$stock = (BD.spline_cot5_15_uni$COT*BD.spline_cot5_15_uni$Dens*10)/10

BD.spline_cot5_15_uni$Dens=NULL
BD.spline_cot5_15_uni$id=NULL
BD.spline_cot5_15_uni$COT=NULL
BD.spline_cot5_15_uni$`soil depth.x`=NULL

names(BD.spline_cot5_15_uni)[1]="x"
names(BD.spline_cot5_15_uni)[2]="y"

coordinates(BD.spline_cot5_15_uni)<- ~x +y
graphics.off()
plot(BD.spline_cot5_15_uni)



writePointsShape(BD.spline_cot5_15_uni, "./shape_ok/stock_5_15.shp")


writeOGR(BD.spline_cot5_15_uni, dsn = "./shape_ok" , layer = "stock_5_15",driver="ESRI Shapefile")



#----------------------------------------------------------------------15-30------------------------------------------



BD.spline_cot15_30 <-mpspline(dados_base_finalcot, var.name ="COT",
                              lam = 0.1, d = t(c(15,30)), vlow = 0,

                              vhigh = 1000, show.progress=TRUE)
site(dados_base_finalcot) <- ~ Longitude # E = longitute = x
site(dados_base_finalcot) <- ~ Latitude #  N  = latitude = y
dataframe_xy <- as.data.frame(dados_base_finalcot@site)
str(dataframe_xy)
#table(dataframe_xy$id)

BD.spline_cot15_30_s <- as.data.frame(BD.spline_cot15_30$var.std)
BD.spline_cot15_30_s$id <- (BD.spline_cot15_30$idcol)
str(BD.spline_cot15_30_s)
BD.spline_cot15_30_s <- join(BD.spline_cot15_30_s, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot15_30_s)[1] = "COT"



BD.spline_cot15_30 <-mpspline(dados_base_finalcot, var.name ="BD_predicted",
                              lam = 0.1, d = t(c(15,30)), vlow = 0,

                              vhigh = 1000, show.progress=TRUE)


BD.spline_cot15_30_d <- as.data.frame(BD.spline_cot15_30$var.std)
BD.spline_cot15_30_d$id <- (BD.spline_cot15_30$idcol)
str(BD.spline_cot15_30_d)
BD.spline_cot15_30_d <- join(BD.spline_cot15_30_d, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot15_30_d)[1] = "Dens"

BD.spline_cot15_30_uni = merge(BD.spline_cot15_30_s, BD.spline_cot15_30_d, by="id")

BD.spline_cot15_30_uni$texlat = BD.spline_cot15_30_uni$Latitude.x == BD.spline_cot15_30_uni$Latitude.y
BD.spline_cot15_30_uni$texlong = BD.spline_cot15_30_uni$Longitude.x == BD.spline_cot15_30_uni$Longitude.y
table(BD.spline_cot15_30_uni$texlat)
table(BD.spline_cot15_30_uni$texlong)

BD.spline_cot15_30_uni$texlong =NULL
BD.spline_cot15_30_uni$texlat =NULL
BD.spline_cot15_30_uni$Latitude.y =NULL
BD.spline_cot15_30_uni$Longitude.y =NULL

BD.spline_cot15_30_uni$`soil depth.y` =NULL
BD.spline_cot15_30_uni$stock = (BD.spline_cot15_30_uni$COT*BD.spline_cot15_30_uni$Dens*15)/10

BD.spline_cot15_30_uni$Dens=NULL
BD.spline_cot15_30_uni$id=NULL
BD.spline_cot15_30_uni$COT=NULL
BD.spline_cot15_30_uni$`soil depth.x`=NULL

names(BD.spline_cot15_30_uni)[1]="x"
names(BD.spline_cot15_30_uni)[2]="y"

coordinates(BD.spline_cot15_30_uni)<- ~x +y
graphics.off()
plot(BD.spline_cot15_30_uni)



writePointsShape(BD.spline_cot15_30_uni, "./shape_ok/stock_15_30.shp")


writeOGR(BD.spline_cot5_15_uni, dsn = "./shape_ok" , layer = "stock_5_15",driver="ESRI Shapefile")



#----------------------------------------------------------------------30-60------------------------------------------



BD.spline_cot30_60 <-mpspline(dados_base_finalcot, var.name ="COT",
                              lam = 0.1, d = t(c(30,60)), vlow = 0,

                              vhigh = 1000, show.progress=TRUE)
#site(dados_base_finalcot) <- ~ Longitude # E = longitute = x
#site(dados_base_finalcot) <- ~ Latitude #  N  = latitude = y
dataframe_xy <- as.data.frame(dados_base_finalcot@site)
str(dataframe_xy)
#table(dataframe_xy$id)

BD.spline_cot30_60_s <- as.data.frame(BD.spline_cot30_60$var.std)
BD.spline_cot30_60_s$id <- (BD.spline_cot30_60$idcol)
str(BD.spline_cot30_60_s)
BD.spline_cot30_60_s <- join(BD.spline_cot30_60_s, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot30_60_s)[1] = "COT"



BD.spline_cot30_60 <-mpspline(dados_base_finalcot, var.name ="BD_predicted",
                              lam = 0.1, d = t(c(30,60)), vlow = 0,

                              vhigh = 1000, show.progress=TRUE)


BD.spline_cot30_60_d <- as.data.frame(BD.spline_cot30_60$var.std)
BD.spline_cot30_60_d$id <- (BD.spline_cot30_60$idcol)
str(BD.spline_cot30_60_d)
BD.spline_cot30_60_d <- join(BD.spline_cot30_60_d, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot30_60_d)[1] = "Dens"

BD.spline_cot30_60_uni = merge(BD.spline_cot30_60_s, BD.spline_cot30_60_d, by="id")

BD.spline_cot30_60_uni$texlat = BD.spline_cot30_60_uni$Latitude.x == BD.spline_cot30_60_uni$Latitude.y
BD.spline_cot30_60_uni$texlong = BD.spline_cot30_60_uni$Longitude.x == BD.spline_cot30_60_uni$Longitude.y
table(BD.spline_cot30_60_uni$texlat)
table(BD.spline_cot30_60_uni$texlong)

BD.spline_cot30_60_uni$texlong =NULL
BD.spline_cot30_60_uni$texlat =NULL
BD.spline_cot30_60_uni$Latitude.y =NULL
BD.spline_cot30_60_uni$Longitude.y =NULL

BD.spline_cot30_60_uni$`soil depth.y` =NULL
BD.spline_cot30_60_uni$stock = (BD.spline_cot30_60_uni$COT*BD.spline_cot30_60_uni$Dens*30)/10

BD.spline_cot30_60_uni$Dens=NULL
BD.spline_cot30_60_uni$id=NULL
BD.spline_cot30_60_uni$COT=NULL
BD.spline_cot30_60_uni$`soil depth.x`=NULL

names(BD.spline_cot30_60_uni)[1]="x"
names(BD.spline_cot30_60_uni)[2]="y"

coordinates(BD.spline_cot30_60_uni)<- ~x +y
graphics.off()
plot(BD.spline_cot30_60_uni)



writePointsShape(BD.spline_cot30_60_uni, "./shape_ok/stock_30_60.shp")


writeOGR(BD.spline_cot5_15_uni, dsn = "./shape_ok" , layer = "stock_5_15",driver="ESRI Shapefile")


#----------------------------------------------------------------------60-100------------------------------------------



BD.spline_cot60_100 <-mpspline(dados_base_finalcot, var.name ="COT",
                               lam = 0.1, d = t(c(60,100)), vlow = 0,

                               vhigh = 1000, show.progress=TRUE)
#site(dados_base_finalcot) <- ~ Longitude # E = longitute = x
#site(dados_base_finalcot) <- ~ Latitude #  N  = latitude = y
dataframe_xy <- as.data.frame(dados_base_finalcot@site)
str(dataframe_xy)
#table(dataframe_xy$id)

BD.spline_cot60_100_s <- as.data.frame(BD.spline_cot60_100$var.std)
BD.spline_cot60_100_s$id <- (BD.spline_cot60_100$idcol)
str(BD.spline_cot60_100_s)
BD.spline_cot60_100_s <- join(BD.spline_cot60_100_s, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot60_100_s)[1] = "COT"



BD.spline_cot60_100 <-mpspline(dados_base_finalcot, var.name ="BD_predicted",
                               lam = 0.1, d = t(c(60,100)), vlow = 0,

                               vhigh = 1000, show.progress=TRUE)


BD.spline_cot60_100_d <- as.data.frame(BD.spline_cot60_100$var.std)
BD.spline_cot60_100_d$id <- (BD.spline_cot60_100$idcol)
str(BD.spline_cot60_100_d)
BD.spline_cot60_100_d <- join(BD.spline_cot60_100_d, dataframe_xy, by = "id", type = "left", match = "all")
names(BD.spline_cot60_100_d)[1] = "Dens"

BD.spline_cot60_100_uni = merge(BD.spline_cot60_100_s, BD.spline_cot60_100_d, by="id")

BD.spline_cot60_100_uni$texlat = BD.spline_cot60_100_uni$Latitude.x == BD.spline_cot60_100_uni$Latitude.y
BD.spline_cot60_100_uni$texlong = BD.spline_cot60_100_uni$Longitude.x == BD.spline_cot60_100_uni$Longitude.y
table(BD.spline_cot60_100_uni$texlat)
table(BD.spline_cot60_100_uni$texlong)

BD.spline_cot60_100_uni$texlong =NULL
BD.spline_cot60_100_uni$texlat =NULL
BD.spline_cot60_100_uni$Latitude.y =NULL
BD.spline_cot60_100_uni$Longitude.y =NULL

BD.spline_cot60_100_uni$`soil depth.y` =NULL
BD.spline_cot60_100_uni$stock = (BD.spline_cot60_100_uni$COT*BD.spline_cot60_100_uni$Dens*40)/10

BD.spline_cot60_100_uni$Dens=NULL
BD.spline_cot60_100_uni$id=NULL
BD.spline_cot60_100_uni$COT=NULL
BD.spline_cot60_100_uni$`soil depth.x`=NULL

names(BD.spline_cot60_100_uni)[1]="x"
names(BD.spline_cot60_100_uni)[2]="y"

coordinates(BD.spline_cot60_100_uni)<- ~x +y
graphics.off()
plot(BD.spline_cot60_100_uni)



writePointsShape(BD.spline_cot60_100_uni, "./shape_ok/stock_60_100.shp")


#writeOGR(BD.spline_cot5_15_uni, dsn = "./shape_ok" , layer = "stock_5_15",driver="ESRI Shapefile")