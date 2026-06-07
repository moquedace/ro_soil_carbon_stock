
pkg <- c("dplyr", "sf", "terra", "beepr", "tmap", "tmaptools", "exactextractr",
         "stringr", "tidyr", "data.table", "ggplot2", "ggridges", "ggpubr",
         "RColorBrewer","rnaturalearth", "patchwork")

sapply(pkg, require, character.only = T)

rm(list = ls())

gc()
setwd("C:/R/co2")

igh <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"




# c solo ------------------------------------------------------------------


rst_rs_rh_cs <- list.files(path = "./results_cs/dif", pattern = ".tif$",
                           full.names = T) %>%
  grep(pattern = "_rh_rs_", value = T, invert = T) %>%
  grep(pattern = "585", value = T) %>%
  rast()



rst_bio1_p <- rast("./covariaveis/present_future/present/bio1.tif") %>%
  `names<-`("bio1_p") %>%
  resample(rst_rs_rh_cs[[1]])




rst_bio1_delta_585 <- rast("./covariaveis/dif/bio1_change_585.tif") %>%
  `names<-`("bio1_delta_585") %>%
  resample(rst_rs_rh_cs[[1]])




rst_cs <- c(rst_rs_rh_cs, rst_bio1_p, rst_bio1_delta_585)


df_cs <- terra::as.data.frame(rst_cs)



head(df_cs)



df_g_cs <- df_cs %>%
#  sample_n(100000) %>%
  gather(key = "var", value = "v_resp", -c("bio1_p", "bio1_delta_585"))


head(df_g_cs)



ggplot(df_g_cs, aes(y = v_resp, x = bio1_p)) +
  geom_point() +
  facet_wrap(~resp, scales = "free")






ggplot(df_g_cs, aes(y = v_resp, x = bio1_delta_585)) +
  geom_point() +
  facet_wrap(~resp, scales = "free")





# s solo ------------------------------------------------------------------


rst_rs_rh_ss <- list.files(path = "./results_ss/dif", pattern = ".tif$",
                           full.names = T) %>%
  grep(pattern = "_rh_rs_", value = T, invert = T) %>%
  grep(pattern = "585", value = T) %>%
  rast()



rst_bio1_p <- rast("./covariaveis/present_future/present/bio1.tif") %>%
  `names<-`("bio1_p") %>%
  resample(rst_rs_rh_ss[[1]])




rst_bio1_delta_585 <- rast("./covariaveis/dif/bio1_change_585.tif") %>%
  `names<-`("bio1_delta_585") %>%
  resample(rst_rs_rh_ss[[1]])




rst_ss <- c(rst_rs_rh_ss, rst_bio1_p, rst_bio1_delta_585)


df_ss <- terra::as.data.frame(rst_ss)



head(df_ss)



df_g_ss <- df_ss %>%
 # sample_n(100000) %>%
  gather(key = "var", value = "v_resp", -c("bio1_p", "bio1_delta_585"))


head(df_g_ss)



ggplot(df_g_ss, aes(y = v_resp, x = bio1_p)) +
  geom_point() +
  facet_wrap(~resp, scales = "free")






ggplot(df_g_ss, aes(y = v_resp, x = bio1_delta_585)) +
  geom_point() +
  facet_wrap(~resp, scales = "free")





# group -------------------------------------------------------------------

df_g_cs$type <- "+soil"
df_g_ss$type <- "-soil"



dfg <- rbind(df_g_cs, df_g_ss) %>%
  as.data.table()





seq_bio_p <- round(seq(min(dfg$bio1_delta_585),
                       max(dfg$bio1_delta_585), 1), 0)






dfg_int <- dfg %>%
  mutate(int_bio_pres = factor(case_when(bio1_delta_585 < seq_bio_p[2] ~ "0-1",
                                         bio1_delta_585 >= seq_bio_p[2] & bio1_delta_585 < seq_bio_p[3] ~ "1-2",
                                         bio1_delta_585 >= seq_bio_p[3] & bio1_delta_585 < seq_bio_p[4] ~ "2-3",
                                         bio1_delta_585 >= seq_bio_p[4] & bio1_delta_585 < seq_bio_p[5] ~ "3-4",
                                         bio1_delta_585 >= seq_bio_p[5] & bio1_delta_585 < seq_bio_p[6] ~ "4-5",
                                         bio1_delta_585 >= seq_bio_p[6] & bio1_delta_585 < seq_bio_p[7] ~ "5-6",
                                         bio1_delta_585 >= seq_bio_p[7] & bio1_delta_585 < seq_bio_p[8] ~ "6-7",
                                         bio1_delta_585 >= seq_bio_p[8] & bio1_delta_585 < seq_bio_p[9] ~ "7-8",
                                         bio1_delta_585 >= seq_bio_p[9] & bio1_delta_585 < seq_bio_p[10] ~ "8-9",
                                         bio1_delta_585 >= seq_bio_p[10] & bio1_delta_585 < seq_bio_p[11] ~ "9-10",
                                         bio1_delta_585 >= seq_bio_p[11] & bio1_delta_585 < seq_bio_p[12] ~ "10-11",
                                         bio1_delta_585 >= seq_bio_p[12] & bio1_delta_585 < seq_bio_p[13] ~ "11-12",
                                         bio1_delta_585 >= seq_bio_p[13] & bio1_delta_585 < seq_bio_p[14] ~ "12-13",
                                         bio1_delta_585 >= seq_bio_p[14] ~ "13-14",
                                         TRUE ~ NA_character_),
                               levels = paste0(seq_bio_p[1:14], "-", seq_bio_p[2:15]))) %>%
  group_by(int_bio_pres, type, var) %>%
  summarise_at(.funs = list(media = mean, desvio = sd), .vars = vars(bio1_delta_585, bio1_delta_585, v_resp)) %>%
  mutate(type = factor(type,levels = c("-soil", "+soil")),
         var = factor(recode(var,
                             rs_media_2071_2100_585 = "Delta~Rs~(g~C~m^{-2}~year^{-1})",
                             rh_media_2071_2100_585 = "Delta~Rh~(g~C~m^{-2}~year^{-1})"),
                      levels = c("Delta~Rs~(g~C~m^{-2}~year^{-1})",
                                 "Delta~Rh~(g~C~m^{-2}~year^{-1})"))) %>%
  as.data.table()


# save(dfg_int, file = "./results_cs/img_out/temper_class_summarise.RData")

head(dfg_int)
names(dfg_int)




ggplot(dfg_int, aes(x = int_bio_pres, y = v_resp_media,
                    col = type, group = type)) +

  geom_ribbon(aes(ymax = v_resp_media + v_resp_desvio, ymin = v_resp_media,
                  col = NULL, fill = type),
              alpha = 0.2) +
  geom_line(lwd = 2, lineend = "round") +
  geom_point(size = 3) +
  labs(x = expression(Delta~BIO~1~(ºC)), y = NULL, fill = NULL, col = NULL) +
  scale_color_manual(values = colorRampPalette(brewer.pal(11, "Spectral"))(11)[c(2, 10)]) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Spectral"))(11)[c(2, 10)]) +
  theme(strip.background = element_blank(),
        # axis.ticks.x = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        axis.text = element_text(size = 26, color = "black"),
        axis.title = element_text(size = 26, color = "black"),
        strip.text = element_text(size = 26, color = "black"),
        legend.text = element_text(size = 30, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) +
  facet_wrap(~var, strip.position = "left", labeller = label_parsed)














# intervalo ---------------------------------------------------------------

seq_bio_p <- round(seq(min(dfg$bio1_p), max(dfg$bio1_p), 5), 0)









dfg_int <- dfg %>%
  mutate(int_bio_pres = factor(case_when(bio1_p  >= min(dfg$bio1_p) & bio1_p < seq_bio_p[2] ~ "-30 a -25",
                                         bio1_p >= seq_bio_p[2] & bio1_p < seq_bio_p[3] ~ "-25 a -20",
                                         bio1_p >= seq_bio_p[3] & bio1_p < seq_bio_p[4] ~ "-20 a -15",
                                         bio1_p >= seq_bio_p[4] & bio1_p < seq_bio_p[5] ~ "-15 a -10",
                                         bio1_p >= seq_bio_p[5] & bio1_p < seq_bio_p[6] ~ "-10 a -5",
                                         bio1_p >= seq_bio_p[6] & bio1_p < seq_bio_p[7] ~ "-5 a 0",
                                         bio1_p >= seq_bio_p[7] & bio1_p < seq_bio_p[8] ~ "0 a 5",
                                         bio1_p >= seq_bio_p[8] & bio1_p < seq_bio_p[9] ~ "5 a 10",
                                         bio1_p >= seq_bio_p[9] & bio1_p < seq_bio_p[10] ~ "10 a 15",
                                         bio1_p >= seq_bio_p[10] & bio1_p < seq_bio_p[11] ~ "15 a 20",
                                         bio1_p >= seq_bio_p[11] & bio1_p < seq_bio_p[12] ~ "20 a 25",
                                         bio1_p >= seq_bio_p[12] & bio1_p  <= max(dfg$bio1_p) ~ "25 a 30",
                                         TRUE ~ NA_character_),
                               levels = c("-30 a -25", "-25 a -20", "-20 a -15", "-15 a -10", "-10 a -5",
                                          "-5 a 0", "0 a 5", "5 a 10", "10 a 15", "15 a 20", "20 a 25",
                                          "25 a 30"))) %>%
  group_by(int_bio_pres, type, resp) %>%
  summarise_at(.funs = list(media = mean, desvio = sd), .vars = vars(bio1_p, bio1_delta_585, v_resp))















seq_bio_p <- round(seq(min(dfg$bio1_p), max(dfg$bio1_p), 2), 0)











dfg_int <- dfg %>%
  mutate(int_bio_pres = factor(case_when(bio1_p  >= min(dfg$bio1_p) & bio1_p < seq_bio_p[2] ~ "-30 a -28",
                                         bio1_p >= seq_bio_p[2] & bio1_p < seq_bio_p[3] ~ "-28 a -26",
                                         bio1_p >= seq_bio_p[3] & bio1_p < seq_bio_p[4] ~ "-26 a -24",
                                         bio1_p >= seq_bio_p[4] & bio1_p < seq_bio_p[5] ~ "-24 a -22",
                                         bio1_p >= seq_bio_p[5] & bio1_p < seq_bio_p[6] ~ "-22 a -20",
                                         bio1_p >= seq_bio_p[6] & bio1_p < seq_bio_p[7] ~ "-20 a -18",
                                         bio1_p >= seq_bio_p[7] & bio1_p < seq_bio_p[8] ~ "-18 a -16",
                                         bio1_p >= seq_bio_p[8] & bio1_p < seq_bio_p[9] ~ "-16 a -14",
                                         bio1_p >= seq_bio_p[9] & bio1_p < seq_bio_p[10] ~ "-14 a -12",
                                         bio1_p >= seq_bio_p[10] & bio1_p < seq_bio_p[11] ~ "-12 a -10",
                                         bio1_p >= seq_bio_p[11] & bio1_p < seq_bio_p[12] ~ "-10 a -8",

                                         bio1_p >= seq_bio_p[12] & bio1_p < seq_bio_p[13] ~ "-8 a -6",
                                         bio1_p >= seq_bio_p[13] & bio1_p < seq_bio_p[14] ~ "-6 a -4",
                                         bio1_p >= seq_bio_p[14] & bio1_p < seq_bio_p[15] ~ "-4 a -2",
                                         bio1_p >= seq_bio_p[15] & bio1_p < seq_bio_p[16] ~ "-2 a 0",
                                         bio1_p >= seq_bio_p[16] & bio1_p < seq_bio_p[17] ~ "0 a 2",
                                         bio1_p >= seq_bio_p[17] & bio1_p < seq_bio_p[18] ~ "2 a 4",
                                         bio1_p >= seq_bio_p[18] & bio1_p < seq_bio_p[19] ~ "4 a 6",
                                         bio1_p >= seq_bio_p[19] & bio1_p < seq_bio_p[20] ~ "6 a 8",
                                         bio1_p >= seq_bio_p[20] & bio1_p < seq_bio_p[21] ~ "8 a 10",
                                         bio1_p >= seq_bio_p[21] & bio1_p < seq_bio_p[22] ~ "10 a 12",
                                         bio1_p >= seq_bio_p[22] & bio1_p < seq_bio_p[23] ~ "12 a 14",
                                         bio1_p >= seq_bio_p[23] & bio1_p < seq_bio_p[24] ~ "14 a 16",
                                         bio1_p >= seq_bio_p[24] & bio1_p < seq_bio_p[25] ~ "16 a 18",
                                         bio1_p >= seq_bio_p[25] & bio1_p < seq_bio_p[26] ~ "18 a 20",
                                         bio1_p >= seq_bio_p[26] & bio1_p < seq_bio_p[27] ~ "20 a 22",
                                         bio1_p >= seq_bio_p[27] & bio1_p < seq_bio_p[28] ~ "22 a 24",
                                         bio1_p >= seq_bio_p[28] & bio1_p < seq_bio_p[29] ~ "24 a 26",
                                         bio1_p >= seq_bio_p[29] & bio1_p < seq_bio_p[30] ~ "26 a 28",
                                         bio1_p >= seq_bio_p[30] & bio1_p < seq_bio_p[31] ~ "28 a 30",
                                         bio1_p >= seq_bio_p[31] & bio1_p < seq_bio_p[32] ~ "30 a 32",
                                          bio1_p >= seq_bio_p[32] & bio1_p  <= max(dfg$bio1_p) ~ "32 a 34",
                                         TRUE ~ NA_character_),
                               levels = paste0(seq_bio_p[1:32], " a ", seq_bio_p[2:33]))) %>%
  group_by(int_bio_pres, type, resp) %>%
  summarise_at(.funs = list(media = mean, desvio = sd), .vars = vars(bio1_p, bio1_delta_585, v_resp))




paste0(seq_bio_p[1:32], " a ", seq_bio_p[2:33])









head(dfg_int)
names(dfg_int)



ggplot(dfg, aes(y = v_resp, x = bio1_delta_585, col = type)) +
  geom_point(alpha = 0.05) +
  facet_wrap(~resp)



ggplot(dfg_int, aes(x = int_bio_pres, y = v_resp_media,
                       col = type, group = type)) +

  geom_ribbon(aes(ymax = v_resp_media + v_resp_desvio, ymin = v_resp_media, col = NULL, fill = type),
              alpha = 0.2) +
  geom_line(lwd = 2, lineend = "round") +
  geom_point(size = 3) +
  scale_color_manual(values = colorRampPalette(brewer.pal(11, "Spectral"))(11)[c(2, 10)]) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Spectral"))(11)[c(2, 10)]) +
  facet_wrap(~resp)
