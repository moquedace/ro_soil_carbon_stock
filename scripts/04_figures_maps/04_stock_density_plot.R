
pkg <- c("geobr", "sf", "dplyr", "stringr", "tmap", "tmaptools",
         "readxl", "ggplot2", "RColorBrewer", "ggridges", "scales",
         "tidyr")

sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("D:/Usuario/cassio/R/ro_soil_carbon/")


options(scipen = 999)






stk <- st_read(dsn = "./shp/spline_ocs.shp") %>%
  as.data.frame() %>%
  select(-geometry, -perfil) %>%
  gather(key = "depth", value = "value") %>%
  na.omit() %>%
  mutate(depth = factor(recode(depth,
                               "OCS_0_5" = "0-5",
                               "OCS_5_15" = "5-15",
                               "OCS_30_60" = "30-60",
                               "OCS_15_30" = "15-30",
                               "OCS_60_100" = "60-100"),
                        levels = c("0-5", "5-15", "15-30", "30-60", "60-100")))






stock_geral_p <- ggplot(stk, aes(x = value, y = depth,
                                         fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = 4, scale = 3) +
  scale_x_continuous(n.breaks = 8, position = "top", limits = c(-4, 70),
                     expand = c(0, 0.35)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = alpha(soilpalettes::soil_palette("vitrixerand", 4),
                                   0.5),
                    labels = c("0-25%", "25-50%", "50-75%", "75-100%")) +
  labs(x = expression(SOC~stock~(Mg~ha^{-1})), y = "Depth (cm)",
       fill = "Quantiles:") +
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
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.placement = "output") +
  guides(alpha = "none") ; stock_geral_p


ggsave(stock_geral_p, filename = "./fig/stock_density.png", dpi = 1200,
       width = 19, height = 10.69, units = "in")
