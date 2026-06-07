
pkg <- c("stringr", "readxl", "dplyr", "tidyr", "ggplot2")

sapply(pkg, require, character.only = T)


setwd("C:/R/ro_soil_carbon")

rm(list = ls())  # clean memory
gc()

df33 <- read_excel("./sheet/database/ctb0033.xlsx",
                   sheet = "camada") %>%
  dplyr::select(1:2, 5:SIO2, -TIO2, -FCH, -densidade_particula_xxx,
                -`0...26`, -`0...21`)

coord_33 <- read_excel("./sheet/database/ctb0033.xlsx",
                       sheet = "observacao") %>%
  dplyr::select(observacao_id, coord_x, coord_y)



df33 <- df33 %>%
  left_join(coord_33, by = "observacao_id") %>%
  filter(!is.na(coord_y)) %>%
  dplyr::select(-S, -MO, -FE203, -AL2O3, -SIO2)


names(df33)



df34 <- read_excel("./sheet/database/ctb0034.xlsx",
                   sheet = "camada") %>%
  dplyr::select(observacao_id, camada_nome, dsi_xxx)



coord_34 <- read_excel("./sheet/database/ctb0034.xlsx",
                       sheet = "observacao") %>%
  dplyr::select(observacao_id, coord_x, coord_y)




df34 <- df34 %>%
  left_join(coord_34, by = "observacao_id")





df_ocs <- df33 %>%
  dplyr::select(observacao_id, camada_id, profund_sup, profund_inf,
                carbono_xxx_xxx_xxx, coord_x, coord_y) %>%
  rename(soc = "carbono_xxx_xxx_xxx") %>%
  mutate(t_depht = ifelse(profund_sup > profund_inf, profund_inf,
                          profund_sup),
         b_depht = ifelse(profund_inf < profund_sup, profund_sup,
                          profund_inf)) %>%
  dplyr::select(observacao_id, t_depht, b_depht,  soc,
                coord_x, coord_y) %>%
  filter(soc > 0) %>%
  arrange(observacao_id) %>%
  mutate(observacao_id = as.factor(observacao_id))

names(df_ocs)



df_prof <- df_ocs %>%
  mutate(int = paste(t_depht, b_depht, sep = "-")) %>%
  group_by(int) %>%
  count(int) %>%
  filter(n > 49)


gg_sample <- ggplot(df_prof,
                    aes(x = n, y = reorder(int, n, decreasing = T))) +
  geom_col(alpha = 0.5) +
  labs(x = "Samples", y = "Depth (cm)") +
  scale_x_continuous(expand = c(0, 0, 0.05, 0)) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        axis.text = element_text(size = 24, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) ; gg_sample



ggsave(gg_sample, filename = "./fig/gg_sample.png", dpi = 1200,
       width = 19, height = 10.69, units = "in")
