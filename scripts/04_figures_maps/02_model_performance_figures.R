



pkg <- c("dplyr", "ggplot2", "stringr", "tidyr", "RColorBrewer", "data.table")


sapply(pkg, require, character.only = T)

rm(list = ls())

setwd("D:/Usuario/cassio/R/ro_soil_carbon/")

options(scipen=999)




# perfomance --------------------------------------------------------------



df <- list.files(path = "./", pattern = ".csv$", full.names = T, recursive = T) %>%
  grep(pattern = "_performance.csv", value = T) %>%
  grep(pattern = "dsi_xxx_", value = T, invert = T) %>%
  lapply(read.csv2) %>%
  rbindlist() %>%
  dplyr::select(-n_train, -n_test) %>%
  gather(key = "metric", value = "value", -c("model", "var")) %>%
  mutate(model = ifelse(metric %in% c("MAE_null_train", "RMSE_null_train",
                                      "MAE_null_test", "RMSE_null_test"),
                        "null model", model),
         data = factor(dplyr::recode(metric,
                                     MAE_train = "Train",
                                     RMSE_train = "Train",
                                     NSE_train = "Train",
                                     PBIAS_train = "Train",
                                     aPBIAS_train = "Train",
                                     Rsquared_train = "Train",
                                     CCC_train = "Train",
                                     MAE_test = "Test",
                                     RMSE_test = "Test",
                                     NSE_test = "Test",
                                     PBIAS_test = "Test",
                                     aPBIAS_test = "Test",
                                     Rsquared_test = "Test",
                                     CCC_test = "Test",
                                     MAE_null_train = "Train",
                                     RMSE_null_train = "Train",
                                     MAE_null_test = "Test",
                                     RMSE_null_test = "Test"),
                       levels = c("Train", "Test")),
         metric = dplyr::recode(metric,
                                MAE_train = "MAE~(Mg~C~ha^-1)",
                                RMSE_train = "RMSE~(Mg~C~ha^-1)",
                                NSE_train = "NSE",
                                PBIAS_train = "PBIAS~(`%`)",
                                aPBIAS_train = "aPBIAS~(`%`)",
                                Rsquared_train = "R^2",
                                CCC_train = "rho[c]",
                                MAE_test = "MAE~(Mg~C~ha^-1)",
                                RMSE_test = "RMSE~(Mg~C~ha^-1)",
                                NSE_test = "NSE",
                                PBIAS_test = "PBIAS~(`%`)",
                                aPBIAS_test = "aPBIAS~(`%`)",
                                Rsquared_test = "R^2",
                                CCC_test = "rho[c]",
                                MAE_null_train = "MAE~(Mg~C~ha^-1)",
                                RMSE_null_train = "RMSE~(Mg~C~ha^-1)",
                                MAE_null_test = "MAE~(Mg~C~ha^-1)",
                                RMSE_null_test = "RMSE~(Mg~C~ha^-1)"),
         prof = factor(dplyr::recode(var,
                                     OCS_0_5 = "0-5",
                                     OCS_5_15 = "5-15",
                                     OCS_15_30 = "15-30",
                                     OCS_30_60 = "30-60",
                                     OCS_60_100 = "60-100"),
                       levels = c("0-5", "5-15", "15-30", "30-60", "60-100")),
         model = factor(dplyr::recode(model,
                                      `Random Forest` = "rf",
                                      Cubist = "cubist",
                                      `Stochastic Gradient Boosting` = "gbm",
                                      `k-Nearest Neighbors` = "k-knn",
                                      `Support Vector Machines with Radial Basis Function Kernel` = "svmRadial"),
                        levels = c("null model", "cubist", "gbm", "glmnet", "k-knn", "rf", "svmRadial"))) %>%
  dplyr::select(-var)





dfgg <- df %>%
  #  group_by(model, prof, metric, data) %>%
  #  summarise(value = mean(value)) %>%
  filter(
    data == "Test",
    metric %in% c("MAE~(Mg~C~ha^-1)", "RMSE~(Mg~C~ha^-1)", "rho[c]"))



fig_perf <- ggplot(dfgg, aes(x = value, y = reorder(prof, value), fill = model)) +
  stat_boxplot(geom = "errorbar", col = "black") +
  geom_boxplot(col = "black") +
  facet_wrap(~metric, scales = "free_x", labeller = label_parsed,
             strip.position = "bottom") +
  scale_fill_manual(values = rev(soilpalettes::soil_palette("redox2", 7))) +
  #  scale_y_discrete(limits = rev) +
  labs(x = NULL, y = "Depth (cm)", fill = NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.key.size = unit(1.5, "cm"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"),
        axis.text = element_text(color = "black", size = 24),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title = element_text(color = "black", size = 24),
        strip.text = element_text(color = "black", size = 24),
        legend.text = element_text(color = "black", size = 24)) +
  guides(fill = guide_legend(nrow = 1)) +
  stat_summary(fun = mean, geom = "point", col = "black", shape = 15, size = 2,
               aes(group = model), position = position_dodge(0.75)) ; fig_perf



ggsave(fig_perf, filename = "./fig/fig_perf.png", dpi = 1200,
       width = 19, height = 10.69, units = "in")







dfgg <- df %>%
  group_by(model, prof, metric, data) %>%
  summarise(value = mean(value)) %>%
  filter(
    #data == "Test",
    metric %in% c("MAE~(Mg~C~ha^-1)", "RMSE~(Mg~C~ha^-1)", "rho[c]"))


ggplot(dfgg, aes(x = value, y = reorder(prof, value), fill = model)) +
  geom_col(position = "dodge") +
  facet_wrap(~data + metric, scales = "free_x", labeller = label_parsed,
             strip.position = "bottom") +
  scale_fill_brewer(palette = "Spectral") +
  # scale_y_discrete(limits = rev) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top") +
  guides(fill = guide_legend(nrow = 1))










# importancia -------------------------------------------------------------




l_imp <- list.files(path = "./", pattern = ".csv$", full.names = T, recursive = T) %>%
  grep(pattern = "imp_pred_TOTAL", value = T) %>%
  grep(pattern = "100/rf", value = T, invert = F)


p <- c("0-5", "15-30", "30-60", "5-15", "60-100")

for (i in seq_along(l_imp)) {

  df <- read.csv2(l_imp[i]) %>%
    mutate(prof = p[i])

  if (i == 1) {
    df_imp <- df
  } else {
    df_imp <- rbind(df_imp, df)
  }

}




varfact <- c("geology", "drainage", "landforms_tpi_based",
             "surface_specific_points", "lulc",
             "terrain_surface_classification_iwahashi", "soil", "slope_idx",
             "valley_idx") %>% sort()






for (h in seq_along(varfact)) {

  df_imp <- df_imp %>%
    mutate(predictor = str_replace(predictor, paste0(".*", varfact[h], ".*"),
                                   varfact[h]))
}








df_imp <- df_imp %>%
  group_by(rep, predictor, prof) %>%
  summarise(importance = mean(importance))




df_imp_2 <- df_imp %>%
  group_by(predictor, prof) %>%
  summarise(frequencia = n(),
            importance = mean(importance)) %>%
  mutate(imp = frequencia / importance,
         imp = ifelse(is.infinite(imp), 0, imp))



for (i in seq_along(p)) {

  d <- df_imp_2 %>%
    filter(prof == p[i])

 # d2 <- max(d$imp)

  d <- d %>%
    mutate(imp =  frequencia^2 / importance)


  if (i == 1) {
    dfg_imp <- d
  } else {
    dfg_imp <- rbind(dfg_imp, d)
  }


}

# write.csv2(df_imp_2, file = "./data.csv", row.names = F)


n_cova <- read.csv2("./sheet/name_cova.csv")


dfg_imp <- left_join(dfg_imp, n_cova, by = c("predictor" = "name_abr"))



ggplot(dfg_imp, aes(x = imp, y = reorder(name_comp, imp))) +
  geom_col(fill = "grey25") +
  labs(x = expression(Frequence^2~`/`~Importance), y = NULL) +
  scale_x_continuous(expand = c(0, 0, 0.05, 0)) +
  theme(axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"),
        axis.text = element_text(color = "black")) +
  facet_wrap(~prof, scales = "free_x")







n_fun <- function(x) {
  return(data.frame(y = 3.5 * 30,
                    label = length(x)))
}


ggplot(dfg_imp, aes(x = importance, y = reorder(name_comp, importance))) +
  geom_col(fill = "grey25") +
  labs(x = expression(Frequence^2~`/`~Importance), y = NULL) +
  scale_x_continuous(expand = c(0, 0, 0.05, 0)) +
  theme(axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"),
        axis.text = element_text(color = "black")) +
  facet_wrap(~prof, scales = "free_x") +
  stat_summary(fun.data = n_fun, geom = "text", col = "red",
               hjust = 0.5, position = position_dodge(0.6),
               fontface = "bold")





# boxplot -----------------------------------------------------------------

n_cova <- read.csv2("./sheet/name_cova.csv")
dfg_imp <- left_join(df_imp, n_cova, by = c("predictor" = "name_abr")) %>%
  mutate(prof = factor(prof, levels = c("0-5", "5-15", "15-30", "30-60", "60-100")))


n_fun <- function(x) {
  return(data.frame(y = 3.5 * 30,
                    label = length(x)))
}

bbox <- dfg_imp %>%
  group_by(name_comp) %>%
  summarise(boxplot = list(setNames(boxplot.stats(importance)$stats,
                                    c('lower_whisker',
                                      'lower_hinge',
                                      'median',
                                      'upper_hinge',
                                      'upper_whisker')))) %>%
  unnest_wider(boxplot)

min_box <- min(bbox$lower_whisker)
max_box <- max(bbox$lower_whisker)



levels_pred <- dfg_imp %>%
  filter(importance <= max_box,
         importance >= min_box) %>%
  group_by(name_comp) %>%
  summarise(importance = mean(importance)) %>%
  arrange(importance) %>%
  ungroup() %>%
  select(name_comp) %>%
  pull()



dfg_imp <- dfg_imp %>%
  mutate(name_comp = factor(name_comp,
                         levels = rev(levels_pred)))
















imp_pred <- ggplot(dfg_imp, aes(x = importance, y = name_comp, fill = prof)) +
  stat_boxplot(geom = "errorbar", size = 0.5) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = soilpalettes::soil_palette("paleustalf", 6)[-1]) +
  scale_y_discrete(limits = rev) +
  labs(y = "Predictor", fill = "Depth (cm)",
       x = "Relative importance (%)", y = NULL) +
  scale_x_continuous(expand = c(0.01, 0, 0.05, 0)) +
  theme(axis.ticks = element_line(color = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"),
        axis.text = element_text(color = "black", size = 24),
        legend.key.size = unit(2.5, "cm"),
        axis.title = element_text(color = "black", size = 24),
        legend.position = c(0.83, 0.25),
        legend.text = element_text(size = 32, color = "black"),
        legend.title = element_text(size = 32, color = "black"),
        legend.key = element_blank()) +
  facet_wrap(~prof, scales = "free_x") +
  stat_summary(fun.data = n_fun, geom = "text", col = "red", size = 7,
               hjust = 0.5, position = position_dodge(0.6),
               fontface = "bold") +
  stat_summary(fun = mean, geom = "point", col = "black", shape = 15, size = 2.5,
               aes(group = prof), position = position_dodge(0.75)) ; imp_pred





ggsave(imp_pred, filename = "./fig/imp_pred.png", dpi = 1200,
       width = 19, height = 14.69, units = "in")





# rfe ---------------------------------------------------------------------

ldf <- list.files(path = "./", pattern = ".csv$", full.names = T,
                  recursive = T) %>%
  grep(pattern = "R2_MAE_RMSE_TOTAL", value = T) %>%
  grep(pattern = "100/rf", value = T, invert = F)




prof <- stringr::str_split(ldf, pattern = "/", simplify = T)[,8]


for (i in seq_along(ldf)) {

  df <- ldf[i] %>%
    read.csv2() %>%
    mutate(prof = prof[i],
           prof = dplyr:: recode(prof,
                                 OCS_0_5 = "0-5",
                                 OCS_15_30 = "15-30",
                                 OCS_30_60 = "30-60",
                                 OCS_5_15 = "5-15",
                                 OCS_60_100 = "60-100"))


  if (i == 1) {
    dfg <- df
  } else {
    dfg <- rbind(dfg, df)
  }

}





dfgf <- dfg %>%
  select(c(2:5, 9)) %>%
  gather(key = "metric", value = "value", -c(Variables, prof)) %>%
  mutate(metric = factor(recode(metric,
                                RMSE = "RMSE~(Mg~ha^{-1})",
                                MAE = "MAE~(Mg~ha^{-1})",
                                Rsquared = "R^2"),
                         levels = c("MAE~(Mg~ha^{-1})", "R^2", "RMSE~(Mg~ha^{-1})")),
         prof = factor(prof,
                       levels = c("0-5", "5-15", "15-30", "30-60", "60-100"))) %>%
  arrange(Variables)





vars <- dfgf$prof %>% unique() %>% sort()







for (i in seq_along(vars)) {



  df_plot <- filter(dfgf, prof == vars[i])







gg <- ggplot(df_plot, aes(y = as.factor(Variables), x = value)) +
  stat_boxplot(geom = "errorbar", col = "black", size = 0.5) +
  geom_boxplot(size = 0.5, col = "black", fill = "grey90") +
  labs(y = "Number of predictors", x = NULL) +
  facet_wrap(~metric, scales = "free_x", labeller = label_parsed,
             strip.position = "bottom") +
  stat_summary(fun = mean, geom = "point",
               #position = position_dodge(.75),
               color = "darkred", size = 3,
               shape = 15)+
  theme(strip.placement = "output",
        strip.background = element_blank(),
        axis.ticks = element_line(color = "black"),

              strip.text = element_text(color = "black", size = 24),
              panel.background = element_blank(),
              panel.border = element_rect(fill = NA, inherit.blank = T),
              panel.grid = element_line(colour = "grey90"),
              axis.text = element_text(color = "black", size = 24),
              legend.key.size = unit(2.5, "cm"),
              axis.title = element_text(color = "black", size = 24),
              legend.position = c(0.83, 0.25),
              legend.text = element_text(size = 32, color = "black"),
              legend.title = element_text(size = 32, color = "black"),
              legend.key = element_blank()) ; gg




ggsave(gg, filename = paste0("./fig/rfe_", vars[i], ".png"),
       dpi = 600, width = 19, height = 11, units = "in")



}
