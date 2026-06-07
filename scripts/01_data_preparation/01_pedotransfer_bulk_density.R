
pkg <- c("caret", "rgdal", "beepr", "sf", "fasterize", "stringr", "geobr",
         "readxl", "dplyr", "tmap", "tmaptools", "terra", "parallelly", "gbm",
         "parallel", "doParallel", "DescTools", "Cubist", "kknn", "kernlab",
         "tidyr", "RColorBrewer", "mpspline2")

sapply(pkg, require, character.only = T)


setwd("D:/Usuario/cassio/R/ro_soil_carbon")

rm(list = ls())  # clean memory
gc()



# df33 --------------------------------------------------------------------


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










# df34 --------------------------------------------------------------------



df34 <- read_excel("./sheet/database/ctb0034.xlsx",
                   sheet = "camada") %>%
  dplyr::select(observacao_id, camada_nome, dsi_xxx)



coord_34 <- read_excel("./sheet/database/ctb0034.xlsx",
                       sheet = "observacao") %>%
  dplyr::select(observacao_id, coord_x, coord_y)




df34 <- df34 %>%
  left_join(coord_34, by = "observacao_id")







# ML BD -------------------------------------------------------------------




df_pedo <- df34 %>%
  mutate(id = paste0(observacao_id, "_", camada_nome)) %>%
  left_join(mutate(dplyr::select(df33, -coord_x, -coord_y),
                   id = paste0(observacao_id, "_", camada_id)),
            by = "id") %>%
  dplyr::select(-camada_nome, -camada_id, -observacao_id.x, -observacao_id.y) %>%
  relocate(id, coord_x, coord_y, profund_sup, profund_inf)



df_pedo <- df_pedo[, colSums(is.na(df_pedo)) < 7] %>%
  na.omit()

summary(df_pedo)








# model -------------------------------------------------------------------



# parametrization
# folder

path_raiz <- "C:/R/ro_soil_carbon/"


source("C:/R/outros/scripts_aleatorios/gbm_custom.R")
setwd(path_raiz)


nruns <- 1
fold_rfe <- 10
rep_rfe <- 5
metric_otm <- "MAE"
size_rfe <- seq(1, 27, 1)
tn_length <- 10
fold_model <- 10
rep_model <- 10
model <- c("rf", "svmRadial", "cubist", "kknn", "gbm_custom")




varsy <- c("dsi_xxx")



i = 1
n = 1

cl <- parallel::makeCluster(11)
cl <- parallelly::autoStopCluster(cl)

for (z in seq_along(model)) {

  if (z == 1) {
    tfull <- Sys.time()
  }

  tmodel <- Sys.time()


  for (i in seq_along(varsy)) {

    path_results <- "C:/R/ro_soil_carbon/results_bd/"



    tvar <- Sys.time()



    dfbase <- df_pedo[,-(1:5)]

    dfperf <- data.frame(model = integer(nruns),
                         n_cross = integer(nruns),
                         MAE_cross = integer(nruns),
                         RMSE_cross = integer(nruns),
                         Rsquared_cross = integer(nruns),
                         MAE_NULL = integer(nruns),
                         RMSE_NULL = integer(nruns),
                         LCCC_cross = integer(nruns))

    var <- varsy[i]


    if (!dir.exists(paste0(path_results, model[z]))) {
      dir.create(paste0(path_results, model[z]))
    }

    path_results <- paste0(path_results, model[z], "/")


    if (!dir.exists(paste0(path_results, "select"))) {
      dir.create(paste0(path_results, "select"))
    }

    if (!dir.exists(paste0(path_results, "select/cor"))) {
      dir.create(paste0(path_results, "select/cor"))
    }

    if (!dir.exists(paste0(path_results, "select/rfe"))) {
      dir.create(paste0(path_results, "select/rfe"))
    }

    if (!dir.exists(paste0(path_results, "select/rfe/metric"))) {
      dir.create(paste0(path_results, "select/rfe/metric"))
    }

    if (!dir.exists(paste0(path_results, "select/rfe/select"))) {
      dir.create(paste0(path_results, "select/rfe/select"))
    }

    if (!dir.exists(paste0(path_results, "select/rfe/metric/", var))) {
      dir.create(paste0(path_results, "select/rfe/metric/", var))
    }

    if (!dir.exists(paste0(path_results, "select/rfe/select/", var))) {
      dir.create(paste0(path_results, "select/rfe/select/", var))
    }

    if (!dir.exists(paste0(path_results, "performance"))) {
      dir.create(paste0(path_results, "performance"))
    }

    if (!dir.exists(paste0(path_results, "performance/csv"))) {
      dir.create(paste0(path_results, "performance/csv"))
    }

    if (!dir.exists(paste0(path_results, "performance/imp_pred"))) {
      dir.create(paste0(path_results, "performance/imp_pred"))
    }

    if (!dir.exists(paste0(path_results, "performance/imp_pred/", var))) {
      dir.create(paste0(path_results, "performance/imp_pred/", var))
    }

    if (!dir.exists(paste0(path_results, "img"))) {
      dir.create(paste0(path_results, "img"))
    }


    dy <- dfbase %>% dplyr::select({var})
    dx <- dfbase %>% dplyr::select(-{var})

    dyx_sel <- cbind(dy, dx) %>%
      filter(!!sym(var) > 0) %>%
      na.omit()


    # dyx_sel <- dyx_sel %>%
    #   mutate_at(.vars = varfact, as.factor)

    dyx_sel <- dyx_sel %>%
      dplyr::select(-one_of(nearZeroVar(., names = T)))

    mcor <- dyx_sel %>% dplyr::select(-one_of(var)) %>%
      dplyr::select_if(is.numeric) %>%
      cor(method = "spearman")

    fc <- findCorrelation(mcor, cutoff = 0.95, names = T)

    data.frame(fc) %>%
      `colnames<-`(paste0("rem_cor_", var)) %>%
      write.csv2(file = paste0(path_results, "select/cor/", var,
                               "_cor", ".csv"), row.names = F)


    dyx_sel <- dyx_sel %>% dplyr::select(-one_of(fc))



    set.seed(666)
    nseed <- sample(1:100000, nruns)

    # Criando lista vazia para modelos treinados
    lmodel <- list()
    lpredimp <- list()


    lrfepred <- list()
    lrferes <- list()




    for (n in 1:nruns) {

      trun <- Sys.time()

      print(paste(trun, "run", n, "faltando", nruns - n))





      registerDoParallel(cl)
      set.seed(nseed[n])
      rfe_ctrl <- rfeControl(method = "repeatedcv",
                             repeats = rep_rfe,
                             number = fold_rfe,
                             verbose = F)

      set.seed(nseed[n])
      model_ctrl <- trainControl(method = "repeatedcv",
                                 number = fold_rfe,
                                 repeats = rep_rfe,
                                 savePredictions = T)

      formu <- as.formula(paste(var, "~ ."))

      set.seed(nseed[n])
      rfe_fit <- rfe(form = formu,
                     data = dyx_sel,
                     sizes = size_rfe,
                     method = if (model[z] %in% c("gbm_custom")){
                       get(model[z])
                     } else {
                       model[z]
                     },
                     metric = metric_otm,
                     trControl = model_ctrl,
                     tuneLength = tn_length,
                     rfeControl = rfe_ctrl,
                     maximize = ifelse(metric_otm %in% c("RMSE", "MAE"),
                                       FALSE, TRUE))

      print(rfe_fit)
      print("-----------------------------------------------------------------")
      print(paste("RFE run", n, Sys.time() - trun, units(Sys.time() - trun)))

      lrferes[[n]] <- rfe_fit$result

      pick <- caret::pickSizeTolerance(x = lrferes[[n]],
                                       metric = metric_otm,
                                       tol = 5,
                                       maximize = ifelse(
                                         metric_otm %in% c("RMSE", "MAE"),
                                         FALSE, TRUE))
      lrfepred[[n]] <- rfe_fit$optVariables[1:pick]
      print(paste("select", length(lrfepred[[n]])))
      print("-----------------------------------------------------------------")

      write.csv2(data.frame(lrferes[[n]]),
                 file = paste0(path_results, "select/rfe/metric/", var,
                               "/RFE_", "R2_MAE_RMSE_", n, ".csv"), row.names = F)

      write.csv2(data.frame(pred_sel = lrfepred[[n]]),
                 file = paste0(path_results, "select/rfe/select/", var,
                               "/RFE_", "pred_sel_", n, ".csv"), row.names = F)


      dfselrfe <- dyx_sel %>% dplyr::select({var}, one_of(lrfepred[[n]]))



      model_ctrl <- trainControl(method = "repeatedcv",
                                 number = fold_model,
                                 repeats = rep_model,
                                 savePredictions = T)

      formu <- as.formula(paste(var, "~ ."))
      registerDoParallel(cl)
      set.seed(nseed[n])
      model_fit <- train(form = formu,
                         data = dfselrfe,
                         metric = metric_otm,
                         method = if (model[z] %in% c("gbm_custom")){
                           get(model[z])
                         } else {
                           model[z]
                         },
                         trControl = model_ctrl,
                         tuneLength = tn_length,
                         # importance = T,
                         maximize = ifelse(
                           metric_otm %in% c("RMSE", "MAE"), FALSE, TRUE))

      print(model_fit)
      print("-----------------------------------------------------------------")
      print(paste(model_fit[["modelInfo"]][["label"]], n, Sys.time() - trun,
                  units(Sys.time() - trun)))

      lmodel[[n]] <- model_fit


      pr_train <- getTrainPerf(lmodel[[n]])

      pred_imp <- varImp(lmodel[[n]])




      lccc_full<- CCC(dfselrfe[,var], predict(lmodel[[n]], dfselrfe),
                      conf.level = 0.95)


      pr_null <- data.frame(obs = dfselrfe[,var],
                            pred = mean(dfselrfe[,var]))


      pr_null <-  caret:::postResample(pred = pr_null$pred, obs = pr_null$obs)


      lpredimp[[n]] <- data.frame(pred_imp[1]) %>%
        mutate(predictor = row.names(.),
               importance = Overall) %>%
        dplyr::select(-Overall) %>%
        relocate(predictor)


      write.csv2(lpredimp[[n]],
                 paste0(path_results, "performance/imp_pred/", var, "/",
                        "imp_pred_", n, ".csv"), row.names = F)


      dfperf$model[n] <- lmodel[[n]][["modelInfo"]][["label"]]
      dfperf$n_cross[n] <- nrow(dyx_sel)
      dfperf$MAE_cross[n] <- pr_train$TrainMAE
      dfperf$RMSE_cross[n] <- pr_train$TrainRMSE
      dfperf$Rsquared_cross[n] <- pr_train$TrainRsquared
      dfperf$MAE_NULL[n] <- pr_null["MAE"]
      dfperf$RMSE_NULL[n] <- pr_null["RMSE"]
      dfperf$LCCC_cross[n] <- lccc_full$rho.c$est


      write.csv2(dfperf, row.names = F, paste0(
        path_results, "performance/csv/", var, "_performance", ".csv"))

      save.image(paste0(path_results, "img/", var, ".RData"))

      gc()
    }

    # Salvando sele??o RFE geral -------------------------------------------------
    n_obs <- sapply(lrfepred, length)
    seq.max <- seq_len(max(n_obs))

    rfe_pred_full <- as.data.frame(sapply(lrfepred, "[", i = seq.max))


    write.csv2(rfe_pred_full, row.names = F,
               file = paste0(
                 path_results, "select/rfe/select/", var, "/select_TOTAL",
                 ".csv"))



    # Salvando resultado geral RFE -----------------------------------------------
    n_rep <- rep(1:nruns, times = sapply(lrferes, nrow))
    rfe_res_full <- do.call(rbind, lrferes)

    rfe_res_full <- rfe_res_full %>%
      mutate(rep = n_rep) %>%
      relocate(rep)


    write.csv2(rfe_res_full, row.names = F,
               file = paste0(
                 path_results, "select/rfe/metric/", var, "/R2_MAE_RMSE_TOTAL",
                 ".csv"))


    # Salvando import?ncia geral covari?veis -------------------------------------
    n_rep <- rep(1:nruns, times = sapply(lpredimp, nrow))
    pred_imp_full <- do.call(rbind, lpredimp)

    pred_imp_full <- pred_imp_full %>%
      mutate(rep = n_rep) %>%
      relocate(rep)


    write.csv2(pred_imp_full, row.names = F,
               file = paste0(
                 path_results, "performance/imp_pred/", var, "/imp_pred_TOTAL",
                 ".csv"))

    print(paste(var, Sys.time() - tvar, units(Sys.time() - tvar)))





  }

  print(paste("model time", Sys.time() - tmodel, units(Sys.time() - tmodel)))

  if (z == length(model)) {
    print(paste("full time", Sys.time() - tfull, units(Sys.time() - tfull)))
  }

}






























# compare models ----------------------------------------------------------



models <- list.dirs(path = "./results_bd", full.names = T, recursive = F)







for (n in seq_along(models)) {

  df <- list.files(path = paste0(models[n], "/performance/csv"),
                   pattern = ".csv$", full.names = T) %>%
    read.csv2()

  if (n == 1) {

    dfp <- df

  } else {

    dfp <- rbind(dfp, df)

  }
}






dfp <- dfp %>%
  mutate(model = recode(model,
                        `Cubist` = "cubist",
                        `Stochastic Gradient Boosting` = "gbm",
                        `k-Nearest Neighbors` = "kknn",
                        `Random Forest` = "rf",
                        `Support Vector Machines with Radial Basis Function Kernel` = "svmRadial")) %>%
  dplyr::select(-n_cross) %>%
  relocate(model) %>%
  gather(key = "metric", value = "value", -c("model")) %>%
  mutate(data = factor(recode(metric,
                              MAE_cross = "repeatedcv",
                              RMSE_cross = "repeatedcv",
                              Rsquared_cross = "repeatedcv",
                              MAE_NULL = "Null",
                              RMSE_NULL = "Null",
                              LCCC_cross = "repeatedcv")),
         metric = factor(recode(metric,
                                MAE_cross = "MAE~(`%`)",
                                RMSE_cross = "RMSE~(`%`)",
                                Rsquared_cross = "R^2",
                                MAE_NULL = "MAE~(`%`)",
                                RMSE_NULL = "RMSE~(`%`)",
                                LCCC_cross = "LCCC"),
                         levels = c("R^2", "LCCC", "MAE~(`%`)", "RMSE~(`%`)")))




# write.csv2(dfg, file = "./perform.csv", row.names = F)



ggperf <- ggplot(dfp, aes(x = model, y = value, fill = data)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Spectral"))(11)[c(2, 10)]) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        strip.text = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap(~ metric, scales = "free_y", labeller = label_parsed,
             nrow = 4, strip.position = "left") ; ggperf


ggsave(plot = ggperf, filename = "./ggperf.jpg", dpi = 600,
       width = 19, height = 13.436, units = "in")




# predict ds --------------------------------------------------------------



load("./results_bd/rf/img/dsi_xxx.RData")

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


names(df_ocs)


df_ocs <- df33 %>%
  mutate(bd = predict(lmodel[[1]], df33)) %>%
  dplyr::select(observacao_id, camada_id, profund_sup, profund_inf,
                bd, carbono_xxx_xxx_xxx, coord_x, coord_y) %>%
  rename(soc = "carbono_xxx_xxx_xxx") %>%
  mutate(t_depht = ifelse(profund_sup > profund_inf, profund_inf,
                          profund_sup),
         b_depht = ifelse(profund_inf < profund_sup, profund_sup,
                          profund_inf)) %>%
  mutate(ocs = (soc * bd * (b_depht - t_depht)) / 10) %>%
  dplyr::select(observacao_id, t_depht, b_depht, bd, soc, ocs,
                coord_x, coord_y) %>%
  filter(ocs > 0) %>%
  arrange(observacao_id) %>%
  mutate(observacao_id = as.factor(observacao_id))



# spline ------------------------------------------------------------------

spline_ocs <- mpspline(obj = df_ocs, var_name = "ocs",
                       lam = 0.1, d = c(0, 5, 15, 30, 60, 100), vlow = 0,
                       vhigh = 1000)




for (i in seq_along(spline_ocs)) {

  if (i == 1) {

    df_spline <- data.frame(perfil = names(spline_ocs)[i], t(spline_ocs[[i]][["est_dcm"]]))


  } else {
    df_spline <- rbind(df_spline,
                       data.frame(perfil = names(spline_ocs)[i],
                                  t(spline_ocs[[i]][["est_dcm"]][1:5])))

  }

}


df_spline <- df_spline %>%
  filter(!X000_005_cm == X005_015_cm |
           !X005_015_cm == X015_030_cm |
           !X015_030_cm == X030_060_cm |
           !X030_060_cm == X060_100_cm) %>%
  rename(OCS_0_5 = X000_005_cm,
         OCS_5_15 = X005_015_cm,
         OCS_15_30 = X015_030_cm,
         OCS_30_60 = X030_060_cm,
         OCS_60_100 = X060_100_cm) %>%
  left_join(coord_33, by = c("perfil" = "observacao_id"))



sf_spline <- df_spline %>%
  st_as_sf(coords = c("coord_x", "coord_y"),
           crs = "EPSG:4326") %>%
  st_transform("ESRI:102015")


plot(st_geometry(sf_spline))


st_write(sf_spline, dsn = "./shp/spline_ocs.shp")
