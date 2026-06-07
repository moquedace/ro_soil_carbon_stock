pkg <- c("hydroGOF", "caret", "rgdal", "beepr", "sf", "fasterize", "stringr", "geobr",
         "readxl", "dplyr", "tmap", "tmaptools", "terra", "parallelly", "gbm",
         "parallel", "doParallel", "DescTools", "Cubist", "kknn", "kernlab",
         "tidyr", "RColorBrewer", "mpspline2")

sapply(pkg, require, character.only = T)

rm(list = ls())  # clean memory
gc()



# parametrization
# folder
path_raiz <- "D:/Usuario/cassio/R/ro_soil_carbon/"



source("D:/Usuario/cassio/R/outros/scripts_aleatorios/gbm_custom.R")




setwd(path_raiz)




nruns <- 1
fold_rfe <- 10
rep_rfe <- 1
metric_otm <- "MAE"
size_rfe <- c(seq(2, 49, 1), seq(50, 120, 10)) #c(seq(2, 14, 1), seq(15, 29, 2), seq(30, 59, 20), seq(60, 120, 30))
tn_length_rfe <- 1
tn_length <- 10
fold_model <- 10
rep_model <- 10




models <- c("rf", "cubist", "earth", "gbm_custom", "glmnet", "kknn",
            "svmRadial")


tol_per <- 2


varsy <- read.csv2("./extract_xy/ocs_yx.csv") %>%
  names() %>%
  .[1:5]


varfact <- c("geology", "drainage", "landforms_tpi_based",
             "surface_specific_points", "lulc",
             "terrain_surface_classification_iwahashi", "soil", "slope_idx",
             "valley_idx") %>% sort()


cl <- parallel::makeCluster(11)
cl <- parallelly::autoStopCluster(cl)



i = 1
j = 1
n = 1

for (i in seq_along(models)[1]) {

  tmodel <- Sys.time()


  for (j in seq_along(varsy)) {

    path_results <- "D:/Usuario/cassio/R/ro_soil_carbon/results_ocs/"

    tvar <- Sys.time()

    dfbase <- read.csv2("./extract_xy/ocs_yx.csv")


    dfperf <- data.frame(model = integer(nruns),
                         var = integer(nruns),
                         n_train = integer(nruns),
                         MAE_train = integer(nruns),
                         RMSE_train = integer(nruns),
                         NSE_train = integer(nruns),
                         PBIAS_train = integer(nruns),
                         aPBIAS_train = integer(nruns),
                         Rsquared_train = integer(nruns),
                         CCC_train = integer(nruns),
                         n_test = integer(nruns),
                         MAE_test = integer(nruns),
                         RMSE_test = integer(nruns),
                         NSE_test = integer(nruns),
                         PBIAS_test = integer(nruns),
                         aPBIAS_test = integer(nruns),
                         Rsquared_test = integer(nruns),
                         CCC_test = integer(nruns),
                         MAE_null_train = integer(nruns),
                         RMSE_null_train = integer(nruns),
                         MAE_null_test = integer(nruns),
                         RMSE_null_test = integer(nruns))

    var <- varsy[j]



    if (!dir.exists(paste0(path_results, models[i]))) {
      dir.create(paste0(path_results, models[i]))
    }


    if (!dir.exists(paste0(path_results, models[i], "/", var))) {
      dir.create(paste0(path_results, models[i], "/", var))
    }



    path_results <- paste0(path_results, models[i], "/", var, "/")


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
    dx <- dfbase %>% dplyr::select(-{varsy})



    dyx_sel <- cbind(dy, dx) %>%
      filter(!!sym(var) > 0) %>%
      na.omit()

    dyx_sel <- dyx_sel %>%
      mutate_at(.vars = varfact, as.factor)

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




    for (n in 77:nruns) {

      trun <- Sys.time()

      status_run <- paste(trun, models[i], var, "run", n, "missing", nruns - n)

      write.table(x = status_run, file = paste0(path_raiz, "status_run.txt"),
                  col.names = F, row.names = F)
      print(status_run)

      set.seed(nseed[n])

      vf <- createDataPartition(dyx_sel[, var], p = 0.8, list = F)


      train <- dyx_sel[vf,]
      test <- dyx_sel[-vf,]


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
                     method = if (models[i] %in% c("gbm_custom")){
                       get(models[i])
                     } else {
                       models[i]
                     },
                     metric = metric_otm,
                     trControl = model_ctrl,
                     tuneLength = tn_length_rfe,
                     rfeControl = rfe_ctrl,
                     maximize = ifelse(metric_otm %in% c("RMSE", "MAE"),
                                       FALSE, TRUE))

      print(rfe_fit)
      print("-----------------------------------------------------------------")
      status_rfe <- paste("RFE run", models[i], var, n, round(Sys.time() - trun, 2),
                          units(Sys.time() - trun))

      write.table(x = status_rfe, file = paste0(path_raiz, "status_rfe.txt"),
                  col.names = F, row.names = F)
      print(status_rfe)


      lrferes[[n]] <- rfe_fit$result

      pick <- caret::pickSizeTolerance(x = lrferes[[n]],
                                       metric = metric_otm,
                                       tol = tol_per,
                                       maximize = ifelse(
                                         metric_otm %in% c("RMSE", "MAE"),
                                         FALSE, TRUE))
      lrfepred[[n]] <- rfe_fit$optVariables[1:pick]
      print(paste("select", pick))
      print("-----------------------------------------------------------------")




      if (grepl(x = paste(lrfepred[[n]], collapse = " "),
                pattern = paste(varfact, collapse = "|"))) {

        for (h in seq_along(varfact)) {
          cf <- data.frame(vsel_rfe = lrfepred[[n]]) %>%
            mutate(vsel_rfe = str_replace(vsel_rfe, paste0(".*", varfact[h], ".*"),
                                          varfact[h])) %>%
            filter(!str_detect(vsel_rfe, paste(varfact[-h], collapse = "|"))) %>%
            unique() %>%
            pull()

          if (h == 1){

            cff <- cf

          } else {

            cff <- c(cff, cf)

          }
        }

        lrfepred[[n]] <- unique(cff)

      }


      write.csv2(data.frame(lrferes[[n]]),
                 file = paste0(path_results, "select/rfe/metric/", var,
                               "/RFE_", "R2_MAE_RMSE_", n, ".csv"), row.names = F)

      write.csv2(data.frame(pred_sel = lrfepred[[n]]),
                 file = paste0(path_results, "select/rfe/select/", var,
                               "/RFE_", "pred_sel_", n, ".csv"), row.names = F)

      dfselrfe <- dyx_sel %>% dplyr::select({var}, one_of(lrfepred[[n]]))



      set.seed(nseed[n])
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
                         method = if (models[i] %in% c("gbm_custom")){
                           get(models[i])
                         } else {
                           models[i]
                         },
                         trControl = model_ctrl,
                         tuneLength = tn_length,
                         # importance = T,
                         maximize = ifelse(
                           metric_otm %in% c("RMSE", "MAE"), FALSE, TRUE))

      print(model_fit)
      print("-----------------------------------------------------------------")
      status_model <- paste(model_fit[["modelInfo"]][["label"]], var, n,
                            round(Sys.time() - trun, 2),
                            units(Sys.time() - trun))

      write.table(x = status_model, file = paste0(path_raiz, "status_model.txt"),
                  col.names = F, row.names = F)
      print(status_model)
      print("-----------------------------------------------------------------")

      lmodel[[n]] <- model_fit

      # rmse mae rsquared
      pr_train <- getTrainPerf(lmodel[[n]])

      pr_test <- predict(lmodel[[n]], test) %>%
        postResample(pred =  ., obs = test[, var])



      # ccc
      ccc_train <- CCC(dfselrfe[,var], predict(lmodel[[n]], dfselrfe),
                       conf.level = 0.95)

      ccc_test <- CCC(dfselrfe[,var], predict(lmodel[[n]], dfselrfe),
                      conf.level = 0.95)



      # rmse mae null
      pr_null_train = rep(mean(dfselrfe[, var]), nrow(dfselrfe)) %>%
        postResample(pred = ., obs = dfselrfe[, var])

      pr_null_test = rep(mean(dfselrfe[, var]), nrow(dfselrfe)) %>%
        postResample(pred = ., obs = dfselrfe[, var])



      # nse
      nse_train <- NSE(sim = predict(lmodel[[n]], dfselrfe), obs = dfselrfe[,var],
                       na.rm = T)

      nse_test <- NSE(sim = predict(lmodel[[n]], dfselrfe), obs = dfselrfe[,var],
                      na.rm = T)



      # pbias
      pbias_train <- pbias(sim = predict(lmodel[[n]], dfselrfe), obs = dfselrfe[,var],
                           na.rm = T)

      pbias_test <- pbias(sim = predict(lmodel[[n]], dfselrfe), obs = dfselrfe[,var],
                          na.rm = T)



      # absolut pbias
      apbias_train <- pbias(sim = predict(lmodel[[n]], dfselrfe), obs = dfselrfe[,var],
                            na.rm = T) %>% abs()

      apbias_test <- pbias(sim = predict(lmodel[[n]], dfselrfe), obs = dfselrfe[,var],
                           na.rm = T) %>% abs()





      pred_imp <- varImp(lmodel[[n]])



      lpredimp[[n]] <- data.frame(pred_imp[1]) %>%
        mutate(predictor = row.names(.),
               importance = Overall) %>%
        dplyr::select(-Overall) %>%
        relocate(predictor)


      # ggplot(lpredimp[[n]], aes(y = reorder(predictor, importance), x = importance)) +
      #   geom_col()


      write.csv2(lpredimp[[n]],
                 paste0(path_results, "performance/imp_pred/", var, "/",
                        "imp_pred_", n, ".csv"), row.names = F)


      dfperf$model[n] <- lmodel[[n]][["modelInfo"]][["label"]]
      dfperf$var[n] <- var
      dfperf$n_train[n] <- nrow(train)
      dfperf$MAE_train[n] <- pr_train$TrainMAE
      dfperf$RMSE_train[n] <- pr_train$TrainRMSE
      dfperf$Rsquared_train[n] <- pr_train$TrainRsquared
      dfperf$n_test[n] <- nrow(test)
      dfperf$MAE_test[n] <- pr_test["MAE"]
      dfperf$RMSE_test[n] <- pr_test["RMSE"]
      dfperf$Rsquared_test[n] <- pr_test["Rsquared"]
      dfperf$MAE_null_train[n] <- pr_null_train["MAE"]
      dfperf$RMSE_null_train[n] <- pr_null_train["RMSE"]
      dfperf$MAE_null_test[n] <- pr_null_test["MAE"]
      dfperf$RMSE_null_test[n] <- pr_null_test["RMSE"]
      dfperf$CCC_train[n] <- ccc_train$rho.c$est
      dfperf$CCC_test[n] <- ccc_test$rho.c$est
      dfperf$NSE_train[n] <- nse_train
      dfperf$NSE_test[n] <- nse_test
      dfperf$PBIAS_train[n] <- pbias_train
      dfperf$PBIAS_test[n] <- pbias_test
      dfperf$aPBIAS_train[n] <- apbias_train
      dfperf$aPBIAS_test[n] <- apbias_test


      write.csv2(dfperf, row.names = F, paste0(
        path_results, "performance/csv/", var, "_performance", ".csv"))

      save.image(paste0(path_results, "img/", var, ".RData"))

      gc()
    }


    # save select rfe -------------------------------------------------
    n_obs <- sapply(lrfepred, length)
    seq.max <- seq_len(max(n_obs))

    rfe_pred_full <- as.data.frame(sapply(lrfepred, "[", i = seq.max))


    write.csv2(rfe_pred_full, row.names = F,
               file = paste0(
                 path_results, "select/rfe/select/", var, "/select_TOTAL",
                 ".csv"))



    # save geral rfe -----------------------------------------------
    n_rep <- rep(1:nruns, times = sapply(lrferes, nrow))
    rfe_res_full <- do.call(rbind, lrferes)

    rfe_res_full <- rfe_res_full %>%
      mutate(rep = n_rep) %>%
      relocate(rep)


    write.csv2(rfe_res_full, row.names = F,
               file = paste0(
                 path_results, "select/rfe/metric/", var, "/R2_MAE_RMSE_TOTAL",
                 ".csv"))


    # save predict importance -------------------------------------
    n_rep <- rep(1:nruns, times = sapply(lpredimp, nrow))
    pred_imp_full <- do.call(rbind, lpredimp)

    pred_imp_full <- pred_imp_full %>%
      mutate(rep = n_rep) %>%
      relocate(rep)


    write.csv2(pred_imp_full, row.names = F,
               file = paste0(
                 path_results, "performance/imp_pred/", var, "/imp_pred_TOTAL",
                 ".csv"))

    status_var <- paste(model_fit[["modelInfo"]][["label"]], var,
                        round(Sys.time() - tvar, 2),
                        units(Sys.time() - tvar))

    write.table(x = status_var, file = paste0(path_raiz, "status_var.txt"),
                col.names = F, row.names = F)
    print(status_var)


  }

  status_model_full <- paste(model_fit[["modelInfo"]][["label"]], var,
                             round(Sys.time() - tmodel, 2),
                             units(Sys.time() - tmodel))

  write.table(x = status_model_full,
              file = paste0(path_raiz, "status_model_full.txt"),
              col.names = F, row.names = F)
  print(status_model_full)

}
