
pkg <- c("dplyr", "caret", "rfUtilities", "quantregForest", "tidyr",
         "beepr", "ggplot2", "stringr")


setwd("C:/R/ro_soil_carbon")

sapply(pkg, require, character.only = T)

rm(list = ls())


df_dep <- function (m, x, yname, xname, lci = 0.25, uci = 0.75, delta = FALSE)
{
  if (!any(class(m) %in% c("randomForest", "list", "train")))
    stop("m is not a randomForest object")
  if (m$modelType != "Regression")
    stop("classification is not supported")
  conf.int <- (uci - lci) * 100
  temp <- sort(x[, xname])
  y.hat.mean <- vector()
  y.hat.lb <- vector()
  y.hat.ub <- vector()
  y <- stats::predict(m, x)
  for (i in 1:length(temp)) {
    x[, xname] <- temp[i]
    y.hat <- stats::predict(m, x)
    if (delta == TRUE) {
      y.hat <- y.hat - y
    }
    y.hat.mean[i] <- stats::weighted.mean(y.hat, na.rm = T)
    y.hat.lb[i] <- stats::quantile(y.hat, lci, na.rm = T)
    y.hat.ub[i] <- stats::quantile(y.hat, uci, na.rm = T)
  }
  m.ci <- as.data.frame(cbind(temp, y.hat.mean, y.hat.lb,
                              y.hat.ub))
  names(m.ci) <- c(xname, yname, "lci", "uci")

  return(m.ci)
}



models_ <- list.files(path = "./results_ocs/rf", pattern = ".RData$",
                     full.names = T, recursive = T)


i = 1
h = 1
lg <- list()

for (i in seq_along(models_)) {

  t11 <- Sys.time()
  load(models_[i])

  lxvars <- names(dfselrfe)[-1]
  lyvar <- names(dfselrfe)[1]

  dfs <- list()
  for (h in seq_along(lxvars)) {

    dfs[[h]] <- df_dep(m = lmodel[[1]], x = dfselrfe, yname = lyvar,
                       xname = lxvars[h], lci = 0.25, uci = 0.75)

    print(h)


  }

  lg[[i]] <- dfs

  print(Sys.time() - t11)
  beep(2)

}


# save(lg, file = "./results/img_out/partial_dependency_median.RData")

# load --------------------------------------------------------------------


load("./results_cs/img_out/partial_dependency_median.RData")

ggplot(lg[[1]][[1]], aes(y = rs, x = npp)) +

  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F)




for (i in seq_along(lg[[1]])) {

  d <- lg[[1]][[i]] %>%
    gather(key = "var", value = "cci", -c(OCS_0_5, lci, uci))

  if (i == 1){
    df <- d


  } else {

    df <- rbind(df, d)

  }

}

dfrs <- df

ggplot(df, aes(y = OCS_0_5, x = cci)) +

  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~var, scales = "free")










for (i in seq_along(lg[[2]])) {

  d <- lg[[2]][[i]] %>%
    gather(key = "var", value = "cci", -c(rh, lci, uci))

  if (i == 1){
    df <- d


  } else {

    df <- rbind(df, d)

  }

}

dfrh <- df
ggplot(df, aes(y = rh, x = cci)) +

  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~var, scales = "free")






dfrs <- dfrs %>%
  gather(key = "tr", value = "vr", -c("var", "lci", "uci", "cci"))

dfrh <- dfrh %>%
  gather(key = "tr", value = "vr", -c("var", "lci", "uci", "cci"))


dfg <- rbind(dfrs, dfrh) %>%
  filter(var %in% c("bio1", "bio6", "bio12", "bio13", "npp")) %>%
  mutate(tr = factor(recode(tr,
                            rs = "Rs~(g~C~m^{-2}~year^{-1})",
                            rh = "Rh~(g~C~m^{-2}~year^{-1})"),
                     levels = c("Rs~(g~C~m^{-2}~year^{-1})",
                                "Rh~(g~C~m^{-2}~year^{-1})")),
         var = factor(recode(var,
                             bio1 = "BIO~1~(?C)",
                             bio6 = "BIO~6~(?C)",
                             bio12 = "BIO~12~(kg~m^{-2}~year^{-1})",
                             bio13 = "BIO~13~(kg~m^{-2})",
                             npp = "NPP~(g~C~m^{-2}~year^{-1})"),
                      levels = c("BIO~1~(?C)", "BIO~6~(?C)",
                                 "BIO~12~(kg~m^{-2}~year^{-1})",
                                 "BIO~13~(kg~m^{-2})",
                                 "NPP~(g~C~m^{-2}~year^{-1})"))) %>%
  relocate(tr, var, lci, uci, vr)




gg_model_partial <- ggplot(dfg, aes(y = vr, x = cci)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = NULL, y = NULL) +
  facet_grid(rows = vars(tr), cols = vars(var), scales = "free",
             labeller = label_parsed, switch = "both") +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        strip.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 22, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) ; gg_model_partial




ggsave(gg_model_partial, filename = "./figuras/gg_model_partial.jpg", dpi = 600,
       width = 19, height = 6.718, units = "in")









# gg geral ----------------------------------------------------------------



unique(dfg$var)


dfg <- rbind(dfrs, dfrh) %>%
  # filter(var %in% c("bio1", "bio6", "bio12", "bio13", "npp")) %>%
  mutate(tr = factor(recode(tr,
                            rs = "Rs~(g~C~m^{-2}~year^{-1})",
                            rh = "Rh~(g~C~m^{-2}~year^{-1})"),
                     levels = c("Rs~(g~C~m^{-2}~year^{-1})",
                                "Rh~(g~C~m^{-2}~year^{-1})")),
         var = factor(recode(var,
                             bio1 = "BIO~1~(?C)",
                             bio6 = "BIO~6~(?C)",
                             bio12 = "atop(BIO~12, (kg~m^{-2}~year^{-1}))",
                             bio13 = "BIO~13~(kg~m^{-2})",
                             bio19 = "BIO~19~(kg~m^{-2})",
                             bio3 = "BIO~3~(`%`)",
                             bio4 = "BIO~4~(`%`)",
                             clay_0_30cm = "Clay~(g~kg^-1)",
                             soc_sand_0_30cm = "atop(SOC~sand, (kg~C~ha^-1~~kg~`sand`^-1))",
                             npp = "NPP~(g~C~m^{-2}~year^{-1})"),
                      levels = c("BIO~1~(?C)", "BIO~3~(`%`)", "BIO~4~(`%`)",
                                 "BIO~6~(?C)",
                                 "atop(BIO~12, (kg~m^{-2}~year^{-1}))",
                                 "BIO~13~(kg~m^{-2})",
                                 "BIO~19~(kg~m^{-2})",
                                 "NPP~(g~C~m^{-2}~year^{-1})",
                                 "Clay~(g~kg^-1)",
                                 "atop(SOC~sand, (kg~C~ha^-1~~kg~`sand`^-1))"))) %>%
  relocate(tr, var, lci, uci, vr)



# rs ----------------------------------------------------------------------



gg_model_partial_rs <- ggplot(filter(dfg, tr == "Rs~(g~C~m^{-2}~year^{-1})"),
                              aes(y = vr, x = cci)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = NULL, y = expression(Rs~(g~C~m^{-2}~year^{-1}))) +
  facet_wrap(~var, scales = "free_x",
             labeller = label_parsed,
             nrow = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 28, color = "black"),
        strip.text = element_text(size = 23, color = "black"),
        legend.text = element_text(size = 22, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) ; gg_model_partial_rs




ggsave(gg_model_partial_rs, filename = "./figuras/gg_model_partial_rs_geral.jpg", dpi = 600,
       width = 19, height = 13.436, units = "in")






# rh ----------------------------------------------------------------------


gg_model_partial_rh <- ggplot(filter(dfg, tr == "Rh~(g~C~m^{-2}~year^{-1})"),
                              aes(y = vr, x = cci)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = NULL, y = expression(Rh~(g~C~m^{-2}~year^{-1}))) +
  facet_wrap(~var, scales = "free_x",
             labeller = label_parsed,
             nrow = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 28, color = "black"),
        strip.text = element_text(size = 23, color = "black"),
        legend.text = element_text(size = 22, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) ; gg_model_partial_rh



ggsave(gg_model_partial_rh, filename = "./figuras/gg_model_partial_rh_geral.jpg", dpi = 600,
       width = 19, height = 13.436, units = "in")
