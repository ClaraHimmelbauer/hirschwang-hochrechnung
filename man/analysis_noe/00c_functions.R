# functions

model_ltw <- function(df){
  # Modelle LTW
  x <- df
  
  model_ltw <- " ~ -1 + vp18 + sp18 + fp18 + gr18 + ne18 + so18 + nw18"
  
  # cluster 1
  sub <- x[x$ausg == 1 & x$cluster2 == 1, ]
  
  m1_ltw_1 <- glm(paste("vp", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m2_ltw_1 <- glm(paste("sp", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
  m3_ltw_1 <- glm(paste("fp", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
  m4_ltw_1 <- glm(paste("gr", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1))
  m5_ltw_1 <- glm(paste("ne", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1))
  m6_ltw_1 <- glm(paste("so", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m7_ltw_1 <- glm(paste("nw", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7))
  
  # cluster 2
  sub <- x[x$ausg == 1 & x$cluster2 == 2, ]
  
  m1_ltw_2 <- glm(paste("vp", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m2_ltw_2 <- glm(paste("sp", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
  m3_ltw_2 <- glm(paste("fp", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
  m4_ltw_2 <- glm(paste("gr", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1))
  m5_ltw_2 <- glm(paste("ne", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1))
  m6_ltw_2 <- glm(paste("so", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m7_ltw_2 <- glm(paste("nw", model_ltw), data = sub, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7))
  
  
  # Predicts
  x$pred_vp_ltw[x$cluster2 == 1] <- predict(m1_ltw_1, x[x$cluster2 == 1, ])
  x$pred_sp_ltw[x$cluster2 == 1] <- predict(m2_ltw_1, x[x$cluster2 == 1, ])
  x$pred_fp_ltw[x$cluster2 == 1] <- predict(m3_ltw_1, x[x$cluster2 == 1, ])
  x$pred_gr_ltw[x$cluster2 == 1] <- predict(m4_ltw_1, x[x$cluster2 == 1, ])
  x$pred_ne_ltw[x$cluster2 == 1] <- predict(m5_ltw_1, x[x$cluster2 == 1, ])
  x$pred_so_ltw[x$cluster2 == 1] <- predict(m6_ltw_1, x[x$cluster2 == 1, ])
  x$pred_nw_ltw[x$cluster2 == 1] <- predict(m7_ltw_1, x[x$cluster2 == 1, ])
  
  x$pred_vp_ltw[x$cluster2 == 2] <- predict(m1_ltw_2, x[x$cluster2 == 2, ])
  x$pred_sp_ltw[x$cluster2 == 2] <- predict(m2_ltw_2, x[x$cluster2 == 2, ])
  x$pred_fp_ltw[x$cluster2 == 2] <- predict(m3_ltw_2, x[x$cluster2 == 2, ])
  x$pred_gr_ltw[x$cluster2 == 2] <- predict(m4_ltw_2, x[x$cluster2 == 2, ])
  x$pred_ne_ltw[x$cluster2 == 2] <- predict(m5_ltw_2, x[x$cluster2 == 2, ])
  x$pred_so_ltw[x$cluster2 == 2] <- predict(m6_ltw_2, x[x$cluster2 == 2, ])
  x$pred_nw_ltw[x$cluster2 == 2] <- predict(m7_ltw_2, x[x$cluster2 == 2, ])
  
  # Korrektur der Predicts auf die Zahl der Wahlberechtigten
  names_pred_ltw <- c("pred_vp_ltw", "pred_sp_ltw", "pred_fp_ltw", "pred_gr_ltw", "pred_ne_ltw", "pred_so_ltw", "pred_nw_ltw")
  # check
  x$pred_wb_ltw <- rowSums(x[, names(x) %in% names_pred_ltw])
  x[, names(x) %in% names_pred_ltw] <- x[, names(x) %in% names_pred_ltw] * x$wber / x$pred_wb_ltw
  x$pred_wb_ltw <- rowSums(x[, names(x) %in% names_pred_ltw])
  # cbind(rowSums(data[, names(data) %in% names_pred_ltw]), x$wber)
  
  return(x)
}

model_nrw <- function(df){
  # Modelle LTW
  x <- df
  
  model_nrw <- " ~ -1 + vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19"
  
  # cluster 1
  sub <- x[x$ausg == 1 & x$cluster2 == 1, ]
  
  m1_nrw_1 <- glm(paste("vp", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m2_nrw_1 <- glm(paste("sp", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
  m3_nrw_1 <- glm(paste("fp", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
  m4_nrw_1 <- glm(paste("gr", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1))
  m5_nrw_1 <- glm(paste("ne", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1))
  m6_nrw_1 <- glm(paste("so", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m7_nrw_1 <- glm(paste("nw", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7))
  
  # cluster 2
  sub <- x[x$ausg == 1 & x$cluster2 == 2, ]
  
  m1_nrw_2 <- glm(paste("vp", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m2_nrw_2 <- glm(paste("sp", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
  m3_nrw_2 <- glm(paste("fp", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
  m4_nrw_2 <- glm(paste("gr", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1))
  m5_nrw_2 <- glm(paste("ne", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1))
  m6_nrw_2 <- glm(paste("so", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
  m7_nrw_2 <- glm(paste("nw", model_nrw), data = sub, family = quasipoisson(link = identity), weight = weight, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7))
  
  
  # Predicts
  x$pred_vp_nrw[x$cluster2 == 1] <- predict(m1_nrw_1, x[x$cluster2 == 1, ])
  x$pred_sp_nrw[x$cluster2 == 1] <- predict(m2_nrw_1, x[x$cluster2 == 1, ])
  x$pred_fp_nrw[x$cluster2 == 1] <- predict(m3_nrw_1, x[x$cluster2 == 1, ])
  x$pred_gr_nrw[x$cluster2 == 1] <- predict(m4_nrw_1, x[x$cluster2 == 1, ])
  x$pred_ne_nrw[x$cluster2 == 1] <- predict(m5_nrw_1, x[x$cluster2 == 1, ])
  x$pred_so_nrw[x$cluster2 == 1] <- predict(m6_nrw_1, x[x$cluster2 == 1, ])
  x$pred_nw_nrw[x$cluster2 == 1] <- predict(m7_nrw_1, x[x$cluster2 == 1, ])
  
  x$pred_vp_nrw[x$cluster2 == 2] <- predict(m1_nrw_2, x[x$cluster2 == 2, ])
  x$pred_sp_nrw[x$cluster2 == 2] <- predict(m2_nrw_2, x[x$cluster2 == 2, ])
  x$pred_fp_nrw[x$cluster2 == 2] <- predict(m3_nrw_2, x[x$cluster2 == 2, ])
  x$pred_gr_nrw[x$cluster2 == 2] <- predict(m4_nrw_2, x[x$cluster2 == 2, ])
  x$pred_ne_nrw[x$cluster2 == 2] <- predict(m5_nrw_2, x[x$cluster2 == 2, ])
  x$pred_so_nrw[x$cluster2 == 2] <- predict(m6_nrw_2, x[x$cluster2 == 2, ])
  x$pred_nw_nrw[x$cluster2 == 2] <- predict(m7_nrw_2, x[x$cluster2 == 2, ])
  
  # Korrektur der Predicts auf die Zahl der Wahlberechtigten
  names_pred_nrw <- c("pred_vp_nrw", "pred_sp_nrw", "pred_fp_nrw", "pred_gr_nrw", "pred_ne_nrw", "pred_so_nrw", "pred_nw_nrw")
  # check
  x$pred_wb_nrw <- rowSums(x[, names(x) %in% names_pred_nrw])
  x[, names(x) %in% names_pred_nrw] <- x[, names(x) %in% names_pred_nrw] * x$wber / x$pred_wb_nrw
  x$pred_wb_nrw <- rowSums(x[, names(x) %in% names_pred_nrw])
  # cbind(rowSums(data[, names(data) %in% names_pred_nrw]), x$wber)
  
  return(x)
}

model_merge <- function(df){
  x <- df
  
  ausg <- x[x$ausg == 1, ]
  ausg[names_dif] <- abs(cbind(ausg[names], ausg[names]) - cbind(ausg[names_pred_ltw], ausg[names_pred_nrw]))
  
  abw_ltw <- colSums(ausg[names_dif_ltw])
  abw_nrw <- colSums(ausg[names_dif_nrw])
  
  lambda <- abw_ltw / (abw_ltw + abw_nrw)
  
  x$pred_vp <- x$pred_vp_ltw * (1-lambda[1]) + x$pred_vp_nrw * lambda[1]
  x$pred_sp <- x$pred_sp_ltw * (1-lambda[2]) + x$pred_sp_nrw * lambda[2]
  x$pred_fp <- x$pred_fp_ltw * (1-lambda[3]) + x$pred_fp_nrw * lambda[3]
  x$pred_gr <- x$pred_gr_ltw * (1-lambda[4]) + x$pred_gr_nrw * lambda[4]
  x$pred_ne <- x$pred_ne_ltw * (1-lambda[5]) + x$pred_ne_nrw * lambda[5]
  x$pred_so <- x$pred_so_ltw * (1-lambda[6]) + x$pred_so_nrw * lambda[6]
  x$pred_nw <- x$pred_nw_ltw * (1-lambda[7]) + x$pred_nw_nrw * lambda[7]
  
  x$pred_vp[x$ausg == 1] <- x$vp[x$ausg == 1]
  x$pred_sp[x$ausg == 1] <- x$sp[x$ausg == 1]
  x$pred_fp[x$ausg == 1] <- x$fp[x$ausg == 1]
  x$pred_gr[x$ausg == 1] <- x$gr[x$ausg == 1]
  x$pred_ne[x$ausg == 1] <- x$ne[x$ausg == 1]
  x$pred_so[x$ausg == 1] <- x$so[x$ausg == 1]
  x$pred_nw[x$ausg == 1] <- x$nw[x$ausg == 1]
  
  # Negative Predicts korrigieren
  mins <- apply(x[names_pred], 1, min)
  mins <- which(mins < 0)
  if(length(mins) > 0) {
    print(paste("In der Gemeinde", x$name[mins], "gibt es negative predicts!"))
    
    # negative Predicts korrigieren
    x$pred_vp[x$pred_vp < 0] <- 0
    x$pred_sp[x$pred_sp < 0] <- 0
    x$pred_fp[x$pred_fp < 0] <- 0
    x$pred_gr[x$pred_gr < 0] <- 0
    x$pred_ne[x$pred_ne < 0] <- 0
    x$pred_so[x$pred_so < 0] <- 0
    x$pred_nw[x$pred_nw < 0] <- 0
    
    # Check prediction Urnenwahlergebnis
    # check3a <- round(colSums(x[, names(x) %in% names_pred]) / sum(x$wber) * 100, 1)
    # check3b <- round(check3a[1:7] / sum(check3a[1:6]) * 100, 1)
    print("Hochrechnung Urnenstimmen korrigiert")
    # print(cbind(check3a, c(check3b, NA)))
  }
  
  x$pred_wb <- rowSums(x[, names(x) %in% names_pred])
  
  x[, names(x) %in% names_pred] <- x[, names(x) %in% names_pred] * x$wber / x$pred_wb
  x$pred_wb <- rowSums(x[, names(x) %in% names_pred])
  
  return(x)
}

model_merge_bootstrap <- function(df){
  x <- df
  
  ausg <- x[x$ausg == 1, ]
  ausg[names_dif] <- abs(cbind(ausg[names], ausg[names]) - cbind(ausg[names_pred_ltw], ausg[names_pred_nrw]))
  
  abw_ltw <- colSums(ausg[names_dif_ltw])
  abw_nrw <- colSums(ausg[names_dif_nrw])
  
  lambda <- abw_ltw / (abw_ltw + abw_nrw)
  
  x$pred_vp <- x$pred_vp_ltw * (1-lambda[1]) + x$pred_vp_nrw * lambda[1]
  x$pred_sp <- x$pred_sp_ltw * (1-lambda[2]) + x$pred_sp_nrw * lambda[2]
  x$pred_fp <- x$pred_fp_ltw * (1-lambda[3]) + x$pred_fp_nrw * lambda[3]
  x$pred_gr <- x$pred_gr_ltw * (1-lambda[4]) + x$pred_gr_nrw * lambda[4]
  x$pred_ne <- x$pred_ne_ltw * (1-lambda[5]) + x$pred_ne_nrw * lambda[5]
  x$pred_so <- x$pred_so_ltw * (1-lambda[6]) + x$pred_so_nrw * lambda[6]
  x$pred_nw <- x$pred_nw_ltw * (1-lambda[7]) + x$pred_nw_nrw * lambda[7]
  
  x$pred_vp[x$ausg == 1] <- x$vp[x$ausg == 1]
  x$pred_sp[x$ausg == 1] <- x$sp[x$ausg == 1]
  x$pred_fp[x$ausg == 1] <- x$fp[x$ausg == 1]
  x$pred_gr[x$ausg == 1] <- x$gr[x$ausg == 1]
  x$pred_ne[x$ausg == 1] <- x$ne[x$ausg == 1]
  x$pred_so[x$ausg == 1] <- x$so[x$ausg == 1]
  x$pred_nw[x$ausg == 1] <- x$nw[x$ausg == 1]
  
  # Negative Predicts korrigieren
  mins <- apply(x[names_pred], 1, min)
  mins <- which(mins < 0)
  if(length(mins) > 0) {
    # print(paste("In der Gemeinde", x$name[mins], "gibt es negative predicts!"))
    
    # negative Predicts korrigieren
    x$pred_vp[x$pred_vp < 0] <- 0
    x$pred_sp[x$pred_sp < 0] <- 0
    x$pred_fp[x$pred_fp < 0] <- 0
    x$pred_gr[x$pred_gr < 0] <- 0
    x$pred_ne[x$pred_ne < 0] <- 0
    x$pred_so[x$pred_so < 0] <- 0
    x$pred_nw[x$pred_nw < 0] <- 0
    
    # Check prediction Urnenwahlergebnis
    # check3a <- round(colSums(x[, names(x) %in% names_pred]) / sum(x$wber) * 100, 1)
    # check3b <- round(check3a[1:7] / sum(check3a[1:6]) * 100, 1)
    # print("Hochrechnung Urnenstimmen korrigiert")
    # print(cbind(check3a, c(check3b, NA)))
  }
  
  x$pred_wb <- rowSums(x[, names(x) %in% names_pred])
  
  x[, names(x) %in% names_pred] <- x[, names(x) %in% names_pred] * x$wber / x$pred_wb
  x$pred_wb <- rowSums(x[, names(x) %in% names_pred])
  
  return(x)
}
