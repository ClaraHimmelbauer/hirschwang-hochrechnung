rm(list = ls())

packages <- c("readxl", "tidyverse", "httr")
lapply(packages, library, character.only = T)

#--------------------------------------------------------------------------------------------------

# Verschiedene Schätzarten ausprobieren und schauen wie nah/weit wir um 17:00 damit daneben gelgen wären
  # nicht geclustert
  # Impfquote als zusätzliche Variable
  # Impfquote x VP
  # impfquote x FP
  # gemgr x sp

#--------------------------------------------------------------------------------------------------

# zuerst aber alles einlesen ----------------------------------------------------------------------

source("analysis/00b_names.R", encoding = "UTF-8")
source("analysis/00c_functions.R", encoding = "UTF-8")

# Daten von 17:00 ---------------------------------------------------------------------------------

df <- read.csv2("save/urnenstimmen_2023-01-29-17-00-54.csv")

# impfdaten ---------------------------------------------------------------------------------------
covid <- read.csv2("data/covid_impfung_gemeinden_20230329.csv")
covid$vac1 <- covid$vaccination_1 / covid$municipality_population
covid$vac3 <- covid$vaccination_3 / covid$municipality_population
covid <- covid[c("municipality_id", "vac1", "vac3")]

df <- left_join(df, covid, by = c("gkz" = "municipality_id"))

# Hocdfechnung ergebnisse checken -- was wäre bei Clustering herausgekommen -----------------------
# checks nur noch mit ltw modell, ohne NRW

check_ltw <- round(colSums(df[names_pred_ltw]) / sum(df$wber) * 100, 5)
check_ltwb <- round(check_ltw[1:6] / sum(check_ltw[1:6]) * 100, 5)

check1 <- data.frame("gesamt" = check_ltw , "ohne NW" = c(check_ltwb, NA))
check1 <- round(check1, 1)
print(check1)


#--------------------------------------------------------------------------------------------------
# ohne clustering
#--------------------------------------------------------------------------------------------------

model_ltw <- " ~ -1 + vp18 + sp18 + fp18 + gr18 + ne18 + so18 + nw18"
sub <- df # noch ein überbleibsel vom Clustering
x <- sub[sub$ausg == 1, ]

m1_ltw_1 <- glm(paste("vp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m2_ltw_1 <- glm(paste("sp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
m3_ltw_1 <- glm(paste("fp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
m4_ltw_1 <- glm(paste("gr", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1))
m5_ltw_1 <- glm(paste("ne", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1))
m6_ltw_1 <- glm(paste("so", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m7_ltw_1 <- glm(paste("nw", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7))

# Predicts
sub$pred_vp_ltw <- predict(m1_ltw_1, sub)
sub$pred_sp_ltw <- predict(m2_ltw_1, sub)
sub$pred_fp_ltw <- predict(m3_ltw_1, sub)
sub$pred_gr_ltw <- predict(m4_ltw_1, sub)
sub$pred_ne_ltw <- predict(m5_ltw_1, sub)
sub$pred_so_ltw <- predict(m6_ltw_1, sub)
sub$pred_nw_ltw <- predict(m7_ltw_1, sub)

# Korrektur der Predicts auf die Zahl der Wahlberechtigten
names_pred_ltw <- c("pred_vp_ltw", "pred_sp_ltw", "pred_fp_ltw", "pred_gr_ltw", "pred_ne_ltw", "pred_so_ltw", "pred_nw_ltw")
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])
sub[, names(sub) %in% names_pred_ltw] <- sub[, names(sub) %in% names_pred_ltw] * sub$wber / sub$pred_wb_ltw
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])

# die schon ausgezählten korrigieren
sub[names_pred_ltw][sub$ausg == 1, ] <- sub[names][sub$ausg == 1, ]

# check
check_ltw <- round(colSums(sub[names_pred_ltw]) / sum(sub$wber) * 100, 5)
check_ltwb <- round(check_ltw[1:6] / sum(check_ltw[1:6]) * 100, 5)

check2 <- data.frame("gesamt" = check_ltw , "ohne NW" = c(check_ltwb, NA))
check2 <- round(check2, 1)
print(check2)

#--------------------------------------------------------------------------------------------------
# impfstatus als zusätzliche variable
#--------------------------------------------------------------------------------------------------

sub <- df # noch ein überbleibsel vom Clustering
sub$vac1vp <- sub$vac1 * sub$vp / sub$wber
model_ltw <- " ~ -1 + vp18 + sp18 + fp18 + gr18 + ne18 + so18 + nw18 + vac1"
x <- sub[sub$ausg == 1, ]

m1_ltw_1 <- glm(paste("vp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m2_ltw_1 <- glm(paste("sp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m3_ltw_1 <- glm(paste("fp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
m4_ltw_1 <- glm(paste("gr", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
m5_ltw_1 <- glm(paste("ne", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1))
m6_ltw_1 <- glm(paste("so", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m7_ltw_1 <- glm(paste("nw", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7, 0.1))

# Predicts
sub$pred_vp_ltw <- predict(m1_ltw_1, sub)
sub$pred_sp_ltw <- predict(m2_ltw_1, sub)
sub$pred_fp_ltw <- predict(m3_ltw_1, sub)
sub$pred_gr_ltw <- predict(m4_ltw_1, sub)
sub$pred_ne_ltw <- predict(m5_ltw_1, sub)
sub$pred_so_ltw <- predict(m6_ltw_1, sub)
sub$pred_nw_ltw <- predict(m7_ltw_1, sub)

# Korrektur der Predicts auf die Zahl der Wahlberechtigten
names_pred_ltw <- c("pred_vp_ltw", "pred_sp_ltw", "pred_fp_ltw", "pred_gr_ltw", "pred_ne_ltw", "pred_so_ltw", "pred_nw_ltw")
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])
sub[, names(sub) %in% names_pred_ltw] <- sub[, names(sub) %in% names_pred_ltw] * sub$wber / sub$pred_wb_ltw
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])

# die schon ausgezählten korrigieren
sub[names_pred_ltw][sub$ausg == 1, ] <- sub[names][sub$ausg == 1, ]

# check
check_ltw <- round(colSums(sub[names_pred_ltw]) / sum(sub$wber) * 100, 5)
check_ltwb <- round(check_ltw[1:6] / sum(check_ltw[1:6]) * 100, 5)

check3 <- data.frame("gesamt" = check_ltw , "ohne NW" = c(check_ltwb, NA))
check3 <- round(check3, 1)
print(check3)

# und mit interaktionsterm 1 ----------------------------------------------------------------------


sub <- df # noch ein überbleibsel vom Clustering
sub$vac1vp <- sub$vac1 * sub$vp / sub$wber
model_ltw <- " ~ -1 + vp18 + sp18 + fp18 + gr18 + ne18 + so18 + nw18 + vac1 + vac1vp"
x <- sub[sub$ausg == 1, ]

m1_ltw_1 <- glm(paste("vp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m2_ltw_1 <- glm(paste("sp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m3_ltw_1 <- glm(paste("fp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m4_ltw_1 <- glm(paste("gr", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
m5_ltw_1 <- glm(paste("ne", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
m6_ltw_1 <- glm(paste("so", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m7_ltw_1 <- glm(paste("nw", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1))

# Predicts
sub$pred_vp_ltw <- predict(m1_ltw_1, sub)
sub$pred_sp_ltw <- predict(m2_ltw_1, sub)
sub$pred_fp_ltw <- predict(m3_ltw_1, sub)
sub$pred_gr_ltw <- predict(m4_ltw_1, sub)
sub$pred_ne_ltw <- predict(m5_ltw_1, sub)
sub$pred_so_ltw <- predict(m6_ltw_1, sub)
sub$pred_nw_ltw <- predict(m7_ltw_1, sub)

# Korrektur der Predicts auf die Zahl der Wahlberechtigten
names_pred_ltw <- c("pred_vp_ltw", "pred_sp_ltw", "pred_fp_ltw", "pred_gr_ltw", "pred_ne_ltw", "pred_so_ltw", "pred_nw_ltw")
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])
sub[, names(sub) %in% names_pred_ltw] <- sub[, names(sub) %in% names_pred_ltw] * sub$wber / sub$pred_wb_ltw
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])

# die schon ausgezählten korrigieren
sub[names_pred_ltw][sub$ausg == 1, ] <- sub[names][sub$ausg == 1, ]

# check
check_ltw <- round(colSums(sub[names_pred_ltw]) / sum(sub$wber) * 100, 5)
check_ltwb <- round(check_ltw[1:6] / sum(check_ltw[1:6]) * 100, 5)

check4 <- data.frame("gesamt" = check_ltw , "ohne NW" = c(check_ltwb, NA))
check4 <- round(check4, 1)
print(check4)

# interaktionsterm 2 ------------------------------------------------------------------------------

sub <- df # noch ein überbleibsel vom Clustering
sub$vac1fp <- sub$vac1 * sub$fp / sub$wber
model_ltw <- " ~ -1 + vp18 + sp18 + fp18 + gr18 + ne18 + so18 + nw18 + vac1 + vac1fp"
x <- sub[sub$ausg == 1, ]

m1_ltw_1 <- glm(paste("vp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m2_ltw_1 <- glm(paste("sp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m3_ltw_1 <- glm(paste("fp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
m4_ltw_1 <- glm(paste("gr", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
m5_ltw_1 <- glm(paste("ne", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1))
m6_ltw_1 <- glm(paste("so", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m7_ltw_1 <- glm(paste("nw", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7, 0.1))

# Predicts
sub$pred_vp_ltw <- predict(m1_ltw_1, sub)
sub$pred_sp_ltw <- predict(m2_ltw_1, sub)
sub$pred_fp_ltw <- predict(m3_ltw_1, sub)
sub$pred_gr_ltw <- predict(m4_ltw_1, sub)
sub$pred_ne_ltw <- predict(m5_ltw_1, sub)
sub$pred_so_ltw <- predict(m6_ltw_1, sub)
sub$pred_nw_ltw <- predict(m7_ltw_1, sub)

# Korrektur der Predicts auf die Zahl der Wahlberechtigten
names_pred_ltw <- c("pred_vp_ltw", "pred_sp_ltw", "pred_fp_ltw", "pred_gr_ltw", "pred_ne_ltw", "pred_so_ltw", "pred_nw_ltw")
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])
sub[, names(sub) %in% names_pred_ltw] <- sub[, names(sub) %in% names_pred_ltw] * sub$wber / sub$pred_wb_ltw
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])

# die schon ausgezählten korrigieren
sub[names_pred_ltw][sub$ausg == 1, ] <- sub[names][sub$ausg == 1, ]

# check
check_ltw <- round(colSums(sub[names_pred_ltw]) / sum(sub$wber) * 100, 5)
check_ltwb <- round(check_ltw[1:6] / sum(check_ltw[1:6]) * 100, 5)

check5 <- data.frame("gesamt" = check_ltw , "ohne NW" = c(check_ltwb, NA))
check5 <- round(check5, 1)
print(check5)


#--------------------------------------------------------------------------------------------------
# gemeindegröße x sp
#--------------------------------------------------------------------------------------------------

sub <- df # noch ein überbleibsel vom Clustering
sub$gemgr <- log(sub$wber)
sub$gemgrsp <- sub$gemgr * sub$sp / sub$wber
model_ltw <- " ~ -1 + vp18 + sp18 + fp18 + gr18 + ne18 + so18 + nw18 + gemgr + gemgrsp"
x <- sub[sub$ausg == 1, ]

m1_ltw_1 <- glm(paste("vp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m2_ltw_1 <- glm(paste("sp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m3_ltw_1 <- glm(paste("fp", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m4_ltw_1 <- glm(paste("gr", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1))
m5_ltw_1 <- glm(paste("ne", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1, 0.1, 0.1))
m6_ltw_1 <- glm(paste("so", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
m7_ltw_1 <- glm(paste("nw", model_ltw), data = x, family = quasipoisson(link = identity), weight = weight2, start = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.7, 0.1, 0.1))

# Predicts
sub$pred_vp_ltw <- predict(m1_ltw_1, sub)
sub$pred_sp_ltw <- predict(m2_ltw_1, sub)
sub$pred_fp_ltw <- predict(m3_ltw_1, sub)
sub$pred_gr_ltw <- predict(m4_ltw_1, sub)
sub$pred_ne_ltw <- predict(m5_ltw_1, sub)
sub$pred_so_ltw <- predict(m6_ltw_1, sub)
sub$pred_nw_ltw <- predict(m7_ltw_1, sub)

# Korrektur der Predicts auf die Zahl der Wahlberechtigten
names_pred_ltw <- c("pred_vp_ltw", "pred_sp_ltw", "pred_fp_ltw", "pred_gr_ltw", "pred_ne_ltw", "pred_so_ltw", "pred_nw_ltw")
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])
sub[, names(sub) %in% names_pred_ltw] <- sub[, names(sub) %in% names_pred_ltw] * sub$wber / sub$pred_wb_ltw
sub$pred_wb_ltw <- rowSums(sub[, names(sub) %in% names_pred_ltw])

# die schon ausgezählten korrigieren
sub[names_pred_ltw][sub$ausg == 1, ] <- sub[names][sub$ausg == 1, ]

# check
check_ltw <- round(colSums(sub[names_pred_ltw]) / sum(sub$wber) * 100, 5)
check_ltwb <- round(check_ltw[1:6] / sum(check_ltw[1:6]) * 100, 5)

check6 <- data.frame("gesamt" = check_ltw , "ohne NW" = c(check_ltwb, NA))
check6 <- round(check6, 1)
print(check6)


#--------------------------------------------------------------------------------------------------
# export
#--------------------------------------------------------------------------------------------------

ex <- cbind(check1, check2, check3, check4, check5, check6)
write.csv2(ex, "analysis/modelle_impf_gemgr.csv")
