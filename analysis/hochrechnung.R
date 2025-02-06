#--------------------------------------------------------------------------------------------------
# Hochrechnung
#--------------------------------------------------------------------------------------------------

rm(list = ls()); gc()

# packages
packages = c("tidyverse", "readxl")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path = "R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

#--------------------------------------------------------------------------------------------------
# Hochrechnung hier in R machen
#--------------------------------------------------------------------------------------------------

# import ------------------------------------------------------------------------------------------

gem19 <- readxl::read_xlsx("data/prep_nrw19.xlsx", sheet = "gemeinden")
import <- readr::read_csv2("data/in_results-filtered.csv")


# wrangling ---------------------------------------------------------------------------------------

# nichtwähler:innen berechnen
# bei Hochrechnungen werden Nichtwähler:innen behandelt, als ob sie eine eigene Partei wären
# Damit sich in Summe alles immer auf 100 ausgeht
# Außerdem wird die Schwankungsbreite bei der stärksten "Partei" berechnet - das sind die Nichtwähler:innen
# ungültige Stimmen werden wie Nichtwähler:innen behandelt
gem19$nw <- gem19$wber - gem19$abg
import$nw <- import$wber - import$abg

# names nach jahren vergeben
parteien <- c("vp", "sp", "fp", "gr", "ne", "so", "nw")
namesx <- c("wber", "abg", "ung", "gueltig", parteien)
pred_names <- paste0("pred_", parteien)

merge <- import[, c("gkz", namesx)]
names(merge)[2:ncol(merge)] <- paste0(namesx, 24)

names(gem19)[4:ncol(gem19)] <- paste0(namesx, 19)

# datensatz erstellen
df <- left_join(gem19, merge)

# ausgezählte markieren
df$ausg <- ifelse(!is.na(df$abg24), 1, 0)

# Hochrechnung ------------------------------------------------------------------------------------

mvp <- lm(vp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = df)
msp <- lm(sp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = df)
mfp <- lm(fp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = df)
mgr <- lm(gr24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = df)
mne <- lm(ne24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = df)
mso <- lm(so24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = df)
mnw <- lm(nw24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = df)


# predictions -------------------------------------------------------------------------------------

df$pred_vp <- predict(mvp, newdata = df)
df$pred_sp <- predict(msp, newdata = df)
df$pred_fp <- predict(mfp, newdata = df)
df$pred_gr <- predict(mgr, newdata = df)
df$pred_ne <- predict(mne, newdata = df)
df$pred_so <- predict(mso, newdata = df)
df$pred_nw <- predict(mnw, newdata = df)

# predictions wo schon ausgezählt durch tatsächliche Werte Ersetzen
df[df$ausg == 1, pred_names] <- df[df$ausg == 1, paste0(parteien, 24)]

# predictions: nicht unter 0 abgegebenen Stimmen
# eleganter ließe sich das lösen, wenn bei Modellen nicht normalverteilung sondern poisson
df[, pred_names] <- sapply(df[, pred_names], function(x){return(ifelse(x < 0, 0, x))})

# summe der abgegeben Stimmen darf Summe der Wahlberechtigten nicht übersteigen
pred_sum <- rowSums(df[, pred_names])
df[, pred_names] <- df[, pred_names] * df$wber24 / pred_sum
sum(df[, pred_names]) == sum(df$wber24)

# results
results <- colSums(df[, pred_names]) / sum(df$wber24)
# results nur für parteien und gerundet
round(results[1:6] / sum(results[1:6]) * 100, 2)


# Schwankungsbreite -------------------------------------------------------------------------------
# bootstrapping: datensatz erstellen durch ziehen mit zurücklegen
# MSE = (1/n) * Σ(actual – prediction)2
# Schwankungsbreite nur für die Urnenstimmen!

out <- c()

n <- 100
for(i in 1:n){
  
  sample <- sample(1:nrow(df), nrow(df), replace = T)
  dfx <- df[sample, ]
  
  modellist <- hr_modelle(dfx)
  dfx <- hr_pred(dfx, modellist)
  
  bootstrap_fp <- sum(dfx$pred_fp) / sum(dfx[, pred_names[1:6]])
  
  out <- c(out, bootstrap_fp)
}

e <- (out - mean(out))*100
mse <- as.numeric(t(e) %*% e) / n
sb <- mse / mean(out*100) * 100
sb


