# eine funktion, um die nichtwähler zu predicten
# zur Berechnung der Schwankungsbreite
# fehler!

pred_schwankungsbreite <- function(df, dfx, partei = "nw"){
  
  # settings
  parteien <- c("vp", "sp", "fp", "gr", "ne", "so", "nw")
  pred_names <- paste0("pred_", parteien)
  
  # modelle
  mvp <- lm(vp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = dfx)
  msp <- lm(sp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = dfx)
  mfp <- lm(fp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = dfx)
  mgr <- lm(gr24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = dfx)
  mne <- lm(ne24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = dfx)
  mso <- lm(so24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = dfx)
  mnw <- lm(nw24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = dfx)
  
  # predictions
  dfx$pred_vp <- predict(mvp, newdata = dfx)
  dfx$pred_sp <- predict(msp, newdata = dfx)
  dfx$pred_fp <- predict(mfp, newdata = dfx)
  dfx$pred_gr <- predict(mgr, newdata = dfx)
  dfx$pred_ne <- predict(mne, newdata = dfx)
  dfx$pred_so <- predict(mso, newdata = dfx)
  dfx$pred_nw <- predict(mnw, newdata = dfx)
  
  # predictions wo schon ausgezählt durch tatsächliche Werte Ersetzen
  dfx[dfx$ausg == 1, pred_names] <- dfx[dfx$ausg == 1, paste0(parteien, 24)]
  
  # predictions: nicht unter 0 abgegebenen Stimmen
  dfx[, pred_names] <- sapply(dfx[, pred_names], function(x){return(ifelse(x < 0, 0, x))})
  
  # summe der abgegeben Stimmen darf Summe der Wahlberechtigten nicht übersteigen
  pred_sum <- rowSums(dfx[, pred_names])
  dfx[, pred_names] <- dfx[, pred_names] * dfx$wber24 / pred_sum
  
  # return
  returnname <- paste0("pred_", partei)
  # out <- sum(dfx[returnname]) / sum(dfx$wber24)

  return(dfx[returnname])
}
