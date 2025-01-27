hr_pred <- function(data, modellist){
  
  data$pred_vp <- predict(modellist[[1]], newdata = data)
  data$pred_sp <- predict(modellist[[2]], newdata = data)
  data$pred_fp <- predict(modellist[[3]], newdata = data)
  data$pred_gr <- predict(modellist[[4]], newdata = data)
  data$pred_ne <- predict(modellist[[5]], newdata = data)
  data$pred_so <- predict(modellist[[6]], newdata = data)
  data$pred_nw <- predict(modellist[[7]], newdata = data)
  
  # predictions wo schon ausgezählt durch tatsächliche Werte Ersetzen
  # data[data$ausg == 1, pred_names] <- data[data$ausg == 1, paste0(parteien, 24)]
  
  # predictions: nicht unter 0 abgegebenen Stimmen
  # eleganter ließe sich das lösen, wenn bei Modellen nicht normalverteilung sondern poisson
  data[, pred_names] <- sapply(data[, pred_names], function(x){return(ifelse(x < 0, 0, x))})
  
  # summe der abgegeben Stimmen darf Summe der Wahlberechtigten nicht übersteigen
  pred_sum <- rowSums(data[, pred_names])
  data[, pred_names] <- data[, pred_names] * data$wber24 / pred_sum

  return(data)
}
