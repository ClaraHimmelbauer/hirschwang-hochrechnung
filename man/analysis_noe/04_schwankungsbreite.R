# 04: Schwankungsbreite

bootstrap_vp <- c()

# time <- Sys.time()
# sb_iterations <- 50 # vorne in Steuerung festlegen
n <- sb_iterations
for(i in 1:n){
  tryCatch({
    resample <- data[sample(1:nrow(data), nrow(data), replace = T), ]
    
    resample_hr <- model_ltw(resample)
    resample_hr <- model_nrw(resample_hr)
    resample_hr <- model_merge_bootstrap(resample_hr)
    
    sample_vp <- sum(resample_hr$pred_vp) / sum(resample_hr[, names_pred[1:6]])
    bootstrap_vp <- c(bootstrap_vp, sample_vp)
  }, error = function(e){cat()})
}
# Sys.time() - time
sb <- 2 * 100 * sqrt (var(bootstrap_vp) - 1/40000)
# e <- (bootstrap_vp - mean(bootstrap_vp))*100
# mse <- as.numeric(t(e) %*% e) / n
# sb <- mse / mean(bootstrap_vp*100) * 100

print(paste0("Schwankungsbreite: ", round(sb, 1)))
print(paste0("Anzahl Iterationen: ", length(bootstrap_vp)))
