# 03_model

HR <- model_ltw(data)
HR <- model_nrw(HR)
HR <- model_merge(HR)


# Hochrechnung ergebnisse checken
check3a <- round(colSums(HR[names_pred]) / sum(HR$wber) * 100, 5)
check3b <- round(check3a[1:6] / sum(check3a[1:6]) * 100, 5)

check3a_ltw <- round(colSums(HR[names_pred_ltw]) / sum(HR$wber) * 100, 5)
check3b_ltw <- round(check3a_ltw[1:6] / sum(check3a_ltw[1:6]) * 100, 5)

check3a_nrw <- round(colSums(HR[names_pred_nrw]) / sum(HR$wber) * 100, 5)
check3b_nrw <- round(check3a_nrw[1:6] / sum(check3a_nrw[1:6]) * 100, 5)

check3 <- data.frame("gesamt" = check3a , "ohne NW" = c(check3b, NA),
           "LTW" = check3a_ltw, "LTW2" = c(check3b_ltw, NA),
           "NRW" = check3a_nrw, "NRW2" = c(check3b_nrw, NA))
# print(check3) <- 
check3 <- round(check3, 1)
print(check3)

# mit timestamp abspreichern
name <- paste0("save/urnenstimmen_", Sys.time(), ".csv")
name <- gsub(" ", "-", name)
name <- gsub(":", "-", name)
write.csv2(HR, name, row.names = F)

