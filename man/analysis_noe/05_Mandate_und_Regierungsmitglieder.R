#######################################################################
###                                                                 ###
###   Prognose der Mandatsverteilung und der Regierungsmitglieder   ###
###                                                                 ###
#######################################################################




landnames <- c ("pred_vp", "pred_sp", "pred_fp", "pred_gr", "pred_ne", "pred_so")
land <- apply (HR[landnames], 2, sum)




### Check der 4-Prozent-Hürde, Nullstellen der Sonstigen

land_prozente <- land / sum(land) * 100
land_ueber4 <- ifelse (land_prozente > 4, 1, 0)
land_ueber4 [length (land_prozente)] <- 0




### Mandatsberechnung

dhondt <- matrix (rep (0, 56*length(land)), nrow = 56)
for (i in 1:56) {dhondt [i,] <- land / i}
for (j in 1:length(land)) {dhondt [,j] <- dhondt [,j] * land_ueber4 [j]}

kleinste_stimmenzahl_fuer_mandat    <- dhondt [order (dhondt, decreasing=T) [56]]
mandate   <- apply (ifelse(dhondt >= kleinste_stimmenzahl_fuer_mandat, 1, 0), 2, sum)




### Regierungsmitglieder

dhondt_reg <- matrix (rep (0, 9*length (land)), nrow=9)
for (i in 1:9) {dhondt_reg [i,] <- mandate / i + land_prozente/1000}
for (j in 1:length(land)) {dhondt_reg [,j] <- dhondt_reg [,j] * land_ueber4 [j]}

kleinste_stimmenzahl_fuer_landesrat <- dhondt_reg [order (dhondt_reg, decreasing=T) [9]]
regierung <- apply (ifelse(dhondt_reg >= kleinste_stimmenzahl_fuer_landesrat, 1, 0), 2, sum)

sitze <- as.data.frame (rbind (mandate, regierung))
names (sitze) <- landnames

print(sitze)
