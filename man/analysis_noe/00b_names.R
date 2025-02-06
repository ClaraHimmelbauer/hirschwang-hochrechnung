# all names

names <- c("vp", "sp", "fp", "gr", "ne", "so", "nw")
names_ltw <- c("vp18", "sp18", "fp18", "gr18", "ne18", "so18", "nw18")
names_nrw <- c("vp19", "sp19", "fp19", "gr19", "ne19", "so19", "nw19")

names_pred <- c("pred_vp", "pred_sp", "pred_fp", "pred_gr", "pred_ne", "pred_so", "pred_nw")
names_pred_ltw <- c("pred_vp_ltw", "pred_sp_ltw", "pred_fp_ltw", "pred_gr_ltw", "pred_ne_ltw", "pred_so_ltw", "pred_nw_ltw")
names_pred_nrw <- c("pred_vp_nrw", "pred_sp_nrw", "pred_fp_nrw", "pred_gr_nrw", "pred_ne_nrw", "pred_so_nrw", "pred_nw_nrw")

names_dif <- paste0(c(names_ltw, names_nrw), "_dif")
names_dif_ltw <- names_dif[1:7]
names_dif_nrw <- names_dif[8:14]
