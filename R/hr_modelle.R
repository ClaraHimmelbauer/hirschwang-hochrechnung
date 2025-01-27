# modelle für die hochrechnung und dann in Liste rausgeben

hr_modelle <- function(data){

  # modelle
  mvp <- lm(vp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = data)
  msp <- lm(sp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = data)
  mfp <- lm(fp24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = data)
  mgr <- lm(gr24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = data)
  mne <- lm(ne24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = data)
  mso <- lm(so24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = data)
  mnw <- lm(nw24 ~ vp19 + sp19 + fp19 + gr19 + ne19 + so19 + nw19, data = data)
  
  outlist <- list(mvp, msp, mfp, mgr, mne, mso, mnw)
  return(outlist)
}
