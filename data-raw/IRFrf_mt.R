varnm = 'y1'
fit = fit$fit_rf

shockvar = which(names(fit$yvar) %in% varnm)
d <- sd(unlist(fit$fit_rf$yvar[shockvar]))
rlt <- list()
for (i in 1:20) {
  rlt <- lapply(1:46, VARrf_forcast, fit = fit$fit_rf, s = 12, shockvar = shockvar, d = d) %>%
    c(rlt)
}
