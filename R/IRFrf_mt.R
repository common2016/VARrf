#' GIRF
#'
#' Compute GIRF by Martin (2011)
#'

IRFrf_mt <- function(fit, shock_varnm, s = 12,
                     n = 10, ncores = 7){
  shockvar <- which(names(fit$yvar) %in% shock_varnm)
  d <- sd(unlist(fit$yvar[shockvar]))

  # initialize
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  # parallel compute
  irf <- foreach::foreach (i = 1:n, fitp = rep(fit, n),
                           sp = rep(s, n), shockvarp = rep(shockvar, n),
                           dp = rep(d, n),
                           .packages = c('VARrf')) %dopar% {
                             lapply(1:nrow(fitp$xvar), VARrf_forcast,
                                    fit = fitp, s = sp, shockvar = shockvarp, d = dp)
                           }
  parallel::stopCluster(cl)
  return(irf)
}
