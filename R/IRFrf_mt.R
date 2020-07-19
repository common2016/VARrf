#' GIRF
#'
#' Compute GIRF by Martin (2011)
#'

IRFrf_mt <- function(fit, shock_varnm, s = 12,
                     n = 10, ncores = 7){
  # browser()
  shockvar <- which(names(fit$yvar) %in% shock_varnm)
  # d <- sd(unlist(fit$yvar[shockvar]))
  d <- 1
  nx <- 1:nrow(fit$xvar)

  # initialize
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  # parallel compute
  irf <- foreach::foreach (nxp = rep(list(nx),n), fitp = rep(list(fit), n),
                           sp = rep(s, n), shockvarp = rep(shockvar, n),
                           dp = rep(d, n), .packages = c('tidyverse')) %dopar% {
                             devtools::load_all('E:\\27_MyRPackages\\VARrf')
                             lapply(nxp, VARrf_forcast,
                                    fit = fitp, s = sp, shockvar = shockvarp, d = dp)
                           }
  parallel::stopCluster(cl)
  return(irf)
}
