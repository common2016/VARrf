#' Generalized IRF
#'
#' @description GIRF using LP method by
#' @inheritParams IRFrf
#' @importFrom  foreach `%dopar%`
#' @export

IRFrf_gen <- function(data,pmax = 5, s = 12, d = 1,shockvar = 1,ncores = 6){

  # para set up
  nhist <- nrow(data) - pmax - s # nrow of xvar

  # initialize
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  # parallel compute
  picdata <- foreach::foreach(i = 1:nhist,itevar = rep(list(data = data), nhist),dp = rep(d,nhist),
                              pmaxp = rep(pmax,nhist),sp = rep(s,nhist), shockvarp = rep(shockvar,nhist),
                              .packages = c('VARrf','tidyverse')) %dopar% {
    IRFrf(data = itevar, pmax = pmaxp, s = sp, shockvar = shockvarp,d = dp, histime = i)
  }
  parallel::stopCluster(cl)
  return(picdata)
}

