#' Generalized IRF
#'
#' @inheritParams
#' @import foreach
#' @export


IRFrf_gen <- function(data,pmax = 5, s = 12, d = 1,shockvar = 1,ncores = 6){
  # para set up
  itevar <- rep(list(data = data), nrow(data) - pmax)
  nhist <- nrow(data) - pmax - s # nrow of xvar
  pmax_para <- rep(pmax,nhist)
  s <- rep(s,nhist)
  d <- rep(d,nhist)
  shockvar <- rep(shockvar,nhist)

  # initialize
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  # parallel compute
  picdata <- foreach::foreach(i = 1:nhist,itevar = itevar,
                              pmax = pmax_para,s = s, shockvar = shockvar,
                              .packages = 'tidyverse') %dopar% {
    devtools::load_all()
    IRFrf(data = itevar, pmax = pmax, s = s, shockvar = shockvar,d = d, histime = i)
  }
  parallel::stopCluster(cl)
  return(picdata)
}

