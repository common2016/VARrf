#' Generalized IRF
#'
#'
#' @export


IRFrf_gen <- function(data,pmax = 5, s = 12, shockvar = 1,ncores = 6){
  # para set up
  itevar <- rep(list(data = data), nrow(data) - pmax)
  nhist <- nrow(data) - pmax - s # nrow of xvar
  pmax_para <- rep(pmax,nhist)
  s <- rep(s,nhist)
  shockvar <- rep(shockvar,nhist)

  # initialize
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  # parallel compute
  picdata <- foreach::foreach(i = 1:nhist,itevar = itevar,
                              pmax = pmax_para,s = s, shockvar = shockvar,
                              .packages = 'tidyverse') %dopar% {
    devtools::load_all()
    IRFrf(data = itevar, pmax = pmax, s = s, shockvar = shockvar,histime = i)
  }
  parallel::stopCluster(cl)
  return(picdata)
}

