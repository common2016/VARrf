#' Compute Local Projection IRF by Random Forest Method
#'
#' @param data
#' @param indx if \code{data} is time series, \code{indx} is NULL. If \code{data}
#' is panal data, \code{indx} is a character string vector whose length is 2, the 1st element
#' is coloumn name of individual id, and the 2nd element is coloumn names of time.
#' @param pmax max lag order where select a lag order which has minimum OOB MSE.
#' @param p set up the lag order directly, and don't select from \code{pmax}.
#' @param s the horizon of IRF
#' @param shockvar a numeric scalor which denotes the shock variable.
#' @param d the size of shock
#' Impulse Response Function by Random Forest
#' @param histime the numerical scalor that represents the time of shock, i.e. row index
#' of \code{data}.
#' @examples
#' # see VARrf function
#'
#' @export
IRFrf <- function(data,indx = NULL, pmax = 5, p = NULL, s = 12, shockvar = 1, d = 1,histime = 1){
  # initialize, and get yvar and shock var
  # browser()
  yvar <- dt_gen(regdata = data, indx = indx, s = s, plag = 1, yname = T)[['yname']] # just for yvar
  # fit <- VARrf(data = data, p = 1)
  # yvar <- names(fit$fit_rf$yvar)
  irf_bench <- matrix(NA, nrow = s, ncol = 1 + length(yvar)) %>% as.data.frame()
  names(irf_bench) <- c('s',yvar)
  irf_bench$s <- 1:s
  irf_diff <- irf_innov <- irf_bench

  for (j in 1:s){
    shockvar_med <- paste(yvar[shockvar],'.', as.character(j), sep = '')

    fit <- VARrf(data = data, indx = indx, pmax = pmax, p = p, s = j)
    innov <- fit$fit_rf$xvar
    innov <- innov[histime,]

    # get d_i
    # innov[,shockvar_med] <- 0
    bench <- innov
    innov[,shockvar_med] <- innov[,shockvar_med] + d

    # E[y|d_i=1] - E[y_i|d_i=0]
    for (i in yvar) {
      irf_bench[irf_bench$s == j, i] <-
        randomForestSRC::predict.rfsrc(fit$fit_rf, newdata = bench)[['regrOutput']][[i]][['predicted']]
      irf_innov[irf_innov$s == j, i] <-
        randomForestSRC::predict.rfsrc(fit$fit_rf, newdata = innov)[['regrOutput']][[i]][['predicted']]
      irf_diff[irf_innov$s == j, i] <- irf_innov[irf_innov$s == j, i] - irf_bench[irf_innov$s == j, i]
    }
  }
  return(irf_diff)
}
