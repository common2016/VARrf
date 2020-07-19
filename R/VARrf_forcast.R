#' Forcast Y Using Random Forest method
#'
#'
#' @param fit a object from \code{rfsrc}
#' @param s a numerical scalor, horizon of predicting.
#' @param startvalue a data frame with 1 row, and its coloums is same with xvar in
#' the estimated result from \code{rfsrc}.
#' @param shockvar which variable is shock variable.
#' @param d size of shock, default value is 1.
#' @param n_eps
#'
#' @note each variable is de-meaned
#' @details algorithm as follow:
#'\enumerate{
#'   \item step 1, estimate the model, and get the residuals
#'   \eqn{\hat{e}_1,\cdots,\hat{e}_T}.
#'   \item step 2, simulate the model for \code{s} horizons with \code{s}
#'   residuals are randomly drawn from
#'   \item step 3,
#'}
#' @return a list with 2 elements, the 1st elements is a forcast with no shock, and
#' the 2nd element is a forcast with shock.

VARrf_forcast <- function(fit, s = 12, startvalue = NULL,  shockvar = 1, d = 1, n_eps = 1){
  # initialize
  eps <- fit$yvar - randomForestSRC::get.mv.predicted(fit)
  stvalue_shock <- stvalue_bench <- fit$xvar[nrow(fit$xvar),]
  if (!is.null(startvalue)){
    stvalue_shock[1,] <- stvalue_bench[1,] <- startvalue
  }
  bench_fst <- matrix(NA, nrow = s, ncol = ncol(eps)) %>%
    as.data.frame()
  names(bench_fst) <- names(eps)
  shock_fst <- bench_fst
  pmax <- str_split_fixed(names(stvalue_bench),'\\.',2)[,2] %>% as.numeric() %>%
    max()
  yname <- str_split_fixed(names(stvalue_bench),'\\.',2)[,1] %>% unique()
  # browser()

  for (j in 1:s) {
    # the 1st horizon
    if (j == 1){
      # haty with no epsilon
      bench_fst[j,] <- randomForestSRC::predict.rfsrc(fit, newdata = stvalue_bench) %>%
        randomForestSRC::get.mv.predicted()

      shock <- matrix(0, nrow = 1, ncol = ncol(eps)) %>% as.data.frame()
      shock[,shockvar] <- d
      # haty with epsilon
      shock_fst[j,] <- bench_fst[j,] + eps[n_eps,] + shock
      bench_fst[j,] <- bench_fst[j,] + eps[sample(1:nrow(eps),1),]
    }else {
      # other horizons
      ans <- eps[sample(1:nrow(eps),1),]
      bench_fst[j,] <- randomForestSRC::predict.rfsrc(fit, newdata = stvalue_bench) %>%
        randomForestSRC::get.mv.predicted() %>%
        magrittr::add(ans)
      shock_fst[j,] <- randomForestSRC::predict.rfsrc(fit, newdata = stvalue_shock) %>%
        randomForestSRC::get.mv.predicted() %>%
        magrittr::add(ans)
    }

    # update stvalue_bench, stvalue_shock
    if (pmax >= 2){ # lag order is larger than 2
      for (i in seq(pmax,2,-1)) {
        stvalue_bench[,paste(yname,as.character(i),sep = '.')] <-
          stvalue_bench[,paste(yname,as.character(i-1),sep = '.')]
        stvalue_shock[,paste(yname,as.character(i),sep = '.')] <-
          stvalue_shock[,paste(yname,as.character(i-1),sep = '.')]
      }
    }
    stvalue_bench[,paste(yname,'1',sep = '.')] <- bench_fst[j,]
    stvalue_shock[,paste(yname,'1',sep = '.')] <- shock_fst[j,]
  }
  irf <- shock_fst - bench_fst
  irf$s <- 1:s
  # shock_fst$s <- bench_fst$s <- 1:s
  return(irf = irf)
}
