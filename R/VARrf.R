#' Estimate VAR by random forest
#'
#' @param data a data.frame, and all variables is endogenous variables in VAR.
#' @param indx if \code{data} is time series, \code{indx} is NULL. If \code{data}
#' is panal data, \code{indx} is a character string vector whose length is 2, the 1st element
#' is coloumn name of individual id, and the 2nd element is coloumn names of time.
#' @param pmax a max lag order, default value is 5.
#' @param p a fixed lag order. When \code{p} is set up, \code{pmax} is unuseful.
#' @param  s a horizon in local projections, default is 1. Gnerally don't change it.
#' @return An list:
#' \enumerate{
#'   \item{fit_rf}{An object of class \code{rfsrc, grow} from the package \code{randomForestSRC},
#'   and it has minimum OOB MSE in 1:\code{pmax}.}
#'   \item{p}{optimal lag order,and it has minimum OOB MSE.}
#' }
#' @examples
#' B1 <- matrix(c(0.2, 0.1, 0.2, 0.3,0.3,0.4,0.2,0.1), 2)
#' sim_dt <- vars::VAR.sim(B1, n = 200, lag = 2,include = 'none',show.parMat = T) %>%
#'   as.data.frame()
#'   colnames(sim_dt) <- c('y1','y2')
#' # fit a VAR with max lag order equal 5
#' fit <- VARrf(sim_dt, pmax = 5)
#' # fit a VAR with fixed lag order equal 4
#' fit <- VARrf(sim_dt, p = 4)
#' # IRF
#' picdata <- IRFrf(data = sim_dt, pmax = 5, s = 12, shockvar = 1)
#' ggplot(data = picdata, aes(x = s, y = y1)) + geom_line() +
#'   geom_hline(yintercept = 0) + theme_bw()
#' # Generalized IRF
#' IRFrf_gen(data = sim_dt[1:20,], pmax = 3, s = 5, shockvar = 1, ncores = 7)
#'
#'
#' @export
#' @import magrittr


VARrf <- function(data,indx = NULL, pmax = 5, p = NULL, s = 1){
  if (is.null(p)){
    mse_star <- numeric(pmax)
    fit_rf <- list()
    # select lag order or set up lag order?
    for (i in 1:pmax) {
      # panel data or not?
      if (is.null(indx)){
        regdata <- dt_gen(data, s = s, plag = i, yname = T)[['regdata']]
        yname <- dt_gen(data, s = s, plag = i, yname = T)[['yname']]
      }else {
        ans <- dt_gen_panel(data, indx = indx,s = s, plag = i, yname = T)
        regdata <- ans[['regdata']] %>% dplyr::select(-indx[1],-indx[2])
        yname <- ans[['yname']]
      }

      fml <- paste('fml <- cbind(',paste(yname,collapse = ','),')~.',sep = '')
      eval(parse(text = fml))
      fit_rf[[i]] <- randomForestSRC::rfsrc(fml, data = regdata)
      # minimum OOB MSE
      mse <- NULL
      for (j in 1:length(fit_rf[[i]]$regrOutput)) {
        mse <- c(mse,mean((fit_rf[[i]]$yvar[,j] - fit_rf[[i]]$regrOutput[[j]]$predicted.oob)^2))
      }
      mse_star[i] <- mean(mse)
    }
    # browser()
    fit_rf <- fit_rf[[which.min(mse_star)]]
    p_sele <- which.min(mse_star)
  }else {
    # panel data or not?
    if (is.null(indx)){
      regdata <- dt_gen(data, s = s, plag = p, yname = T)[['regdata']]
      yname <- dt_gen(data, s = s, plag = p, yname = T)[['yname']]
    }else {
      ans <- dt_gen_panel(data,indx = indx, s = s, plag = p, yname = T)
      regdata <- ans[['regdata']] %>% dplyr::select(-indx[1],-indx[2])
      yname <- ans[['yname']]
    }

    fml <- paste('fml <- cbind(',paste(yname,collapse = ','),')~.',sep = '')
    eval(parse(text = fml))
    fit_rf <- randomForestSRC::rfsrc(fml, data = regdata)
    p_sele <- p
  }
  return(list(fit_rf = fit_rf, p = p_sele, s = s))
}
