#' Estimate VAR by random forest
#'
#' @param data a data.frame
#' @param pmax a max lag order
#' @param p a fixed lag order. When \code{p} is set up, \code{pmax} is unuseful.
#' @param  s a horizon in local projections, default is 1. Gnerally don't change it.
#' @return An list:
#' \enumerate{
#'   \item{fit_rf}{An object of class \code{rfsrc, grow} from the package \code{randomForestSRC},
#'   and it has minimum OOB MSE in 1:\code{pmax}.}
#'   \item{p}{optimal lag order,and it has minimum OOB MSE.}
#' }


VARrf <- function(data, pmax = 5, p = NULL, s = 1){
  if (is.null(p)){
    mse_star <- numeric(pmax)
    fit_rf <- list()
    for (i in 1:pmax) {
      # generate X and Y
      ans <- bvartools::gen_var(ts(data), p = s - 1 + i, deterministic = 'none')
      regdata <- cbind(t(ans$Y),t(ans$Z)) %>% as.data.frame()
      # delete coloums between Y_t+s and Y_t
      if ( s != 1){
        del_col <- NULL
        for (j in 1:nrow(ans$Y)) {
          del_col <- c(del_col,paste(colnames(t(ans$Y))[j],'.',as.character(1:(s-1)), sep = ''))
        }
        regdata <- dplyr::select(regdata,-ends_with(del_col))
      }

      fml <- paste('fml <- cbind(',paste(colnames(t(ans$Y)),collapse = ','),')~.',sep = '')
      eval(parse(text = fml))
      fit_rf[[i]] <- randomForestSRC::rfsrc(fml, data = regdata)
      # minimum OOB MSE
      mse <- NULL
      for (j in 1:length(fit_rf[[i]]$regrOutput)) {
        mse <- c(mse,mean((fit_rf[[i]]$yvar[,j] - fit_rf[[i]]$regrOutput[[j]]$predicted.oob)^2))
      }
      mse_star[i] <- mean(mse)
    }
    fit_rf <- fit_rf[[which.min(mse_star)]]
    p_sele <- which.min(mse_star)
  }else {
    # generate X and Y
    ans <- bvartools::gen_var(ts(data), p = s -1 + p, deterministic = 'none')
    regdata <- cbind(t(ans$Y),t(ans$Z)) %>% as.data.frame()
    # delete coloums between Y_t+s and Y_t
    if ( s != 1){
      del_col <- NULL
      for (j in 1:nrow(ans$Y)) {
        del_col <- c(del_col,paste(colnames(t(ans$Y))[j],'.',as.character(1:(s-1)), sep = ''))
      }
      regdata <- dplyr::select(regdata,-ends_with(del_col))
    }

    fml <- paste('fml <- cbind(',paste(colnames(t(ans$Y)),collapse = ','),')~.',sep = '')
    eval(parse(text = fml))
    fit_rf <- randomForestSRC::rfsrc(fml, data = regdata)
    p_sele <- p
  }
  return(list(fit_rf = fit_rf, p = p_sele, s = s))
}
