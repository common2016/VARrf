#' Transfer Data
#'
#' @description transfer raw data into VAR data.
#'
#' @param regdata a data frame, either a time series or panal data
#' @param indx if \code{regdata} is time series, \code{indx} is NULL. If \code{regdata}
#' is panal data, \code{indx} is a character string vector whose length is 2, the 1st element
#' is coloumn name of individual id, and the 2nd element is coloumn names of time.
#' @param s a horizon in local projections, default is 1.
#' @param plag lag order of VAR.
#' @param yname logical, whether return the name of dependent variables.
#' @return if \code{yname} is FALSE, return a data.frame include indepedent variables and
#' depedent variables in VAR. If \code{yname} is TRUE, returen a list, the 1st element is
#' data.frame, the 2nd element is names of dependent variables.
#'

dt_gen <- function(regdata, indx = NULL, s = 1, plag = 1,yname = FALSE){
  # generate X and Y
  if (is.null(indx)){
    ans <- ts(regdata) %>% bvartools::gen_var(., p = s - 1 + plag, deterministic = 'none')
  }else {
    id_yr <- regdata[,indx]
    ans <- dplyr::select(regdata, -indx[1], -indx[2]) %>% ts() %>%
      bvartools::gen_var(., p = s - 1 + plag, deterministic = 'none')
  }

  regdata <- cbind(t(ans$Y),t(ans$Z)) %>% as.data.frame()
  # delete coloums between Y_t+s and Y_t
  if ( s != 1){
    del_col <- NULL
    for (j in 1:nrow(ans$Y)) {
      del_col <- c(del_col,paste(colnames(t(ans$Y))[j],'.',as.character(1:(s-1)), sep = ''))
    }
    regdata <- dplyr::select(regdata,-ends_with(del_col))
  }

  # panal data or not?
  if (!is.null(indx))  regdata <- cbind(id_yr[-(1:(s-1+plag)),],regdata)
  ifelse (yname,
          return(list(regdata = regdata, yname = colnames(t(ans$Y)))),
          return(regdata))
}
