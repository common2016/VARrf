#' Transfer Panel Data
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


dt_gen_panel <- function(regdata, indx = c('id','year'), s = 1, plag = 1, yname =FALSE){
  id <- unique(regdata[,indx[1]])
  pndata <- list()
  for (k in 1:length(id)) {
    pndata[[k]] <- regdata[regdata[,indx[1]] %in% id[k],]
  }
  yvar <- dt_gen(pndata[[1]],indx = indx, s = s, plag = plag, yname = T)[['yname']]
  ans <- lapply(pndata,dt_gen,indx = indx, s = s, plag = plag)
  ans <- dplyr::bind_rows(ans)

  ifelse (yname,
          return(list(regdata = ans, yname = yvar)),
          return(ans))
}




