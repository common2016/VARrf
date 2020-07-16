#' Draw IRF
#'
#' @description draw IRF picture according to results from \code{IRFrf_gen}
#' @param irf a returned objective from \code{IRFrf_gen}
#' @param res_var a string vector composed with names of response variable
#' @examples
#'
#' @import ggplot2
#' @export

draw_irf <- function(irf, res_var){
  ans <- lapply(irf, reshape2::melt, id.vars = 's')
  ans <- bind_rows(ans)
  picdata <- group_by(ans, s, variable) %>% summarise(v50 = median(value),
                                                      v025 = quantile(value, 0.025),
                                                      v975 = quantile(value, 0.975))
  p <- ggplot(picdata[picdata$variable %in% res_var,], aes(x = s , y = v50)) +
    geom_line(aes(y = v025), linetype = 2) +
    geom_line(aes(y = v975), linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_line() + facet_wrap(~ variable, ncol = 3) + theme_bw()
  return(p)
}

