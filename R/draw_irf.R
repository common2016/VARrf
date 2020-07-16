#' Draw IRF
#'
#' @description draw IRF picture according to results from \code{IRFrf_gen}
#' @param irf a returned objective from \code{IRFrf_gen}
#' @param res_var a string vector composed with names of response variable
#' @param ncol_pic multiple pictures are arranged \code{ncol_pic} coloumns
#' @examples
#'
#' @import ggplot2
#' @export

draw_irf <- function(irf, res_var, ncol_pic = 2){
  ans <- lapply(irf, reshape2::melt, id.vars = 's')
  ans <- bind_rows(ans)
  picdata <- group_by(ans, s, variable) %>% summarise(v50 = median(value),
                                                      v025 = quantile(value, 0.025),
                                                      v975 = quantile(value, 0.975))
  p <- list()
  pcode <- 'p[[1]]'
  for (i in 1:length(res_var)) {
    p[[res_var[i]]] <- ggplot(picdata[picdata$variable %in% res_var[i],], aes(x = s , y = v50)) +
      geom_line(aes(y = v025), linetype = 2) +
      geom_line(aes(y = v975), linetype = 2) +
      geom_hline(yintercept = 0) + labs(title = res_var[i]) +
      geom_line() + theme_bw()
    if (i != 1) pcode <- paste(pcode, '+p[[',as.character(i),']]',sep = '')
    if (i == length(res_var)) pcode <- paste('ans <- ',pcode, '+ patchwork::plot_layout(ncol =',
                                             as.character(ncol_pic),')', sep = '')
  }
  eval(parse(text = pcode))
  return(ans)
}

