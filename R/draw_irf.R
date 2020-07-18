#' Draw IRF
#'
#' @description draw IRF picture according to results from \code{IRFrf_gen}
#' @param irf a returned objective from \code{IRFrf_gen}
#' @param res_var a string vector composed with names of response variable
#' @param ncol_pic multiple pictures are arranged \code{ncol_pic} coloumns
#' @param ci confidence interval, default is 0.9, and if it's NULL, don't draw
#' confidence interval.
#' @examples
#'
#' @import ggplot2 patchwork
#' @export

draw_irf <- function(irf, res_var, ncol_pic = 2, ci = 0.9){
  ans <- lapply(irf, reshape2::melt, id.vars = 's')
  ans <- bind_rows(ans)
  dw <- (1 - ci)/2 # quantile
  picdata <- group_by(ans, s, variable) %>% summarise(v50 = median(value),
                                                      v025 = quantile(value, dw),
                                                      v975 = quantile(value, 1-dw))
  p <- list()
  pcode <- 'p[[1]]'
  for (i in 1:length(res_var)) {
    if (!is.null(ci)){
      p[[res_var[i]]] <- ggplot(picdata[picdata$variable %in% res_var[i],], aes(x = s , y = v50)) +
        geom_line(aes(y = v025), linetype = 2) +
        geom_line(aes(y = v975), linetype = 2)
    }else {
      p[[res_var[i]]] <- ggplot(picdata[picdata$variable %in% res_var[i],], aes(x = s , y = v50))
    }
    p[[res_var[i]]] <- p[[res_var[i]]] + geom_hline(yintercept = 0) + labs(title = res_var[i]) +
      geom_line() + theme_bw()
    if (i != 1) pcode <- paste(pcode, '+p[[',as.character(i),']]',sep = '')
    if (i == length(res_var)) pcode <- paste('ans <- ',pcode, '+ patchwork::plot_layout(ncol =',
                                             as.character(ncol_pic),')', sep = '')
  }
  eval(parse(text = pcode))
  return(ans)
}

