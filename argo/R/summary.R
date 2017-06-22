#' performance summary of ARGO applied on CDC's ILI data
#'
#' This function is only used to reproduce the ARGO table of PNAS paper.
#' If you use this summary function for other dataset, an error is highly likely to occur.
#'
#' @examples
#' summary_argo
#'
#' @return A list of summary tables for the inputed periods, including RMSE, MAE, MAPE, corr
#'
#' @export
summary_argo <- function(GFT_xts, model_names, legend_names, periods,
                         whole_period="2009-03/2015-10"){
  periods <- c(whole_period, periods)
  corr_by_period <- function(x,y, periods){
    sapply(periods, function(p) {
      re <- try(cor(x[p],y[p], use="complete.obs"), silent = T)
      if(class(re)=="try-error")
        return(NA)
      else
        return(re)
    })
  }

  RMSE_by_period <- function(x,y, periods){
    sapply(periods, function(p) {
      re <- try(sqrt(mean((x[p]-y[p])^2, na.rm=TRUE)),silent = T)
      if(class(re)=="try-error")
        return(NA)
      else
        return(re)
    })
  }

  ABSE_by_period <- function(x,y, periods){
    sapply(periods, function(p) {
      re <- try(mean(abs(x[p]-y[p]), na.rm=TRUE),silent = T)
      if(class(re)=="try-error")
        return(NA)
      else
        return(re)
    })
  }

  corr_martingale_diff <- function(x,y, periods){
    sapply(periods, function(p) {
      re <- try(cor(diff(x[p]),diff(y[p]), use="complete.obs"), silent = T)
      if(class(re)=="try-error")
        return(NA)
      else
        return(re)
    })
  }


  MAPE_by_period <- function(x_truth,y, periods){
    sapply(periods, function(p) {
      re <- try(mean(abs(x_truth[p]-y[p])/x_truth[p], na.rm=TRUE),silent = T)
      if(class(re)=="try-error")
        return(NA)
      else
        return(re)
    })
  }



  periods_short <- periods



  corr_period_tab <-
    sapply(model_names, function(name)
      corr_by_period(GFT_xts$CDC.data, GFT_xts[,name], periods))
  colnames(corr_period_tab) <- legend_names
  rownames(corr_period_tab) <- periods_short

  RMSE_period_tab <-
    sapply(model_names, function(name)
      RMSE_by_period(GFT_xts$CDC.data, GFT_xts[,name], periods))
  colnames(RMSE_period_tab) <- legend_names
  rownames(RMSE_period_tab) <- periods_short

  ABSE_period_tab <-
    sapply(model_names, function(name)
      ABSE_by_period(GFT_xts$CDC.data, GFT_xts[,name], periods))
  colnames(ABSE_period_tab) <- legend_names
  rownames(ABSE_period_tab) <- periods_short

  corr_martingale_diff_period_tab <-
    sapply(model_names, function(name)
      corr_martingale_diff(GFT_xts$CDC.data, GFT_xts[,name], periods))
  colnames(corr_martingale_diff_period_tab) <- legend_names
  rownames(corr_martingale_diff_period_tab) <- periods_short

  MAPE_period_tab <-
    sapply(model_names, function(name)
      MAPE_by_period(GFT_xts$CDC.data, GFT_xts[,name], periods))
  colnames(MAPE_period_tab) <- legend_names
  rownames(MAPE_period_tab) <- periods_short



  RMSE_period_tab_normed <-
    format(round(RMSE_period_tab/RMSE_period_tab[,ncol(RMSE_period_tab)], 3),
           nsmall=3)
  RMSE_period_tab_print <-
    matrix(paste0(RMSE_period_tab_normed, " (",format(round(RMSE_period_tab, 3), nsmall=3),")"),
           nrow=nrow(RMSE_period_tab),
           ncol=ncol(RMSE_period_tab),
           dimnames=dimnames(RMSE_period_tab))
  RMSE_period_tab_print[,-ncol(RMSE_period_tab_print)] <-
    RMSE_period_tab_normed[,-ncol(RMSE_period_tab)]


  ABSE_period_tab_normed <-
    format(round(ABSE_period_tab/ABSE_period_tab[,ncol(ABSE_period_tab)], 3),
           nsmall=3)
  ABSE_period_tab_print <-
    matrix(paste0(ABSE_period_tab_normed, " (",format(round(ABSE_period_tab, 3), nsmall=3),")"),
           nrow=nrow(ABSE_period_tab),
           ncol=ncol(ABSE_period_tab),
           dimnames=dimnames(ABSE_period_tab))
  ABSE_period_tab_print[,-ncol(ABSE_period_tab_print)] <-
    ABSE_period_tab_normed[,-ncol(ABSE_period_tab_normed)]


  MAPE_period_tab_normed <-
    format(round(MAPE_period_tab/MAPE_period_tab[,ncol(MAPE_period_tab)], 3),
           nsmall=3)
  MAPE_period_tab_print <-
    matrix(paste0(MAPE_period_tab_normed, " (",format(round(MAPE_period_tab, 3), nsmall=3),")"),
           nrow=nrow(MAPE_period_tab),
           ncol=ncol(MAPE_period_tab),
           dimnames=dimnames(MAPE_period_tab))
  MAPE_period_tab_print[,-ncol(MAPE_period_tab_print)] <-
    MAPE_period_tab_normed[,-ncol(MAPE_period_tab_normed)]


  add_bold_to_print_tab <- function(tab_print, tab_orig, smallerbetter=TRUE){
    if(!smallerbetter)
      tab_orig <- -tab_orig
    for(i in 1:ncol(tab_orig)){
      id <- order(tab_orig[,i])[1]
      tab_print[id,i] <- paste0("\\textbf{",tab_print[id,i],"}")
    }
    return(tab_print)
  }

  corr_period_tab_print <-
    matrix(format(round(corr_period_tab, 3), nsmall=3),
           nrow=nrow(corr_period_tab),
           ncol=ncol(corr_period_tab),
           dimnames=dimnames(corr_period_tab))

  corr_martingale_diff_period_tab_print <-
    matrix(format(round(corr_martingale_diff_period_tab, 3), nsmall=3),
           nrow=nrow(corr_martingale_diff_period_tab),
           ncol=ncol(corr_martingale_diff_period_tab),
           dimnames=dimnames(corr_martingale_diff_period_tab))

  corr_period_tab_print <-
    add_bold_to_print_tab(t(corr_period_tab_print), t(corr_period_tab), FALSE)

  corr_martingale_diff_period_tab_print <-
    add_bold_to_print_tab(t(corr_martingale_diff_period_tab_print), t(corr_martingale_diff_period_tab), FALSE)

  RMSE_period_tab_print <-
    add_bold_to_print_tab(t(RMSE_period_tab_print), t(RMSE_period_tab))

  ABSE_period_tab_print <-
    add_bold_to_print_tab(t(ABSE_period_tab_print), t(ABSE_period_tab))

  MAPE_period_tab_print <-
    add_bold_to_print_tab(t(MAPE_period_tab_print), t(MAPE_period_tab))


  return(list(corr=t(corr_period_tab),
              rmse=t(RMSE_period_tab),
              abse=t(ABSE_period_tab),
              mape=t(MAPE_period_tab),
              corr_diff=t(corr_martingale_diff_period_tab),
              corr_print=(corr_period_tab_print),
              rmse_print=(RMSE_period_tab_print),
              abse_print=(ABSE_period_tab_print),
              mape_print=(MAPE_period_tab_print),
              corr_diff_print=(corr_martingale_diff_period_tab_print)
  ))

}
