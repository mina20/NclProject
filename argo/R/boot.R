#' bootstrap relative efficiency confidence interval
#'
#' This function is used to reproduce the ARGO bootstrap confidence interval of PNAS paper.
#'
#' @param pred_data A matrix that contains the truth vector and the predictions.
#' It can be data.frame or xts object
#'
#' @param model_good The model to evaluate, must be in the column names of pred_data
#' @param model_bench The model to compare to, must be in the column names of pred_data
#' @param l stationary bootstrap mean block length
#' @param N number of bootstrap samples
#' @param truth the column name of the truth
#' @param sim simulation method, pass to boot::tsboot
#' @param conf confidence level
#' @param type Must be one of "mse" (mean square error),
#' "mape" (mean absolute percentage error), or
#' "mae" (mean absolute error)
#'
#' @return A vector of point estimate and corresponding bootstrap confidence interval
#'
#' @export
bootstrap_relative_efficiency <-
  function(pred_data, model_good, model_bench, l=50, N = 1e4, truth="CDC.data",
           sim = "geom", conf = 0.95, type=c("mse", "mape", "mae")){

  if(type[1]=="mse"){
    err.fun <- function(tsb) {
      log(mean(tsb[,2]^2)) - log(mean(tsb[,1]^2))
    }
  }else if (type[1] %in% c("mae", "mape")){
    err.fun <- function(tsb) {
      log(mean(abs(tsb[,2]))) - log(mean(abs(tsb[,1])))
    }
  }




  pred_error <- xts::merge.xts(pred_data[,model_good]-pred_data[,truth],
                               pred_data[,model_bench]-pred_data[,truth])
  if(type[1] == "mape"){
    pred_error <- xts::merge.xts(
      (pred_data[,model_good]-pred_data[,truth])/pred_data[,truth],
      (pred_data[,model_bench]-pred_data[,truth])/pred_data[,truth])
  }
  pred_error <- na.omit(pred_error)
  err.fun(pred_error)
  boot.fit <- boot::tsboot(pred_error, err.fun, R = 1e4, l = l, sim = sim)


  CI <- boot::boot.ci(boot.fit, conf = conf, type = c("norm","basic","perc"))

  result <- exp(c(err.fun(pred_error),CI$basic[,4:5], CI$normal[,2:3],
                  CI$percent[,4:5]))
  names(result) <- c("point", "basic_lb", "basic_ub", "normal_lb", "normal_ub",
                     "percent_lb", "percent_ub")
  result
}
