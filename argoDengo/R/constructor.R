#' Construct ARGO object
#'
#' Wrapper for ARGO. The real work horse is glmnet package and/or linear model.
#'
#' This function takes the time series and exogenous variables (optional) as
#' input, and produces out-of-sample prediction for each time point.
#'
#' @param data response variable as xts, last element can be NA
#' @param exogen exogenous predictors, default is NULL
#' @param N_lag vector of the AR model lags used,
#' if NULL then no AR lags will be used
#' @param N_training number of training points, if use_all_previous is true,
#' this is the least number of training points required
#' @param alpha penalty between lasso and ridge, alpha=1 represents lasso,
#' alpha=0 represents ridge, alpha=NA represents no penalty
#' @param use_all_previous boolean variable indicating whether to use "all available
#' data" or "a sliding window" for training
#'
#' @return A list of following named objects
#' \itemize{
#' \item \code{pred} An xts object with the same index as input,
#' which contains historical nowcast estimation
#'
#' \item \code{coef} A matrix contains historical coefficient values of the predictors.
#'
#' \item \code{parm} Parameter values passed to argo function.
#' }
#'
#' @references
#' Yang, S., Santillana, M., & Kou, S. C. (2015). Accurate estimation of influenza epidemics using Google search data via ARGO. Proceedings of the National Academy of Sciences, \href{https://dx.doi.org/10.1073/pnas.1515373112}{doi: 10.1073/pnas.1515373112}.
#' @export
argo <- function(data, exogen=xts::xts(NULL), N_lag=1:52, N_training=104,
                 alpha=1, use_all_previous=FALSE){
  parm <- list(N_lag = N_lag, N_training = N_training,
               alpha = alpha, use_all_previous = use_all_previous)
  lasso.pred <- c()
  lasso.coef <- list()

  if(length(exogen)>0) # exogenous variables must have the same timestamp as y
    if(!all(zoo::index(data)==zoo::index(exogen)))
      stop("error in data and exogen: their time steps must match")

  starttime <- 1+N_training+max(c(N_lag,0))
  endtime <- nrow(data)-1

  for(i in starttime:endtime){
    if(use_all_previous){
      training_idx <- (1 + max(c(N_lag, 0))):i
    }else{
      training_idx <- (i - N_training + 1):i
    }

    lagged_y <- sapply(N_lag, function(l)
      as.numeric(data[(training_idx) - l]))

    if(length(lagged_y) == 0){
      lagged_y <- NULL
    }else{
      colnames(lagged_y) <- paste0("lag_", N_lag)
    }
    if(length(exogen) > 0){
      design_matrix <- cbind(lagged_y, data.matrix(exogen[training_idx, ]))
    }else{
      design_matrix <- cbind(lagged_y)
    }


    y.response <- data[training_idx]

    if(is.finite(alpha)){
      lasso.fit <- glmnet::cv.glmnet(x=design_matrix,y=y.response,
                                     nfolds=10, grouped=FALSE, alpha=alpha)
    }else{
      lasso.fit <- lm(y.response ~ ., data=data.frame(design_matrix))
    }


    if(is.finite(alpha)){
      lasso.coef[[i]] <- as.matrix(coef(lasso.fit, lambda = lasso.fit$lambda.1se))
    }else{
      lasso.coef[[i]] <- as.matrix(coef(lasso.fit))
    }
    lagged_y_next <- matrix(sapply(N_lag, function(l)
      as.numeric(data[i + 1 - l])), nrow=1)

    if(length(lagged_y_next) == 0)
      lagged_y_next <- NULL
    if(length(exogen) > 0){
      newx <- cbind(lagged_y_next, data.matrix(exogen[i + 1,]))
    }else{
      newx <- lagged_y_next
    }
    if(is.finite(alpha)){
      lasso.pred[i + 1] <- predict(lasso.fit, newx = newx, s = "lambda.1se")
    }else{
      colnames(newx) <- c(paste0("lag_", N_lag), names(exogen))
      newx <- as.data.frame(newx)
      lasso.pred[i+1] <- predict(lasso.fit, newdata = newx)
    }
  }

  data$predict <- lasso.pred
  lasso.coef <- do.call("cbind", lasso.coef)
  colnames(lasso.coef) <- as.character(zoo::index(data))[starttime:endtime]
  argo <- list(pred = data$predict, coef = lasso.coef, parm = parm)
  class(argo) <- "argo"
  argo
}
