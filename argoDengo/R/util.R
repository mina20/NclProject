#' logit function
#'
#' @export
logit <- function(x) log(x) - log(1-x)

#' inverse logit function
#'
#' @export
logit_inv <- function(x) exp(x) / (1 + exp(x))
