#' Print an unitary linear regression
#'
#' @description
#' print.ulr is to print an unitary linear regression model
#' @param model, fitted ulr model
#' @details
#' Just like the famous print.lm,
#' print.ulr is to print the information of an unitary linear regression,
#' including the intercept and the slope.
#'
#' @export
print.ulr <- function(model) {
    model_info <- attr(model, "info")
    cat("\n---------------------------------------")
    cat("\n    Unary linear regression model")
    cat("\n---------------------------------------")
    cat("\nIntercept: ", model[[1]])
    cat("\nSlope: ", model[[2]])
    cat("\nFitted model: y=", model[[2]], "*x+", model[[1]], sep = "")
    cat("\n---------------------------------------")
    cat("\nModel created @", as.character(model_info$created_time), "\n\n", sep = "")
}
