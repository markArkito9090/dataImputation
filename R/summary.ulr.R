#' Summarizing unary linear regression model fits
#'
#' @description
#' summary method for class "ulr".
#' @param model, unary linear model
#' @details
#' Summary for ulr model.
#'
#' @return a list, more information about the model
#' @export
summary.ulr <- function(model) {
    model_info <- attr(model, "info")
    cat("\n    Unary linear regression model")
    cat("\n=======================================")
    cat("\nIntercept: ", model[[1]])
    cat("\nSlope: ", model[[2]])
    cat("\nFitted model: y=", model[[2]], "*x+", model[[1]], sep = "")
    cat("\nModel created @", as.character(model_info$created_time), sep = "")
    cat("\n---------------------------------------")
    cat("\ncoefficient of determination:", model_info$R_squared)
    cat("\nF-statistic:",  model_info$F_statistic)
    cat("\np-value:", model_info$p_value)
    cat("\n=======================================\n\n")
    invisible(model_info)
}




