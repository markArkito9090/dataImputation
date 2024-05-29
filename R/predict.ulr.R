#' Predict method for unary linear regression model fits
#'
#' @description
#' Predicted values based on unary linear model object.
#' @param model, unary linear model
#' @param x, new data
#' @details
#' predict.ulr produces predicted values,
#' obtained by evaluating the regression function in the newdata x
#'
#' @return a numeric vector, the predicted values
#' @export
predict.ulr <- function(model, x) {
    model[[2]]*x + model[[1]]
}




