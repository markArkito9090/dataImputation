#' Fitting unitary linear regression model
#'
#' @description
#' ulr is used to fit unitary linear regression model models
#' @param x, independent variable
#' @param y, dependent variable
#' @details
#' The model is fitted by OLS in the following way:
#' \deqn{B =(X^TX)^{-1}X^TY}
#'
#'
#'
#' @return ulr model
#' @export
ulr <- function(x, y) {
    
    X <- cbind(rep(1, length(x)), x)
    b_hat <- solve(t(X) %*% X)%*%t(X)%*% y
    model <- b_hat
    attr(model, "class") <- "ulr"
    
    y_hat <- predict(model, x)
    SST <- sum((y - mean(y))^2)
    SSR <- sum((y_hat - mean(y))^2)
    SSE <- sum((y_hat - y)^2)
    
    R_squared <- 1-SSE/SST
    F_statistic <- SSR/(SSE/(length(x) - 2))
    p_value <- pf(F_statistic, 1, length(x) - 2, lower.tail = FALSE)
    
    model_info <- list(SST = SST,
                       SSR = SSR,
                       SSE = SSE,
                       R_squared = R_squared,
                       F_statistic = F_statistic,
                       p_value = p_value,
                       created_time = Sys.time())
    
    attr(model, "info") <- model_info
    
    return(model)
}



