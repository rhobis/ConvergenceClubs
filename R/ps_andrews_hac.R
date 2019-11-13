#' Long run variance of errors
#'
#' Estimate long run variance of errors through Andrews' method
#'
#' @param x vector of residuals
#'
#' @return  a numeric value representing the long run variance of errors
#'
#'
#' @details This function computes the long run variance of residuals of an lm model
#'     by means of Andrews' with Quadratic Spectral kernel and fixed bandwidth.
#'     The code is an adaptation of Phillips and Sul (2007)'s code,
#'     which was written in \emph{GAUSS}.
#'
#' @references
#'
#' Andrews, D. W., 1991. Heteroskedasticity and autocorrelation consistent covariance matrix estimation.
#' Econometrica: Journal of the Econometric Society, 817-858.
#'
#'  Phillips, P. C.; Sul, D., 2007.
#' Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#'
#' @keywords internal


ps_andrews_hac <- function(x){
    t <- length(x)
    x1 <- x[ seq(1, t-1) ]
    x2 <- x[ seq(2, t) ]
    b1 <- sum(x1*x2)/sum(x1**2)
    ee <- x2 - x1*b1
    ## compute alpha
    a <- (4 * b1**2) / ((1-b1)**4)
    ## Compute bandwidth
    B <- 1.3221 * (a*t)**0.2
    l <- seq_len(t-1)
    z <- 1.2*pi * l/B
    ## Compute kernel
    k <- (sin(z)/z - cos(z)) * (3/z**2)

    ## Compute vcov matrix
    t2 <- t-1
    out <- 0
    for(i in seq_len(t2-1)){
        o <- (t(x[seq(1,t2-i)]) %*% x[seq(i+1, t2)] ) * k[i]/t2
        o1 <- t(t(x[seq(1,t2-i)]) %*% x[seq(i+1, t2)] ) * k[i]/t2
        out <- out + o + o1
    }
    s <- t(x) %*% x / t2
    out <- c(out + s)
    #
    return(out)
}
