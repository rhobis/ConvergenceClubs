#' Log-t test for convergence
#'
#' @description  Estimates the \emph{log-t} regression model proposed by Phillips and Sul (2007, 2009)
#' in order to investigate the presence of convergence by adopting the
#' Andrews estimator of long-run variance (fixed or adaptive bandwidth of the kernel).
#'
#' @param H vector of H values
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running \emph{log t} regression model.
#' Phillips and Sul (2007, 2009) suggest to discard the first third of the period.
#' @param HACmethod string indicating whether a Fixed Quadratic Spectral Bandwidth (\code{HACmethod="FQSB"}) or
#' an Adaptive Quadratic Spectral Bandwidth (\code{HACmethod="AQSB"}) should be used for the truncation
#' of the Quadratic Spectral kernel in estimating the \emph{log t} regression model
#' with heteroskedasticity and autocorrelation consistent standard errors.
#' The default method is "FQSB".
#'
#'
#' @return  A named vector containing information about the model used to run the t-test
#' on the units in the club: beta coefficient, standard deviation, t-statistics and p-value.
#'
#' @details The following linear model is estimated:
#' \deqn{\log\frac{H_1}{H_t} - 2\log(\log{t}) = \alpha + \beta \log{t} + u_t}{
#'            log[H(1)/H(t)] - 2log[log(t)] = \alpha + \beta log(t) + u(t)  }
#'
#' Heteroskedasticity and autocorrelation consistent (HAC) standard errors are used with
#' Quadratic Spectral kernel (Andrews, 1991), If  \code{HACmethod}="FQSB",
#' a fixed bandwidth parameter is applied, while with \code{HACmethod}="AQSB" an
#' adaptive bandwidth parameter is employed.
#'
#' @references
#'
#' Andrews, D. W., 1991. Heteroskedasticity and autocorrelation consistent covariance matrix estimation.
#' Econometrica: Journal of the Econometric Society, 817-858.
#'
#' Phillips, P. C.; Sul, D., 2007.
#' Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth.
#' Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#'
#'
#'
#' @export
#'


estimateMod <- function(H, time_trim=1/3, HACmethod = c('FQSB','AQSB')){

    HACmethod <- match.arg(HACmethod)

    ### Initialise variables -----------------------------------------------
    nT <- length(H)
    rT <- seq( round(nT*time_trim) + 1, nT)
    logt <- log(rT)
    rH <- log( H[1]/H[rT] ) - 2*log( logt )

    if (HACmethod=='FQSB'){
        ### Estimation ---------------------------------------------------------
        mod <- lm( rH~logt )
        mm <- model.matrix(mod)
        r <- residuals(mod)
        hac <- ps_andrews_hac(r)
        se <- sqrt( diag( solve( t(mm) %*% mm) ) * hac )[2]
        b <- coef(mod)[2]
        tv <- b/se
        ### Output -------------------------------------------------------------
        return(c(beta    = unname(b),
                 std.err = unname(se),
                 tvalue  = unname(tv),
                 pvalue  = unname(pnorm(q=tv)) )
        )
    }else if(HACmethod=="AQSB"){
        ### Estimation ---------------------------------------------------------
        mod <- lm( rH~logt )
        out <- lmtest::coeftest(mod,vcov=sandwich::vcovHAC(mod))
        ### Output -------------------------------------------------------------
        return(c(beta    = unname(out[2,1]),
                 std.err = unname(out[2,2]),
                 tvalue  = unname(out[2,3]),
                 pvalue  = unname(pnorm(q=out[2,3]))
                 )
        )
    }else stop("An error occurred, check the value of HACmethod!")
}
