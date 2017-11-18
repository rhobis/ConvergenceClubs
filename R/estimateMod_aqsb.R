#' Log t test for convergence - adaptive bandwidth
#'
#' Estimates the \emph{log t} regression model proposed by Phillips and Sul (2007, 2009)
#' in order to investigate the presence of convergence by adopting the
#' Andrews estimator of long-run variance (adaptive kernel bandwidth).
#'
#' @param H vector of H values
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running \emph{log t} regression model.
#' Phillips and Sul (2007, 2009) suggest to discard the first third of the period.
#'
#' @return  A list containing information about the model used to run the t-test
#' on the regions in the club: beta coefficient, standard deviation, t-statistics and p-value.
#'
#' @details
#' The following linear model is estimated:
#'     \deqn{\log\frac{H_1}{H_t} – 2\log(\log{t} = \alpha + \beta \log{t} + u_t}{
#'            log[H(1)/H(t)] – 2log[log(t)] = \alpha + \beta log(t) + u(t)  }
#' Heteroskedasticity and autocorrelation consistent (HAC) standard errors are used
#' (Quadratic Spectral kernel with adaptive bandwidth parameter, according to Andrews (1991)). .
#'
#' @references
#' Phillips, P. C.; Sul, D., 2007.
#' Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth.
#' Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#' Andrews, D. W., 1991. Heteroskedasticity and autocorrelation consistent covariance matrix estimation.
#' Econometrica: Journal of the Econometric Society, 817-858.
#'
#'


estimateMod_aqsb <- function(H, time_trim){
    ### Initialise variables ---------------------------------------------------
    nT <- length(H)
    rT <- (round(nT*time_trim) + 1):nT
    logt <- log(rT)
    rH <- log(H[1]/H[rT]) - 2*log(logt)
    ### Estimation -------------------------------------------------------------
    mod <- lm(rH ~ logt)
    mod <- lmtest::coeftest(mod,vcov=sandwich::vcovHAC(mod))
    ### Output -----------------------------------------------------------------
    return(list(beta= mod[2,1],
                st.dev = mod[2,2],
                tvalue = mod[2,3],
                pvalue = mod[2,4]))
}


