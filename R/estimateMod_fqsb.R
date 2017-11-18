#' Log t test for convergence - fixed bandwidth
#'
#' @description  Estimates the \emph{log t} regression model proposed by Phillips and Sul (2007, 2009)
#' in order to investigate the presence of convergence by adopting the
#' Andrews estimator of long-run variance (fixed bandwidth of the kernel).
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
#' (Quadratic Spectral kernel with fixed bandwidth parameter, according to Andrews (1991)).
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
#'

estimateMod_fqsb <- function(H, time_trim){


    ### Initialise variables ---------------------------------------------------
    nT <- length(H)
    rT <- (round(nT*time_trim) + 1):nT
    logt <- log(rT)
    rH <- log(H[1]/H[rT]) - 2*log(logt)
    ### Estimation -------------------------------------------------------------
    xx		<- cbind(1, logt)						# construct design matrix
    b		<- solve(t(xx) %*% xx) %*% t(xx) %*% rH	# OLS
    re		<- rH - xx %*% b						# construct residuals

    lrv		<- andrs2(re)              				#long-run variance of errors
    var.b	<- diag(solve(t(xx) %*% xx))*c(lrv)
    se.b	<- sqrt(var.b)
    tstat.b	<- b[2]/se.b[2]
    result	<- list(beta=b,
                   st.dev = se.b[2],
                   tvalue = tstat.b,
                   pvalue = pnorm(q=tstat.b)
    )
    ### Output -----------------------------------------------------------------
    return(result)
}


#' Long run variance of errors
#'
#' Estimate long run variance of errors by Andrews method
#'
#' @param x vector of residuals
#'
#' @return  a numeric value representing the long run variance of errors
#'

andrs2		<- function(x){
    t		<- length(x[, 1])
    n		<- length(x[1, ])
    x1		<- x[1:(t - 1), ]
    y1		<- x[2:t, ]
    b1		<- sum(x1*y1)/sum(x1^2)
    ee		<- y1 - x1*b1

    a1		<- (4*b1^2)/(((1 - b1)^2)*((1 + b1)^2))
    a2		<- (4*b1^2)/((1 - b1)^4)
    band1	<- 1.1447*(a1*t)^(1/3)
    band2	<- 1.3221*(a2*t)^(1/5)
    jb2		<- as.matrix((1:(t - 1))/c(band2))
    jband2	<- jb2*1.2*pi
    kern1	<- ((sin(jband2)/jband2 - cos(jband2))/((jb2*pi)^2*12))*25

    tt		<- length(ee)
    lam		<- as.matrix(0)
    for(j in 1:(tt - 1)){
        ttp1	<- (t(x[1:(tt - j), ]) %*% x[(1 + j):tt, ])*(as.matrix(kern1[j, ]/tt))[1, 1]
        ttp	<- t(t(x[1:(tt - j), ]) %*% x[(1 + j):tt, ])*as.matrix(kern1[j, ]/tt)[1, 1]
        lam	<- lam + ttp + ttp1
    }
    sigm	<- (t(x) %*% x)/as.matrix(tt)
    lam		<- sigm[1] + lam

    return (lam)
}

