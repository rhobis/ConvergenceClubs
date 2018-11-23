#' Find core (primary) group
#'
#' Find the Core (primary) group according to step 2 of the clustering algorithm
#' by Phillips and Sul (2007, 2009)
#'
#' @param X matrix or dataframe containing data (preferably filtered data in order to remove business cycles)
#' @param dataCols integer vector with the column indices of the data
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running log t regression model.
#' Phillips and Sul (2007, 2009) suggest to discard the first third of the period.
#' @param threshold numeric value indicating the threshold to be used to perform
#' the one-tail t test; default is -1.65.
#' @param HACmethod string indicating whether a Fixed Quadratic Spheric Bandwidth (\code{HACmethod="FQSB"}) or
#' an Adaptive Quadratic Spheric Bandwidth (\code{HACmethod="AQSB"}) should be used for the truncation
#' of the Quadratic Spectral kernel in estimating the \emph{log t} regression model
#' with heteroskedasticity and autocorrelation consistent standard errors.
#' The default method is "FQSB".
#' @param type one of "max" or "all";
#'             "max" includes only the region with maximum t-value. The default option is "max";
#'             "all" includes all regions that pass the test t in the core formation (step 2).
#'
#' @return A numeric vector containing the row indices of the regions included
#' in the core group; if a core group cannot be found, returns \code{FALSE}.
#'
#' @details According to the second step of the Phillips and Sul clustering algorithm (2007, 2009),
#'          the \emph{log t} regression should be run for the first k units \eqn{2 < k < N}
#'          maximizing k under the condition that \eqn{t-value > -1.65}.
#'          In other words, the core group size \eqn{k^*}{k*} is chosen as follows:
#'              \deqn{ k^* = argmax_{k} \{t_k\} }{k* = argmax t(k) }  subject to
#'              \deqn{\min{t_k} > -1.65}{min t(k) > -1.65}
#
#'          Such behavior is obtained with \code{type="max"}; if \code{type="all"},
#'          all units that satisfy \eqn{t_k > -1.65}{t(k) > -1.65} are added to core group.
#'
#'
#'          If the condition \eqn{t_k > -1.65}{t(k) > -1.65} does not hold for \eqn{k = 2} (the first two units),
#'          the algorithm drops the first unit and repeats the same procedure for the next pair of units.
#'          If \eqn{t_k > -1.65}{t(k) > -1.65} does not hold for any couple of units, the whole panel diverges.
#'
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'


coreG <- function(X,
                  dataCols,
                  time_trim,
                  threshold = -1.65,
                  HACmethod = c('FQSB', 'AQSB'),
                  type=c('max', 'all')){

    ### Initialisation ---------------------------------------------------------
    HACmethod <- match.arg(HACmethod)
    type <- match.arg(type)
    nr <- nrow(X) #number of regions
    ### Find first couple ------------------------------------------------------
    i <- 1
    while (i < nr){
        #select a couple of regions (i, i+1)
        i <- i + 1
        H <- computeH( X[ c(i-1, i), dataCols ])
        tvalue <- estimateMod(H, time_trim, HACmethod = HACmethod)['tvalue']
        #t-test (if t>-1.65 --> next step; otherwise repeat for regions (i+1,i+2) )
        if (tvalue > threshold){
            if (i == nr){
                return( c(i-1, i) )
            }else break
        }else if (i == nr){ #if no core group is found, return FALSE
            return(FALSE)
        }
    }

    ### Find core group --------------------------------------------------------
    units <- c(i-1, i)
    k <- i
    lgroup <- list() #list with units groups
    vt <- vector() #vector with t-values
    lgroup[[1]] <- units
    vt[1] <- tvalue
    l <- 2
    while (k < nr){
        # Groups obtained adding regions sequentially until t > -1.65
        k <- k + 1
        units <- c(units, k)
        H <- computeH(X[units, dataCols])
        tvalue <- estimateMod(H, time_trim, HACmethod = HACmethod)['tvalue']
        if (tvalue > threshold){
            vt <- c(vt, tvalue)
            lgroup[[l]] <- units
            l <- l + 1
        }else break
    }

    ### Output -----------------------------------------------------------------
    if ( type=='max' ){ #group of units for which t is max
        return( lgroup[[ which(abs(vt) == max(abs(vt)) )]] ) #
    }else if ( type== 'all' ){ #all units
        return(lgroup[[length(lgroup)]])
    }else stop("Invalid value for 'type' argument. Should be one of 'max' or 'all'! ")
}
