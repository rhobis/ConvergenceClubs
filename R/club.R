#' Find a club
#'
#' Add units to core group according to step 3 of the clustering algorithm by
#' Phillips and Sul (2007, 2009), in order to find the enlarged club.
#'
#' @param X matrix or dataframe containing data (preferably filtered data in order to remove business cycles)
#' @param dataCols integer vector with the column indices of the data
#' @param core an integer vector containing the id's of units in core group
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running log t regression model.
#' Phillips and Sul (2007, 2009) suggest to discard the first third of the period.
#' @param HACmethod string indicating whether a Fixed Quadratic Spheric Bandwidth (\code{HACmethod="FQSB"}) or
#' an Adaptive Quadratic Spheric Bandwidth (\code{HACmethod="AQSB"}) should be used for the truncation
#' of the Quadratic Spectral kernel in estimating the \eqn{log t} regression model
#' with heteroskedasticity and autocorrelation consistent standard errors.
#' The default method is "FQSB".
#' @param cstar numeric scalar, indicating the threshold value of the sieve criterion \eqn{c^*}
#' to include units in the detected core (primary) group (step 3 of Phillips and Sul (2007, 2009) clustering algorithm).
#' The default value is 0.
#'
#' @return A list of three objects: \code{id}, a vector containing the row indices
#' of club regions in the original dataframe (input of function \code{findClubs});
#' \code{rows}, a vector of row indices of club units in the current dataset
#' (input of function \code{club}); \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club.
#'
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'


club <- function(X,
                 dataCols,
                 core,
                 time_trim,
                 HACmethod = c('FQSB', 'AQSB'),
                 cstar = 0){

    ### Initialisation ---------------------------------------------------------
    HACmethod <- match.arg(HACmethod)

    X$row <- seq_len(nrow(X))
    unitsNoCore <- X[-core, ] #Data without units of the core group
    tvalue <- vector()

    ### t-test for core + one unit ---------------------------------------------
    for (k in seq_len(nrow(unitsNoCore)) ){
        #compute H
        H <- computeH( X[ c(core, unitsNoCore$row[k]), dataCols ])
        tvalue <- c(tvalue, estimateMod(H, time_trim, HACmethod = HACmethod)['tvalue'])
    }
    #Find group (core + regions) such that (core + i)  gives t > cstar
    clubCandidates_id <- unitsNoCore[which(tvalue > cstar), 'id']
    clubCandidates_row <- unitsNoCore[which(tvalue > cstar), 'row']

    ### Output -----------------------------------------------------------------
    clubId <- c(X[core, 'id'], clubCandidates_id)
    clubRows <- c(core, clubCandidates_row)

    H <- computeH(X[clubRows, dataCols])
    mod <- estimateMod(H, time_trim, HACmethod = HACmethod)
    # tvalue <- mod$tvalue

    #return club info
    return( list(id = clubId, #id of units in the club
                 rows = clubRows, #row indices of club units in input
                 model = mod) )
}
