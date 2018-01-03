#' Finds convergence clubs
#'
#' Find convergence clubs by means of Phillips and Sul clustering procedure.
#'
#' @param X dataframe containing data (preferably filtered data in order to remove business cycles)
#' @param dataCols integer vector with the column indices of the data
#' @param regions integer scalar indicating, if present, the index of a column
#' with codes of the regions
#' @param refCol integer scalar indicating the index of the column to use for ordering
#' data
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running log t regression model.
#' Phillips and Sul (2007, 2009) suggest to discard the first third of the period.
#' @param cstar numeric scalar, indicating the threshold value of the sieve criterion \eqn{c^*}
#' to include units in the detected core (primary) group (step 3 of Phillips and Sul (2007, 2009) clustering algorithm).
#' The default value is 0.
#' @param HACmethod string indicating whether a Fixed Quadratic Spectral Bandwidth (\code{HACmethod="FQSB"}) or
#' an Adaptive Quadratic Spectral Bandwidth (\code{HACmethod="AQSB"}) should be used for the truncation
#' of the Quadratic Spectral kernel in estimating the \emph{log-t} regression model
#' with heteroskedasticity and autocorrelation consistent standard errors.
#' The default method is "FQSB".
#'
#'
#' @return Ad object of class \code{convergence.clubs}, containing a list of
#' Convergence Clubs, for each club a list is return with the
#' following objects: \code{id}, a vector containing the row indices
#' of the regions in the club; \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club;
#' \code{regions}, a vector containing the names of the regions of the club (optional,
#' only included if parameter \code{regions} is given)
#'
#'
#' @details In order to investigate the presence of convergence clubs according
#' to the Phillips and Sul clustering procedure, the following steps are implemented:
#'    \enumerate{
#'        \item (Cross section last observation ordering):
#'              Sort units in descending order according to the last panel observation of the period;
#'        \item (Core group formation):
#'        Run the log t regression for the first k units \eqn{(2 < k < N)} maximizing k
#'        under the condition that t-value is \eqn{> -1.65}. In other words, chose the core group size k* as follows:
#'
#'        \deqn{k^*= argmax_k \{t_k\} }{k* = argmax \, t(k)  }  subject to \deqn{ min\{t_k \} > -1.65}{min t(k) > 1.65}
#'
#'        If the condition \eqn{t_k >-1.65}{t(k) > -1.65} does not hold for \eqn{k = 2} (the first two units),
#'        drop the first unit and repeat the same procedure. If \eqn{t_k >-1.65}{t(k) > -1.65} does not hold
#'        for any units chosen, the whole panel diverges;
#'        \item (Sieve the data for club membership): After the core group  is detected,
#'        run the \eqn{log t}  regression for the core group adding (one by one)
#'        each unit that does not belong to the latter. If \eqn{t_k }{t(k)} is greater than a critical value \eqn{c^*}{c*}
#'        add the new unit in the convergence club.
#'        All these units (those included in the core group \eqn{k^*}{k*} plus those added) form the first convergence club;
#'        \item (Recursion and stopping rule): If there are units for which the
#'        previous condition fails, gather all these units in one group and run
#'        the \emph{log-t} test to see if  the condition \eqn{t_k >-1.65}{t(k) > -1.65} holds.
#'        If the condition is satisfied, conclude that there are two convergence clubs.
#'        Otherwise, step 1 to 3 should be repeated on the same group to determine
#'        whether there are other subgroups that constitute convergence clubs.
#'        If no further convergence clubs are found (hence, no k in step 2 satisfies
#'        the condition \eqn{t_k >-1.65}{t(k) > -1.65}), the remaining regions diverge.
#'    }
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#' Andrews, D. W., 1991. Heteroskedasticity and autocorrelation consistent covariance matrix estimation. Econometrica: Journal of the Econometric Society, 817-858.
#'
#' @seealso
#' \code{\link{mergeClubs}}, Merges a list of clubs created by \code{findClubs};
#'
#' \code{\link{mergeDivergent}}, Merges divergent units according to the algorithm proposed by von Lyncker and Thoennessen (2016)
#'
#'
#'
#' @examples
#' data("countryGDP")
#'
#' \dontrun{
#' # Cluster Countries using GDP from year 2000 to year 2014
#' clubs <- findClubs(countryGDP, dataCols=2:35, regions = 1, refCol=35, time_trim = 1/3,
#'                    cstar = 0, HACmethod = "FQSB")
#' }
#'
#' clubs <- findClubs(countryGDP, dataCols=2:35, regions = 1, refCol=35, time_trim = 1/3,
#'                    cstar = 0, HACmethod = "AQSB")
#' summary(clubs)
#'
#'
#' @export




findClubs<- function(X, #data matrix or data.frame
                     dataCols, #vector with column indices of data,
                     regions = NULL, #column index of regions, if present
                     refCol, #column index of year to be used as reference (lastT)
                     time_trim = 1/3, #portion of years to remove from computations (a value between >0 and <1)
                     cstar = 0, #c* value for the second step,
                     HACmethod = c('FQSB','AQSB')){


    ### Initialise variables ---------------------------------------------------
    HACmethod <- match.arg(HACmethod)
    returnRegions <- switch(class(regions),
                            NULL = FALSE,
                            numeric = TRUE,
                            integer = TRUE,
                            stop('Not a valid value for regions; it should be an integer'))

    N <- nrow(X)
    t <- length(dataCols)

    threshold <- -1.65

    #output
    clubs <- structure(list(),
                       class = c("convergence.clubs", "list"),
                       data = X,
                       dataCols = dataCols,
                       refCol = refCol,
                       time_trim = time_trim,
                       cstar = cstar,
                       HACmethod = HACmethod
                       )
    ### Check inputs -----------------------------------------------------------

    #regions
    if(length(regions) > 1) stop('regions must be an integer-valued scalar')
    if( returnRegions){ if(regions %% 1 != 0)  stop('regions must be an integer-valued scalar')}

    #X
    if(!is.data.frame(X)) stop('X must be an object of class data.frame')
    X[,regions] <- as.character(X[,regions])

    #dataCols
    if(!all(apply(X[,dataCols],2,is.numeric)) ) stop('Some of the data columns are non-numeric')

    #length of time series
    if(t < 2) stop('At least two time periods are needed to run this procedure')

    #time_trim
    if( length(time_trim) > 1 | !is.numeric(time_trim) ) stop('time_trim must be a numeric scalar')
    if( time_trim > 1 | time_trim <= 0 ) stop('invalid value for time_trim; should be a value between 0 and 1')
    if( (t - round(t*time_trim)) < 2) stop('either the number of time periods is too small or the value of time_trim is too high')

    #refCol
    if( length(refCol) > 1 ) stop('refCol must be an integer-valued scalar')
    if( !is.numeric(refCol) ) stop('refCol must be an integer value indicating the column number of reference year')
    if( refCol %% 1 != 0 ) stop('refCol must be an integer-valued scalar')
    if( refCol > ncol(X) ) stop('Wrong refCol value; there is no such column')

    #cstar
    if(!is.numeric(cstar) | length(cstar) > 1) stop('cstar must be a numeric scalar')


    ### Set methods  -----------------------------------------------------------
    #select functions to compute t-values
    # estimateMod <<- if(HACmethod=='FQSB'){
    #     estimateMod_fqsb
    # }else estimateMod_aqsb
    #
    # estimateMod <- function(H, time_trim) estimateMod(H, time_trim, HACmethod=HACmethod)

    ### Other preliminary operations -------------------------------------------

    #add id column to dataset
    X$id <- 1:N
    #Sort data by clustering variable (decreasing)
    dati <- X[order(X[,refCol],decreasing = TRUE),]

    ### Find clubs -------------------------------------------------------------
    #Cluster procedure
    l <- 1
    while(TRUE){
        if (nrow(dati) == 1){
            clubs$divergent$id <- dati$id
            break #break while loop if out of regions
        }else if(nrow(dati) == 0){
            clubs$divergent$message <- "there are no divergent regions"
            break
        }

        #Test all regions
        H_all <- computeH(dati[,dataCols])
        mod_all <- estimateMod(H_all, time_trim, HACmethod=HACmethod)
        t_all <- mod_all$tvalue
        # if tvalue > -1.65, they all form one club,
        #otherwise go one with clustering
        if (t_all > threshold) {
            clubs[[paste('club',l,sep = '')]] <- list(
                # regions = as.character(dati[,IDvar]),
                id =  dati$id,
                model = list(
                    # threshold = threshold,
                    beta = mod_all$beta,
                    st.dev = mod_all$st.dev,
                    tvalue = t_all,
                    pvalue = mod_all$pvalue
                )
            )
            break
        }

        #find core group (returns the row indices of regions in core Group)
        coreGroup <- coreG(X=dati, refCol, dataCols, time_trim, threshold,
                           HACmethod = HACmethod, type="max")
        #if no more core groups are found, add divergent to output and return
        if (identical(coreGroup, FALSE) ){
            # nl <- length(clubs)
            clubs$divergent$id <- dati$id
            break
        }

        #add regions to core group
        convClub <- club(X = dati, dataCols, core = coreGroup, time_trim,
                         HACmethod = HACmethod, cstar = cstar)
        # newcstar <- clubConv$model$cstar
        # xidclub <- which(X[,IDvar] %in% as.character(clubConv$units))
        clubs[[paste('club',l,sep = '')]] <- list( id = convClub$id,
                                                   model = convClub$model
        )
        dati <- dati[-convClub$rows,]#remove the club found from the dataset
        l <- l + 1
    }#end of while, end of clustering

    ### Return -----------------------------------------------------------------
    #if returnRegions, then add region codes to output
    if(returnRegions){
        for(club in names(clubs) ){
            clubs[[club]]$regions <- X[clubs[[club]]$id, regions]
        }
        return(clubs)
    }else return(clubs)
}
