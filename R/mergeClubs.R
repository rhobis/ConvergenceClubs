#' Merge convergence clubs
#'
#' Merges a list of clubs created with the function findClubs
#' by either Phillips and Sul method or von Lyncker and Thoennessen procedure
#'
#'
#' @param clubs an object of class \code{convergence.clubs} (created by findClub function)
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running log t regression model; if omitted, the same
#' value used for \code{clubs} is used.
#' @param HACmethod string indicating whether a Fixed Quadratic Spheric Bandwidth (HACmethod="FQSB") or
#' an Adaptive Quadratic Spheric Bandwidth (HACmethod="AQSB") should be used for the truncation
#' of the Quadratic Spectral kernel in estimating the \emph{log t} regression model
#' with heteroskedasticity and autocorrelation consistent standard errors.
#' The default method is "FQSB".
#' @param mergeMethod character string indicating the merging method to use. Methods
#' available are \code{'PS'} for Phillips and Sul (2009) and \code{'vLT'} for
#' von Lyncker and Thoennessen (2016).
#' @param mergeDivergent logical, if TRUE, indicates that merging of divergent regions
#' should be tried.
#' @param threshold a numeric value indicating the threshold to be used with the t-test.
#'
#' @return A list of Convergence Clubs, for each club a list is return with the
#' following objects: \code{id}, a vector containing the row indices
#' of the regions in the club; \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club;
#' \code{regions}, a vector containing the names of the regions of the club (optional,
#' only included if it is present in the \code{clubs} object given in input.
#'
#'
#' @details Phillips and Sul (2009) suggest a "club merging algorithm" to avoid
#' over determination due to the selection of the parameter \eqn{c*}.
#' This algorithm suggests to merge for adjacent groups. In particular, it works as follows:
#' \enumerate{
#'     \item Take the first two groups detected in the basic clustering mechanism
#'     and run the log-t test. If the t¬-statistic is larger than -1.65,
#'     these groups together form a new convergence club;
#'     \item Repeat the test adding the next group and continue until the
#'     basic condition (t-statistic > -1.65) holds;
#'     \item If convergence hypothesis is rejected, conclude that all previous groups
#'     converge, except the last one. Hence, start again the test merging algorithm
#'     beginning from the group for which the hypothesis of convergence did not hold.
#'     On the other hand, von Lyncker and Thoennessen (2016), propose a modified version
#'      of the club merging algorithm that works as follows:
#'         \enumerate{
#'             \item Take all the groups detected in the basic clustering mechanism (P)
#'             and run the t-test for adjacent groups, obtaining a (M × 1) vector
#'             of convergence test statistics t (where M = P – 1 and m = 1,.., M);
#'             \item Merge for adjacent groups starting from the first, under the
#'             conditions \eqn{t(m) > -1.65} and \eqn{t(m) > t(m+1)}.
#'             In particular, if both conditions hold, the two clubs determining
#'             \eqn{t(m)} are merged and the algorithm starts again from step 1,
#'             otherwise it continues for all following pairs;
#'             \item For the last element of vector M (the value of the last two clubs)
#'             the only condition required for merging is \eqn{t(m=M) > -1.65}.
#'         }
#'
#' }
#'
#'
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#' von Lyncker, K.; Thoennessen, R., 2016. Regional club convergence in the EU: evidence from a panel data analysis. Empirical Economics, doi:10.1007/s00181-016-1096-2, 1-29.
#'
#'
#' @seealso
#' \code{\link{findClubs}}, finds convergence clubs by means of Phillips and Sul clustering procedure.
#'
#' \code{\link{mergeDivergent}}, merges divergent units according to the algorithm proposed by von Lyncker and Thoennessen (2016).
#'
#'
#'
#'@examples
#'#' data("countryGDP")
#'countryGDP[,2:35] <- log(countryGDP[,2:35])
#'
#'# Cluster NUTS regions using GDP from year 2000 to year 2014
#'clubs <- findClubs(countryGDP, dataCols=2:35, regions = 1, refCol=35, time_trim = 1/3,
#'                    cstar = 0, HACmethod = "AQSB")
#'summary(clubs)
#'
#'# Merge clusters
#'mclubs <- mergeClubs(clubs, HACmethod='AQSB', mergeMethod='PS', mergeDivergent=FALSE)
#'summary(mclubs)
#'
#'mclubs <- mergeClubs(clubs, HACmethod='AQSB', mergeMethod='vLT', mergeDivergent=FALSE)
#'summary(mclubs)
#'
#' @export


mergeClubs <- function(clubs,
                       time_trim,
                       HACmethod = c('FQSB','AQSB'),
                       mergeMethod=c('PS','vLT'),
                       mergeDivergent=FALSE,
                       threshold = -1.65){

    ### Check inputs -----------------------------------------------------------
    if(!inherits(clubs,'convergence.clubs')) stop('clubs must be an object of class convergence.clubs')

    X <- attr(clubs, 'data')
    dataCols <- attr(clubs, 'dataCols')
    refCol <- attr(clubs, 'refCol')

    #length of time series
    t <- length(dataCols)
    if(t < 2) stop('At least two time periods are needed to run this procedure')

    #trimming parameter of the time series
    if(missing(time_trim)){
        time_trim <- attr(clubs, 'time_trim')
    } else{
        if( length(time_trim) > 1 | !is.numeric(time_trim) ) stop('time_trim must be a numeric scalar')
        if( time_trim > 1 | time_trim <= 0 ) stop('invalid value for time_trim; should be a value between 0 and 1')
        if( (t - round(t*time_trim)) < 2) stop('either the number of time periods is too small or the value of time_trim is too high')
    }


    ### Initialise variables ---------------------------------------------------
    HACmethod <- match.arg(HACmethod)
    mergeMethod <- match.arg(mergeMethod)

    ll <- length(clubs) - 1 #the last element is 'divergent'
    if(ll<2) stop('There is only one club')

    #output
    pclub <- structure(list(),
                       class = c("convergence.clubs", "list"),
                       data = X,
                       dataCols = dataCols,
                       refCol = refCol,
                       time_trim = time_trim,
                       cstar = attr(clubs, 'cstar'),
                       HACmethod = HACmethod
    )
    n <- 0
    appendLast <- FALSE
    club_names <- names(clubs)


    ### Set methods  -----------------------------------------------------------
    #select functions to compute t-values
    estimateMod <<- if(HACmethod=='FQSB'){
        estimateMod_fqsb
    }else estimateMod_aqsb

    ### Merging procedure ------------------------------------------------------
    i <- 1
    while(i<ll){
        units <- clubs[[i]]$id
        cnm <- club_names[i]  #club name
        mod <- list()
        returnRegions <- !is.null(clubs[[i]]$regions)
        if(returnRegions) regions <- clubs[[i]]$regions
        for(k in (i+1):ll){
            addunits <- clubs[[k]]$id
            if(returnRegions) addregions <- clubs[[k]]$regions
            H <- computeH(X[c(units,addunits), dataCols])
            mod <- estimateMod(H, time_trim)
            tvalue <- mod$tvalue
            #check if a couple of clubs can be merged
            if(tvalue > threshold){
                if(mergeMethod=='vLT' & k <= ll-1){#method by von Lyncker and Rasmus Thoennessen (2016)
                    nextcouple <- c(clubs[[k]]$id,clubs[[k+1]]$id)
                    H <- computeH(X[nextcouple, dataCols])
                    mod2 <- estimateMod(H,time_trim)
                    tvalue2 <- mod2$tvalue
                    if(tvalue > tvalue2){#if true, merge
                        units <- c(units,addunits)
                        if(returnRegions) regions <- c(regions,addregions)
                        cnm <- c(cnm, club_names[k])
                    }else break
                }else{#method by Phillips and Sul (2009)
                    #if so, store units and names of clubs tested
                    #until now, then keep scanning the club list
                    # and repeat thetest adding another club
                    units <- c(units,addunits)
                    if(returnRegions) regions <- c(regions,addregions)
                    cnm <- c(cnm, club_names[k])
                }
            }else{
                if(k==ll){
                    appendLast <- TRUE
                }
                #end if
                #if not, store in output the highest club (i)
                #and start again from i+1
                break
            }
        }#end for
        i <- k
        n <- n+1
        #store new club
        H <- computeH(X[units, dataCols])
        pclub[[paste('club',n,sep='')]] <- list(clubs = cnm,
                                                id = units,
                                                model = estimateMod(H, time_trim)
        )
        if(returnRegions) pclub[[paste('club',n,sep='')]]$regions <- regions
        if(appendLast){
            pclub[[paste('club',n+2,sep='')]] <- list(clubs = club_names[ll],
                                                      id = clubs[[ll]]$id,
                                                      model = clubs[[ll]]$model
            )
            if(returnRegions) pclub[[paste('club',n+2,sep='')]]$regions <- clubs[[ll]]$regions
        }
    }
    pclub$divergent <- clubs$divergent
    if(mergeDivergent){
        return(mergeDivergent(pclub, time_trim, threshold))
    }else return(pclub)
}
