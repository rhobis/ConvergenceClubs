#' Merge convergence clubs
#'
#' Merges a list of clubs created with the function findClubs
#' by either Phillips and Sul method or von Lyncker and Thoennessen procedure
#'
#' Returns as output a list with merged clubs.
#'
#' @param clubs a club list (created by findClub function)
#' @param X dataframe containing data
#' @param dataCols integer vector with the column indices of the data
#' @param refCol integer scalar indicating the index of the column to use for ordering
#' data
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to ignore when computing tvalues
#' @param cstar threshold for the tvalue for inclusion of regions in phase 3?.....
#' @param HACmethod character string indicating whether a Fixed Quadratic Spheric Bandwidth (FQSB) or
#' an Adaptive Quadratic Spheric Bandwidth (AQSB) should be used for......... ###########
#' @param mergeMethod character string indicating the merging method to use. Methods
#' available are \code{'PS'} for Phillips and Sul (2009) and \code{'vLT'} for
#' von Lyncker and Thoennessen (2016).
#' @param mergeDivergent logical, if TRUE, indicates that merging of divergent regions
#' should be tried.
#'
#' @return A list of Convergence Clubs, for each club a list is return with the
#' following objects: \code{id}, a vector containing the row indices
#' of the regions in the club; \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club;
#' \code{regions}, a vector containing the names of the regions of the club (optional,
#' only included if it is present in the \code{clubs} object given in input.
#'
#' @export



mergeClubs <- function(clubs,
                       X,
                       dataCols,
                       refCol,
                       time_trim=1/3,
                       HACmethod = c('FQSB','AQSB'),
                       mergeMethod=c('PS','vLT'),
                       mergeDivergent=FALSE,
                       threshold = -1.65){

    ### Check inputs -----------------------------------------------------------

    #X
    if(!is.data.frame(X)) stop('X must be an object of class data.frame')

    #dataCols
    if(!all(apply(X[,dataCols],2,is.numeric)) ) stop('Some of the data columns are non-numeric')

    #length of time series
    t <- length(dataCols)
    if(t < 2) stop('At least two time periods are needed to run this procedure')

    #time_trim
    if( length(time_trim) > 1 | !is.numeric(time_trim) ) stop('time_trim must be a numeric scalar')
    if( time_trim > 1 | time_trim <= 0 ) stop('invalid value for time_trim; should be a value between 0 and 1')
    if( (t - round(t*time_trim)) < 2) stop('either the number of time periods is too small or the value of time_trim is too high')


    ### Initialise variables ---------------------------------------------------
    HACmethod <- match.arg(HACmethod)
    mergeMethod <- match.arg(mergeMethod)

    ll <- length(clubs) - 1 #the last element is 'divergent'
    if(ll<2) stop('There is only one club')

    pclub <- list()
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
            if(returnRegions) pclub[[paste('club',n+2,sep='')]]$regions = clubs[[ll]]$regions
        }
    }
    # ## If clubs list has regions, add them to output
    # if(!is.null(clubs$club1$regions)){
    #     for(new_club in names(pclub)){
    #         addRegions <- vector()
    #         for(old_club in pclub[[new_club]]$clubs){
    #              addRegions <- c(addRegions, clubs[[old_club]]$regions)
    #         }
    #         pclub[[new_club]]$regions <- addRegions
    #     }
    # }
    if(mergeDivergent){
        pclub$divergent <- clubs$divergent
        return(mergeDivergent(clubs=pclub, X, dataCols, time_trim, threshold))
    }else return(pclub)
}
