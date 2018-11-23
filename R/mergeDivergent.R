#' Merge divergent units
#'
#' Merges divergent units according the algorithm proposed by von Lyncker and Thoennessen (2016)
#'
#'
#' @param clubs an object of class \code{convergence.clubs} (created by \code{findClub}
#' or \code{mergeClubs} function)
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running log t regression model; if omitted, the same
#' value used for \code{clubs} is used.
#' @param threshold a numeric value indicating the threshold to be used with the t-test.
#'
#'
#' @return A list of Convergence Clubs, for each club a list is return with the
#' following objects: \code{id}, a vector containing the row indices
#' of the regions in the club; \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club;
#' \code{regions}, a vector containing the names of the regions of the club (optional,
#' only included if it is present in the \code{clubs} object given in input).
#'
#'
#' @details von Lyncker and Thoennessen (2016) claim that units identified as divergent
#' by the basic clustering procedure by Phillips and Sul might not necessarily still
#' diverge in the case of new convergence clubs detected with the club merging algorithm.
#' To test if divergent regions may be included in one of the new convergence clubs,
#' they propose the following algorithm:
#' \enumerate{
#'     \item Run a log t-test for all diverging regions, and if \eqn{t_k > -1.65}{t(k) > -1.65}
#'     all these regions form a convergence club (This step is implicitly included
#'     in Phillips and Sul basic algorithm);
#'     \item Run a log t-test for each diverging regions and each club, creating a
#'     matrix of t-values with dimensions \eqn{d \times p}{d x p}, where each row d represents
#'     a divergent region and each column p a convergence club;
#'     \item Take the highest \eqn{t > e^*}{t-value > e*}
#'     and add the respective region to the respective club and restart from the step 1.
#'     the authors suggest to use \eqn{e^* = t = -1.65 }{e* = t = -1.65};
#'     \item The algorithm stops when no t-value > e* is found in step 3,
#'     and as a consequence all remaining regions are considered divergent.
#'}
#'
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#' von Lyncker, K.; Thoennessen, R., 2016. Regional club convergence in the EU: evidence from a panel data analysis. Empirical Economics, doi:10.1007/s00181-016-1096-2, 1-29.
#'
#' @seealso
#' \code{\link{mergeClubs}}, Merges a list of clubs created by \code{findClubs};
#'
#' \code{\link{mergeDivergent}}, merges divergent units according to the algorithm proposed by von Lyncker and Thoennessen (2016).
#'
#'
#' @examples
#' data("filteredGDP")
#'
#' #Cluster Countries using GDP from year 1970 to year 2003
#' clubs <- findClubs(filteredGDP, dataCols=2:35, regions = 1, refCol=35,
#'                    time_trim = 1/3, cstar = 0, HACmethod = "FQSB")
#' summary(clubs)
#'
#' # Merge clusters and divergent regions
#' mclubs <- mergeClubs(clubs, mergeDivergent=TRUE)
#' summary(mclubs)
#'
#' @export



mergeDivergent <- function(clubs,
                           time_trim,
                           threshold = -1.65
                           ){

    ### Check inputs -----------------------------------------------------------
    if(!inherits(clubs,'convergence.clubs')) stop('clubs must be an object of class convergence.clubs')

    X <- attr(clubs, 'data')
    dataCols <- attr(clubs, 'dataCols')
    refCol <- attr(clubs, 'refCol')
    HACmethod <- attr(clubs, 'HACmethod')

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



    ### Initialize variables ---------------------------------------------------
    cn <- length(clubs)-1 #club number
    club_names <- names(clubs)[-(cn+1)]

    dunits <- clubs$divergent$id #id of divergent units

    returnRegions <- !is.null(clubs$divergent$regions)
    #counter for messages that indicate merging a divergent unit
    messcount <- rep(1,length=cn)
    while(length(dunits)>0){
        #matrix of t-values
        tmatrix <- matrix(0,nrow=cn,ncol=length(dunits))

        ##compute matrix of t-values
        #(one for each combination club + divergent region)
        for(i in 1:cn){
            for(j in seq_along(dunits) ){
                H <- computeH(X[c(clubs[[i]]$id,dunits[j]), dataCols])
                tmatrix[i,j] <- estimateMod(H, time_trim, HACmethod = HACmethod)['tvalue']
            }
        }
        #if in the matrix max(t-value) > threshold
        if(max(tmatrix)>threshold){
            #merge the club and divergent region
            #corresponding to max(t-value) and start over
            max.ind <- which(tmatrix==max(tmatrix), arr.ind=TRUE)
            club.ind <- max.ind[1]
            diver.ind <- max.ind[2]
            #merged unit indices
            munits <- c(clubs[[club.ind]]$id, dunits[diver.ind])
            # #merged regions
            if(returnRegions){
                cc <- as.character(clubs[[club.ind]]$regions)
                cd <- as.character(clubs$divergent$regions[diver.ind])
                mregions <- c(cc,cd)
            }
            H <- computeH(X[munits, dataCols])
            mod <- estimateMod(H, time_trim, HACmethod = HACmethod)
            #modify club list
            if(returnRegions) clubs[[club.ind]]$regions <- mregions
            clubs[[club.ind]]$id <- munits
            clubs[[club.ind]]$model <- mod
            if(returnRegions){
                clubs[[club.ind]]$message[[messcount[club.ind]]] <- paste(sprintf("merged with divergent region %s",clubs$divergent$regions[diver.ind]),
                                                                          sprintf("which index is %d",dunits[diver.ind]),
                                                                          sep='')
            }else{
                clubs[[club.ind]]$message[[messcount[club.ind]]] <- paste(sprintf("merged with divergent region with id %d", dunits[diver.ind]),
                                                                          sep='')
            }
            messcount[club.ind] <- messcount[club.ind] + 1
            #remove unit merged with club from divergent units
            dunits <- clubs$divergent$id[-diver.ind]
            # # modify list of divergent units
            if(returnRegions) clubs$divergent$regions <- clubs$divergent$regions[-diver.ind]
            clubs$divergent$id <- dunits
        }else{#if max(t-value) <= threshold, stop algorithm
            return(clubs)
        }
    }
    #out of divergent regions, return output
    return(clubs)
}
