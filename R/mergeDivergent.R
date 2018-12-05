#' Merge divergent units
#'
#' Merges divergent units according the algorithm proposed by von Lyncker and Thoennessen (2017)
#'
#'
#' @param clubs an object of class \code{convergence.clubs} (created by \code{findClub}
#' or \code{mergeClubs} function)
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to trim when running log t regression model; if omitted, the same
#' value used for \code{clubs} is used.
#' @param estar a numeric value indicating the threshold \eqn{e^*}{e*} to test
#' if divergent units may be included in one of the new convergence clubs.
#' To be used only if \code{mergeDivergent=TRUE}.
#'
#'
#' @return A list of Convergence Clubs, for each club a list is return with the
#' following objects: \code{id}, a vector containing the row indices
#' of the units in the club; \code{model}, a list containing information
#' about the model used to run the t-test on the units in the club;
#' \code{unit_names}, a vector containing the names of the units of the club (optional,
#' only included if it is present in the \code{clubs} object given in input).
#'
#'
#' @details von Lyncker and Thoennessen (2017) claim that units identified as divergent
#' by the basic clustering procedure by Phillips and Sul might not necessarily still
#' diverge in the case of new convergence clubs detected with the club merging algorithm.
#' To test if divergent units may be included in one of the new convergence clubs,
#' they propose the following algorithm:
#' \enumerate{
#'     \item Run a log t-test for all diverging units, and if \eqn{t_k > -1.65}{t(k) > -1.65}
#'     all these units form a convergence club (This step is implicitly included
#'     in Phillips and Sul basic algorithm);
#'     \item Run a log t-test for each diverging units and each club, creating a
#'     matrix of t-values with dimensions \eqn{d \times p}{d x p}, where each row d represents
#'     a divergent region and each column p a convergence club;
#'     \item Take the highest \eqn{t > e^*}{t-value > e*}
#'     and add the respective region to the respective club and restart from the step 1.
#'     the authors suggest to use \eqn{e^* = t = -1.65 }{e* = t = -1.65};
#'     \item The algorithm stops when no t-value > e* is found in step 3,
#'     and as a consequence all remaining units are considered divergent.
#'}
#'
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#' von Lyncker, K.; Thoennessen, R., 2017. Regional club convergence in the EU: evidence from a panel data analysis.
#' Empirical Economics 52 (2),  525-553
#'
#'
#' @seealso
#' \code{\link{mergeClubs}}, Merges a list of clubs created by \code{findClubs};
#'
#'
#'
#' @examples
#' data("filteredGDP")
#'
#' #Cluster Countries using GDP from year 1970 to year 2003
#' clubs <- findClubs(filteredGDP, dataCols=2:35, unit_names = 1, refCol=35,
#'                    time_trim = 1/3, cstar = 0, HACmethod = "FQSB")
#' summary(clubs)
#'
#' # Merge clusters and divergent units
#' mclubs <- mergeClubs(clubs, mergeDivergent=TRUE)
#' summary(mclubs)
#'
#' @export



mergeDivergent <- function(clubs,
                           time_trim,
                           estar = -1.65
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

    returnNames <- !is.null(clubs$divergent$unit_names)
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
        #if in the matrix max(t-value) > estar
        if(max(tmatrix)>estar){
            #merge the club and divergent region
            #corresponding to max(t-value) and start over
            max.ind <- which(tmatrix==max(tmatrix), arr.ind=TRUE)
            club.ind <- max.ind[1]
            diver.ind <- max.ind[2]
            #merged unit indices
            munits <- c(clubs[[club.ind]]$id, dunits[diver.ind])
            # merged units
            if(returnNames){
                cc <- as.character(clubs[[club.ind]]$unit_names)
                cd <- as.character(clubs$divergent$unit_names[diver.ind])
                mnames <- c(cc,cd)
            }
            H <- computeH(X[munits, dataCols])
            mod <- estimateMod(H, time_trim, HACmethod = HACmethod)
            #modify club list
            if(returnNames) clubs[[club.ind]]$unit_names <- mnames
            clubs[[club.ind]]$id <- munits
            clubs[[club.ind]]$model <- mod
            if(returnNames){
                clubs[[club.ind]]$message[[messcount[club.ind]]] <-
                    paste(sprintf("merged with divergent region %s",clubs$divergent$unit_names[diver.ind]),
                          sprintf("which index is %d",dunits[diver.ind]), sep='')
            }else{
                clubs[[club.ind]]$message[[messcount[club.ind]]] <-
                    paste(sprintf("merged with divergent region with id %d", dunits[diver.ind]),
                          sep='')
            }
            messcount[club.ind] <- messcount[club.ind] + 1
            #remove unit merged with club from divergent units
            dunits <- clubs$divergent$id[-diver.ind]
            # # modify list of divergent units
            if(returnNames) clubs$divergent$unit_names <- clubs$divergent$unit_names[-diver.ind]
            clubs$divergent$id <- dunits
        }else{#if max(t-value) <= estar, stop algorithm
            return(clubs)
        }
    }
    #out of divergent units, return output
    return(clubs)
}
