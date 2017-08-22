#' Merge convergence clubs
#'
#' Merges a list of clubs created with the function findClubs
#' by either Phillips and Sul method or von Lyncker and Thoennessen procedure
#'
#' Returns as output a list with merged clubs.
#'
#' @param clubs a club list (created by findClub or mergeClubs functions)
#' @param X dataframe containing data
#' @param dataCols integer vector with the column indices of the data
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to ignore when computing tvalues
#'
#' @return A list of Convergence Clubs, for each club a list is return with the
#' following objects: \code{id}, a vector containing the row indices
#' of the regions in the club; \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club;
#' \code{regions}, a vector containing the names of the regions of the club (optional,
#' only included if it is present in the \code{clubs} object given in input).
#'
#' @export



mergeDivergent <- function(clubs,
                           X,
                           dataCols,
                           time_trim,
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
            for(j in 1:length(dunits)){
                H <- computeH(X[c(clubs[[i]]$id,dunits[j]), dataCols])
                tmatrix[i,j] <- estimateMod(H, time_trim)$tvalue
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
            mod <- estimateMod(H, time_trim)
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
