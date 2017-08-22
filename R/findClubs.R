#' Finds convergence clubs by means of Phillips and Sul  clustering procedure.
#'
#' @param X dataframe containing data
#' @param dataCols integer vector with the column indices of the data
#' @param regions integer scalar indicating, if present, the index of a column
#' with codes of the regions
#' @param refCol integer scalar indicating the index of the column to use for ordering
#' data
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to ignore when computing tvalues
#' @param cstar threshold for the tvalue for inclusion of regions in phase 3?.....
#' @param HACmethod string indicating whether a Fixed Quadratic Spheric Bandwidth (FQSB) or
#' an Adaptive Quadratic Spheric Bandwidth (AQSB) should be used for......... ###########
#'
#'
#' @return A list of Convergence Clubs, for each club a list is return with the
#' following objects: \code{id}, a vector containing the row indices
#' of the regions in the club; \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club;
#' \code{regions}, a vector containing the names of the regions of the club (optional,
#' only included if parameter \code{regions} is given)
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

    clubs <- list() #output
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
    estimateMod <<- if(HACmethod=='FQSB'){
        estimateMod_fqsb
    }else estimateMod_aqsb


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
        mod_all <- estimateMod(H_all, time_trim)
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
        coreGroup <- coreG(X=dati, refCol, dataCols, time_trim, threshold, type="max")
        #if no more core groups are found, add divergent to output and return
        if (identical(coreGroup, FALSE) ){
            # nl <- length(clubs)
            clubs$divergent$id <- dati$id
            break
        }

        #add regions to core group
        convClub <- club(X = dati, dataCols, core = coreGroup, time_trim, cstar = cstar)
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
        for(i in 1:(length(clubs)-1) ){
            clubs[[i]]$regions <- X[clubs[[i]]$id, regions]
        }
        if(!is.null(clubs$divergent$id)) clubs$divergent$regions <- X[clubs$divergent$id, regions]
        return(clubs)
    }else return(clubs)
}
