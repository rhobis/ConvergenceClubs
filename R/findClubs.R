#' Finds convergence clubs by means of Phillips and Sul  clustering procedure.
#'
#' @param X matrix or dataframe containing data
#' @param regions integer scalar indicating, if present, the index of a column 
#' with codes of the regions
#' @param refCol integer scalar indicating the index of the column of the time period 
#' to which the relative convergence must be referred
#' @param time_trim a numeric value between 0 and 1, representing the portion of 
#' time periods to ignore when computing tvalues
#' @param cstar threshold for the tvalue for inclusion of regions in phase 3?.....
#' @param HACmethod string indicating whether a Fixed Quadratic Spheric Bandwidth (FQSB) or 
#' an Adaptive Quadratic Spheric Bandwidth (AQSB) should be used for......... ###########
#' 
#' 
#' @export



findClubs<- function(X, #data matrix or data.frame
                     regions = NULL, #column index of regions, if present
                     refCol, #column index of year to be used as reference (lastT)
                     time_trim = 1/3, #portion of years to remove from computations (a value between >0 and <1)
                     cstar = 0, #c* value for the second step,
                     HACmethod = c('FQSB','AQSB')){ 
    
    
    ### Initialise variables ---------------------------------------------------
    HACmethod <- match.arg(HACmethod)
    
    N <- nrow(X)
    t <- ncol(X) - as.numeric(returnRegions)
    
    threshold <- -1.65
    
    clubs <- list() #output
    ### Check inputs -----------------------------------------------------------

    #regions
    returnRegions <- switch(class(regions),
                            NULL = FALSE,
                            numeric = TRUE,
                            integer = TRUE,
                            stop('Not a valid value for regions; it should be an integer'))
    
    if(length(regions) > 1) stop('regions must be an integer-valued scalar')
    if( returnRegions & (regions %% 1 != 0) ) stop('regions must be an integer-valued scalar')
    
    #X
    if(returnRegions){
        if(!all(apply(X[,-regions],2,is.numeric)) ) stop('Some of the data columns are non-numeric')
        dataCols <- c(1:ncol(X))[-regions] #vector of indeces of data columns
    }else{
        if(!all(apply(X,2,is.numeric)) ) stop('Some of the data columns are non-numeric')
        dataCols <- 1:ncol(X)
    }

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
    #select functions to compute tvalues
    computeH <<- if(HACmethod=='FQSB'){
        computeH_fqsb
    }else computeH_aqsb
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
            clubs$divergent$id <- list(id = dati$id)
            break #break while loop if out of regions
        }else if(nrow(dati) == 0){
            clubs$divergent$message <- "there are no divergent regions"
            break
        }

        #Test all regions
        H_all <- computeH(dati, id = 1:nrow(dati), dataCols)
        mod_all <- estimateMod(H_all, dataCols, time_trim)
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
            return(clubs)
        }
        
        #find core group
        coreGroup <- coreG(X=dati, refCol, dataCols, time_trim, threshold, type="max")
        #if no more groups are found, add divergent to output and return
        if (identical(coreGroup, FALSE) ){
            # nl <- length(clubs)
            clubs$divergent$id <- list(id = dati$id)
            return(clubs)
        }
        
        #add regions to core group
        convClub <- club(X = dati, coreG = coreGroup, time_trim, cstar = cstar)
        # newcstar <- clubConv$model$cstar
        # xidclub <- which(X[,IDvar] %in% as.character(clubConv$units))
        clubs[[paste('club',l,sep = '')]] <- list( id = convClub$id_regions,
                                                   model = convClub$model
        )
        #     regions = as.character(clubConv$units),
        #     id = xidclub,
        #     model = clubConv$model
        # )
        l <- l + 1
        # unitINclub <- c(unitINclub, xidclub)

        #remove the club found from the dataset
        dati <- dati[-clubConv$rows,]
    }#end of while, end of clustering

    ### Return -----------------------------------------------------------------
    #if returnRegions, then add region codes to output #################
    if(returnRegions){
        for(i in 1:(length(clubs)-1) ){
            clubs[[i]]$regions <- dati[clubs[[i]]$id, regions]
        }
        if(!is.null(clubs$divergent$id)) clubs$divergent$regions <- dati[clubs$divergent$id, regions]
    }else return(clubs)
}
