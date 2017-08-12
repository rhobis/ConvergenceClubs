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
                     cstar = 0, #c* value for the second step
                     HACmethod = c('FQSB','AQSB')){ 
    
    
    ### Initialise variables ---------------------------------------------------
    
    returnRegions <- switch(class(regions),
                            NULL = FALSE,
                            numeric = TRUE,
                            integer = TRUE,
                            stop('Not a valid value for regions; it should be an integer'))
    
    HACmethod <- match.arg(HACmethod)
    
    N <- nrow(X)
    t <- ncol(X) - as.numeric(returnRegions)
    
    #Sort data by clustering variable (decreasing)
    dati <- X[order(X[,lastT],decreasing = T),] ################################
    
    
    ### Check inputs -----------------------------------------------------------
    
    #regions
    if(length(regions) > 1) stop('regions must be an integer-valued scalar')
    if( returnRegions & (regions %% 1 != 0) ) stop('regions must be an integer-valued scalar')
    
    
    #length of time series
    if(t < 2) stop('You need at least two time periods to run this procedure')
    
    #time_trim
    if( length(time_trim) > 1 | !is.numeric(time_trim) ) stop('time_trim must be a numeric scalar')
    if( time_trim > 1 | time_trim <= 0 ) stop('invalid value for time_trim; should be a value between 0 and 1')
    if( (t - round(t*time_trim)) < 2) stop('either the number of time periods is too small or the value of time_trim is too high')
    
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
    
    
    
    
    
    
    ### Find clubs -------------------------------------------------------------
    
    
    
    #Cluster procedure
    unitINclub <- vector()
    clubs <- list()
    l <- 1
    while(TRUE){
        if (nrow(dati) == 1){
            clubs$divergent <- list(regions = as.character(dati[,IDvar]),
                                    id = which(X[,IDvar] %in% as.character(dati[,IDvar])))
            break #break while loop if out of regions
        }else if(nrow(dati) == 0){
            clubs$divergent <- "there are no divergent units"
        }
        
        #Test all regions
        H_all <- computeH(dati,id = 1:nrow(dati),yearVar)
        mod_all <- estimateMod(H_all,yearVar)
        t_all <- mod_all$tvalue
        # if tvalue > -1.65, they all form one club,
        #otherwise go one with clustering
        if (t_all > -1.65) {
            return(clubs$clubs[[paste('club',l,sep = '')]] <- list(
                regions = as.character(dati[,IDvar]),
                id =  which(X[,IDvar] %in% as.character(dati[,IDvar])),
                model = list(
                    threshold = -1.65,
                    beta = mod_all$beta,
                    st.dev = mod_all$st.dev,
                    tvalue = mod_all$tvalue,
                    pvalue = mod_all$pvalue
                )
            ))
        }
        
        coreGroup <- coreG(X = dati, lastT)
        #if no more groups are found, add divergent to output and return
        if (identical(coreGroup, FALSE) ) {
            nl <- length(clubs)
            clubs$divergent <-
                list(regions = as.character(dati[,IDvar]),
                     id = which(X[,IDvar] %in% as.character(dati[,IDvar])))
            return(clubs)
        }
        clubConv <- club(X = dati, cstar = cstar, coreG = coreGroup)
        # newcstar <- clubConv$model$cstar
        
        xidclub <- which(X[,IDvar] %in% as.character(clubConv$units))
        clubs$clubs[[paste('club',l,sep = '')]] <- list(
            regions = as.character(clubConv$units),
            id = xidclub,
            model = clubConv$model
        )
        l <- l + 1
        unitINclub <- c(unitINclub, xidclub)
        
        #take the club found off the dataset
        dati <- dati[-which(dati[,IDvar] %in% as.character(clubConv$units)),]
    }#end of while, end of clustering
    
    
    
    ### Return 
    
    #if returnRegions, then add region codes to output
    return(clubs)
}


### ----------------------------------------------------------------------------
### OLD VERSION
#' #' Finds convergence clubs by means of Phillips and Sul  clustering procedure.
#' #'
#' #' @param X matrix or dataframe containing data
#' #' @param yearVar vector containing the indices of the variable columns (e.g. GDP of years 2000 to 2016)
#' #' @param lastT column index of the last time period, representing the clustering variable
#' #' @param cstar threshold value for test t (c*)
#' #'
#' #' @export
#' 
#' 
#' 
#' findClubs<- function(X, IDvar, yearVar, lastT, cstar = 0){
#'     ### returns a list of convergence clubs and divergent regions;
#'     ###     output is composed of two lists:
#'     ###     $clubs, which includes all clubs, each defined by a list:
#'     ###         $regions --> vector with regions in the club
#'     ###         $model -->threshold, beta, st.dev and pvalue
#'     ###                 of the model estimated  on the club
#'     ###         $id --> index of clustered regions in the
#'     ###                 original dataset X
#'     ###     $divergent, which includes all divergent regions and
#'     ###                 their indices in the original dataset X
#' 
#'     # require(lmtest) 
#'     # require(sandwich)
#' 
#'     #Sort data by clustering variable (decreasing)
#'     dati <- X[order(X[,lastT],decreasing = T),]
#' 
#'     #Cluster procedure
#'     unitINclub <- vector()
#'     clubs <- list()
#'     l <- 1
#'     while(TRUE){
#'         if (nrow(dati) == 1){
#'             clubs$divergent <- list(regions = as.character(dati[,IDvar]),
#'                                     id = which(X[,IDvar] %in% as.character(dati[,IDvar])))
#'             break #break while loop if out of regions
#'         }else if(nrow(dati) == 0){
#'             clubs$divergent <- "there are no divergent units"
#'         }
#' 
#'         #Test all regions
#'         H_all <- computeH(dati,id = 1:nrow(dati),yearVar)
#'         mod_all <- estimateMod(H_all,yearVar)
#'         t_all <- mod_all$tvalue
#'         # if tvalue > -1.65, they all form one club,
#'         #otherwise go one with clustering
#'         if (t_all > -1.65) {
#'             return(clubs$clubs[[paste('club',l,sep = '')]] <- list(
#'                 regions = as.character(dati[,IDvar]),
#'                 id =  which(X[,IDvar] %in% as.character(dati[,IDvar])),
#'                 model = list(
#'                     threshold = -1.65,
#'                     beta = mod_all$beta,
#'                     st.dev = mod_all$st.dev,
#'                     tvalue = mod_all$tvalue,
#'                     pvalue = mod_all$pvalue
#'                 )
#'             ))
#'         }
#' 
#'         coreGroup <- coreG(X = dati, lastT)
#'         #if no more groups are found, add divergent to output and return
#'         if (identical(coreGroup, FALSE) ) {
#'             nl <- length(clubs)
#'             clubs$divergent <-
#'                 list(regions = as.character(dati[,IDvar]),
#'                      id = which(X[,IDvar] %in% as.character(dati[,IDvar])))
#'             return(clubs)
#'         }
#'         clubConv <- club(X = dati, cstar = cstar, coreG = coreGroup)
#'         # newcstar <- clubConv$model$cstar
#' 
#'         xidclub <- which(X[,IDvar] %in% as.character(clubConv$units))
#'         clubs$clubs[[paste('club',l,sep = '')]] <- list(
#'             regions = as.character(clubConv$units),
#'             id = xidclub,
#'             model = clubConv$model
#'         )
#'         l <- l + 1
#'         unitINclub <- c(unitINclub, xidclub)
#' 
#'         #take the club found off the dataset
#'         dati <- dati[-which(dati[,IDvar] %in% as.character(clubConv$units)),]
#'     }#end of while, end of clustering
#' 
#'     #return output
#'     return(clubs)
#' }
