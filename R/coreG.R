#' Find core group
#'
#' @param X matrix or dataframe containing data
#' @param refCol integer scalar indicating the index of the column of the time period 
#' to which the relative convergence must be referred
#' @param dataCols integer vector with the column indices of the data 
#' @param time_trim a numeric value between 0 and 1, representing the portion of 
#' time periods to ignore when computing tvalues
#' @param threshold numeric value indicating the threshold to be used to perform
#' the one-tail t test
#' @param type one of "all" or "max", the first option includes all regions that
#' pass the test t in the core, the latter one includes only the region with the maximum t-value;
#' currently, only the option "max" is 
#' 
#' @return A numeric vector containing the row indices of the regions included 
#' in the core group; if a core group cannot be found, returns FALSE

coreG <- function(X,
                  refCol,
                  dataCols,
                  time_trim,
                  threshold = -1.65,
                  type=c("max","all")){
    
    ### Initialisation ---------------------------------------------------------
    type <- match.arg(type)
    nr <- nrow(X) #number of regions
    
    ### Find first couple ------------------------------------------------------
    i <- 1
    while(i < nr){
        #select a couple of regions (i, i+1)
        i <- i + 1
        # j <- j + 1
        H <- computeH( X[c(i-1,i),], dataCols)
        tvalue <- estimateMod(H, dataCols, time_trim)$tvalue
        #t-test (if t>-1.65 --> next step; otherwise repeat
        # for regions (i+1,i+2) )
        if(tvalue > threshold){
            if(i == nr){
                return(c(i-1,i))
            }else break
        }else if(i == nr){#if no core group is found, return FALSE
            return(FALSE)
        } 
    }
    
    ### Find core group --------------------------------------------------------
    units <- c(i-1,i)
    k <- i
    lgroup <- list() #list with units groups
    vt <- vector() #vector with t-values
    lgroup[[1]] <- c(i-1,i)
    vt[1] <- tvalue
    l <- 2
    while(k < nr){
        # Groups obtained adding regions sequentially until t > -1.65
        k <- k + 1
        units <- c(units, k)
        H <- computeH(X[units, ], dataCols)
        tvalue <- estimateMod(H, dataCols, time_trim)$tvalue
        if(tvalue > threshold){
            vt <- c(vt,tvalue)    
            lgroup[[l]] <- units
            l <- l + 1
        }else break
    }
    
    ### Output -----------------------------------------------------------------
    if(type=="max"){#group of units for which t is max
        return( lgroup[[ which(abs(vt) == max(abs(vt)) )]] ) #    
    }else if(type== "all"){#all units
        return(lgroup[[length(lgroup)]])
    }else stop("Invalid value for 'type' argument. Should be one of 'max' or 'all'! ")
}