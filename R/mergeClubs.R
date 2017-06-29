#' Merge convergence clubs
#' 
#' Merges a list of clubs created with the function findClubs
#' by either Phillips and Sul method or von Lyncker and Thoennessen procedure
#'
#' Returns as output a list with merged clubs.
#'
#' @param clubs a club list (created by findClub function)
#' @param X matrix or dataframe with data
#' @param IDVar column index of regions
#' @param yearVar column indices of year variables
#' @param lastT column index of the last time period (clustering variable)
#' 
#' @export



mergeClubs <- function(clubs,X,IDvar, yearVar, lastT, method='', divergent=FALSE, threshold = -1.65){

    #initialise variables
    ll <- length(clubs$clubs)
    pclub <- list()
    n <- 0
    appendLast <- FALSE

    if(ll<2) stop('There is only one club')

    i <- 1
    while(i<ll){
        regions <- clubs$clubs[[i]]$regions
        units <- clubs$clubs[[i]]$id
        clubnames <- names(clubs$clubs)[i]
        mod <- list()
        for(k in (i+1):ll){
            addunits <- clubs$clubs[[k]]$id
            addregions <- clubs$clubs[[k]]$regions
            H <- computeH(X, id=c(units,addunits), yearVar)
            # rmod <- mod #model to be returned
            mod <- estimateMod(H,yearVar)
            tvalue <- mod$tvalue
            #check if a couple of clubs can be merged
            if(tvalue > -1.65){
                if(method=='vLT' & k <= ll-1){#method by von Lyncker and Rasmus Thoennessen (2016)
                    nextcouple <- c(clubs$clubs[[k]]$id,clubs$clubs[[k+1]]$id)
                    H <- computeH(X, id=nextcouple,yearVar)
                    # rmod <- mod #model to be returned
                    mod2 <- estimateMod(H,yearVar)
                    tvalue2 <- mod2$tvalue
                    if(tvalue > tvalue2){#if true, merge
                        units <- c(units,addunits)
                        regions <- c(regions,addregions)
                        clubnames <- c(clubnames, names(clubs$clubs)[k])
                        # rmod <- mod
                    }else break
                }else{#method by Phillips and Sul (2009)
                    #if so, store units and names of clubs tested
                    #until now, then keep scanning the club list
                    # and repeat thetest adding another club
                    units <- c(units,addunits)
                    regions <- c(regions,addregions)
                    clubnames <- c(clubnames, names(clubs$clubs)[k])
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
        pclub[[paste('club',n,sep='')]]$clubs <- clubnames
        pclub[[paste('club',n,sep='')]]$regions <- regions
        pclub[[paste('club',n,sep='')]]$id<- units
        H <- computeH(X, id=units,yearVar)
        pclub[[paste('club',n,sep='')]]$model <- estimateMod(H,yearVar)

        if(appendLast){
            pclub[[paste('club',n+2,sep='')]]$clubs <- names(clubs$clubs)[ll]
            pclub[[paste('club',n+2,sep='')]]$regions <- clubs$clubs[[ll]]$regions
            pclub[[paste('club',n+2,sep='')]]$id<- clubs$clubs[[ll]]$id
            pclub[[paste('club',n+2,sep='')]]$model <- clubs$clubs[[ll]]$model
        }
    }
    if(divergent){
        dmerge <- list()
        dmerge$clubs <- pclub
        dmerge$divergent <- clubs$divergent
        return(mergeDivergent(clubs=dmerge,X,threshold))
    }else return(pclub)
    return(pclub)
}
