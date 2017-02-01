############################################################
### Finds core group in a dataset X, ordering for variable 
### with index lastT.
###
### If type='max', the group which t-value is maximum among 
### all the groups that satisfy the condition is returned.
### If type='all', the group with all the regions that 
### satisfy the condition is returned.
###
### authors:        Roberto Sichera, Pietro Pizzuto
### last modified:  13/10/2016

coreG <- function(X,lastT,type="max"){
    ### X: matrix or data.frame with data
    ### clusterVar: clustering variable
    ### type: if 'max', the club which maximazes t-value is 
    ###         returned, if 'all' returns the club with 
    ###         all regions which satisfy t > -1.65
    ### returns core group (row indices), or FALSE if no 
    ### core group is available
    
    
    #Find first couple
    i <- 0
    j <- 1
    while(j < dim(X)[1]){
        #select a couple of regions (i, i+1)
        i <- i + 1
        j <- j + 1
        #compute H (computed on time period floor(0.3 * T):T)
        H <- computeH(X,id=c(i,j),yearVar)
        #model tvalue 
        tvalue <- estimateMod(H,yearVar)$tvalue
        #t-test (if t>-1.65 --> next step; otherwise repeat
        # for regions (i+1,i+2) )
        if(tvalue > -1.65){
            if(j == dim(X)[1]){
                return(c(i,j))
            }else break
        }else if(j == dim(X)[1]){
            return(FALSE)
        } 
    }
    
    #Find core group
    units <- c(i,j)
    k <- j
    lgroup <- list() #list with units groups
    vt <- vector() #vector with t-values
    lgroup[[1]] <- c(i,j)
    vt[1] <- tvalue
    l <- 2
    while(k < dim(X)[1]){
        # Groups obtained adding regions sequentially until t > -1.65
        k <- k + 1
        units <- c(units, k)
        H <- computeH(X,units,yearVar)
        tvalue <- estimateMod(H,yearVar)$tvalue
        if(tvalue > -1.65){
            vt <- c(vt,tvalue)    
            lgroup[[l]] <- units
            l <- l + 1
        }else break
    }
    
    #output
    if(type=="max"){#group of units for which t is max
        return(lgroup[[which(vt==max(vt))]])    
    }else if(type== "all"){#all units
        return(lgroup[[length(lgroup)]])
    }else stop("Invalid value for 'type' argument. Should be one of 'max' or 'all'! ")
}