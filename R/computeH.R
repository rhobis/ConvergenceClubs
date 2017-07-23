############################################################
### Helper function: computes H values, given the dataset, 
### the units for which H must be computed and the indices 
### of the column in the dataset to compute H for
###
### authors:        Roberto Sichera, Pietro Pizzuto
### last modified:  13/10/2016

computeH <- function(X,id,yearVar){
    ### X:          matrix or data.frame with data
    ### id:         row indices of regions for which H is to be computed
    ### yearVar:    vector containing the indices of the variable columns
    ###
    ### returns a vector with H quantities
    
    h <- apply(X[id,yearVar], 2, function(x) x/mean(x))
    H <- apply(h, 2, function(h) mean((h-1)^2) )
    
    # N <- length(id)
    # nT <- length(yearVar)
    # # yearVar1 <- yearVar[rT]
    # h <- matrix(0,length(id),nT) 
    # i <- 0
    # for(x in id){#for sulle regioni
    #     i <- i + 1
    #     j <- 0
    #     for(t in yearVar){#for sul tempo
    #         j <- j+1
    #         h[i,j] = X[x,t]/mean(X[id,t])
    #     }
    # }
    # H <- vector(length=nT)
    # for(t in 1:length(yearVar)){
    #     H[t] <- sum((h[,t] - 1)^2)/N
    # }
    return(H)
}
