############################################################
### Find a club of convergence, given a dataset, a threshold
### and a core group.
###
### Its main purpose is to be called inside the main function 
### findClub().
###
### authors:        Roberto Sichera, Pietro Pizzuto
### last modified:  13/10/2016



club <- function(X,cstar = 0, coreG){
    ### X matrix or dataframe with data
    ### cstar = numeric, threshold value for test t
    ### coreG = vector of units of the core group (ids in X)
    ###
    ### returns a club of convergence, given a core group
    
    #Data without units in the core group
    unitsNoCore <- X[-coreG,]
    
    tvalue <- vector()
    #t test for core + 1 unit
    for(k in 1:nrow(unitsNoCore)){
        #compute H
        H <- computeH(X,id=which(X[,IDvar] %in% c(as.character(X[coreG,IDvar]),as.character(unitsNoCore[k,IDvar]))),yearVar)
        tvalue <- c(tvalue, estimateMod(H,yearVar)$tvalue)
    }
    #Find group (core + regions) such that (core + i)  gives t > cstar
    clubCandidates <- which(X[,IDvar] %in% unitsNoCore[which(tvalue > cstar),IDvar])
    
    #prepare output
    out <- c(coreG,clubCandidates)
    H <- computeH(X,id = out,yearVar)
    mod <- estimateMod(H, yearVar)
    tvalue <- mod$tvalue
    
    #return club info
    return(list(units = X[out,IDvar],
                model = list(beta = mod$beta,
                             st.dev = mod$st.dev,
                             tvalue = mod$tvalue,
                             pvalue = mod$pvalue)))
}


