mergeDivergent <- function(clubs, X, threshold = -1.65){
    #clubs is a list of clubs and divergent regions
    #
    #returns a list of clubs merged (or not) with divergent regions

    ###initialize variables
    #id of divergent units
    dunits <- clubs$divergent$id
    #counter for messages of merging a divergent unit
    messcount <- rep(1,length=length(names(clubs$clubs)))
    while(length(dunits)>0){
        #matrix of t-values
        tmatrix <- matrix(0,nrow=length(names(clubs$clubs)),ncol=length(dunits))

        ##compute matrix of t-values
        #(one for each combination club + divergent region)
        for(i in 1:length(names(clubs$clubs))){
            for(j in 1:length(dunits)){
                H <- computeH(X,id=c(clubs$clubs[[i]]$id,dunits[j]),yearVar)
                tmatrix[i,j] <- estimateMod(H,yearVar)$tvalue
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
            munits <- c(clubs$clubs[[club.ind]]$id, dunits[diver.ind])
            #merged regions
            cc <- as.character(clubs$clubs[[club.ind]]$regions)
            cd <- as.character(clubs$divergent$regions[diver.ind])
            mregions <- c(cc,cd)
            H <- computeH(X,id=munits,yearVar)
            mod <- estimateMod(H,yearVar)
            #modify club list
            clubs$clubs[[club.ind]]$regions <- mregions
            clubs$clubs[[club.ind]]$id <- munits
            clubs$clubs[[club.ind]]$model <- mod
            clubs$clubs[[club.ind]]$message[[messcount[club.ind]]] <- paste(sprintf("merged with divergent region %s",clubs$divergent$regions[diver.ind]),
                                                     sprintf("which index is %d",dunits[diver.ind]),
                                                     sep='')
            messcount[club.ind] <- messcount[club.ind] + 1
            #remove unit merged with club from divergent units
            dunits <- clubs$divergent$id[-diver.ind]
            #modify list of divergent units
            clubs$divergent$regions <- clubs$divergent$regions[-diver.ind]
            clubs$divergent$id <- dunits
        }else{#if max(t-value) <= threshold, stop algorithm
            return(clubs)
        }
    }
    #out of divergent regions, return output
    return(clubs)
}
