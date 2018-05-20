#'Plot method for S3 object \code{convergence.clubs}
#'
#'@param object an object of class \code{convergence.clubs}.
#'@param ... other parameters to pass to function \code{plot()}.
#'
#'@export


plot.convergence.clubs <- function(object, y_fixed = FALSE, ...){

    divergent <- !is.null( object$divergent )
    nc <- length(object) - ifelse( divergent , 1, 0)

    regions <- attributes(object)$regions
    X <- if( identical( regions, NULL) ){
        attributes(object)$data
    } else attributes(object)$data[, -regions]
    avT <- matrix(0, nc, ncol(X[,
                                -1]))




    h <- computeh(X[,-1])







    # with fixed ylim
    par(mfrow=c(2,3))
    for(i in seq_len(nc)){
        avT[i,] <- colMeans(h[ clubs[[i]]$id, ])
        matplot( t( h[ clubs[[i]]$id, ] ), type='l',
                 ylim = c(min(h)-0.1, max(h)+0.1),
                 ylab="Relative transition path", main = paste("Club", i))
        abline(h=1)
    }
    matplot( t( avT ), type='l',
             ylim = c(min(h)-0.1, max(h)+0.1),
             ylab="Relative transition path", main = "All Clubs" )
    abline(h=1)
    par(mfrow=c(1,1))



    # with variable ylim
    par(mfrow=c(2,3))
    for(i in seq_len(nc)){
        avT[i,] <- colMeans(h[ clubs[[i]]$id, ])
        matplot( t( h[ clubs[[i]]$id, ] ), type='l',
                 ylim = c(min(h)-0.1, max(h)+0.1),
                 ylab="Relative transition path", main = paste("Club", i))
        abline(h=1)
    }
    matplot( t( avT ), type='l',
             ylim = c(min(h)-0.1, max(h)+0.1),
             ylab="Relative transition path", main = "All Clubs" )
    abline(h=1)
    par(mfrow=c(1,1))

}



data("countryGDP")

## Not run:
# Cluster Countries using GDP from year 2000 to year 2014
clubs <- findClubs(countryGDP, dataCols=2:35, regions = 1, refCol=35, time_trim = 1/3,
                   cstar = 0, HACmethod = "FQSB")

## End(Not run)

clubs <- findClubs(countryGDP, dataCols=2:35, regions = 1, refCol=35, time_trim = 1/3,
                   cstar = 0, HACmethod = "AQSB")
summary(clubs)











