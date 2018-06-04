#'Plot method for S3 x \code{convergence.clubs}
#'
#'@param x an x of class \code{convergence.clubs}.
#'@param y unused, added for compatibility with function \code{plot}
#'@param nrows number of rows of the graphical layout, if NULL, it is automatically defined
#'@param ncols number of columns of the graphical layout, if NULL, it is automatically defined
#'@param y_fixed logical, should the scale of the y axis be the same for all plots?
#'@param ... other parameters to pass to function \code{plot()}.
#'
#'
#'@details
#'\code{nrows} and \code{ncols} are optional parameters used to define the row and
#'column number for the plot layout. Both or just one of them may be specified.
#'If none of them is specified, the layout dimension is chosen automatically.
#'
#'
#'
#'@examples
#'
#' data("countryGDP")
#'
#' clubs <- findClubs(countryGDP, dataCols=2:35, regions = 1, refCol=35, time_trim = 1/3,
#'                    cstar = 0, HACmethod = "AQSB")
#'
#' plot(clubs)
#' plot(clubs, y_fixed=TRUE)
#' plot(clubs, 2,3)
#' plot(clubs, nrows=3)
#' plot(clubs, ncols=3)
#'
#'
#'@export
#'
#'@importFrom grDevices n2mfrow
#'@importFrom graphics par matplot abline


plot.convergence.clubs <- function(x, y = NULL, nrows=NULL, ncols=NULL, y_fixed = FALSE, ...){

    divergent <- !is.null( x$divergent )
    nclub <- dim(x)[1]
    nplots <- nclub + 1  # one plot per club plus the club averages

    regions <- attributes(x)$regions

    if( identical( regions, NULL) ){

        data <- attributes(x)$data

    } else data <- attributes(x)$data[, -regions]

    avT <- matrix(0, nclub, ncol(data))
    h <- computeH(data, quantity = "h")


    ### Compute dimensions plot layout ---
    if( any( !is.null(nrows) & !is.numeric(nrows),
             !is.null(ncols) & !is.numeric(ncols) ) )
        stop("nrows and ncols must be either NULL or an integer scalar!")


    if( !is.null(nrows) & !is.null(ncols) ){

        pm <- floor( c(nrows, ncols) )
        if( prod(pm) < nplots )
            warning( paste0("The chosen values for nrows and ncols produce a plot grid ",
                            "with an insufficient number of cells... some plots will not be diplayed!\n",
                            "Try to change those values or set nrows and ncols to NULL.")
            )

    }else if( !is.null(nrows) ){

        if(nrows < 1) nrows <- 1 else nrows <- floor(nrows)
        pm <- c( nrows, ceiling(nplots/nrows) )

    }else if( !is.null(ncols) ){

        if(ncols < 1) ncols <- 1 else ncols <- floor(ncols)
        pm <- c( ceiling(nplots/ncols), ncols )

    }else  pm <- grDevices::n2mfrow(nplots)


    ### Generate plots ---
    i <- 1
    par( mfrow=pm )
    while( i < prod(pm) & i < nplots ) {
        avT[i,] <- colMeans(h[ x[[i]]$id, ])
        matplot( t( h[ x[[i]]$id, ] ), type='l',
                 ylim = if(y_fixed){ c(min(h)-0.1, max(h)+0.1) } else NULL ,
                 ylab="Relative transition path", main = paste("Club", i))
        abline(h=1, lwd=2, col="black")
        i <- i+1
    }
    matplot( t( avT ), type='l',
             ylim = if(y_fixed){ c(min(h)-0.1, max(h)+0.1) } else NULL ,
             ylab="Relative transition path", main = "All Clubs" )
    abline(h=1, lwd=2, col="black")
    par( mfrow=c(1,1) )

}










