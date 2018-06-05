#'Plot method for S3 x \code{convergence.clubs}
#'
#'@param x an x of class \code{convergence.clubs}.
#'@param y unused, added for compatibility with function \code{plot}
#'@param nrows number of rows of the graphical layout, if NULL, it is automatically defined
#'@param ncols number of columns of the graphical layout, if NULL, it is automatically defined
#'@param y_fixed logical, should the scale of the y axis be the same for all plots?
#'@param save logical, should the plot be saved as a file?
#'@param filename optional, a string indicating the name of the file where the plot
#'    should be saved; must include the extension (e.g. "plot.pdf")
#'@param path optional, a string representing the path of the directory where the plot should
#'    saved; the path should not contain the a final slash symbol ("/")
#'@param width the width of the plot, in inches.
#'@param height the height of the plot, in inches.
#'@param device string indicating the format to be used to save the plot;
#'    one of "pdf", "png" or "jpeg".
#'@param res the resolution of the image, in ppi; only used with \code{device="png"} and \code{device="jpeg"}
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
#'@importFrom grDevices n2mfrow pdf png jpeg dev.off
#'@importFrom graphics par matplot abline



plot.convergence.clubs <- function(x,
                                   y = NULL,
                                   nrows=NULL,
                                   ncols=NULL,
                                   y_fixed = FALSE,
                                   save = FALSE,
                                   filename,
                                   path,
                                   width = 7,
                                   height = 7,
                                   device = c("pdf", "png", "jpeg"),
                                   res,
                                   ...){

    ### Check input and initialise values ---

    if( any( !is.null(nrows) & !is.numeric(nrows),
             !is.null(ncols) & !is.numeric(ncols) ) )
        stop("nrows and ncols must be either NULL or an integer scalar!")
    if( !is.logical(y_fixed) ) y_fixed <- FALSE
    if( !is.logical(save) ) save <- FALSE
    if( missing(path) ) path <- getwd()
    if( !file.exists(path) ) path <- dir.create(path, recursive=TRUE)
    if( any( !is.numeric(width), !is.numeric(height) ) ) stop("width and height must be numeric scalars!")
    if( !all( width>0, height>0) ) stop("width and height must be positive scalars!")
    device <- match.arg(device)

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


    ### Generate/save plots  ---
    ### Save plot ---
    if( save ){

        if( identical(device, 'pdf') ){
            if( missing(filename) ) filename <- "Rplot%03d.pdf"

            pdf(file = file.path(path, filename),
                width=width,
                height=height
            )
        }else if( identical(device, 'png') ){
            if( missing(filename) ) filename <- "Rplot%03d.png"
            if( missing(res) ) res <- 300

            png(filename = file.path(path, filename),
                width=width,
                height=height,
                units='in',
                res=res
            )
        }else{
            if( missing(filename) ) filename <- "Rplot%03d.jpeg"
            if( missing(res) ) res <- 300

            jpeg(filename = file.path(path, filename),
                 width=width,
                 height=height,
                 units='in',
                 res=res
            )
        }
    }


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


    if( save ) dev.off()


}










