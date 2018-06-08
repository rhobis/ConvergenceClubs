#'Plot method for S3 x \code{convergence.clubs}
#'
#'@param x an x of class \code{convergence.clubs}.
#'@param y unused, added for compatibility with function \code{plot}
#'@param nrows number of rows of the graphical layout, if NULL, it is automatically defined
#'@param ncols number of columns of the graphical layout, if NULL, it is automatically defined
#'@param y_fixed logical, should the scale of the y axis be the same for all plots?
#'@param legend logical, should a legend be displayed?
#'@param clubs numeric scalar or vector, indicating for which clubs the transition
#'path plot should be generated. Optional, if omitted, plots for all clubs are produced
#'@param avgTP logical, indicates if a plot of the average transition paths of
#'the convergence clubs should be produced, default to \code{TRUE}
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
#'Graphical parameters of the horizontal line plotted at y=1 may be modified by
#'using the following regular plot parameters:
#'\itemize{
#'    \item \code{lty} defines the type of line, default is "solid"
#'    \item \code{lwd} defines the width of the line, default is 2
#'    \item \code{col} defines the color of the line, default is \code{"black"};
#'        set it to \code{"white"} to remove the horizontal line.
#'}
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
#' plot(clubs, nrows=2,ncols=3)
#' plot(clubs, nrows=3)
#' plot(clubs, ncols=3)
#' plot(clubs, ncols=3, avgTP=FALSE, clubs=c(2,4,5s) )
#' plot(clubs, ncols=3, lty='dotdash', lwd=3, col="blue")
#' plot(clubs, ncols=3, y_fixed=TRUE, lty='dotdash', lwd=3, col="blue")
#'
#'
#'@export
#'
#'@importFrom grDevices n2mfrow pdf png jpeg dev.off
#'@importFrom graphics par matplot abline layout legend
#'




plot.convergence.clubs <- function(x,
                                   y = NULL,
                                   nrows=NULL,
                                   ncols=NULL,
                                   avgTP = TRUE,
                                   clubs,
                                   y_fixed = FALSE,
                                   legend = FALSE,
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
    if( !is.logical(legend) ) legend <- FALSE
    if( !is.logical(save) ) save <- FALSE
    if( !is.logical(avgTP) ) avgTP <- TRUE

    if( missing(path) ) path <- getwd()
    if( !file.exists(path) ) path <- dir.create(path, recursive=TRUE)
    if( any( !is.numeric(width), !is.numeric(height) ) ) stop("width and height must be numeric scalars!")
    if( !all( width>0, height>0) ) stop("width and height must be positive scalars!")
    device <- match.arg(device)

    num_clubs <- dim(x)[1]
    if( missing(clubs) ){
        clubs <- seq_len(num_clubs)
    }else{
        clubs <- suppressWarnings( as.numeric(clubs) )
        clubs <- floor( clubs[!is.na(clubs)])
    }
    if( any(clubs<1) | any(clubs>num_clubs) )
        stop("Invalid value for argument clubs! The total number of clubs is ",
             num_clubs, ", please be sure to include values within 1 and ", num_clubs )

    divergent <- !is.null( x$divergent )
    nplots <- length(clubs) + (avgTP)  # one plot per club plus the club averages (if avgTP is TRUE)

    regions <- attributes(x)$regions
    if( identical( regions, NULL) ){
        data <- attributes(x)$data
    } else data <- attributes(x)$data[, -regions]


    #graphical parameters
    arguments <- list(...)
    ltype <- ifelse( !is.null(arguments$lty), arguments$lty, "solid" )
    lw    <- ifelse( !is.null(arguments$lwd), arguments$lwd, 2 )
    lcol  <- ifelse( !is.null(arguments$col), arguments$col, "black" )
    legend_lab <- ifelse( identical(regions, NULL), "id", "regions")

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
    if( save ){

        if( identical(device, 'pdf') ){
            if( missing(filename) ) filename <- "Rplot%03d.pdf"

            grDevices::pdf(file = file.path(path, filename),
                           width=width,
                           height=height
            )
        }else if( identical(device, 'png') ){
            if( missing(filename) ) filename <- "Rplot%03d.png"
            if( missing(res) ) res <- 300

            grDevices::png(filename = file.path(path, filename),
                           width=width,
                           height=height,
                           units='in',
                           res=res
            )
        }else{
            if( missing(filename) ) filename <- "Rplot%03d.jpeg"
            if( missing(res) ) res <- 300

            grDevices::jpeg(filename = file.path(path, filename),
                            width=width,
                            height=height,
                            units='in',
                            res=res
            )
        }
    }

    if( avgTP ) avT <- matrix(0, num_clubs, ncol(data))
    h <- computeH(data, quantity = "h")

    graphics::par( mfrow=pm )

    if( legend ){
        default_mar <- graphics::par()$mar
        mar_plt <- default_mar; mar_plt[4] <- 0.2  #No margin on the right side of the plot
        mar_lgn <- default_mar; mar_lgn[2] <- 0.2  #No margin on the left side of the legend

        graphics::layout(matrix(c(1,2),nrow=1), width=c(4,1))

        graphics::par( mar = mar_plt)
    }

    i <- 1
    while( i <= (prod(pm)-avgTP) & i <= (nplots-avgTP) ) {
        if( avgTP) avT[i,] <- colMeans(h[ x[[ clubs[i] ]]$id, ])
        graphics::matplot( t( h[ x[[ clubs[i] ]]$id, ] ), type='l',
                           ylim = if(y_fixed){ c(min(h)-0.1, max(h)+0.1) } else NULL ,
                           ylab="Relative transition path", main = paste("Club", clubs[i]))
        graphics::abline(h=1, lty=ltype, lwd=lw, col=lcol)
        if( legend ){
            par( mar=mar_lgn )

            lgn_labs <- x[[ clubs[i] ]][[ legend_lab ]]
            plot(c(0,1),type="n", axes=F, xlab="", ylab="")
            graphics::legend("top",
                   legend=lgn_labs,
                   col=seq_along(lgn_labs),
                   lty=seq_along(lgn_labs),
                   cex=0.8
            )
            graphics::par( mar = mar_plt)
        }

        i <- i+1
    }
    if( avgTP ){
        graphics::matplot( t( avT ), type='l',
                           ylim = if(y_fixed){ c(min(h)-0.1, max(h)+0.1) } else NULL ,
                           ylab="Relative transition path", main = "All Clubs" )
        graphics::abline(h=1, lty = ltype, lwd=lw, col=lcol)
        if( legend ){
            clubs_labs <- paste0('clubs', clubs)
            graphics::par( mar=mar_lgn )
            plot(c(0,1),type="n", axes=F, xlab="", ylab="")
            graphics::legend("top",
                             legend=clubs_labs,
                             col=seq_along(clubs_labs),
                             lty=seq_along(clubs_labs),
                             cex=0.8
            )
            graphics::par( mar=default_mar)
        }
    }
    graphics::par( mfrow=c(1,1) )

    if( save ) grDevices::dev.off()

}

