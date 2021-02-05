#'Plot method for S3 class \code{convergence.clubs}
#'
#'Plot the transition paths of units in the convergence clubs and the
#'average transition paths of those clubs.
#'
#'@param x an object of class \code{convergence.clubs}.
#'@param y unused, added for compatibility with function \code{plot}
#'@param nrows number of rows of the graphical layout, if NULL, it is automatically defined
#'@param ncols number of columns of the graphical layout, if NULL, it is automatically defined
#'@param y_fixed logical, should the scale of the y axis be the same for all plots? Default is \code{FALSE}.
#'@param legend logical, should a legend be displayed? Default is \code{FALSE}.
#'@param clubs numeric scalar or vector, indicating for which clubs the transition
#'path plot should be generated. Optional, if omitted, plots for all clubs are produced.
#'If \code{clubs=NULL}, transition path are not plotted for any club.
#'@param avgTP logical, indicates if a plot with the average transition paths of
#'each convergence club should be produced. Default is \code{TRUE}.
#'@param avgTP_clubs numeric scalar or vector, indicating for which clubs the average
#'transition path should be displayed. Optional, if omitted, average transition paths
#'for all clubs are plotted.
#'@param save logical, should the plot be saved as a file?
#'@param filename optional, a string indicating the name of the file where the plot
#'    should be saved; must include the extension (e.g. "plot.pdf")
#'@param path optional, a string representing the path of the directory where the
#'plot should be saved; the path should not end with a slash symbol ("/")
#'@param width the image width when saving the plot, in inches.
#'@param height the image height when saving the plot, in inches.
#'@param device string indicating the format to be used to save the plot;
#'one of "pdf", "png" or "jpeg". The default is "pdf".
#'@param res the resolution of the image, in ppi; only used with \code{device="png"} and \code{device="jpeg"}
#'@param plot_args optional, a named list with the graphical parameters for the plot, see Details section.
#'@param legend_args optional, a named list with the graphical parameters for the legend, see Details section.
#'@param breaks a vector of integer values representing the columns (time periods)
#'to be plotted. Accepted values are integers from 1 to \code{T}, that is the number
#'of time periods included in the convergence procedure. Optional, if omitted, all
#'periods are plotted.
#'@param ... other parameters to pass to function \code{plot()}.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'@details
#'\code{nrows} and \code{ncols} are optional parameters used to define the row and
#'column number for the plot layout. Both or just one of them may be specified.
#'If none of them is specified, the layout dimension is chosen automatically.
#'
# Graphical parameters of the horizontal line plotted at y=1 may be modified by
# using the following regular plot parameters:
# \itemize{
#    \item \code{lty} defines the type of line, default is "solid"
#    \item \code{lwd} defines the width of the line, default is 2
#    \item \code{col} defines the color of the line, default is \code{"black"};
#        set it to \code{"white"} to remove the horizontal line.
# }
#'
#'If \code{legend=TRUE} and a column with units' names is available in the
#'\code{x} object, those names are truncated to fit the plot's legend. The graphical
#'parameter \code{cex} may be used to modify the size of the legend's labels, default is 0.8
#'
#'
#'
#' Note that, when using RStudio, one may incur in an error if the plot window is too small.
#' Enlarging the plot window usually solves the problem.
#'
#'
#'
#'
#'
#'
#'List of argument that could be included in \code{plot_args} as a list:
#'\itemize{
#' \item \code{lty} numeric scalar or vector indicating the line type (values available range from 1 to 6)
#' \item \code{type} a string indicating whether the points (markers) should be displayed.
#' If  'l' no markers are displayed; if  'o'  markers are displayed;
#' \item \code{pch} numeric scalar or vector to specify symbols to use when plotting
#' points (markers). If omitted, customized markers are used for each line.
#' If fixed (e.g. pch=1) the same marker is used for each line. (Values available range from 0 to 25)
#' \item \code{cex} number indicating the amount by which plotting text and symbols
#' should be scaled relative to the default. 1=default, 1.5 is 50\% larger,
#' 0.5 is 50\% smaller, and so on. Default is 1.
#' \item \code{lwd} number indicating the line width. Default is 1.
#' \item \code{xlab} string indicating x-axis label. If omitted, 'Time', is displayed
#' \item \code{ylab} string indicating y-axis label. If omitted, 'Relative transition path', is displayed
#' \item \code{cex.lab} number indicating the amount by which plotting x and y
#' labels should be scaled relative to cex. Default is 1.
#' \item \code{col} option to specify colors for each line. Colors could be specified
#' by index, name, hexadecimal, or RGB. For example col=1, col="white",
#' and col="#FFFFFF" are equivalent. If omitted, colors are chosen randomly.
#' \item \code{col_hline} color of the horizontal line for h=1. Default is 'black'.
#' \item \code{xmarks} vector with tic marks to be displayed in the x axis.
#' \item \code{xlabs} vector with labels of marks to be displayed in the x axis.
#' \item \code{xlabs_dir} number indicating the direction of x-axis labels.
#' For horizontal labels xlabs_dir=0; for vertical labels xlabs_dir=2.
#' }
#'
#'List of argument that could be included in \code{legend_args} as a list:
#'\itemize{
#' \item \code{cex} number indicating the amount by which plotting text and symbols
#' should be scaled relative to the default. Default is 0.9
#' \item \code{lwd} Number indicating the line width. Default is 1.
#' \item \code{y.intersp} number indicating the space between each legend entry. Default is 1.
#' \item \code{max_length_labels} maximum length of the labels displayed for each legend entry.
#'
#'}
#'
#'
#'
#' Note that, when using \emph{RStudio}, one may incur in an error if the plot window
#' is too small.
#' Enlarging the plot window usually solves the problem. We suggest to export plots
#' in the available formats ("pdf", "png" or "jpeg") using adequate values of width and height.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'@examples
#'
#' data("filteredGDP")
#'
#' clubs <- findClubs(filteredGDP, dataCols=2:35, unit_names = 1, refCol=35, time_trim = 1/3,
#'                    cstar = 0, HACmethod = "FQSB")
#'
#'
#'
#' ### Plot transition paths for all clubs
#' plot(clubs)
#' plot(clubs, y_fixed=TRUE)
#' plot(clubs, nrows=2,ncols=4)
#'
#' plot(clubs, ncols=3, lty='dotdash', lwd=3, col="blue")
#' plot(clubs, ncols=3, y_fixed=TRUE, lty='dotdash', lwd=3, col="blue")
#'
#' ### Plot transition paths only for some clubs
#' plot(clubs, clubs=c(2,4,5))
#' plot(clubs, nrows=1, ncols=3, clubs=c(2,4,5), avgTP = FALSE)
#' plot(clubs, nrows=1, ncols=3, clubs=c(2,4,5), avgTP = FALSE, legend=TRUE)
#' plot(clubs, clubs=c(2,4,5), avgTP_clubs = c(1,3))
#' plot(clubs, clubs=c(2,4,5), avgTP_clubs = c(1,3), legend=TRUE)
#'
#'
#' ### Export customized plots
#' #Only plot average transition paths
#' plot(clubs, clubs=NULL, avgTP = TRUE, legend=TRUE)
#'
#' #only lines, without markers and legend
#' plot(clubs, save = TRUE, filename ="name.pdf" , path = tempdir(), width = 15, height = 10)
#'
#' #markers and legend (up to the fourth characther is shown)
#' plot(clubs, legend=TRUE, plot_args=list(type='o'),
#'     legend_args=list(max_length_labels=4, y.intersp=1),
#'     save = TRUE, filename ="name.pdf", path = tempdir(), width = 15, height = 10)
#'
#' #for large samples the legend could be better displayed by plotting each club
#' #in turn and by increasing the plot dimension (through width and height)
#' plot(clubs, clubs=1, avgTP=FALSE, legend=TRUE, plot_args=list(type='o'),
#'     legend_args=list(max_length_labels=8, y.intersp=1),
#'     save = TRUE, filename ="name.pdf", path = tempdir(), width = 20, height = 15)
#'
#' #customize x-labels - 1
#' plot(clubs, legend=TRUE, plot_args=list(type='o', xmarks=seq(1,34),xlabs=seq(1970,2003),
#'     xlabs_dir=0), legend_args=list(max_length_labels=4, y.intersp=1),
#'     save = TRUE, filename ="name.pdf" , path = tempdir(), width = 15, height = 10)
#'
#' #customize x-labels - 2
#' plot(clubs, legend=TRUE, plot_args=list(type='o', xmarks=seq(1,34,1), xlabs=seq(1970,2003,1),
#'     xlabs_dir=2), legend_args=list(max_length_labels=4, y.intersp=1),
#'     save = TRUE, filename ="name.pdf" , path = tempdir(), width = 15, height = 10)
#'
#' #show only the plot with the average transition paths of each club
#' plot(clubs, clubs=NULL, avgTP=TRUE, legend=TRUE,
#'     plot_args=list(type='o', xmarks=seq(1,34), xlabs=seq(1970,2003), xlabs_dir=0),
#'     save = TRUE, filename ="name.pdf" , path = tempdir(), width = 15, height = 10)
#'
#' #markers and legend - png format
#' plot(clubs, legend=TRUE, plot_args=list(type='o'),
#'     legend_args=list(max_length_labels=4, y.intersp=1),
#'     save = TRUE, filename ="name.png" , path = tempdir(), width = 15, height = 10,
#'     device= "png", res=100)
#'
#'
#'
#'
#'
#'
#'@export
#'
#'@importFrom grDevices n2mfrow pdf png jpeg dev.off dev.flush dev.hold
#'@importFrom graphics par plot matplot abline layout legend strwidth strheight axis
#'




plot.convergence.clubs <- function(x,
                                   y = NULL,
                                   nrows=NULL,
                                   ncols=NULL,
                                   clubs,
                                   avgTP=TRUE,
                                   avgTP_clubs,
                                   y_fixed = FALSE,
                                   legend = FALSE,
                                   save = FALSE,
                                   filename,
                                   path,
                                   width = 20,
                                   height = 15,
                                   device = c("pdf", "png", "jpeg"),
                                   res,
                                   plot_args,
                                   legend_args,
                                   breaks,
                                   ...
){

    ### Check input and initialise values ----

    if( any( !is.null(nrows) & !is.numeric(nrows),
             !is.null(ncols) & !is.numeric(ncols) ) )
        stop("nrows and ncols must be either NULL or an integer scalar!")

    if( !is.logical(y_fixed) ) y_fixed <- FALSE
    if( !is.logical(legend) ) legend <- FALSE
    if( !is.logical(save) ) save <- FALSE
    if( !is.logical(avgTP) ) avgTP <- TRUE

    if(save){
        if( missing(path) ) path <- getwd()
        if( !file.exists(path) ) path <- dir.create(path, recursive=TRUE)
        if( any( !is.numeric(width), !is.numeric(height) ) ) stop("width and height must be numeric scalars!")
        if( !all( width>0, height>0) ) stop("width and height must be positive scalars!")
        device <- match.arg(device)
    }

    num_clubs <- dim(x)[1]
    if( missing(clubs)  ){
        clubs <- seq_len(num_clubs)
    }else if(is.null(clubs)){
        if(!avgTP){
            stop("Nothing plotted, please modify your `clubs` and/or `avgTP` argument settings!")
        }
    }
    else if( !is.null(clubs) & !is.vector(clubs) ){
        stop("argument clubs should be vector!")
    }else{
        clubs <- suppressWarnings( as.numeric(clubs) )
        clubs <- floor( clubs[!is.na(clubs)])
    }
    if( missing(avgTP_clubs) ){
        avgTP_clubs <- seq_len(num_clubs)
    }else if( is.null(avgTP_clubs) ){
        avgTP_clubs <- seq_len(num_clubs)
    }else if( !is.vector(avgTP_clubs) ){
        stop("argument avgTP_clubs should be vector!")
    }else{
        avgTP_clubs <- suppressWarnings( as.numeric(avgTP_clubs) )
        avgTP_clubs <- floor( avgTP_clubs[!is.na(avgTP_clubs)])
    }
    if( any(clubs<1) | any(clubs>num_clubs) )
        stop("Invalid value for argument clubs! The total number of clubs is ",
             num_clubs, ", please be sure to include values within 1 and ", num_clubs )



    # divergent <- !is.null( x$divergent )
    nplots <- length(clubs) + (avgTP)  # one plot per club plus the club averages (if avgTP is TRUE)
    data <- attributes(x)$data[,attributes(x)$dataCols]


    ## graphical parameters
    def.par <- par(no.readonly = TRUE)


    # default values
    default_plot   <- list(lty  = seq_len(6),
                           type = 'l',
                           pch  = seq(15,25,1),
                           cex  = 1,
                           lwd  = 1,
                           xlab = 'Time',
                           ylab = 'Relative transition path',
                           cex.lab = 1,
                           col  = seq(1,655,5),
                           col_hline = 'black',
                           xmarks = axis_marks(ncol(data)), #vector with tic marks for the x axis
                           xlabs = NULL, #vector with labels of marks for the x axis
                           xlabs_dir = 0 #0 for horizontal labels, 2 for perpendicular
    )
    if(missing(plot_args)){
        plot_args <- default_plot
    } else {
        for(par in names(default_plot)){
            if(is.null(plot_args[[par]]))
                plot_args[[par]] <- default_plot[[par]]
        }
    }
    if( legend ){
        default_legend   <- list(
            cex  = 0.9,
            lwd  = 1,
            y.intersp = 1,
            max_length_labels = 15
        )

        if(missing(legend_args)){
            legend_args <- default_legend
        } else {
            for(par in names(default_legend)){
                if(is.null(legend_args[[par]]))
                    legend_args[[par]] <- default_legend[[par]]
            }
        }

        unit_names <- attributes(x)$unit_names
        legend_lab <- ifelse( is.null(unit_names), "id", "unit_names")
        labs <- if(is.null(unit_names)) seq_len(nrow(data)) else attributes(x)$data[,unit_names]

    }


    #breaks
    if(missing(breaks)){
        breaks <- seq_len(ncol(data))
    }else if(is.null(breaks)){
        breaks <- seq_len(ncol(data))
    }else if(!is.numeric(breaks)){
        breaks <- seq_len(ncol(data))
        message('The breaks argument supplied is not numeric, it will be ignored!')
    }else{
        breaks  <- as.integer(breaks)
        outside <- any(breaks<1) | any(breaks>ncol(data))
        breaks  <- breaks[breaks>0 & breaks<=ncol(data)]
        if(outside){
            message('There were breaks values outside the admissible range, they have been removed!')
        }
    }


    ### Compute dimensions plot layout ----
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
    +

        grDevices::dev.hold()
    on.exit(grDevices::dev.flush())



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


    graphics::par( mfrow=pm )

    if( legend ){

        mar_plt <- def.par$mar; mar_plt[4] <- 0.2  #No margin on the right side of the plot
        mar_lgn <- def.par$mar; mar_lgn[2] <- 0.2  #No margin on the left side of the legend

        # plot(seq_len(ncol(data)),type="n", axes=F, xlab="", ylab="")
        labs_len  <- min( max(nchar(labs)), legend_args[['max_length_labels']])
        lgn_width <- max(strwidth( substr(labs, 1, labs_len), units='inches'))
        plt_width <- def.par$pin[1]


        graphics::layout( matrix(seq_len(2*prod(pm)), nrow=pm[1], byrow=TRUE),
                          widths = rep(c(plt_width, lgn_width+0.5), pm[2])
                          # height = rep(plt_height, pm[1])
        )


        graphics::par( mar = mar_plt)
    }

    ### transition paths ---
    h <- computeH(data, quantity = "h")
    i <- 1
    while( i <= (prod(pm)-avgTP) & i <= (nplots-avgTP) ) {
        graphics::matplot( t( h[ x[[ clubs[i] ]]$id, breaks ] ),
                           lty  = plot_args[['lty']],
                           type = plot_args[['type']],
                           pch  = plot_args[['pch']],
                           cex  = plot_args[['cex']],
                           lwd  = plot_args[['lwd']],
                           xlab = plot_args[['xlab']],
                           ylab = plot_args[['ylab']],
                           cex.lab = plot_args[['cex.lab']],
                           col  = plot_args[['col']],
                           ylim = if(y_fixed){ c(min(h)-0.1, max(h)+0.1) } else NULL,
                           main = paste("Club", clubs[i]),
                           xaxt = 'n'
        )
        graphics::axis(1, at = plot_args[['xmarks']],
                       labels = if(is.null(plot_args[['xmarks']])) plot_args[['xmarks']] else plot_args[['xlabs']],
                       las = plot_args[['xlabs_dir']])

        graphics::abline(h=1, lty=plot_args[['lty']], lwd=plot_args[['lwd']],
                         col=plot_args[['col_hline']])

        if( legend ){
            par( mar=mar_lgn )
            lgn_labs <-
                substr( x[[ clubs[i] ]][[ legend_lab ]], 1,legend_args[['max_length_labels']])

            plot(c(0,1),type="n", axes=F, xlab="", ylab="")
            graphics::legend("top",
                             bty = 'n',
                             legend = lgn_labs,
                             lwd = legend_args[['lwd']],
                             cex = legend_args[['cex']],
                             # horiz = legend_args[['horiz']],
                             lty = plot_args[['lty']],
                             # pch = plot_args[['pch']],
                             pch = (if(plot_args[['type']] != 'l') plot_args[['pch']] else NA),
                             col = plot_args[['col']],
                             seg.len = 1,
                             xjust = 1,
                             x.intersp=0.5,
                             y.intersp = legend_args[['y.intersp']]
            )
            graphics::par( mar = mar_plt)
        }
        i <- i+1
    }

    if( avgTP ){
        atpm <- matrix(0, length(avgTP_clubs), ncol(data))
        for(i in seq_along(avgTP_clubs) ){
            atpm[i,] <- colMeans(h[ x[[ avgTP_clubs[i] ]]$id, ])
        }

        graphics::matplot( t( atpm[,breaks] ),
                           ylim = if(y_fixed){ c(min(h)-0.1, max(h)+0.1) } else NULL ,
                           main = "Average transition paths - All clubs",
                           lty  = plot_args[['lty']],
                           type = plot_args[['type']],
                           pch  = plot_args[['pch']],
                           cex  = plot_args[['cex']],
                           lwd  = plot_args[['lwd']],
                           xlab = plot_args[['xlab']],
                           ylab = plot_args[['ylab']],
                           cex.lab = plot_args[['cex.lab']],
                           col  = plot_args[['col']],
                           xaxt = 'n'
        )
        graphics::axis(1, at = plot_args[['xmarks']],
                       labels = if(is.null(plot_args[['xmarks']])) plot_args[['xmarks']] else plot_args[['xlabs']],
                       las = plot_args[['xlabs_dir']])

        graphics::abline(h=1, lty = plot_args[['lty']],
                         lwd = plot_args[['lwd']], col=plot_args[['col_hline']])

        if( legend ){
            clubs_labs <- paste0('club', avgTP_clubs)
            graphics::par( mar=mar_lgn )
            plot(c(0,1),type="n", axes=F, xlab="", ylab="")
            graphics::legend("top",
                             bty = 'n',
                             legend = clubs_labs,
                             lwd = legend_args[['lwd']],
                             cex = legend_args[['cex']],
                             # horiz = legend_args[['horiz']],
                             lty = plot_args[['lty']],
                             # pch = plot_args[['pch']],
                             pch = (if(plot_args[['type']] != 'l') plot_args[['pch']] else NA),
                             col = plot_args[['col']],
                             seg.len = 1,
                             xjust = 1,
                             x.intersp=0.5,
                             y.intersp = legend_args[['y.intersp']]
            )
            graphics::par( mar=def.par$mar )
        }
    }

    if( save ) grDevices::dev.off()
    par(def.par)

}
