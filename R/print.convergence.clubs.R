#'Print method for S3 object \code{convergence.clubs}
#'
#'
#'@param x an object of class \code{convergence.clubs}.
#'@param ... other parameters to pass to function \code{summary()}.
#'@export


print.convergence.clubs <- function(x, ...){
    # nm <- names(x)
    # attributes(x) <- NULL
    # print(setNames(x, nm))

    what <-  ifelse( is.null(x[[1]][['regions']]), 'id', 'regions' )
    width <- getOption('width') - 10

    for(item in names(x)){
        beta   <- round(x[[item]][['model']]['beta'],4)
        std.err <- round(x[[item]][['model']]['std.err'],4)
        tvalue <- round(x[[item]][['model']]['tvalue'],4)
        pvalue <- round(x[[item]][['model']]['pvalue'],4)
        cat(strrep('=', width), '\n')
        cat(strsplit(item, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)[[1]])
        cat('\n')
        cat(strrep('-', width), '\n')
        cat( strwrap(paste0(x[[item]][[what]], collapse=', '), width=width), fill=width)
        cat('\n')
        cat('beta:    ', paste0(ifelse(beta<0, '', ' '), beta, collapse=''), '\n')
        cat('std.err:  ',  std.err, '\n')
        cat('tvalue   ', paste0(ifelse(tvalue<0, '', ' '), tvalue, collapse=''), '\n')
        cat('pvalue:   ', pvalue, '\n')
        cat('\n')
    }
}

