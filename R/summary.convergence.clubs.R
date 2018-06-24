#'Summary method for S3 object \code{convergence.clubs}
#'
#'@param object an object of class \code{convergence.clubs}.
#'@param ... other parameters to pass to function \code{summary()}.
#'
#'@export


summary.convergence.clubs <- function(object, ...){
    #table with number of regions per club
    summary_table <- as.data.frame(vapply(object,
                                          FUN=function(x) length(x$id),
                                          FUN.VALUE=1) )
    colnames(summary_table) <- ('# of regions')

    #if the clubs have been merged, add a column showing which ones are merged
    merged <- !is.null(object$club1$club)
    if( merged ){

        mc <- sapply(object, FUN = function(x) paste(x$clubs, collapse = ' + ') )
        mc <- as.character( mc )
        summary_table["old clubs"] <- mc

    }


    # print summaries
    anyDivergent <- 1 * ('divergent' %in% names(object))
    cat(sprintf('Number of convergence clubs: %d', length(object)-anyDivergent), '\n',
        sprintf('Number of divergent units: %d', length(object$divergent$id)), '\n',
        '\n',
        sep='')

    # print.data.frame(summary_table, right=FALSE, row.names = TRUE)
    print_table(summary_table, merged)
}


print_table <- function(x, merged){
    # x <- cbind(data.frame(club=seq_len(nrow(x))),x)
    cn <- colnames(x)
    if(merged){
        old_clubs <- sapply(x[,2],
                            function(s){
                                a <- unlist( strsplit(s, '[+]') )
                                a <- gsub('[a-z ]', '', a)
                                b <- nchar( paste(a, collapse=''), type='width')
                                if( b>10 ) a <- c( a[1:5], '...' )
                                return( paste0( 'clubs: ', paste(a, collapse=', ') ) )
                            })
        x[,2] <- old_clubs
        width_col <- pmin(40, nchar(x[,2], type='width'))
    }


    mrcn <- max( nchar( rownames(x), type='width') )

    cat( paste(rep(' ', mrcn+2), collapse='' ),
         paste(cn, collapse=" | ")); cat('\n')
    cat( paste(rep('-', mrcn+2), collapse='' ),
         paste(rep('-', nchar(cn[1], type='width')+1), collapse='' ),
         if(merged) paste(rep('-', max(width_col)+3), collapse='') ); cat('\n')

    for(r in seq_len(nrow(x))){
        cat(rownames(x)[r],
            paste(rep(" ", mrcn - nchar(rownames(x)[r], type='width')), collapse=""),
            "|",
            paste( x[r,], collapse= '\t       | ') ); cat('\n')
    }

}

