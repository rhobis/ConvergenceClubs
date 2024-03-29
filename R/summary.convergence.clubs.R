#'Summary method for S3 object \code{convergence.clubs}
#'
#'@param object an object of class \code{convergence.clubs}.
#'@param ... other parameters to pass to function \code{summary()}.
#'
#'@export


summary.convergence.clubs <- function(object, ...){

    x <- object
    x[['divergent']] <- NULL

    # if there are merged clubs, create string vector that indicates which
    # clubs where merged
    merged <- !is.null(object$club1$clubs)
    if( merged ){
        mc <- sapply(x, FUN = function(x) paste(x$clubs, collapse = ' + ') )
        mc <- as.character( mc )
    }

    x <- object; x[['divergent']] <- NULL
    #table with number of units per club
    summary_table <-
        data.frame(
            vapply(x,
                   FUN=function(x) length(x$id),
                   FUN.VALUE=1),
            beta = round(vapply(x,
                                FUN=function(x) x$model['beta'],
                                FUN.VALUE=1), 3),
            std.err = round(vapply(x,
                                   FUN=function(x) x$model['std.err'],
                                   FUN.VALUE=1), 3),
            tvalue = round(vapply(x,
                                  FUN=function(x) x$model['tvalue'],
                                  FUN.VALUE=1), 3),
            stringsAsFactors = FALSE
        )
    if(merged){
        summary_table <- data.frame(mc, summary_table, stringsAsFactors = FALSE)
    }else{
        summary_table <- data.frame(summary_table,
                                    cstar = round(vapply(x, FUN=function(x) x$cstar,
                                                         FUN.VALUE=1), 3),
                                    stringsAsFactors = FALSE)
    }
    colnames(summary_table) <- c(if(merged) 'merged clubs', '# of units', 'beta',
                                 'std.err', 'tvalue', if(!merged) 'cstar')

    #if the clubs have been merged, add a column showing which ones are merged



    # print summaries
    anyDivergent <- 1 * ('divergent' %in% names(object))
    cat(sprintf('Number of convergence clubs: %d', length(object)-anyDivergent), '\n',
        sprintf('Number of divergent units: %d', length(object$divergent$id)), '\n',
        '\n',
        sep='')



    # class(summary_table) <- c('summary.convergence.clubs', 'data.frame')
    attr(summary_table, 'merged') <- merged

    print_table(summary_table)
    return(invisible(summary_table))
    # summary_table
}



#'Print method for S3 object \code{summary.convergence.clubs}
#'
#'
#'@param x an object of class \code{summary.convergence.clubs}.
#'@param ... only present for compatibility with function \code{print()}
#'
#'
#' @keywords internal

print_table <- function(x, ...){
    merged <- attr(x, 'merged')
    if(merged){
        x[, 'merged clubs'] <- sapply(x[,'merged clubs'],
                                      function(s){
                                          a <- unlist( strsplit(s, '[+]') )
                                          a <- gsub('[a-z ]', '', a)
                                          b <- nchar( paste(a, collapse=''), type='width')
                                          if( b>10 ) a <- c( a[1:5], '...' )
                                          return( paste0( 'clubs: ', paste(a, collapse=', ') ) )
                                      })
        # x[,'merged clubs'] <- merged_clubs
        width_col <- pmin(40, nchar(x[,'merged clubs'], type='width'))
    }

    x1 <- data.frame(rownames(x), x, row.names = NULL, stringsAsFactors = FALSE)
    colnames(x1) <- c('', colnames(x))

    #extra padding to the left if value is positive (to align with negative values)
    is_num <- sapply(x1, is.numeric)
    x1[,is_num] <- sapply(x1[is_num], function(x) paste0(ifelse(x<0, ' ', '  '), x)  )

    length_header <- nchar(colnames(x1), type='width')
    length_data   <- matrix(sapply(seq_len(ncol(x1)), function(i) nchar(x1[,i], type='width')),
                               nrow=nrow(x1), ncol=ncol(x1))
    max_length    <- sapply(seq_len(ncol(x1)), function(i) max(length_header[i], length_data[,i]) )
    max_lenght_rn <- max(nchar(x1[,1], type='width'))

    padding_header <-  sapply(max_length-length_header, function(x) paste0(rep(' ', x+2), collapse=''))
    hline_header   <-  sapply(max_length, function(x) paste0(rep('-', x+3), collapse=''))


    ## Print summary
    cat(paste0(' ', colnames(x1), padding_header, collapse='|'), sep=''); cat('\n')
    cat(paste0('', hline_header, collapse=' '), sep=' '); cat('\n')
    for(r in seq_len(nrow(x1))){
        padding <- sapply(max_length-length_data[r,], function(x) paste0(rep(' ', x+2), collapse='') )
        cat(paste0(' ', x1[r,], padding, collapse='|'), sep=''); cat('\n')
    }

}


