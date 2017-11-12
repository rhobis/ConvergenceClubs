#'Summary method for S3 object \code{convergence.clubs}
#'
#'@param object an object of class \code{convergence.clubs}.
#'@param ... other parameters to pass to function \code{summary()}.
#'
#'@export


summary.convergence.clubs <- function(object, ...){
    #table with number of regions per club
    summary_table <- as.data.frame(sapply(object, function(x) length(x$id)))
    colnames(summary_table) <- ('# of regions')

    cat(sprintf('Number of convergence clubs: %d', length(object)-1), '\n',
        sprintf('Number of divergent units: %d', length(object$divergent$id)), '\n',
        '\n',
        sep='')

    print(summary_table)
}



