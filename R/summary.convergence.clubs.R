#'Summary method for S3 object \code{convergence.clubs}
#'
#'
#'@export


summary.convergence.clubs <- function(clubs){
    #table with number of regions per club
    summary_table <- as.data.frame(sapply(clubs, function(x) length(x$id)))
    colnames(summary_table) <- ('# of regions')

    cat(sprintf('Number of convergence clubs: %d', length(clubs)-1), '\n',
        sprintf('Number of divergent units: %d', length(clubs$divergent$id)), '\n',
        '\n',
        sep='')

    print(summary_table)
}



