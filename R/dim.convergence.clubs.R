#' \code{dim} method for S3 object \code{convergence.clubs}
#'
#'
#'@param x an object of class \code{convergence.clubs}.
#'@param ... other parameters to pass to function \code{summary()}.
#'
#'
#'
#'@value an integer vector with two values: the first one indicates the number of
#'clubs, the second one the number of divergent units
#'
#'
#'@export

dim.convergence.clubs <- function(x, ...){
    ndiv   <- ifelse( is.null(x$divergent), 0, length(x$divergent$id) )
    nclub  <- ifelse( identical( ndiv, 0 ), length(x), length(x) - 1 )

    return( c(nclub, ndiv) )
}
