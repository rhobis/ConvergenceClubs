#'Print method for S3 object \code{convergence.clubs}
#'
#'
#'@param x an object of class \code{convergence.clubs}.
#'@param ... other parameters to pass to function \code{summary()}.
#'@export

print.convergence.clubs <- function(x, ...){
    nm <- names(x)
    attributes(x) <- NULL
    print(setNames(x, nm))
}
