#'Print method for S3 object \code{convergence.clubs}
#'
#'
#'@export

print.convergence.clubs <- function(clubs){
    nm <- names(clubs)
    attributes(clubs) <- NULL
    print(setNames(clubs, nm))
}
