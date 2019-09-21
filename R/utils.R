#' Define the vector of tick marks for the axis of a plot
#'
#'
#'
#' @param lx integer scalar, representing the length of the x or y vector
#'
#'
#' @keywords internal
#'


axis_marks <- function(lx) {
    i <- 0
    while(T){
        if(lx/10^i <=100) break
        i <- i+1
    }
    return(seq(0, lx, by=5*10^i))
}
