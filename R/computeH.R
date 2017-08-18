#' H values
#' 
#' Computes H values (cross sectional variance)
#' 
#' @param X matrix or dataframe containing data
#' @param dataCols integer vector with the column indices of the data 
#' @param id optional; row index of regions for which H values are to be computed; 
#' if missing, all regions are used
#' 
#' 

computeH <- function(X, dataCols, id){
    if(missing(id)) id <- 1:nrow(X)
    
    h <- apply(X[id,dataCols], 2, function(x) x/mean(x))
    H <- apply(h, 2, function(h) mean((h-1)^2) )
    
    return(H)
}
