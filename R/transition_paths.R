#' Extract transition paths from a \code{convergence.clubs} object
#'
#' Given a \code{convergence.clubs} object (created by either \code{findClubs}
#' or \code{mergeClubs} function), returns a list with transition paths for each
#' club.
#'
#'
#' @param clubs an object of class \code{convergence.clubs} (created by either function
#' \code{findClubs} or \code{\link{mergeClubs}}).
#' @param include_unit_names logical, if TRUE (the default) adds a column with unit names (only
#' if present in the \code{convergence.clubs} object passed to \code{clubs}).
#' @param output_type string indicating if the function should output a list or a data frame.
#' Possible options are "list" and "data.frame", default is "list".
#'
#'
#'
#'
#' @return If \code{output_type=="list"}, a list of data frames, one for each club;
#' each data frame will contain transition paths for the units in the correspondent club.
#' If \code{output_type=="data.frame"}, a data.frame.
#'
#'
#' @references
#'
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#'
#'
#' @seealso
#' \code{\link{findClubs}}, Finds Convergence Clubs;
#' \code{\link{mergeClubs}}, Merges a list of clubs created by \code{findClubs};
#' \code{\link{plot.convergence.clubs}}, Plots transition paths from a \code{convergence.clubs} object.
#'
#'
#'
#'
#' @examples
#' data("filteredGDP")
#'
#' # Cluster Countries using GDP from year 1970 to year 2003
#' clubs <- findClubs(filteredGDP, dataCols=2:35, unit_names = 1, refCol=35,
#'                    time_trim = 1/3, cstar = 0, HACmethod = "FQSB")
#'
#' # Extract Transition Paths
#' tp <- transition_paths(clubs)
#' tp <- transition_paths(clubs, output_type = 'data.frame')
#'
#'
#'
#'
#'
#' @export
#'



transition_paths <- function(clubs, include_unit_names = TRUE, output_type = c("list", "data.frame")){

    #check input
    if(!inherits(clubs,'convergence.clubs')) stop('clubs must be an object of class convergence.clubs')
    if(!is.logical(include_unit_names)) stop('include_unit_names argument should be a logical value')
    output_type <- match.arg(output_type)

    #extract data from club data
    data <- attributes(clubs)$data[, attributes(clubs)$dataCols]

    if(include_unit_names){
        unit_names_ind <- attributes(clubs)$unit_names
        if(is.null(unit_names_ind)){
            message('`include_unit_names=TRUE` but the object passed to argument `clubs` does not include unit_names, unit_names will be ignored')
            include_unit_names <- FALSE
        }else{
            unit_names <- attributes(clubs)$data[,unit_names_ind]
        }
    }

    #extract transition paths
    h <- computeH(data, quantity = "h")

    nm  <- names(clubs)
    out <- lapply(nm,
                  function(n){
                      id <- clubs[[n]][['id']]
                      if(include_unit_names){
                          dt <- data.frame(unit_name=unit_names[id],
                                           h[id, ]
                          )
                      }else{
                          dt <- as.data.frame( h[id, ] )
                      }
                      if( output_type == "data.frame"){
                          dt <- data.frame(club = n, dt)
                      }

                      return(dt)
                  }
    )
    if( output_type == "list"){
        names(out) <- nm
    }else{
        out <- Reduce(rbind, out)
    }

    #return output
    return(out)
}


