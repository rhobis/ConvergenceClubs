#' Compute H values
#'
#' Computes H values (cross sectional variance) according to the clustering
#' algorithm by Phillips and Sul (2007, 2009)
#'
#' @param X matrix or dataframe containing data (preferably filtered, in order to remove business cycles)
#' @param id optional; row index of regions for which H values are to be computed;
#' if missing, all regions are used
#'
#' @return A numeric vector
#'
#' @details The cross sectional variation $H_{it}$ is computed as the quadratic
#'          distance measure for the panel from the common limit and under the
#'          hypothesis of the model should converge to zero as \emph{t} tends towards infinity:
#'              \deqn{H_t = N^{-1} \sum_{i=1}^N (h_{it}-1)^2 \rightarrow 0  \qquad	\text{as}   t\rightarrow \inf}{
#'                   H(t) = 1/N \sum [h(it)-1]^2 --> 0  	as   t-> infinity
#'              }
#'          where
#'              \deqn{h_{it} = \frac{\log y_{it}}{(N^{-1} \sum_{i=1}^N log y_{it}} }{
#'                   h(it) = N log[y(it)] / \sum log[y(it)]
#'              }
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#'
#' @export




computeH <- function(X, id){
    if(missing(id)){
        xx <- X
    }else xx <- X[id,]

    h <- apply(xx, 2, function(x) x/mean(x))
    H <- apply(h, 2, function(h) mean((h-1)^2) )

    return(H)
}
