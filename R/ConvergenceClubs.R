#' @section Main functions:
#' The package's main functions are \code{findClubs} and \code{mergeClubs}. The
#' former finds clubs of convergence, given a dataset with units in rows and
#' years in columns, returning an object of class \code{convergence.clubs}. The
#' latter takes as argument an object of class \code{convergence.clubs} and
#' applies the clustering procedure to the convergence clubs contained in the argument,
#' according to either Phillips and Sul (2009) or  von Lyncker and Thoennessen (2016) procedure.
#'
#'
#'
#' @references
#' Phillips, P. C.; Sul, D., 2007. Transition modeling and econometric convergence tests. Econometrica 75 (6), 1771-1855.
#'
#' Phillips, P. C.; Sul, D., 2009. Economic transition and growth. Journal of Applied Econometrics 24 (7), 1153-1185.
#'
#' von Lyncker, K.; Thoennessen, R., 2017. Regional club convergence in the EU: evidence from a panel data analysis.
#' Empirical Economics 52 (2),  525-553
#'
#' Sichera, R.; Pizzuto, P., 2019. ConvergenceClubs: A Package for Performing the Phillips and
#' Sul's Club Convergence Clustering Procedure. The R Journal.
#'
#'
#' @importFrom stats lm pnorm setNames coef model.matrix residuals
#'
#'
"_PACKAGE"
