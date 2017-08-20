#' Core group
#'
#' Add units to core group to complete clubbing procedure
#'
#' @param X matrix or dataframe containing data
#' @param dataCols integer vector with the column indices of the data
#' @param core an integer vector containing the id's of units in core group
#' @param time_trim a numeric value between 0 and 1, representing the portion of
#' time periods to ignore when computing tvalues
#' @param cstar threshold for the tvalue for inclusion of regions in phase 3?.....
#'
#' @return A list of three objects: \code{id}, a vector containing the row indices
#' of club regions in the original dataframe (input of function \code{findClubs});
#' \code{rows}, a vector of row indices of club units in the current dataset
#' (input of function \code{club}); \code{model}, a list containing information
#' about the model used to run the t-test on the regions in the club.
#'
#'


club <- function(X, dataCols, core, time_trim, cstar = 0){

    ### Initialisation ---------------------------------------------------------
    X$row <- 1:nrow(X)
    unitsNoCore <- X[-core,] #Data without units of the core group
    tvalue <- vector()

    ### t-test for core + one unit ---------------------------------------------
    for(k in 1:nrow(unitsNoCore)){
        #compute H
        H <- computeH( X[ c(core,unitsNoCore$row[k]), ], dataCols)
        tvalue <- c(tvalue, estimateMod(H, time_trim)$tvalue)
    }
    #Find group (core + regions) such that (core + i)  gives t > cstar
    clubCandidates_id <- unitsNoCore[which(tvalue > cstar), 'id']
    clubCandidates_row <- unitsNoCore[which(tvalue > cstar), 'row']

    ### Output -----------------------------------------------------------------
    clubId <- c(X[core, 'id'], clubCandidates_id)
    clubRows <- c(core, clubCandidates_row)

    H <- computeH(X[clubRows,], dataCols)
    mod <- estimateMod(H, time_trim)
    # tvalue <- mod$tvalue

    #return club info
    return( list(id = clubId, #id of units in the club
                 rows = clubRows, #row indices of club units in the current dataset 'dati'
                 model = list(beta = mod$beta,
                              st.dev = mod$st.dev,
                              tvalue = mod$tvalue,
                              pvalue = mod$pvalue)))
}


