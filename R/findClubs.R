#' Finds convergence clubs by means of Phillips and Sul  clustering procedure.
#'
#' @param X matrix or dataframe containing data
#' @param yearVar vector containing the indices of the variable columns (e.g. GDP of years 2000 to 2016)
#' @param lastT column index of the last time period, representing the clustering variable
#' @param cstar threshold value for test t (c*)
#'
#' @export



findClubs<- function(X, IDvar, yearVar, lastT, cstar = 0){
    ### returns a list of convergence clubs and divergent regions;
    ###     output is composed of two lists:
    ###     $clubs, which includes all clubs, each defined by a list:
    ###         $regions --> vector with regions in the club
    ###         $model -->threshold, beta, st.dev and pvalue
    ###                 of the model estimated  on the club
    ###         $id --> index of clustered regions in the
    ###                 original dataset X
    ###     $divergent, which includes all divergent regions and
    ###                 their indices in the original dataset X

    # require(lmtest) 
    # require(sandwich)

    #Sort data by clustering variable (decreasing)
    dati <- X[order(X[,lastT],decreasing = T),]

    #Cluster procedure
    unitINclub <- vector()
    clubs <- list()
    l <- 1
    while(TRUE){
        if (dim(dati)[1] < 2){
            clubs$divergent <- list(regions = as.character(dati[,IDvar]),
                                    id = which(X[,IDvar] %in% as.character(dati[,IDvar])))
            break #break while loop if out of regions
        }

        #Test all regions
        H_all <- computeH(dati,id = 1:nrow(dati),yearVar)
        mod_all <- estimateMod(H_all,yearVar)
        t_all <- mod_all$tvalue
        # if tvalue > -1.65, they all form one club,
        #otherwise go one with clustering
        if (t_all > -1.65) {
            return(clubs$clubs[[paste('club',l,sep = '')]] <- list(
                regions = as.character(dati[,IDvar]),
                id =  which(X[,IDvar] %in% as.character(dati[,IDvar])),
                model = list(
                    threshold = -1.65,
                    beta = mod_all$beta,
                    st.dev = mod_all$st.dev,
                    tvalue = mod_all$tvalue,
                    pvalue = mod_all$pvalue
                )
            ))
        }

        coreGroup <- coreG(X = dati, lastT)
        #if no more groups are found, add divergent to output and return
        if (identical(coreGroup, FALSE) ) {
            nl <- length(clubs)
            clubs$divergent <-
                list(regions = as.character(dati[,IDvar]),
                     id = which(X[,IDvar] %in% as.character(dati[,IDvar])))
            return(clubs)
        }
        clubConv <- club(X = dati, cstar = cstar, coreG = coreGroup)
        # newcstar <- clubConv$model$cstar

        xidclub <- which(X[,IDvar] %in% as.character(clubConv$units))
        clubs$clubs[[paste('club',l,sep = '')]] <- list(
            regions = as.character(clubConv$units),
            id = xidclub,
            model = clubConv$model
        )
        l <- l + 1
        unitINclub <- c(unitINclub, xidclub)

        #take the club found off the dataset
        dati <- X[-unitINclub,]
    }#end of while, end of clustering

    #return output
    return(clubs)
}
