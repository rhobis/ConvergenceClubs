#' Test convergence
#' 
#' @param H vector of H values
#' @param dataCols integer vector with thecolumn numers of the data 
#' @param time_trim a numeric value between 0 and 1, representing the portion of 
#' time periods to ignore when computing tvalues
#' 
#' 
#' @return  a list containing information about the model used to run the t-test 
#' on the regions in the club: beta coefficient, standard deviation, t-statistics and p-value.
#' 
#' @export


estimateMod_aqsb <- function(H, dataCols, time_trim){
    ### Initialise variables ---------------------------------------------------
    nT <- length(dataCols)
    rT <- (round(nT*time_trim) + 1):nT
    logt <- log(rT)
    rH <- log(H[1]/H[rT]) - 2*log(logt)
    ### Estimation -------------------------------------------------------------
    mod <- lm(rH ~ logt) 
    mod <- lmtest::coeftest(mod,vcov=sandwich::vcovHAC(mod))
    ### Output -----------------------------------------------------------------
    return(list(beta= mod[2,1],
                st.dev = mod[2,2],
                tvalue = mod[2,3],
                pvalue = mod[2,4]))
}


