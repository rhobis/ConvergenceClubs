############################################################
### Compute log model, given H values and y. Returns main
### output from the model (coefficients,st.dev,t-value, pvalue)
###
### Helper function, called inside function findClub.
###
### authors:        Roberto Sichera, Pietro Pizzuto
### last modified:  13/10/2016

estimateMod <- function(H,y){
    # H: vector with H values
    # y: response variable
    # returns a list with different quantities: 
    #  parameters, std. deviation, tvalue and pvalue of the model
    nT <- length(y)
    rT <- (floor(nT*0.3)):nT 
    logt <- log(rT)
    rH <- log(H[1]/H[rT]) - 2*log(logt)
    
    mod <- lm(rH~logt)
    mod <- coeftest(mod,vcov=vcovHAC(mod))
    return(list(beta= mod[2,1],
                st.dev = mod[2,2],
                tvalue = mod[2,3],
                pvalue = mod[2,4]))
}


