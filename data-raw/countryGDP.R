# library(mFilter) #for function hpfilter
#
# dat <- read.csv2("data-raw/countryGDP.csv", dec='.', header=T, stringsAsFactors = FALSE)
#
# m <- as.matrix(dat[,-1])
# class(m)
#
# #filter data
# mf <- apply(m, 2, function(country)  hpfilter(country, freq=400, type="lambda")$trend)
#
# #create data.frame
# countryGDP <- data.frame(colnames(m), t(mf))
# colnames(countryGDP) <- c('ID', paste('Y', dat[,1], sep=''))
# rownames(countryGDP) <- NULL
#
# # write.table(countryGDP, 'data-raw/countryGDP.txt')
# devtools::use_data(countryGDP,overwrite=TRUE)
#


dat <- read.csv2("data-raw/countryGDP.csv", dec='.', header=T, stringsAsFactors = FALSE)
head(dat)

m <- log(as.matrix(dat[,-1]))
m1  <- m


# Filtering approach according to Phillips and Sul (2009)
HPMat2 <- function(  # check
    data,
    lambda
){
    eye        <- diag(length(data))
    gy        <- solve(eye + lambda*crossprod(diff(eye, lag = 1, differences = 2)), data)
    cy        <- data - gy
    result    <- data.frame(gy, cy)
    return(result)
}


m1 <- apply(m, 2, function(x) HPMat2(x, 400)$gy)

#create data.frame
countryGDP <- data.frame(colnames(m), t(m1))
colnames(countryGDP) <- c('ID', paste('Y', dat[,1], sep=''))
rownames(countryGDP) <- NULL

# write.table(countryGDP, 'data-raw/countryGDP.txt')
devtools::use_data(countryGDP,overwrite=TRUE)
