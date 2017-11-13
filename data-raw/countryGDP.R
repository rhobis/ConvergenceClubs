library(mFilter) #for function hpfilter

dat <- read.csv2("data-raw/countryGDP.csv", dec='.', header=T, stringsAsFactors = FALSE)

m <- as.matrix(dat[,-1])
class(m)

#filter data
mf <- apply(m, 2, function(country)  hpfilter(country, freq=400, type="lambda")$trend)

#create data.frame
countryGDP <- data.frame(colnames(m), t(mf))
colnames(countryGDP) <- c('ID', paste('Y', dat[,1], sep=''))
rownames(countryGDP) <- NULL

# write.table(countryGDP, 'data-raw/countryGDP.txt')
devtools::use_data(countryGDP,overwrite=TRUE)


