# library(mFilter) #for function hpfilter

dat <- read.csv2("data-raw/countryGDP.csv", dec='.', header=T, stringsAsFactors = FALSE)
head(dat)

GDP <- data.frame(colnames(dat)[-1], t(dat[,-1]), row.names=NULL, stringsAsFactors=FALSE)
colnames(GDP) <- c('Countries', paste0('Y', dat$YEAR) )

filteredGDP <- apply(GDP[,-1], 1,
                     function(x){
                         mFilter::hpfilter(log(x), freq=400, type="lambda")$trend
                     })
filteredGDP <- data.frame(Countries = GDP[,1], t(filteredGDP), row.names=NULL, stringsAsFactors=FALSE )
colnames(filteredGDP) <- colnames(GDP)




write.table(GDP, 'data-raw/GDP.txt')
write.table(filteredGDP, 'data-raw/filteredGDP.txt')

devtools::use_data(GDP,overwrite=TRUE)
devtools::use_data(filteredGDP,overwrite=TRUE)




