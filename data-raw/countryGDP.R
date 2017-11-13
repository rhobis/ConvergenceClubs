library(mFilter)

countryGDP <- read.csv2("data-raw/countryGDP.csv", dec='.', header=T, stringsAsFactors = FALSE)
# countryGDP <- read.csv("data-raw/countryGDP.csv", header=T, stringsAsFactors = FALSE)

cols <- c( 'ID', paste('Y', countryGDP$YEAR, sep=''))
id <- colnames(countryGDP)[-1]


countryGDP <- as.data.frame(t(countryGDP[,-1]))

countryGDP <- cbind(id, countryGDP)
colnames(countryGDP) <- cols
rownames(countryGDP) <- NULL


for (j in 1:152) {
    countryGDP[j,] <- hpfilter(countryGDP[,j], freq=400, type="lambda")$trend
}
head(xfilter)

# write.table(countryGDP, 'data-raw/countryGDP.txt')
devtools::use_data(countryGDP,overwrite=TRUE)

