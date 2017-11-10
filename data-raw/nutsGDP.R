nutsGDP <- read.table("data-raw/GDPfiltered.txt", header=T, stringsAsFactors = FALSE)
colnames(nutsGDP)[2:16] <- paste('Y', 2000:2014, sep='')

devtools::use_data(nutsGDP,overwrite=TRUE)
