awr.miner.stats <- read.table("D:/Temp/awr-miner-stats.txt", quote="\"")


list.of.packages <- c("futile.logger","ggplot2", "dplyr","plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate","gplots")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})



awr.miner.stats <- rename(awr.miner.stats , c("V1"="date","V2"="plots"))
awr.miner.stats$date <- dmy(awr.miner.stats$date)

total.plots <- sum(awr.miner.stats$plots)
