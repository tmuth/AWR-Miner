setwd("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/1-offs/SQL")
#save(DF_IO_HIST,file="DF_IO_HIST.Rda")
#DF_SQL_SUMMARY <- main$DF_SQL_SUMMARY
#DF_SQL_BY_SNAPID <- main$DF_SQL_BY_SNAPID
#save(DF_SQL_SUMMARY,file="DF_SQL_SUMMARY-CALTRANS.Rda")
#save(DF_SQL_BY_SNAPID,file="DF_SQL_BY_SNAPID-CALTRANS.Rda")
#load(file="DF_IO_HIST.Rda")
#load(file="DF_AAS_INT_mainframe.Rda")
rm(DF_SQL_SUMMARY)
rm(DF_SQL_BY_SNAPID)
load(file="DF_SQL_SUMMARY-CALTRANS.Rda")
load(file="DF_SQL_BY_SNAPID-CALTRANS.Rda")
#load(file="DF_IO_HIST_DLA_PR1.Rda")
options(scipen=10)


list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})

DF_SQL_BY_SNAPID$end <-  round_date(DF_SQL_BY_SNAPID$end , "hour")
DF_SQL_BY_SNAPID$hour <- as.numeric(format(DF_SQL_BY_SNAPID$end,"%H"))
DF_SQL_BY_SNAPID$dayName <- format(DF_SQL_BY_SNAPID$end,"%A")
