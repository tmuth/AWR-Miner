# setwd("D:/Temp/awr_miner_test_dyn_samp/")
# or
# set the working directory using the session > set working dir menu in R studio

library("stringr")
library("ggplot2")

# create an empty data frame
cap.times <- data.frame()


loadData <- function(fileNameIn){
  temp.df <- read.csv(fileNameIn)
  # extract the last portion of the directory before the filename, 
  # then add it as a new column to the data frame
  temp.df$test <- str_extract(dirname(fileNameIn),"[A-Za-z_0-9\\-]+$")
  # notice the double "<<" in the assignment since the data frame is outside the scope of this function
  cap.times <<- rbind(cap.times,temp.df)
  return(TRUE)
}


# regular expression of file names to look for
filePattern <- ".+capture_times\\.csv"

# list all files that match the pattern under the setwd() recursively
files <- list.files(pattern=filePattern,recursive=TRUE,full.names=TRUE)

# apply the function "loadData" to each element of the list "files"
lapply(files,loadData)


# using the grammar of graphics package, plot the data
ggplot(data=cap.times,aes(x=section,y=seconds,group=test,colour=test,fill=test))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))

