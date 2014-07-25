fileName <- "awr-hist-389926331-USPS-775-985-timed.out"

theFile <- readLines(fileName)
theFileTXT <- paste(theFile, collapse='\n')

timingVector <- str_extract_all(theFileTXT, "~~END.+?\nElapsed.+?\n")
timingVector <- unlist(timingVector)

if(length(timingVector) < 5){
  print("No Timing")
  return(data.frame())
}

timingVector <- gsub('~~END\\-(.+)~~\n','\\1\n', timingVector)
timingVector <- gsub('Elapsed: ','', timingVector)

timing_DF <- colsplit(timingVector, "\n", names=c("section", "elapsed"))
options(digits.secs=2)
timing_DF$seconds <- second(strptime(timing_DF$elapsed,"%H:%M:%OS"))
head(timing_DF)


timing_DF$seconds <- seconds(hms(timing_DF$elapsed))

foo <- sum(timing_DF$seconds)