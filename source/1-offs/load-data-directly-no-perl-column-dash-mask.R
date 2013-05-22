library(stringr)
#WORK_DIR <- 'E:/Portable-AWR-Miner/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/GitHub/AWR-Miner/test-case'
#setwd(WORK_DIR)
#theFile <- readLines('E:/Portable-AWR-Miner/awr-hist-1113023392-P01-49572-50291.out.gz')
#theFile <- readLines('M:/Dropbox/MyFiles/GitHub/AWR-Miner/test-cases/awr-hist-1326070300-DB110g-21717-21899.out')
theFile <- readLines('M:/Dropbox/MyFiles/Accounts/Federal/DLA/Accenture-SAP/AWR-Mining/awr/awr-hist-3672362206-PB1-38344-38863.out')

# unzip code to add:
# http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data

theBody <- NULL
theVector <- NULL
getSection <- function(inFile,blockName,searchPatternIn=NULL,replacePatternIn=NULL){
  beginBlock <- paste0('~~BEGIN-',blockName,'~~')
  endBlock <- paste0('~~END-',blockName,'~~')
  thePattern <- paste0(beginBlock,'(.*)',endBlock)
  body <- str_extract(paste(inFile, collapse='\n'), thePattern)
  
  body <- gsub('\n\n', '\n', body) # remove empty lines
  
  body <- gsub('~~.+~~\n','\n', body) # remove begin block
  body <- gsub('\n~~.+~~','\n', body) # remove end block
  
  
  #print(body)
  # Get the dashes which we'll use to get a vector of column lengths
  dashesLine <- str_extract(body, '\n[- ]+\n')
  #print(dashesLine)
  dashesLine <- gsub('\n', '', dashesLine)
  dashesVector <- str_split(dashesLine,' ')
  #print(dashesVector)
  #dashesVectorLengths <- lapply(dashesVector, nchar)
  dashesVectorLengths <- lapply(dashesVector,function(x){return(nchar(x)+1)})
  
  #extract the titles separately as read.fwf + header=TRUE requires a separator, which we don't have
  theTitles <- str_extract(body, perl("^\n([[:alnum:]_ ])+\n"))
  print(theTitles)
  theTitles <- str_replace_all(theTitles,'\n','')
  theTitles <- str_replace_all(theTitles,'^[ ]+','')
  print(theTitles)
  theTitles <- str_split(theTitles,' +')
  #print(theTitles)
  #theColNames <- unlist(theTitles[[1]])
  theColNames <- unlist(theTitles)
  #DF = read.fwf(text=theBody,header = TRUE,skip=1,nrows=numRows,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE)
  
  
  
  print(dashesVectorLengths)
  #headerSep <- str_extract(paste(body, collapse='\n'), '\n[- ]+\n')
  
  
  body <- gsub('\n[- ]+\n', '\n', body)
  #body <- gsub("([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2})","'\\1'", body) # find dates and enclose in quotes so it doesn't break into 2 columns
  
  if(!is.null(searchPatternIn)){
    body <- gsub(searchPatternIn,replacePatternIn, body)
  }
  #print(body)
  
  
  numRows <- str_count(body,'\n')-1
  #numRows <- 10
  theBody <<- body
  #theVector <<- dashesVectorLengths
  #dfInt = read.table(text=body,header = TRUE,skip=1,nrows=numRows,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE)
  dfInt = read.fwf(file=textConnection(body),skip=2,col.names=theColNames,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,widths=unlist(dashesVectorLengths[[1]]),dec=',')
   
  #remove any rows that are actually repeating headers
  index1 <- with(dfInt, grepl("--",dfInt[,1]))
  dfInt <- dfInt[!index1,]
  
  #remove any rows that are actually repeating headers
  index2 <- with(dfInt, grepl(colnames(dfInt[1]),dfInt[,1]))
  dfInt <- dfInt[!index2,]
  #print(dfInt)
  return(dfInt)
}

# DF_OS <- getSection(theFile,'OS-INFORMATION')
DF_MEMORY <- getSection(theFile,'MEMORY')
# DF_SIZE <- getSection(theFile,'SIZE-ON-DISK')
DF_MAIN <- getSection(theFile,'MAIN-METRICS')
# 
DF_DB_PARAMETERS <- getSection(theFile,'DATABASE-PARAMETERS')

searchPattern <- "\n([[:digit:] ]{10}) ([[:print:] ]{20}) ([[:print:] ]{10})"
replacePattern <- "\n'\\1' '\\2' '\\3'"
DF_AAS <- getSection(theFile,'AVERAGE-ACTIVE-SESSIONS')
#DF_AAS <- getSection(theFile,'AVERAGE-ACTIVE-SESSIONS',searchPattern,replacePattern)

searchPattern <- "\n([[:digit:] ]{10}) ([[:print:] ]{20}) ([[:print:] ]{37}) ([[:print:] ]{15}) ([[:print:] ]{10})"
replacePattern <- "\n'\\1' '\\2' '\\3' '\\4' '\\5' "
#DF_IO_WAIT_HIST <- getSection(theFile,'IO-WAIT-HISTOGRAM',searchPattern,replacePattern)
DF_IO_WAIT_HIST <- getSection(theFile,'IO-WAIT-HISTOGRAM')
DF_IO_BY_OBJECT_TYPE <- getSection(theFile,'IO-OBJECT-TYPE')
DF_TOP_SQL_SUMMARY <- getSection(theFile,'TOP-SQL-SUMMARY')
DF_TOP_SQL_BY_SNAPID <- getSection(theFile,'TOP-SQL-BY-SNAPID')
DF_DB_PARAMETERS_INT <- getSection(theFile,'DATABASE-PARAMETERS')
rm(theFile)

