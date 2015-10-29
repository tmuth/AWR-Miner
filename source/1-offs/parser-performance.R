library("readr")
library("stringr")
library("data.table")
library("dplyr")
library("lazyeval")
 
#devtools::install_github("hadley/lineprof")
library("lineprof")

#setwd("D:/Temp/sandor/test/done")
# awr-hist-1280915453-EUDRPROD-26484-26689.out
fileName <- "D:/Temp/sandor/test/done/awr-hist-1280915453-EUDRPROD-26484-26689.out" # 9 MB
fileName <- "D:/Temp/mike.gaydos/logs_cdx1db01/TEST2/awr-hist-692203206-SPMP2-47327-51648.out" # 52 MB

old1 <- function(){
  theFile <- readLines(fileName)
  theFileTXT <- paste(theFile,collapse="\n")
}

new1 <- function(){
  theFile3 <- read_file(fileName)
}



f <- function(){
  
  old1()
  new1()
}


x <- lineprof(f(), torture = FALSE)
shine(x)



###########################################



getTheSection <- function(inFile,blockName,decSep='.'){
  beginBlock <- paste0('~~[ ]*BEGIN-',blockName,'[ ]*~~')
  endBlock <- paste0('~~[ ]*END-',blockName,'[ ]*~~')
  thePattern <- paste0(beginBlock,'(.*)',endBlock)
  body <- str_extract(inFile, thePattern)
  body <- gsub('\r\n', '\n', body)
  body <- gsub('\n\n', '\n', body)
  #
  body <- gsub('~~.+~~\n','\n', body)
  body <- gsub('\n~~.+~~','\n', body)
  body <- gsub('\n\n', '\n', body)
  body <- gsub('^\n', '', body)
  
  return(body)
}




#theFile2 <- read_lines(fileName)
#theFile3 <- read_file(fileName)



all.equal(theFile,theFile2)

sectionName <- "MAIN-METRICS"
sectionName <- "SIZE-ON-DISK"







###########################################################################
oldParse <- function(sectionName){
  theFile <- readLines(fileName)
  theFileTXT <- paste(theFile,collapse="\n")
  body <- getTheSection(theFileTXT,sectionName)
  
  
  
  dashesLine <- str_extract(body, '\n[- ]+\n')
  if(is.na(dashesLine)){
    dashesLine <- str_extract(body, '\n[= ]+\n')
  }
  
  #print(dashesLine)
  #foo <<- dashesLine
  dashesLine <- gsub('\n', '', dashesLine)
  dashesVector <- str_split(dashesLine,' ')
  #print(dashesVector)
  #dashesVectorLengths <- lapply(dashesVector, nchar)
  
  #print(str(dashesVector))
  dashesVectorLengths <- lapply(dashesVector,function(x){return(nchar(x)+1)})
  
  #extract the titles separately as read.fwf + header=TRUE requires a separator, which we don't have
  theTitles <- str_extract(body, perl("^ ([[:alnum:]_ ])+\n"))
  theTitlesOrig <- theTitles
  #print(theTitles)
  theTitles <- str_replace_all(theTitles,'\n','')
  theTitles <- str_replace_all(theTitles,'^[ ]+','')
  #print(theTitles)
  theTitles <- str_split(theTitles,' +')
  #print(theTitles)
  #theColNames <- unlist(theTitles[[1]])
  theColNames <- unlist(theTitles)
  
  
  #body <- gsub("([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2})","'\\1'", body) # find dates and enclose in quotes so it doesn't break into 2 columns
  
  #   if(!is.null(searchPatternIn)){
  #     body <- gsub(searchPatternIn,replacePatternIn, body)
  #   }
  #print(body)
  dashesTest <- str_extract(body, '\n[- ]+\n')
  if(is.na(dashesTest)){
    body <- gsub('\n[= ]+\n', '\n', body)
  } else{
    body <- gsub('\n[- ]+\n', '\n', body)
  }
  
  
  
  
  
  
  body <- str_replace_all(body,theTitlesOrig,'')
  
  numRows <- str_count(body,'\n')
  decSep <- '.'
  dfInt = read.fwf(file=textConnection(body),skip=0,nrows=numRows,col.names=theColNames,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,widths=unlist(dashesVectorLengths[[1]]),dec=decSep)
  
  return(dfInt)
}
###########################################################################




#all.equal(body1,body2)


#substr(body2,1,800)
#substr(body4,1,800)
newParse <- function(sectionName){
  theFile3 <- read_file(fileName)
  body2 <- getTheSection(theFile3,sectionName)
  #body2 <- str_replace_all(body2,'\n[ -]+\n','\n')
  dfInt2 = data.table(read_table(body2,col_names=TRUE))
  colNameTxt <- names(dfInt2)[1]
  colName <- parse(text=names(dfInt2)[1])
  #setkeyv(dfInt2,colNameTxt)
  #dfInt2 <- dfInt2[!like(eval(colName),"--")]
  dfInt2 <- dfInt2[!(eval(colName)=="----------")]
  dfInt2 <- type_convert(dfInt2)
  return(dfInt2)
}


newParse2 <- function(sectionName){
  theFile3 <- read_file(fileName)
  body2 <- getTheSection(theFile3,sectionName)
  
  #theTitles <- str_extract(body2, perl("^ ([[:alnum:]_ ])+\n"))
  theTitles <- str_extract(body2, "^ ([[:alnum:]_ ])+\n")
  body2 <- str_replace_all(body2,theTitles,'')
  body2 <- paste0(theTitles,body2)
  
  
  dashesTest <- str_extract(body2, '\n[- ]+\n')
  if(is.na(dashesTest)){
    body2 <- gsub('\n[= ]+\n', '\n', body2)
  } else{
    body2 <- gsub('\n[- ]+\n', '\n', body2)
  }
  
  beginString <- substring(body2,1,1000)
  
  if(str_detect(beginString,' end ')){
    body2 <- str_replace_all(body2,'([0-9]{2}/[0-9]{2}/[0-9]{2}) ([0-9]{2}\\:[0-9]{2})','\\1T\\2')
  }
  
  dfInt2 = data.table(read_table(body2,col_names=TRUE))
  
  if(str_detect(beginString,' end ')){
    dfInt2$end <- as.POSIXct(dfInt2$end , format = "%y/%m/%dT%H:%M",tz="UTC")
  }
  #str(dfInt2)
  
  
  #dfInt2 = data.table(read_table(body2,col_names=TRUE))
  

  #colNameTxt <- names(dfInt2)[1]
  #colName <- parse(text=names(dfInt2)[1])
  #setkeyv(dfInt2,colNameTxt)
  #dfInt2 <- dfInt2[!like(eval(colName),"--")]
  #dfInt2 <- dfInt2[!(eval(colName)=="----------")]
  #dfInt2 <- type_convert(dfInt2)
  return(dfInt2)
}


oldParseWrapper <- function(){
  df.old <- oldParse('MAIN-METRICS')
  print(nrow(df.old))
  df.old <- oldParse('IO-WAIT-HISTOGRAM')
  print(nrow(df.old))
  df.old <- oldParse('IOSTAT-BY-FUNCTION')
  print(nrow(df.old))
  df.old <- oldParse('TOP-SQL-BY-SNAPID')
  print(nrow(df.old))
}

newParseWrapper <- function(){
  df.old <- newParse2('MAIN-METRICS')
  print(nrow(df.old))
  df.old <- newParse2('IO-WAIT-HISTOGRAM')
  print(nrow(df.old))
  df.old <- newParse2('IOSTAT-BY-FUNCTION')
  print(nrow(df.old))
  df.old <- newParse2('TOP-SQL-BY-SNAPID')
  print(nrow(df.old))
}

#df.old <- oldParse('MAIN-METRICS')
#df.new <- newParse()

parseCompare <- function(){
  oldParseWrapper()
  newParseWrapper()
}

rm(y)
y <- lineprof(parseCompare(), torture = FALSE)
shine(y)

rm(x)
all.equal(df.old,df.new)
head(body,n=100)

sectionName <- "MAIN-METRICS"
sectionName <- "IO-WAIT-HISTOGRAM"
sectionName <- "TOP-SQL-BY-SNAPID"



system.time(df.old <- oldParse(sectionName))

system.time(df.new <- newParse(sectionName))

system.time(df.new <- newParse2(sectionName))

df.old <- oldParse(sectionName)
df.new <- newParse(sectionName)
df.new2 <- newParse2(sectionName)
df.old <- data.table(df.old)
all.equal(df.old,df.new2)

df.old.summary1 <- df.old %>%
  group_by(PARSING_SCHEMA_NAME) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

df.new2.summary1 <- df.new2 %>%
  group_by(PARSING_SCHEMA_NAME) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



df.new2 <- head(df.new,n=20)
df.new2[!like(SNAP_ID,"--")]
df.new2[!like(df.new2[,1],"--")]



df.new3 <- df.new2[!like(SNAP_ID,"--")]
df.new3 <- df.new2[!like((names(df.new2)[1]),"--")]
df.new4 <- df.new2[!like(df.new2[,1],"--")]


expr <- bquote(.(as.name(cn)):=mean(a))
expr <- bquote(.(as.name(cn)):=mean(a))


cn <- parse(text=names(df.new2)[1])
df.new3 <- df.new2[!like(eval(cn),"--")]
df.new3 <- df.new2[!(eval(cn)=="----------")]
df.new3 <- df.new2[!SNAP_ID=="----------"]

dfInt2 <- type_convert(dfInt2)


str(df.old)
str(df.new2)



theFile3 <- read_file(fileName)
body2 <- getTheSection(theFile3,sectionName)
system.time(body2 <- getTheSection(theFile3,sectionName))




rm(y)
y <- lineprof(df.new <- newParse(sectionName), torture = FALSE)
shine(y)




######################################################################################

theFile3 <- read_file(fileName)
body2 <- getTheSection(theFile3,sectionName)



#system.time(body2 <- getTheSection(theFile3,sectionName))

dashesLine <- str_extract(body2, '\n[- ]+\n')
if(is.na(dashesLine)){
  dashesLine <- str_extract(body, '\n[= ]+\n')
}

#print(dashesLine)
#foo <<- dashesLine
dashesLine <- gsub('\n', '', dashesLine)
dashesVector <- str_split(dashesLine,' ')
#print(dashesVector)
#dashesVectorLengths <- lapply(dashesVector, nchar)

#print(str(dashesVector))
dashesVectorLengths <- lapply(dashesVector,function(x){return(nchar(x)+1)})
dashesTest <- str_extract(body2, '\n[- ]+\n')
if(is.na(dashesTest)){
  body2 <- gsub('\n[= ]+\n', '\n', body2)
} else{
  body2 <- gsub('\n[- ]+\n', '\n', body2)
}

read_fwf(body2, fwf_widths(dashesVectorLengths2))

dashesVectorLengths2 <- unlist(dashesVectorLengths)


body3 <- str_replace_all(body2,'([0-9]{2}/[0-9]{2}/[0-9]{2}) ([0-9]{2}\\:[0-9]{2})','\\1T\\2')
dfInt2 = data.table(read_table(body3,col_names=TRUE))


dfInt2$end <- as.POSIXct(dfInt2$end , format = "%y/%m/%dT%H:%M",tz="UTC")




str_replace_all('15/06/17 08:50','([0-9]{2}/[0-9]{2}/[0-9]{2})[:space:]([0-9]{2}:[0-9]{2})','\\1T\\2')
str_replace_all('15/06/17 08:50','([0-9]{2}/[0-9]{2}/[0-9]{2}) ([0-9]{2}:[0-9]{2})','20\\1T\\2')





#################################################################

theFile3 <- read_file(fileName)
body2 <- getTheSection(theFile3,sectionName)

theTitles <- str_extract(body2, perl("^ ([[:alnum:]_ ])+\n"))
#theTitlesOrig <- theTitles
body2 <- str_replace_all(body2,theTitles,'')
body2 <- paste0(theTitles,body2)

dashesTest <- str_extract(body2, '\n[- ]+\n')
if(is.na(dashesTest)){
  body2 <- gsub('\n[= ]+\n', '\n', body2)
} else{
  body2 <- gsub('\n[- ]+\n', '\n', body2)
}


beginString <- substring(body2,1,1000)
print(beginString)

if(str_detect(beginString,' end ')){
  body2 <- str_replace_all(body2,'([0-9]{2}/[0-9]{2}/[0-9]{2}) ([0-9]{2}\\:[0-9]{2})','\\1T\\2')
}


dfInt2 = data.table(read_table(body2,col_names=TRUE))


if(str_detect(beginString,' end ')){
  dfInt2$end <- as.POSIXct(dfInt2$end , format = "%y/%m/%dT%H:%M",tz="UTC")
  
}


str(dfInt2)






