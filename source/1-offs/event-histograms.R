list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})



getSection <- function(inFile,blockName,decSep='.',searchPatternIn=NULL,replacePatternIn=NULL){
  flog.debug(paste0("getSection - start - ",blockName),name="getSection")
  beginBlock <- paste0('~~BEGIN-',blockName,'~~')
  endBlock <- paste0('~~END-',blockName,'~~')
  thePattern <- paste0(beginBlock,'(.*)',endBlock)
  body <- str_extract(paste(inFile, collapse='\n'), thePattern)
  
  if(str_detect(body, 'table not in this version')){
    flog.trace("table not in this version",name="getSection")
    dfInt <- data.frame()
    return(dfInt)
  }
  
  
  
  body <- gsub('\n\n', '\n', body)
  #
  body <- gsub('~~.+~~\n','\n', body)
  body <- gsub('\n~~.+~~','\n', body)
  body <- gsub('\n\n', '\n', body)
  
  
  # Get the dashes which we'll use to get a vector of column lengths
  dashesLine <- str_extract(body, '\n[- ]+\n')
  #print(dashesLine)
  dashesLine <- gsub('\n', '', dashesLine)
  dashesVector <- str_split(dashesLine,' ')
  #print(dashesVector)
  #dashesVectorLengths <- lapply(dashesVector, nchar)
  
  #print(str(dashesVector))
  dashesVectorLengths <- lapply(dashesVector,function(x){return(nchar(x)+1)})
  
  #extract the titles separately as read.fwf + header=TRUE requires a separator, which we don't have
  theTitles <- str_extract(body, perl("^\n([[:alnum:]_ ])+\n"))
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
  body <- gsub('\n[- ]+\n', '\n', body)
  
  body <- str_replace_all(body,theTitlesOrig,'')
  
  numRows <- str_count(body,'\n')
  #numRows <- 10
  
  #dfInt = read.table(text=body,header = TRUE,skip=0,nrows=numRows,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,dec=decSep)
  dfInt = read.fwf(file=textConnection(body),skip=0,nrows=numRows,col.names=theColNames,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,widths=unlist(dashesVectorLengths[[1]]),dec=decSep)
  #trim leading and trailing spaces from every column in the data frame
  #dfInt2 <- dfInt
  #dfInt<-colwise(str_trim)(dfInt2)
  
  
  #remove any rows that are actually repeating headers
  index1 <- with(dfInt, grepl("--",dfInt[,1]))
  dfInt <- dfInt[!index1,]
  
  #remove any rows that are actually repeating headers
  index2 <- with(dfInt, grepl(colnames(dfInt[1]),dfInt[,1]))
  dfInt <- dfInt[!index2,]
  #print(dfInt)
  
  
  flog.debug(paste0("getSection - end - ",blockName),name="getSection")
  #dfInt <- na.omit(dfInt)
  return(dfInt)
}



build_data_frame <- function(fileIn) {
 # flog.debug("build_data_frames - start",name="build_data_frames")
#  flog.info(paste0("dbid-dbname: ",dbid,'-',dbname),name="build_data_frames")
  #   dbid <- 1499932796
  #   dbname <- main$current_db_name
  #y<-grep("^.+651066032.+$",os_files)
  #filePatternInt <- paste0("^.+",dbid,".+$")
  #fileIndex<-grep(filePatternInt,main$awrFiles)
  #> os_files[y]
  theFile <- readLines(fileIn)
  flog.debug(length(theFile))
  
  
  DF_TEMP <- getSection(theFile,'MEMORY')
  countDecimals <- sum(str_count(DF_TEMP$PGA,'\\.'))+sum(str_count(DF_TEMP$SGA,'\\.'))
  countCommas <- sum(str_count(DF_TEMP$PGA,'\\,'))+sum(str_count(DF_TEMP$SGA,'\\,'))
  
  computedDecSep <- '.'
  if(countCommas>countDecimals){
    computedDecSep <- ','
  }
  
  
  
  DATA_FRAME_INT <- NULL
  #file_pattern=paste(WORK_DIR,paste(dbname,dbid,sep="-"),sep="/")
  DF_OS_INT <- getSection(theFile,'OS-INFORMATION',computedDecSep)
  
  flog.trace('DF_OS_INT',DF_OS_INT,name="build_data_frames",capture=TRUE)
  DF_MEMORY_INT <- getSection(theFile,'MEMORY',computedDecSep)
  #flog.trace('DF_MEMORY_INT',DF_MEMORY_INT,name="build_data_frames",capture=TRUE)
  DF_SPACE_INT <- getSection(theFile,'SIZE-ON-DISK',computedDecSep)
  DF_MAIN_INT <- getSection(theFile,'MAIN-METRICS',computedDecSep)
  
  DF_MAIN_INT <- data.table(DF_MAIN_INT)
  flog.trace("YEP0.1",name="build_data_frames")
  DF_DB_PARAMETERS_INT <- getSection(theFile,'DATABASE-PARAMETERS',computedDecSep)
  flog.trace("YEP0.2",name="build_data_frames")
  index1 <- with(DF_DB_PARAMETERS_INT, grepl("(log_archive|db_create|user_dump_dest|dg_broke)",DF_DB_PARAMETERS_INT$PARAMETER_NAME))
  index2 <- with(DF_DB_PARAMETERS_INT, grepl("DESCRIPTION",DF_DB_PARAMETERS_INT$VALUE))
  DF_DB_PARAMETERS_INT <- DF_DB_PARAMETERS_INT[!index1,]
  DF_DB_PARAMETERS_INT <- DF_DB_PARAMETERS_INT[!index2,]
  DF_DB_PARAMETERS_INT <- na.omit(DF_DB_PARAMETERS_INT)
  flog.trace("YEP0.3",name="build_data_frames")
  flog.trace('DF_DB_PARAMETERS_INT',DF_DB_PARAMETERS_INT,name="build_data_frames",capture=TRUE)
  #DF_DB_PARAMETERS_INT <-  subset(DF_DB_PARAMETERS_INT,nchar(VALUE)>1)
  flog.trace("YEP0.4",name="build_data_frames")
  DF_DB_PARAMETERS_INT$VALUE <- str_sub(DF_DB_PARAMETERS_INT$VALUE,1,25)
  flog.trace("YEP1",name="build_data_frames")
  searchPattern <- "\n([[:digit:] ]{10}) ([[:print:] ]{20}) ([[:print:] ]{10})"
  replacePattern <- "\n'\\1' '\\2' '\\3'"
  #DF_AAS_INT <- getSection(theFile,'AVERAGE-ACTIVE-SESSIONS',computedDecSep,searchPattern,replacePattern)
  DF_AAS_INT <- getSection(theFile,'AVERAGE-ACTIVE-SESSIONS',computedDecSep)
  flog.trace("YEP2",name="build_data_frames")
  DF_AAS_INT$SNAP_ID <- as.numeric(DF_AAS_INT$SNAP_ID)
  DF_AAS_INT$AVG_SESS <- as.numeric(DF_AAS_INT$AVG_SESS)
  DF_AAS_INT <- data.table(DF_AAS_INT)
  flog.trace("DF_AAS_INT1",DF_AAS_INT,name="build_data_frames",capture=TRUE)
  
  searchPattern <- "\n([[:digit:] ]{10}) ([[:print:] ]{20}) ([[:print:] ]{37}) ([[:print:] ]{15}) ([[:print:] ]{10})"
  replacePattern <- "\n'\\1' '\\2' '\\3' '\\4' '\\5' "
  DF_IO_WAIT_HIST_INT <- getSection(theFile,'IO-WAIT-HISTOGRAM',computedDecSep,searchPattern,replacePattern)
  DF_IO_WAIT_HIST_INT$SNAP_ID <- as.numeric(DF_IO_WAIT_HIST_INT$SNAP_ID)
  DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI <- as.numeric(DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI)
  DF_IO_WAIT_HIST_INT$WAIT_COUNT <- as.numeric(DF_IO_WAIT_HIST_INT$WAIT_COUNT)
  DF_IO_BY_OBJECT_TYPE_INT <- getSection(theFile,'IO-OBJECT-TYPE',computedDecSep)
  DF_SQL_SUMMARY_INT <- getSection(theFile,'TOP-SQL-SUMMARY',computedDecSep)
  
  if(("MODULE" %in% names(DF_SQL_SUMMARY_INT))){
    DF_SQL_SUMMARY_INT$MODULE <- str_sub(DF_SQL_SUMMARY_INT$MODULE,1,10)
  }
  else{
    DF_SQL_SUMMARY_INT$MODULE <- NA
  }
  
  
  
  tryCatch(  DF_SQL_SUMMARY_INT[with(DF_SQL_SUMMARY_INT, grepl("PL/SQLEXECUTE", COMMAND_NAME)),]$COMMAND_NAME<-"PL/SQL",
             #DF_ATTRIBUTES_INT <- subset(DF_ATTRIBUTES_INT, db == main$current_db_name),
             error = function(e) {
               #traceback()
               #browser()
               
             }
  )
  
  
  DF_SQL_BY_SNAPID_INT <- getSection(theFile,'TOP-SQL-BY-SNAPID',computedDecSep)
  rm(theFile)
  
  
  # Normalize dates by snap_id
  
  
  options(tz="")
  
  DF_SNAP_ID_DATE_INT <- ddply(DF_MAIN_INT, .(snap), summarise, 
                               end=min(as.POSIXct(end,format="%y/%m/%d %H:%M",tz="UTC")))
  
  DF_SNAP_ID_DATE_INT <- data.table(DF_SNAP_ID_DATE_INT)
  
  #names(DF_SNAP_ID_DATE_INT)[names(DF_SNAP_ID_DATE_INT)=="snap"] <- "SNAP_ID"
  
  setnames(DF_SNAP_ID_DATE_INT,"snap","SNAP_ID")
  DF_SNAP_ID_DATE_INT$SNAP_ID <- as.numeric(DF_SNAP_ID_DATE_INT$SNAP_ID)
  # Tyler just added to fix issue with hours_bars
  #DF_MAIN_INT$end <- as.POSIXct(strptime(DF_MAIN_INT$end,format="%y/%m/%d %H:%M",tz=""),tz="")
  #DF_MAIN_INT$end <- as.POSIXct(strptime(DF_MAIN_INT$end,format="%y/%m/%d %H:%M",tz=""),tz="")
  DF_MAIN_INT$end <- as.POSIXct(DF_MAIN_INT$end,format="%y/%m/%d %H:%M",tz="UTC")
  
  # load / generate attributes (attributes.csv) so we can use them to filter
  
  main$current_plot_attributes <<- load_plot_attributes()
  if(length(main$current_plot_attributes)<2){
    main$current_plot_attributes <- generate_plot_attributes(DF_SNAP_ID_DATE_INT)
  }
  apply_current_attributes()
  flog.trace(paste0('Snaps:',attr$filter_snap_min,' - ',attr$filter_snap_max))
  attr$filter_snap_min <- as.numeric(attr$filter_snap_min)
  attr$filter_snap_max <- as.numeric(attr$filter_snap_max)
  
  
  setkey(DF_SNAP_ID_DATE_INT,SNAP_ID,end)
  setkey(DF_AAS_INT,SNAP_ID)
  setkey(DF_MAIN_INT,snap,end)
  
  
  filter_n_days <- function(DF_IN){
    #    filter_snap_min 
    flog.trace(str(attr$filter_snap_min),name="build_data_frames")
    flog.trace(str(attr$filter_snap_max),name="build_data_frames")
    if(inherits(DF_IN,what='data.table')){
      flog.trace('Its a data.table',name="build_data_frames")
      
      DF_IN_TMP <- DF_IN
      setkey(DF_IN_TMP,SNAP_ID)
      flog.trace(nrow(DF_IN_TMP),name="build_data_frames")
      
      DF_IN_TMP <- DF_IN_TMP[SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max]
      flog.trace(nrow(DF_IN_TMP),name="build_data_frames")
      return(DF_IN_TMP)
    }
    else{
      flog.trace('Its a data.frame',name="build_data_frames")
      return(subset(DF_IN, SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max))  
    }
    
  }
  
  
  
  DF_SNAP_ID_DATE_INT<-filter_n_days(DF_SNAP_ID_DATE_INT)
  #DF_SNAP_ID_DATE_INT <- subset(DF_SNAP_ID_DATE_INT, SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max)
  
  numDaysTotal <- difftime(max(DF_MAIN_INT$end), min(DF_MAIN_INT$end), unit="days")
  DF_MAIN_INT <- subset(DF_MAIN_INT, snap >= attr$filter_snap_min & snap <= attr$filter_snap_max)
  numDaysFiltered <- difftime(max(DF_MAIN_INT$end), min(DF_MAIN_INT$end), unit="days")
  DF_OS_INT <- rbind(DF_OS_INT,data.frame(STAT_NAME='DAYS',STAT_VALUE=round(numDaysTotal,1)))
  DF_OS_INT <- rbind(DF_OS_INT,data.frame(STAT_NAME='DAYS_FILTERED',STAT_VALUE=round(numDaysFiltered,1)))
  
  flog.trace("DF_AAS_INT2.1",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  
  #   DF_AAS_INT<-data.frame(DF_AAS_INT)
  DF_AAS_INT<-filter_n_days(DF_AAS_INT)
  flog.trace("DF_AAS_INT2.2",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  DF_MEMORY_INT<-filter_n_days(DF_MEMORY_INT)
  DF_SQL_BY_SNAPID_INT<-filter_n_days(DF_SQL_BY_SNAPID_INT)
  
  flog.trace("DF_SNAP_ID_DATE_INT1",head(DF_SNAP_ID_DATE_INT),name="build_data_frames",capture=TRUE)
  flog.trace("DF_AAS_INT2.3",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  
  flog.trace("DF_SNAP_ID_DATE_INT1",str(DF_SNAP_ID_DATE_INT),name="build_data_frames",capture=TRUE)
  flog.trace("DF_AAS_INT2.3",str(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  
  #DF_AAS_INT<-data.frame(DF_AAS_INT)
  #DF_SNAP_ID_DATE_INT<-data.frame(DF_SNAP_ID_DATE_INT)
  flog.trace("DF_AAS_INT2.4",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  DF_AAS_INT <- merge(DF_AAS_INT,DF_SNAP_ID_DATE_INT)
  DF_MEMORY_INT <- merge(DF_MEMORY_INT,DF_SNAP_ID_DATE_INT)
  DF_SQL_BY_SNAPID_INT <- merge(DF_SQL_BY_SNAPID_INT,DF_SNAP_ID_DATE_INT)
  #print(head(DF_AAS_INT))
  
  flog.trace("DF_AAS_INT3",DF_AAS_INT,name="build_data_frames",capture=TRUE)
  DF_AAS_INT[with(DF_AAS_INT, grepl("DB CPU", WAIT_CLASS)),]$WAIT_CLASS<-"CPU"
  # due to a bug in the 2.7 sql script
  #DF_AAS_INT[with(DF_AAS_INT, grepl("Administrati", WAIT_CLASS)),]$WAIT_CLASS<-"Administrative"
  
  #min_snap_id <- min(subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))$snap)
  #DF_MAIN_INT <- subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))
  # tyler changed for Pearson
  
  #flog.debug(summary(DF_AAS_INT))
  
  #print(head(DF_OS_INT))
  flog.debug("build_data_frames - end",name="build_data_frames")
  return(list(DF_OS_INT,DF_MAIN_INT,DF_MEMORY_INT,DF_SPACE_INT,DF_AAS_INT,DF_SQL_SUMMARY_INT,DF_SQL_BY_SNAPID_INT,DF_SNAP_ID_DATE_INT,DF_IO_WAIT_HIST_INT,DF_DB_PARAMETERS_INT,DF_IO_BY_OBJECT_TYPE_INT))
}



setwd("M:/Dropbox/MyFiles/Accounts/Unknown/andy.colvin")
build_data_frame()