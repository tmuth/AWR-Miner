library(futile.logger)
#====================================================================================================================
# Change Variables in this sections as needed 

#WORK_DIR <- 'E:/Portable-AWR-Miner'
#setwd(WORK_DIR)
#WORK_DIR <- 'E:/Portable-AWR-Miner/CSVs/LAD-Cust-Help'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/NC/Public Schools/Pearson/Pearson-POV/Results/Test-H/CSVs'

#filePattern <- "^awr-hist*.*(\\.out|\\.gz)$"
filePattern <- "^awr-hist.+P01.+(\\.out|\\.gz)$"


MAX_DAYS <- 30


flog.threshold(TRACE) #TRACE,DEBUG,INFO, WARN, ERROR, FATAL
#====================================================================================================================

library(plyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(reshape)
library(xtable)
library(ggthemes)
library(stringr)

main <- new.env()

attr <- new.env()
attr$filter_snap_min <- 1
attr$filter_snap_max <- 1000000000
attr$vertical_line <- theme()
attr$vertical_text <- theme()

main$awrFiles <- list.files(pattern=filePattern)

log_it.info <- function(x){
  #flog.info(x, name=main$current_db_name)
  flog.info(x)
  #print(x)
}

log_it.warn <- function(x){
  flog.warn(x, name=main$current_db_name)
  #print(x)
}

log_it.error <- function(x){
  flog.error(x, name=main$current_db_name)
  flog.error(x)
  #print(x)
}

log_it.debug <- function(x){
  #flog.debug(x, name=main$current_db_name)
  flog.debug(x)
  #print(x)
}


log_it.trace <- function(x){
  flog.trace(x, name=main$current_db_name)
  flog.trace(x)
  #print(x)
}


main$db_name = vector()
main$db_id = vector()

get_db_names <- function(){
  log_it.debug("get_db_names - start")
  
  namePattern <- "awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-.*"
  for (f in main$awrFiles) {
    #main$db_name <- c(main$db_name,gsub(pattern = "([a-zA-Z0-9_]+)-.*", replacement="\\1", f))
    main$db_name <- c(main$db_name,gsub(pattern = namePattern, replacement="\\2", f))
    main$db_id <- c(main$db_id,gsub(pattern = namePattern, replacement="\\2", f))
    log_it.info(paste0('Found file for: ',gsub(pattern = namePattern, replacement="\\2", f)))
    flog.trace(f)
  }
  main$db_id <- unique(main$db_id)
  main$db_name <- unique(main$db_name)
  log_it.debug("get_db_names - stop")
}


# *ATTRIBUTE FUNCTIONS* ===============================================================================================

load_plot_attributes <- function(){
  #main$attributes_file 
  DF_ATTRIBUTES_INT <- data.frame()
  log_it.debug('load_plot_attributes - start')
  if(file.exists('attributes.csv')){
    
    tryCatch(DF_ATTRIBUTES_INT <- read.csv('attributes.csv', head=TRUE,sep=",",stringsAsFactors=FALSE),
             #DF_ATTRIBUTES_INT <- subset(DF_ATTRIBUTES_INT, db == main$current_db_name),
             error = function(e) {
               #traceback()
               file.remove('attributes.csv')
               flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
               log_it.error(paste0("Error in ",main$current_db_name,": ",e))
               flog.remove(main$current_db_name)
               #browser()
               
             }
    )
  
  }
  
  log_it.debug('load_plot_attributes - end')
  DF_ATTRIBUTES_INT <- unique(DF_ATTRIBUTES_INT)
  print(head(DF_ATTRIBUTES_INT))
  return(DF_ATTRIBUTES_INT)
}
#====================================================================================================================





# *DATA LOADING AND MANIPULATION FUNCTIONS* =========================================================================


getSection <- function(inFile,blockName,searchPatternIn=NULL,replacePatternIn=NULL){
  beginBlock <- paste0('~~BEGIN-',blockName,'~~')
  endBlock <- paste0('~~END-',blockName,'~~')
  thePattern <- paste0(beginBlock,'(.*)',endBlock)
  body <- str_extract(paste(inFile, collapse='\n'), thePattern)
  
  #headerSep <- str_extract(paste(body, collapse='\n'), '\n[- ]+\n')
  
  body <- gsub('\n\n', '\n', body)
  body <- gsub('\n[- ]+\n', '\n', body)
  body <- gsub('~~.+~~\n','\n', body)
  body <- gsub('\n~~.+~~','\n', body)
  
  body <- gsub("([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2})","'\\1'", body) # find dates and enclose in quotes so it doesn't break into 2 columns
  
  if(!is.null(searchPatternIn)){
    body <- gsub(searchPatternIn,replacePatternIn, body)
  }
  #print(body)
  
  numRows <- str_count(body,'\n')-1
  #numRows <- 10
  
  dfInt = read.table(text=body,header = TRUE,skip=1,nrows=numRows,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE)
  
  #remove any rows that are actually repeating headers
  index1 <- with(dfInt, grepl("--",dfInt[,1]))
  dfInt <- dfInt[!index1,]
  
  #remove any rows that are actually repeating headers
  index2 <- with(dfInt, grepl(colnames(dfInt[1]),dfInt[,1]))
  dfInt <- dfInt[!index2,]
  #print(dfInt)
  return(dfInt)
}


build_data_frames <- function(dbid,dbname) {
  log_it.debug("build_data_frames - start")
  log_it.info(paste0("dbid-dbname: ",dbid,'-',dbname))
  #   dbid <- 1499932796
  #   dbname <- main$current_db_name
  #y<-grep("^.+651066032.+$",os_files)
  filePatternInt <- paste0("^.+",dbid,".+$")
  fileIndex<-grep(filePatternInt,main$awrFiles)
  #> os_files[y]
  theFile <- readLines(main$awrFiles[fileIndex])
  log_it.debug(length(theFile))
  
  
  DATA_FRAME_INT <- NULL
  #file_pattern=paste(WORK_DIR,paste(dbname,dbid,sep="-"),sep="/")
  DF_OS_INT <- getSection(theFile,'OS-INFORMATION')
  DF_MEMORY_INT <- getSection(theFile,'MEMORY')
  DF_SPACE_INT <- getSection(theFile,'SIZE-ON-DISK')
  DF_MAIN_INT <- getSection(theFile,'MAIN-METRICS')
  
  
  DF_DB_PARAMETERS_INT <- getSection(theFile,'DATABASE-PARAMETERS')
  
  searchPattern <- "\n([[:digit:] ]{10}) ([[:print:] ]{20}) ([[:print:] ]{10})"
  replacePattern <- "\n'\\1' '\\2' '\\3'"
  DF_AAS_INT <- getSection(theFile,'AVERAGE-ACTIVE-SESSIONS',searchPattern,replacePattern)

  
  searchPattern <- "\n([[:digit:] ]{10}) ([[:print:] ]{20}) ([[:print:] ]{37}) ([[:print:] ]{15}) ([[:print:] ]{10})"
  replacePattern <- "\n'\\1' '\\2' '\\3' '\\4' '\\5' "
  DF_IO_WAIT_HIST_INT <- getSection(theFile,'IO-WAIT-HISTOGRAM',searchPattern,replacePattern)
  DF_IO_BY_OBJECT_TYPE_INT <- getSection(theFile,'IO-OBJECT-TYPE')
  DF_SQL_SUMMARY_INT <- getSection(theFile,'TOP-SQL-SUMMARY')
  DF_SQL_BY_SNAPID_INT <- getSection(theFile,'TOP-SQL-BY-SNAPID')
  rm(theFile)
  
  
#   DF_OS_INT <- read.csv(paste(file_pattern,"-os.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=FALSE)
#   DF_MAIN_INT <- read.csv(paste(file_pattern,"-main.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
#   DF_MEMORY_INT <- read.csv(paste(file_pattern,"-memory.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
#   DF_SPACE_INT <- read.csv(paste(file_pattern,"-space.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
#   DF_AAS_INT <- read.csv(paste(file_pattern,"-aas.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=FALSE)
  
  #flog.trace('Summary',summary(DF_AAS_INT),capture=TRUE)
  
#   DF_SQL_SUMMARY_INT <- read.csv(paste(file_pattern,"-sql-summary.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
#   DF_SQL_BY_SNAPID_INT <- read.csv(paste(file_pattern,"-sql-by-snap.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
#   
  #DF_MAIN_INT$end <- strptime(DF_MAIN_INT$end, "%y/%m/%d %H:%M")
  #DF_MAIN_INT$end <- as.POSIXct(DF_MAIN_INT$end, format = "%y/%m/%d %H:%M")
  
  # Normalize dates by snap_id
  
  DF_SNAP_ID_DATE_INT <- ddply(DF_MAIN_INT, .(snap), summarise, 
                               end=min(as.POSIXct(end)))
  
  
  filter_n_days <- function(DF_IN){
    #    filter_snap_min 
    return(subset(DF_IN, SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max))
  }
  
  
  
  names(DF_SNAP_ID_DATE_INT)[names(DF_SNAP_ID_DATE_INT)=="snap"] <- "SNAP_ID"
  DF_SNAP_ID_DATE_INT <- subset(DF_SNAP_ID_DATE_INT, SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max)
  
  
  DF_MAIN_INT <- subset(DF_MAIN_INT, snap >= attr$filter_snap_min & snap <= attr$filter_snap_max)
  DF_AAS_INT<-filter_n_days(DF_AAS_INT)
  DF_MEMORY_INT<-filter_n_days(DF_MEMORY_INT)
  DF_SQL_BY_SNAPID_INT<-filter_n_days(DF_SQL_BY_SNAPID_INT)
  
  DF_AAS_INT <- merge(DF_AAS_INT,DF_SNAP_ID_DATE_INT)
  DF_MEMORY_INT <- merge(DF_MEMORY_INT,DF_SNAP_ID_DATE_INT)
  DF_SQL_BY_SNAPID_INT <- merge(DF_SQL_BY_SNAPID_INT,DF_SNAP_ID_DATE_INT)
  #print(head(DF_AAS_INT))
  DF_AAS_INT[with(DF_AAS_INT, grepl("DB CPU", WAIT_CLASS)),]$WAIT_CLASS<-"CPU"
  # due to a bug in the 2.7 sql script
  #DF_AAS_INT[with(DF_AAS_INT, grepl("Administrati", WAIT_CLASS)),]$WAIT_CLASS<-"Administrative"
  
  #min_snap_id <- min(subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))$snap)
  #DF_MAIN_INT <- subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))
  # tyler changed for Pearson
  
  #log_it.debug(summary(DF_AAS_INT))
  
  #print(head(DF_OS_INT))
  log_it.debug("build_data_frames - end")
  return(list(DF_OS_INT,DF_MAIN_INT,DF_MEMORY_INT,DF_SPACE_INT,DF_AAS_INT,DF_SQL_SUMMARY_INT,DF_SQL_BY_SNAPID_INT,DF_SNAP_ID_DATE_INT))
}



':=' = function(lhs, rhs) {
  frame = parent.frame()
  lhs = as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs = lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs = list(rhs)
  if (length(lhs) > length(rhs))
    rhs = c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

#====================================================================================================================




#====================================================================================================================
# MAIN LOOP STRUCTURES THAT CALL OTHER FUNCTIONs

main$overall_summary_df <- NULL
main$overall_combined_df <- NULL
main$DATA_FRAME <- NULL
main$DF_OS <- NULL
main$DF_MEMORY <- NULL
main$DF_MAIN <- NULL
main$DF_SNAP_ID_DATE <- NULL
main$DF_SNAP_ID_SUBSET <- NULL
main$DF_SNAP_ID_DATE2 <- NULL
main$gg_hour_bars <- NULL
main$cpu_cores <- NULL
main$current_db_name=""
main$attributes_file="attributes.csv"
main$plot_attributes <- NULL
main$current_plot_attributes <- NULL


main$mainFunction <- function(f){
  
  vector_element = which(main$db_id==f)
  main$current_db_name=main$db_name[which(main$db_id==f)]
  
  #flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
  
  main$DATA_FRAME <- NULL
  main$DF_OS <- NULL
  main$DF_MEMORY <- NULL
  main$DF_MAIN <- NULL
  main$DF_MAIN_BY_SNAP <- NULL
  main$current_plot_attributes <- NULL
  
  tryCatch(main$current_plot_attributes <- load_plot_attributes(), 
           error = function(e) {
             #traceback()
             flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
             log_it.error(paste0("Error in ",main$current_db_name,": ",e))
             #flog.remove(name=main$current_db_name)
             #browser()
             
           }
           #,finally=print("finished")
  )
  
  
  
  c(main$DF_OS, main$DF_MAIN,main$DF_MEMORY,main$DF_SPACE,main$DF_AAS,main$DF_SQL_SUMMARY,main$DF_SQL_BY_SNAPID,main$DF_SNAP_ID_DATE) := build_data_frames(f,main$current_db_name)
  flog.remove(main$current_db_name)
}

main$mainLoop <- function(){
  for (f in main$db_id) {
    #main$mainFunction(f)
    tryCatch(main$mainFunction(f), 
             error = function(e) {
              #traceback()
               flog.appender(appender.file(paste0(main$current_db_name,'.err')), name=main$current_db_name)
               log_it.error(paste0("Error in ",main$current_db_name,": ",e))
               flog.remove(name=main$current_db_name)
               #browser()
              
             }
             #,finally=print("finished")
    )
  }
  write.csv(main$overall_summary_df,'OverallSummary.csv')
  if(length(main$plot_attributes) > 0){
    write.csv(main$plot_attributes,'attributes.csv',row.names=FALSE)
  }
}

get_db_names()
main$mainLoop()
#====================================================================================================================
