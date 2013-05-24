#Sys.setenv(http_proxy="http://www-proxy.us.oracle.com:80")

list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})

#====================================================================================================================
# Change Variables in this sections as needed 

#WORK_DIR <- 'E:/Portable-AWR-Miner'
#setwd(WORK_DIR)


#filePatternOverride <- "^awr-hist.+DB110g.+(\\.out|\\.gz)$"
#rm(filePatternOverride)

MAX_DAYS <- 30

outFileSuffix <- '1'


#====================================================================================================================

debugMode <- FALSE
flog.threshold(INFO) #TRACE, DEBUG, INFO, WARN, ERROR, FATAL
flog.threshold(ERROR,name='getSection') #TRACE, DEBUG, INFO, WARN, ERROR, FATAL 
flog.threshold(ERROR,name='build_data_frames') #TRACE, DEBUG, INFO, WARN, ERROR, FATAL 
#name: build_data_frames, mainFunction,set_date_break_vars,plot_RAC_activity

#debugModeOverride <- TRUE  | rm(debugModeOverride)
if(exists("debugModeOverride")){
  if(!is.null(debugModeOverride)){
    if(debugModeOverride){
      debugMode <- TRUE
      print('In debug mode')
    }
    else{
      debugMode <- FALSE
    }
  }
}



awrMinerPlotVersion <- '3.0.2'

filePattern <- "^awr-hist*.*(\\.out|\\.gz)$"
if(exists("filePatternOverride")){
  if(!is.null(filePatternOverride)){
    if(nchar(filePatternOverride)>1){
      filePattern <- filePatternOverride
    }
  }
}

 
print('Looking in this directory:')
print(getwd())
print(paste0('for files that match the pattern: ',filePattern))
Sys.sleep(2)


#library(plyr)
#library(ggplot2)
#library(gridExtra)
#library(scales)
#library(reshape)
#library(xtable)
#library(ggthemes)
#library(stringr)
#library(data.table)



if(interactive()){
  #WORK_DIR <- 'E:/Portable-AWR-Miner/CSVs'
}else{
  WORK_DIR <- (commandArgs(TRUE)[1])
  setwd(WORK_DIR)
  #print("arg")
  #print(args[1])
}



main <- new.env()
awrM <- new.env()
awrM$debug <- new.env()

awrM$debug.plotSummary <- data.frame()
awrM$debug.lastFunction <- NULL

attr <- new.env()
attr$filter_snap_min <- 1
attr$filter_snap_max <- 1000000000
attr$vertical_line <- theme()
attr$vertical_text <- theme()

attr$date_break_major_var <- date_breaks("1 day")
attr$date_break_minor_var <- date_breaks("12 hour")


main$awrFiles <- list.files(pattern=filePattern)







# *UTILITY FUNCTIONS* ===============================================================================================




generate_snap_id_labels <- function(DF_SNAP_ID_DATE_INT){
  #DF_SNAP_ID_DATE_INT <- DF_SNAP_ID_DATE
  flog.debug('generate_snap_id_labels - start')
  num_dates <- as.numeric(nrow(DF_SNAP_ID_DATE_INT))
  num_snap_ids <- min(num_dates,40)
  
  mod_num <- round(num_dates/num_snap_ids)
  
  DF_SNAP_ID_SUBSET_INT <- subset(DF_SNAP_ID_DATE_INT,(SNAP_ID %% mod_num)==0,rownames=FALSE)
  flog.debug('generate_snap_id_labels - end')
  return(DF_SNAP_ID_SUBSET_INT)
}

get_os_stat <- function(SEARCH_VAL){
  #print(paste('start get_os_stat',SEARCH_VAL,sep=" - "))
  if(nrow(subset(main$DF_OS, STAT_NAME==SEARCH_VAL)) == 0){
    #print('if get_os_stat')
    return(NA)
  }
  else{
    #print('else get_os_stat')
    return(as.numeric(as.vector(main$DF_OS[with(main$DF_OS,STAT_NAME == SEARCH_VAL),]$STAT_VALUE)))
  }
}

get_os_stat_string <- function(SEARCH_VAL){
  if(nrow(subset(main$DF_OS, STAT_NAME==SEARCH_VAL)) == 0)
    return(NULL)
  else
    return(as.character(as.vector(main$DF_OS[with(main$DF_OS,STAT_NAME == SEARCH_VAL),]$STAT_VALUE)))
}


convert_snap_id_to_posixct <- function(snapID){
  theDateChar <- subset(main$DF_SNAP_ID_DATE2 , snap==snapID)
  theDateReturn <- as.POSIXct(theDateChar$end, format = "%y/%m/%d %H:%M",tz="UTC")
  return(theDateReturn)
}
#====================================================================================================================







main$db_name = vector()
main$db_id = vector()

get_db_names <- function(){
  flog.debug("get_db_names - start")

  
  namePattern <- "awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-.*"
  for (f in main$awrFiles) {
    #main$db_name <- c(main$db_name,gsub(pattern = "([a-zA-Z0-9_]+)-.*", replacement="\\1", f))
    main$db_name <- c(main$db_name,gsub(pattern = namePattern, replacement="\\2", f))
    main$db_id <- c(main$db_id,gsub(pattern = namePattern, replacement="\\2", f))
    flog.info(paste0('Found file for: ',gsub(pattern = namePattern, replacement="\\2", f)))
    flog.trace(f)
  }
  main$db_id <- unique(main$db_id)
  main$db_name <- unique(main$db_name)
  flog.debug("get_db_names - stop")
}





# *ATTRIBUTE FUNCTIONS* ===============================================================================================

myTheme <- theme_stata(scheme = "s2color") +
#myTheme <- theme_few() +
  #myTheme <- theme_bw() +
  
  theme(legend.position =    "bottom",
        #plot.margin =        unit(c(3, 3, 3, 3), "lines"),
        axis.title.y = element_text(vjust = .6),
        text =               element_text(family="sans",face = "plain",
                                          colour = "black", size = 8,
                                          hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
        axis.text.y =       element_text(angle = 0),
        panel.margin =       unit(0.25, "lines"),
        panel.grid.major = element_line(colour="#dedede", size = 0.2,linetype = "dotted"),
        panel.grid.minor = element_line(colour="#dedede", size = 0.1,linetype = "dotted"),
        axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6)
  )

theme_set(myTheme)

attr$themeScaleFill <- scale_fill_stata()
attr$themeScaleColour <- scale_colour_stata()


get_attrs <- function(SEARCH_VAL){
  if(nrow(subset(main$current_plot_attributes, variable==SEARCH_VAL)) == 0){
    return(NA)
  }
  else{
    return(main$current_plot_attributes[with(main$current_plot_attributes,variable==SEARCH_VAL),])
  }
}

load_plot_attributes <- function(){
  #main$attributes_file 
  DF_ATTRIBUTES_INT <- data.frame()
  flog.debug('load_plot_attributes - start')
  if(file.exists('attributes.csv')){
    
    tryCatch(DF_ATTRIBUTES_INT <- read.csv('attributes.csv', head=TRUE,sep=",",stringsAsFactors=FALSE),
             #DF_ATTRIBUTES_INT <- subset(DF_ATTRIBUTES_INT, db == main$current_db_name),
             error = function(e) {
               #traceback()
               file.remove('attributes.csv')
               flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
               flog.error(paste0("Error in ",main$current_db_name,": ",e))
               flog.remove(main$current_db_name)
               #browser()
               
             }
    )
  
  }
  
  flog.debug('load_plot_attributes - end')
  DF_ATTRIBUTES_INT <- unique(DF_ATTRIBUTES_INT)
  flog.trace(head(DF_ATTRIBUTES_INT),capture=TRUE)
  return(DF_ATTRIBUTES_INT)
}


generate_plot_attributes <- function(DF_SNAP_ID_DATE_INT){
  
  flog.debug('generate_plot_attributes - start',name='generate_plot_attributes')
  
  df_plot_attr_int <- NULL
  
  add_df_row <- function(df,attr,val1,val2){
    df<-rbind(df,data.frame(db=main$current_db_name,variable=attr,value1=val1,value2=val2))
    return(df)
  }
  
  
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'$date_range',format(min(DF_SNAP_ID_DATE_INT$end),"%Y-%m-%d %H:%M:%S"),format(max(DF_SNAP_ID_DATE_INT$end),"%Y-%m-%d %H:%M:%S"))
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'$snap_id_range',as.character(min(DF_SNAP_ID_DATE_INT$SNAP_ID)),as.character(max(DF_SNAP_ID_DATE_INT$SNAP_ID)))
  #df_plot_attr_int <- add_df_row(df_plot_attr_int,'date_filter','','')
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'snap_id_filter','','')
  
  #df_plot_attr_int <- add_df_row(df_plot_attr_int,'comments','','')
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'annotated_line','','')
  #print(head(df_plot_attr_int))
  
  flog.trace('df_plot_attr_int',head(df_plot_attr_int),name='generate_plot_attributes')
  flog.debug('generate_plot_attributes - end',name='generate_plot_attributes')
  return(df_plot_attr_int)
}




apply_current_attributes <- function(){
  flog.debug('apply_current_attributes - start',name='apply_current_attributes')
  ## Filter by snap ID
  if(nrow(main$current_plot_attributes) > 0){
    DF_TEMP <- NULL
    DF_TEMP <- get_attrs('snap_id_filter')
   # print(head(DF_TEMP))
    if(nchar(as.character(DF_TEMP[1,]$value1))>1){
      if(nchar(as.character(DF_TEMP[1,]$value1))>1){
        #print(head(as.vector(DF_TEMP$value1)))
        attr$filter_snap_min <<- as.vector(DF_TEMP$value1) }
      if(nchar(as.character(DF_TEMP[1,]$value2))>1){
        attr$filter_snap_max <<- as.vector(DF_TEMP$value2) }
      #attr$filter_snap_max <- as.vector(DF_TEMP$value2)
    }
  }
  
  
  ## Filter by date
  
  #attr$filter_snap_min <- 220
  #attr$filter_snap_max <- 228
  flog.debug('apply_current_attributes - end',name='apply_current_attributes')
  return(TRUE)
}

set_date_break_vars <- function(DF_MAIN_IN){
  flog.debug('set_date_break_vars - start',name='set_date_break_vars')
  #numDays <- as.numeric(max(DF_MAIN_IN$end)-min(DF_MAIN_IN$end))
  numDays <- difftime(max(main$DF_MAIN$end), min(main$DF_MAIN$end), unit="days")
  
  flog.trace(paste0('numDays: ',numDays),name='set_date_break_vars')
  
  if(numDays > 2){
    attr$date_break_major_var <<- date_breaks("1 day")
    attr$date_break_minor_var <<- date_breaks("12 hour")
  }
  else{
    attr$date_break_major_var <<- date_breaks("1 hour")
    attr$date_break_minor_var <<- date_breaks("1 hour")
  }
  
  flog.debug('set_date_break_vars - end',name='set_date_break_vars')
  
  return(TRUE)
  
}
#DF_ATTR <- load_plot_attributes()
#DF_ATTR <- generate_plot_attributes()
#====================================================================================================================





# *DATA LOADING AND MANIPULATION FUNCTIONS* =========================================================================


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



build_data_frames <- function(dbid,dbname) {
  flog.debug("build_data_frames - start",name="build_data_frames")
  flog.info(paste0("dbid-dbname: ",dbid,'-',dbname),name="build_data_frames")
  #   dbid <- 1499932796
  #   dbname <- main$current_db_name
  #y<-grep("^.+651066032.+$",os_files)
  filePatternInt <- paste0("^.+",dbid,".+$")
  fileIndex<-grep(filePatternInt,main$awrFiles)
  #> os_files[y]
  theFile <- readLines(main$awrFiles[fileIndex])
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
  
  DF_SQL_SUMMARY_INT[with(DF_SQL_SUMMARY_INT, grepl("PL/SQLEXECUTE", COMMAND_NAME)),]$COMMAND_NAME<-"PL/SQL"
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



summarise_dfs_by_snap <- function(){
  flog.debug('summarise_dfs_by_snap - start',name='summarise_dfs_by_snap')
  DF_MAIN_BY_SNAP_INT <- ddply(main$DF_MAIN, .(end), summarise, 
                               cpu=max(os_cpu_max),
                               read_iops=sum(read_iops),
                               read_iops_max=sum(read_iops_max),
                               write_iops=sum(write_iops),
                               write_iops_max=sum(write_iops_max),
                               read_mb_s=sum(read_mb_s),
                               read_mb_s_max=sum(read_mb_s_max),
                               write_mb_s=sum(write_mb_s),
                               write_mb_s_max=sum(write_mb_s_max),
                               aas=sum(aas),
                               logons_total=sum(logons_total),
                               exec_s=sum(exec_s),
                               sql_res_t_cs=sum(sql_res_t_cs)
  )
  flog.debug('summarise_dfs_by_snap - end',name='summarise_dfs_by_snap')
  return(list(DF_MAIN_BY_SNAP_INT))
}


build_snap_to_date_df <- function(){
  DF_SNAP_ID_DATE_INT <- ddply(main$DF_MAIN, .(snap), summarise, 
                               end=min(as.POSIXct(end,tz="UTC")),
                               aas=sum(aas)) 
  DF_SNAP_ID_DATE_INT$end <- format(DF_SNAP_ID_DATE_INT$end,"%y/%m/%d %H:%M")
  return(DF_SNAP_ID_DATE_INT)
}

gen_summary_data <- function(){
  flog.debug('gen_summary_data - start')
  ptile <- 0.95
  #MAX_MEM <- as.vector(quantile(main$DF_MEMORY$TOTAL,probs=c(ptile),type=4))
  
  
  DF_MEM_TOTALS <- ddply(main$DF_MEMORY, .(SNAP_ID), summarise, 
                         SGA=sum(SGA), PGA=sum(PGA),TOTAL=sum(TOTAL))
  
  
  DF_MAIN_INT_SUM1 <- ddply(main$DF_MAIN,.(snap), summarise,
                            cpu    = max(os_cpu_max),
                            read_iops = sum(read_iops_max), write_iops = sum(write_iops_max),
                            read_mb_s = sum(read_mb_s_max), write_mb_s = sum(write_mb_s_max),
                            logons_total=sum(logons_total),exec_s=sum(exec_s),commits_s=sum(commits_s),
                            aas=sum(aas))
  
  DF_MAIN_INT_SUM <- ddply(DF_MAIN_INT_SUM1,.(), summarise,
                           cpu    = max(cpu),
                           r_iops = max(read_iops), w_iops = max(write_iops),
                           r_mb_s = max(read_mb_s), w_mb_s = max(write_mb_s),
                           logons_total = max(logons_total), exec_s = max(exec_s),
                           commits_s = max(commits_s), aas = max(aas)
  )
  
  # round the whole data frame
  numVars <- sapply(DF_MAIN_INT_SUM, is.numeric)
  DF_MAIN_INT_SUM[numVars] <- lapply(DF_MAIN_INT_SUM[numVars], round, digits = 0)
  
  num_instances <- as.numeric(as.vector(get_os_stat("INSTANCES")))
  flog.trace(paste('instances: ',num_instances,sep=''))
  #DF_OS_INT <- data.frame(name=main$current_db_name,nodes=num_instances)
  DF_OS_INT <- data.frame(name=main$current_db_name,nodes=num_instances,
                          platform=get_os_stat_string("PLATFORM_NAME"),version=get_os_stat_string("VERSION"),
                          sockets=ifelse(is.na(get_os_stat("NUM_CPU_SOCKETS")) == TRUE, NA, get_os_stat("NUM_CPU_SOCKETS")*num_instances),
                          #sockets=(get_os_stat("NUM_CPU_SOCKETS")*get_os_stat("INSTANCES")),
                          #sockets=(get_os_stat("NUM_CPU_SOCKETS")*get_os_stat("INSTANCES")),
                          cores=(get_os_stat("NUM_CPU_CORES")*num_instances),
                          threads=(get_os_stat("NUM_CPUS")*num_instances),
                          mem=(get_os_stat("PHYSICAL_MEMORY_GB") * num_instances),
                          days=get_os_stat("DAYS"),
                          "days shown"=get_os_stat("DAYS_FILTERED"),
                          AWR.Miner.Capture=get_os_stat_string("AWR_MINER_VER"),
                          AWR.Miner.Graph=awrMinerPlotVersion
  )
  
  DF_OTHER_INT <- data.frame(
    sga=as.numeric(as.vector(max(DF_MEM_TOTALS$SGA))),pga=as.numeric(as.vector(max(DF_MEM_TOTALS$PGA))),
    memused=as.numeric(as.vector(max(DF_MEM_TOTALS$TOTAL))),
    sizegb=as.numeric(as.vector(max(main$DF_SPACE$SIZE_GB))))
  # round the whole data frame
  numVars <- sapply(DF_OTHER_INT, is.numeric)
  DF_OTHER_INT[numVars] <- lapply(DF_OTHER_INT[numVars], round, digits = 0)
  
  
  DF_SUM_COMBINED_INT <- merge(DF_MAIN_INT_SUM,DF_OTHER_INT)
  DF_SUM_COMBINED_INT$.id <- NULL
  DF_OS_INT$.id <- NULL
  
  DF_HOSTS <- data.frame(hosts=get_os_stat_string("HOSTS"))
  DF_SUM_COMBINED_INT2 <- merge(DF_OS_INT,DF_SUM_COMBINED_INT)
  DF_SUM_COMBINED_INT2 <- merge(DF_SUM_COMBINED_INT2,DF_HOSTS)
  
  DF_SUM_COMBINED_INT$.id <- NULL
  
  #df_temp <- summary_df_iostat_by_time_int
  #summary_df_iostat_by_time_int[-1] <- round(df_temp[-1],1) # round all but the 1st column
  #remove(df_temp)
  
  #overall_summary_df <- rbind(overall_summary_df, summary_df_totals)
  #overall_combined_df <- rbind(overall_combined_df, summary_df_cluster_by_snap_id)
  flog.debug('gen_summary_data - end')
  return(list(DF_OS_INT,DF_SUM_COMBINED_INT,DF_SUM_COMBINED_INT2))
  
}

#====================================================================================================================




# *PLOT THEME AND APPEARANCE*========================================================================================



add_vetical_lines <- function(){
  flog.debug("add_vetical_lines - start",name='add_vetical_lines')
  if(nrow(main$current_plot_attributes) > 0){
    DF_TEMP <- NULL
    DF_TEMP <- get_attrs('annotated_line')
    V_VERT_VECT <- numeric(0)
    DF_VERT_TEXT <- data.frame()
    for (i in 1:nrow(DF_TEMP)){
      if(length(DF_TEMP[i,]$value1)>0){
        
        theDate <- DF_TEMP[i,]$value1
        theNumbers <- grepl("^[[:digit:]]+$", theDate) 
        #print(theNumbers)
        #if(as.numeric(theDate) > 0){
        if(theNumbers){
          theDate <- convert_snap_id_to_posixct(theDate)
        }
        else{
          theDate <- as.POSIXct(theDate, format = "%y/%m/%d %H:%M")
        }
        
        V_VERT_VECT <- c(V_VERT_VECT, theDate)
        
        if(grepl("^[[:alnum:]].+$", DF_TEMP[i,]$value2)){
          DF_VERT_TEXT <- rbind(DF_VERT_TEXT,data.frame(end=theDate,label=DF_TEMP[i,]$value2))
        }
      }
    }
    
    
    if(length(V_VERT_VECT)>0){
      attr$vertical_line <- geom_vline(xintercept=V_VERT_VECT, linetype="dotted",color="#00422D",size=0.3,alpha=0.7) 
    }
    
    if(nrow(DF_VERT_TEXT)>0){
      attr$vertical_text <- geom_text(aes(x=end,label=label,y=0), data=DF_VERT_TEXT,angle=90,size=1.5,hjust=0,vjust=-0.2,alpha=0.7,color="#00422D")
    }
    
  }
  flog.debug("add_vetical_lines - end",name='add_vetical_lines')
}



generate_hours_bars <- function(DF_MAIN_INT){
  flog.debug('generate_hours_bars - start')
  #DF_MAIN_INT <- subset(main$DF_MAIN, snap >= 48472 & snap <= 49900)
  #DF_MAIN_INT <- main$DF_MAIN
  #flog.trace("DF_MAIN_INT:",DF_MAIN_INT$end,capture=TRUE)
  
  #flog.trace("MAIN:",DF_MAIN_INT$end,capture=TRUE)
  #DF_NIGHT_HOURS_INT <- data.frame("end"= unique(as.POSIXlt(strptime(format(DF_MAIN_INT$end,"%y/%m/%d"),format="%y/%m/%d"))))
  DF_NIGHT_HOURS_INT <- data.frame("end"= unique(as.POSIXct(as.POSIXlt(strftime(DF_MAIN_INT$end,format="%Y/%m/%d")),tz="UTC")))
  #DF_NIGHT_HOURS_INT <- data.frame("end"= unique(strptime(strftime(DF_MAIN_INT$end,format="%y/%m/%d"),format="%y/%m/%d")))
  flog.trace('yep')
  #flog.trace("hours:",DF_NIGHT_HOURS_INT,capture=TRUE)
  DF_NIGHT_HOURS_INT_A <- DF_NIGHT_HOURS_INT
  DF_NIGHT_HOURS_INT_A$work_start <- DF_NIGHT_HOURS_INT_A$end + 1
  DF_NIGHT_HOURS_INT_A$work_stop <- DF_NIGHT_HOURS_INT_A$work_start + ((3600*7)-1)
  #flog.trace("DF_NIGHT_HOURS_INT_A:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  #flog.trace("DF_NIGHT_HOURS_INT_A:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  
  DF_NIGHT_HOURS_INT_B <- DF_NIGHT_HOURS_INT
  DF_NIGHT_HOURS_INT_B$work_start <- DF_NIGHT_HOURS_INT_B$end + (3600*19)
  DF_NIGHT_HOURS_INT_B$work_stop <- DF_NIGHT_HOURS_INT_B$end + ((3600*24)-1)
  
  #flog.trace("DF_NIGHT_HOURS_INT_B:",DF_NIGHT_HOURS_INT_B,capture=TRUE)
  
  DF_NIGHT_HOURS_INT_A <- rbind(DF_NIGHT_HOURS_INT_A, DF_NIGHT_HOURS_INT_B)
  
  # find date ranges outside of the filtered min and max and REMOVE them
  idx_date_min_rm <- !with(DF_NIGHT_HOURS_INT_A, work_stop < min(main$DF_SNAP_ID_DATE$end))
  #flog.trace("main$DF_SNAP_ID_DATE$end:",main$DF_SNAP_ID_DATE$end,capture=TRUE)
  #flog.trace("idx_date_min_rm1:",idx_date_min_rm,capture=TRUE)
  DF_NIGHT_HOURS_INT_A_BACK <- DF_NIGHT_HOURS_INT_A
  DF_NIGHT_HOURS_INT_A<- DF_NIGHT_HOURS_INT_A[idx_date_min_rm,]
  #flog.trace("DF_NIGHT_HOURS_INT_A1:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  idx_date_max_rm <- !with(DF_NIGHT_HOURS_INT_A, work_start > max(main$DF_SNAP_ID_DATE$end))
  #flog.trace("idx_date_max_rm:",idx_date_max_rm,capture=TRUE)
  DF_NIGHT_HOURS_INT_A<- DF_NIGHT_HOURS_INT_A[idx_date_max_rm,]
  #flog.trace("DF_NIGHT_HOURS_INT_A:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  
  # find remaining date ranges outside of the filtered min and max and CHANGE them
  idx_date_min <- with(DF_NIGHT_HOURS_INT_A, work_start < min(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A$work_start[idx_date_min] <- min(main$DF_SNAP_ID_DATE$end)
  idx_date_max <- with(DF_NIGHT_HOURS_INT_A, work_stop > max(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A$work_stop[idx_date_max] <- max(main$DF_SNAP_ID_DATE$end)
  
  DF_NIGHT_HOURS_INT_A$value <- 0
  
  hour_bars <- geom_rect(data=DF_NIGHT_HOURS_INT_A,aes(xmin=work_start,xmax=work_stop,ymin=-Inf,ymax=Inf),alpha=0.2,fill="#bdd1e6")
  flog.debug('generate_hours_bars - end')
  return(hour_bars)
}


#====================================================================================================================


# *PLOTS* ===========================================================================================================


plot_summary_boxplot_main <- function(){
  flog.debug('plot_summary_boxplot_main - start')
  x.melt <- melt(main$DF_MAIN_BY_SNAP, id.var = c("end"), measure.var = c("cpu","read_iops_max", "write_iops_max",
                                                                          "read_mb_s_max","write_mb_s_max","aas","logons_total","exec_s","sql_res_t_cs"))
  # add a "stat" column so we can facet by stat for avg-max
  #   x.melt$stat <- "Avg"  
  #   idx_max <- with(x.melt, grepl("max", variable))
  #   x.melt[idx_max,]$stat <- "Max"
  
  x.melt$value <- round(x.melt$value,2)
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  x.melt[with(x.melt, grepl("cpu", variable)),]$variable<-"CPU"
  x.melt[with(x.melt, grepl("read_iops", variable)),]$variable<-"Read IOPs"
  x.melt[with(x.melt, grepl("write_iops", variable)),]$variable<-"Write IOPs"
  x.melt[with(x.melt, grepl("read_mb_s", variable)),]$variable<-"Read MB/s"
  x.melt[with(x.melt, grepl("write_mb_s", variable)),]$variable<-"Write MB/s"
  x.melt[with(x.melt, grepl("aas", variable)),]$variable<-"Avg Act Sessions"
  x.melt[with(x.melt, grepl("logons_total", variable)),]$variable<-"Sessions"
  x.melt[with(x.melt, grepl("exec_s", variable)),]$variable<-"Execs/s"
  x.melt[with(x.melt, grepl("sql_res_t_cs", variable)),]$variable<-"SQL Resp Time (cs)"
  x.melt$variable <- factor(x.melt$variable)
  x.melt$id <- 1
  
  median_vals <- ddply(x.melt,.(variable),summarise, value=median(value))
  mean_vals <- ddply(x.melt,.(variable),summarise,value=mean(value))
  quant_low <- ddply(x.melt,.(variable),summarise,value=quantile(value,0.25,na.rm=TRUE))
  quant_high <- ddply(x.melt,.(variable),summarise,value=quantile(value,0.75,na.rm=TRUE))
  
  summary_vals <- rbind(quant_low,quant_high)
  summary_vals$id <- 1
  mean_vals$id <- 1
  median_vals$id <- 1
  
  
  #print(summary(x.melt))
  #print(head(x.melt))
  
  
  p <- ggplot(data=x.melt, aes(x=id, y=value),aes(fill=variable),position="dodge")+
    geom_violin(aes(),fill="#4DAF4A",colour="#000000",size=0.05,alpha=0.6,adjust=0.5) +
    geom_boxplot(aes(),colour="#000000",alpha=.6,show_guide=FALSE,notch = FALSE,outlier.colour = "orange", outlier.size = 1,outlier.alpha=.4,outlier.shape=5)+
    attr$themeScaleColour+attr$themeScaleFill+
    geom_jitter(alpha=.3,size=1,position = position_jitter(width = .2,height=0),aes(colour="gray"))+
    geom_text(data=median_vals,aes(y=value,label=round(value,1)),alpha=0.8,size=2,vjust=-0.8,hjust=0)+
    geom_text(data=summary_vals,aes(y=value,label=round(value,1)),alpha=0.8,size=2,vjust=-0.8,hjust=0,colour="#666666")+
    geom_text(data=mean_vals,aes(y=value,label=round(value,1)),alpha=0.8,size=2,vjust=-0.8,hjust=1.2,colour="#D62728")+
    facet_wrap( ~ variable,scales="free",nrow=1)+
    stat_summary(fun.y=mean, geom="point", shape=5, size=2,alpha=0.7,colour="#D62728")+
    #scale_colour_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+
    #scale_y_continuous(breaks=seq(0, 8000, 500),minor=seq(0, 8000, 100))+
    theme(text =               element_text(size=5),
          axis.title.x  = element_blank(),axis.title.y  = element_blank(),
          legend.position="none",axis.text.x= element_blank(),
          plot.background = element_rect(fill = "#8EB3BD"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(colour="#436974"),axis.text.y = element_text(colour="#436974"),
          strip.background = element_rect(colour="#436974", linetype=1),
          line = element_line(colour = "#436974", linetype=1))
  
  
  #print(p)
  flog.debug('plot_summary_boxplot_main - end')
  return(p)
}






plot_aas_chart <- function(DF_AAS_INT){
  flog.debug('plot_aas_chart - start',name='plot_aas_chart')
#   log_it('plot_aas_chart - start')
#   log_it(min(DF_AAS_INT$end))
#   log_it(max(DF_AAS_INT$end))
#   
#   log_it(min(main$DF_SNAP_ID_SUBSET$end))
#   log_it(max(main$DF_SNAP_ID_SUBSET$end))
  #browser()
  #DF_AAS_INT <- DF_AAS
  
  #print(unique(main$DF_AAS$WAIT_CLASS))
  
  DF_AAS_INT$WAIT_CLASS <- str_trim(DF_AAS_INT$WAIT_CLASS)
  DF_AAS_INT$WAIT_CLASS <- factor(DF_AAS_INT$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
  
  
  vals <- expand.grid(end = unique(DF_AAS_INT$end),
                      WAIT_CLASS = unique(DF_AAS_INT$WAIT_CLASS))
  
  DF_AAS_INT <- merge(vals,DF_AAS_INT)
  rm(vals)
  #print(is.na(DF_AAS_INT))
  #print(head(DF_AAS_INT))
  
  #DF_AAS_INT[is.na(DF_AAS_INT)] <- 0
  #df[df$column_name,]
  #DF_AAS_INT <- DF_AAS_INT[DF_AAS_INT$end,]
  DF_AAS_INT <- subset(DF_AAS_INT,end != TRUE)
  DF_AAS_INT <- subset(DF_AAS_INT,end != FALSE)
  DF_AAS_INT$AVG_SESS[is.na(DF_AAS_INT$AVG_SESS)] <- 0
  #print(head(is.na(DF_AAS_INT)))
  
  
  aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                  "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                  "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                  "System I/O" = "#0993de","User I/O" = "#054ae1")
  
  gg_aas_colors <- scale_fill_manual("", values = aas_colors)
  
  
  DF_AAS_SUM_INT <- ddply(DF_AAS_INT, .(SNAP_ID,end), summarise,AVG_SESS=sum(AVG_SESS))
  
  # get the max vals for each day
  max_vals <- ddply(DF_AAS_SUM_INT, .(format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)
  
  sess_quantile <- quantile(DF_AAS_SUM_INT$AVG_SESS,0.99)
  cpu_cores_line <- geom_hline(yintercept=as.numeric(main$cpu_cores), linetype="dotted",color="red",size=0.7,alpha=0.4)
  df_cpu_cores_label <- data.frame(end=min(min(DF_AAS_INT$end)),AVG_SESS=main$cpu_cores)
  
  ymax <- max(c(main$cpu_cores+(main$cpu_cores*0.2),(max(DF_AAS_SUM_INT$AVG_SESS)+(max(DF_AAS_SUM_INT$AVG_SESS)*0.2))))
  
  plot_aas_wait_class <- ggplot()+
    geom_area(data=DF_AAS_INT, aes(x = end, y = AVG_SESS,
                                   fill = WAIT_CLASS),stat = "identity", position = "stack",alpha=.95)+
    gg_aas_colors+
    theme(axis.title.x  = element_blank(),axis.title.y  = element_blank(),
          legend.key.size =    unit(0.6, "lines"))+
    labs(title=paste("Average Active Sessions (AAS) for ",main$current_db_name,sep=""))+
    main$gg_hour_bars+
    geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5)+
    geom_text(data=main$DF_SNAP_ID_SUBSET,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,alpha=0.2,hjust=-0.1,vjust=0.7)+                                     
    geom_point(data=main$DF_SNAP_ID_SUBSET,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0)+
    cpu_cores_line+
    attr$vertical_line + attr$vertical_text +
    geom_text(data=df_cpu_cores_label, aes(x=end, y=AVG_SESS,label=paste0("CPU Cores - ",main$cpu_cores)),size=2, vjust=-.8, hjust=.5,color="red",alpha=0.4)+
    #scale_x_datetime(breaks=NULL)+
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_AAS_INT$end),max(DF_AAS_INT$end)))
  #                                      scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 days"),
  #                                                       minor_breaks = date_breaks("12 hour"),
  #                                                       limits = c(min(DF_AAS_INT$end),max(DF_AAS_INT$end))
  #                                                       )
  #ylim(-(ymax*0.025),ymax)
  
  
  plot_aas_wait_class_gt <- ggplot_gtable(ggplot_build(plot_aas_wait_class))
  
  sess_quantile2 <- quantile(DF_AAS_INT$AVG_SESS,probs=c(0.95),type=4)
  
  DF_AAS_INT2 <- subset(DF_AAS_INT,AVG_SESS >= sess_quantile2[[1]],rownames=FALSE,stringsasfactors=TRUE)
  DF_AAS_INT2$WAIT_CLASS <- factor(DF_AAS_INT2$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
  
  
  DF_AAS_INT2 <- droplevels(DF_AAS_INT2)
  # get the max vals for each day
  max_vals2 <- ddply(DF_AAS_INT2, .(format(end,"%y/%m/%d"),WAIT_CLASS), subset, subset = rank(-AVG_SESS) <= 1)
  DF_SNAP_ID_SUBSET2 <- main$DF_SNAP_ID_SUBSET
  #DF_AAS_INT2$WAIT_CLASS <- factor(DF_AAS_INT2$WAIT_CLASS)
  last_level <- as.character(levels(DF_AAS_INT2$WAIT_CLASS)[length(levels(DF_AAS_INT2$WAIT_CLASS))])
  DF_SNAP_ID_SUBSET2$WAIT_CLASS <- last_level
  ymax2 <- max(c(main$cpu_cores+(main$cpu_cores*0.2),(max(DF_AAS_INT2$AVG_SESS)+(max(DF_AAS_INT2$AVG_SESS)*0.2))))
  
  
  plot_aas_wait_class2_line <- ggplot()+
    geom_line(data=DF_AAS_INT2, aes(x = end, y = AVG_SESS,color=WAIT_CLASS),stat = "identity",alpha=1,size=.7) +
    scale_color_manual("", values = aas_colors)+
    geom_point(data=max_vals2, aes(x=end, y=AVG_SESS), size=2, shape=21)+
    geom_text(data=max_vals2, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.4, hjust=1)+
    #facet_grid(WAIT_CLASS ~ . )+
    main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var
                     #limits = c(min(DF_AAS_INT2$end),max(DF_AAS_INT2$end))
    )+
    theme(axis.title.x  = element_blank(),axis.title.y  = element_blank())+
    #opts(panel.background = theme_rect(colour = "#777777"))+
    labs(title=paste(paste("Average Active Sessions by Wait Class - ",main$current_db_name,sep=""),"\n Only Values >= 95th Percentile",sep=""))+
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,alpha=0.2,hjust=-0.15,vjust=2.2)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0)+
    #ylim(0,ymax2)+
    theme(legend.position="right")
  
  plot_aas_wait_class2 <- plot_aas_wait_class2_line+
    facet_grid(WAIT_CLASS ~ . )+theme(legend.position="none")
  
  plot_aas_wait_class2_line <- plot_aas_wait_class2_line+cpu_cores_line
  
  plot_aas_wait_class2_gt <- ggplot_gtable(ggplot_build(plot_aas_wait_class2))
  plot_aas_wait_class2_gt$layout$clip[plot_aas_wait_class2_gt$layout$name=="panel"] <- "off"
  #print(plot_aas_wait_class_gt)
  #grid.draw(plot_aas_wait_class2_gt)
  flog.debug('plot_aas_chart - end',name='plot_aas_chart')
  return(list(plot_aas_wait_class_gt,plot_aas_wait_class2_gt,plot_aas_wait_class2_line))
  
  
}

plot_aas_bars_by_date <- function(DF_AAS_INT){
  flog.debug('plot_aas_bars_by_date - start',name='plot_aas_bars_by_date')
  
  DF_AAS_INT <- data.table(DF_AAS_INT)
  setkey(DF_AAS_INT,end, WAIT_CLASS)
  DF_AAS_INT$WAIT_CLASS <- str_trim(DF_AAS_INT$WAIT_CLASS)
  DF_AAS_INT$WAIT_CLASS <- factor(DF_AAS_INT$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
  
  
  vals <- expand.grid(end = unique(DF_AAS_INT$end),
                      WAIT_CLASS = unique(DF_AAS_INT$WAIT_CLASS))
  
  DF_AAS_INT <- merge(vals,DF_AAS_INT)
  rm(vals)

  DF_AAS_INT <- subset(DF_AAS_INT,end != TRUE)
  DF_AAS_INT <- subset(DF_AAS_INT,end != FALSE)
  DF_AAS_INT$AVG_SESS[is.na(DF_AAS_INT$AVG_SESS)] <- 0
  
  DF_AAS_INT$end <-  round_date(DF_AAS_INT$end , "hour")
  DF_AAS_INT <- data.table(DF_AAS_INT)
  
  DF_AAS_INT$hour <- as.numeric(format(DF_AAS_INT$end,"%H"))
  DF_AAS_INT$minute <- as.numeric(format(DF_AAS_INT$end,"%M"))
  DF_AAS_INT$dayName <- format(DF_AAS_INT$end,"%A")
  
  DF_AAS_INT$day_night <- "night (8pm-8am)" 
  DF_AAS_INT[hour >= 8 & hour < 20]$day_night <- "day (8am-8pm)"
  DF_AAS_INT$hour <- as.numeric(format(DF_AAS_INT$end,"%I"))
  
  
  aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                  "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                  "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                  "System I/O" = "#0993de","User I/O" = "#054ae1")
  
  gg_aas_colors <- scale_fill_manual("", values = aas_colors)
  
  
  
  
  DF_AAS_INT_AGG <- data.table(DF_AAS_INT[, list(AVG_SESS = mean(AVG_SESS)), by = list(dayName,day_night,hour,WAIT_CLASS)])
  setkey(DF_AAS_INT_AGG,dayName,hour,WAIT_CLASS)
  
  DF_AAS_INT_AGG<-data.frame(DF_AAS_INT_AGG)
  DF_AAS_INT_AGG$WAIT_CLASS <- factor(DF_AAS_INT_AGG$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
  
  DF_AAS_INT_AGG$hour <- factor(DF_AAS_INT_AGG$hour,c(8,9,10,11,12,1,2,3,4,5,6,7))
  DF_AAS_INT_AGG$dayName <- factor(DF_AAS_INT_AGG$dayName,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  
  aas_bar_plot <- ggplot(DF_AAS_INT_AGG)+
    geom_bar(aes(x=hour,y=AVG_SESS,fill=WAIT_CLASS),stat='identity',position='stack')+
    gg_aas_colors+
    facet_grid(day_night ~ dayName)+
    ylab("Average Active Sessions")+
    labs(title=paste("Average Active Sessions Summarized by Day and Hour for ",main$current_db_name,sep=""))+
    theme(legend.key.size =    unit(0.6, "lines"))
  
  
  
  
  
  
  
  
  flog.debug('plot_aas_bars_by_date - start',name='plot_aas_bars_by_date')
  return(aas_bar_plot)
}



plot_io <- function(DF_MAIN_BY_SNAP_INT){
  flog.debug('plot_io - start',name='plot_io')
  DF_MAIN_INT2 <- DF_MAIN_BY_SNAP_INT
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end"), measure.var = c("read_iops", "read_iops_max","write_iops", "write_iops_max",
                                                                  "read_mb_s","read_mb_s_max","write_mb_s","write_mb_s_max"))
  # add a "stat" column so we can facet by stat for avg-max
  x.melt$stat <- "Avg"  
  idx_max <- with(x.melt, grepl("max", variable))
  x.melt[idx_max,]$stat <- "Max"  
  x.melt$value <- round(x.melt$value)
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  x.melt[with(x.melt, grepl("read_iops", variable)),]$variable<-"Read IOPs"
  x.melt[with(x.melt, grepl("write_iops", variable)),]$variable<-"Write IOPs"
  x.melt[with(x.melt, grepl("read_mb_s", variable)),]$variable<-"Read MB/s"
  x.melt[with(x.melt, grepl("write_mb_s", variable)),]$variable<-"Write MB/s"
  
  
  DF_VAR_INT <- data.frame(unique(x.melt$variable))
  DF_SNAP_ID_SUBSET2 <- merge(DF_VAR_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(DF_SNAP_ID_SUBSET2$end),
                      variable = unique(x.melt$variable))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  DF_SNAP_ID_SUBSET3 <- main$DF_SNAP_ID_SUBSET
  x.melt$variable <- factor(x.melt$variable)
  #DF_AAS_INT2$WAIT_CLASS <- factor(DF_AAS_INT2$WAIT_CLASS)
  last_level <- as.character(levels(x.melt$variable)[length(levels(x.melt$variable))])
  DF_SNAP_ID_SUBSET3$variable <- last_level
  
  # get the max vals for each day
  
  max_vals <- ddply(x.melt, .(variable,stat,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
  max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(color=stat)) +
    geom_line(aes(color=stat), size=.2,alpha=0.7)+
    #stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
    #      aes(color=stat,fill=stat))+
    
    attr$themeScaleColour+attr$themeScaleFill+
    main$gg_avg_max_fill+main$gg_avg_max_color+
    geom_point(data=max_vals, aes(x=end, y=value, fill=stat), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=value, color=stat,label=label),size=2.5, vjust=0.5, hjust=1.25)+
    geom_text(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.1,vjust=0.7,alpha=0.2)+
    geom_point(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0),alpha=0.2,size=1)+
    ylab('')+
    
    facet_grid(variable ~ .,scales="free_y")+
    #opts(axis.title.x  = theme_blank())+
    #opts(plot.margin = unit(c(.1,.1,.1,.1), "cm"),panel.background = theme_rect(colour = "#aaaaaa"))+
    scale_y_continuous(labels=comma)+
    xlim(min(x.melt$end),max(x.melt$end))+
    #opts(axis.title.x  = theme_blank())+
    labs(title=paste("IO Avg and Max by IO Type for ",main$current_db_name,sep=""))+
    main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    #                   opts(axis.text.x=theme_text(angle=-30, hjust=-.1,vjust=1,size=6))+
    #                   opts(panel.grid.major = theme_line("#eeeeee", size = 0.2,linetype = "dotted"))+
    #                   opts(panel.grid.minor = theme_line("#efefef", size = 0.1,linetype = "dotted"))+
    #ylim(0,max(max_vals$value))+
    
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))
  #main$gg_bottom_panel
  
  #p
  
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  
  flog.debug('plot_io - end',name='plot_io')
  return(p_gt)
  #}
}



plot_io_histograms <- function(DF_IO_WAIT_HIST_INT){
  flog.debug('plot_io_histograms - start',name='plot_io_histograms')
  awrM$debug.lastFunction <- 'plot_io_histograms - start'
  DF_IO_WAIT_HIST_INT<- merge(DF_IO_WAIT_HIST_INT,main$DF_SNAP_ID_DATE,by="SNAP_ID")
  DF_IO_WAIT_HIST_INT <- data.table(DF_IO_WAIT_HIST_INT)
  flog.trace('DF_IO_WAIT_HIST_INT:',DF_IO_WAIT_HIST_INT,name='plot_io_histograms',capture=TRUE)
  #summary(DT_IO_HIST)
  #unique(DF_IO_WAIT_HIST_INT$EVENT_NAME)
  DF_IO_WAIT_HIST_INT$EVENT_NAME <- str_trim(DF_IO_WAIT_HIST_INT$EVENT_NAME)
  #DF_IO_WAIT_HIST_INT <- DF_IO_WAIT_HIST_INT
  
  
  DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI <- as.numeric(as.character(DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI))
  DF_IO_WAIT_HIST_INT[WAIT_TIME_MILLI>=64, WAIT_TIME_MILLI := 64]
  DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI <- factor(DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI)
  
  
  
  vals <- expand.grid(SNAP_ID = unique(DF_IO_WAIT_HIST_INT$SNAP_ID),
                      WAIT_TIME_MILLI = unique(DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI))
  
  DF_IO_WAIT_HIST_INT <- merge(vals,DF_IO_WAIT_HIST_INT)
  
  DF_IO_WAIT_HIST_INT <- data.table(DF_IO_WAIT_HIST_INT)
  setkey(DF_IO_WAIT_HIST_INT,end, EVENT_NAME, WAIT_TIME_MILLI)
  DF_IO_WAIT_HIST_INT <- DF_IO_WAIT_HIST_INT[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(end, EVENT_NAME, WAIT_TIME_MILLI)]
  
  DF_IO_WAIT_HIST_INT[with(DF_IO_WAIT_HIST_INT, grepl(64, WAIT_TIME_MILLI)),]$WAIT_TIME_MILLI<-"64+"
  
  DF_IO_WAIT_HIST_INT_GROUP <- DF_IO_WAIT_HIST_INT[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(EVENT_NAME, WAIT_TIME_MILLI)]
  
  DF_IO_WAIT_HIST_INT_GROUP <- DF_IO_WAIT_HIST_INT_GROUP[, list(WAIT_TIME_MILLI=WAIT_TIME_MILLI,WAIT_PCT =WAIT_COUNT/ sum(WAIT_COUNT)), by = list(EVENT_NAME)]
  
  
  
  io_hist_colors2 <- c("1" = "#315280", "2" = "#4575B4", "4" = "#74ADD1", "8" = "#ABD9E9",
                       "16" = "#FDAE61", "32" = "#F46D43", "64+"="#D73027")
  
  gg_io_hist_colors2 <- scale_fill_manual(values = io_hist_colors2,name="wait ms" )
  
  io_hist_plot <- ggplot(DF_IO_WAIT_HIST_INT_GROUP,aes(x=factor(WAIT_TIME_MILLI),fill = WAIT_TIME_MILLI,))+
    geom_bar(stat ='identity',aes(y=WAIT_PCT))+
    facet_grid(. ~ EVENT_NAME,scales="free_y")+
    gg_io_hist_colors2+
    labs(title=paste0("I/O Wait Event Histogram"))+
    scale_y_continuous(labels = percent_format())+
    theme(axis.title.y  = element_blank(),legend.position =    "right" ,
          legend.key.size = unit(.25, "cm"))+
    xlab("Wait Milliseconds")
  

  io_hist_area_plot <- ggplot()+
    geom_area(data=DF_IO_WAIT_HIST_INT, aes(x = end, y = WAIT_COUNT,
                                         fill = WAIT_TIME_MILLI),stat = "identity", position = "stack",alpha=1)+
    facet_grid(EVENT_NAME ~ .,scales="free_y")+
    gg_io_hist_colors2+
    main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    labs(title=paste0("I/O Wait Event Area Chart"))+
    theme(legend.key.size = unit(.25, "cm"))+
    scale_x_datetime(labels = date_format("%a, %b %d %I %p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_IO_WAIT_HIST_INT$end),max(DF_IO_WAIT_HIST_INT$end)))+
    #scale_y_continuous(labels = comma)+
    ylab("Wait Count")+
    theme(axis.title.x=element_blank(),legend.position =    "none" )
  
  
  flog.debug('plot_io_histograms - end',name='plot_io_histograms')
  awrM$debug.lastFunction <- 'plot_io_histograms - end'
  return(list(io_hist_plot,io_hist_area_plot))
}


plot_cpu <- function(DF_MAIN_INT){
  flog.debug('plot_cpu - start',name='plot_cpu')
  #if(dim(DF_MAIN_INT)[1] > 2){
  #DF_MAIN_INT <- DF_MAIN
  #browser()
  DF_INST_INT <- data.frame(unique(DF_MAIN_INT$inst))
  DF_SNAP_ID_SUBSET2 <- merge(DF_INST_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(main$DF_SNAP_ID_SUBSET$end),
                      inst = unique(DF_MAIN_INT$inst))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  x.melt <- melt(DF_MAIN_INT, id.var = c("end","inst"), measure.var = c("os_cpu", "os_cpu_max"))
  x.melt$variable<-gsub( "os_cpu_max" , "OS CPU Max" , x.melt$variable)
  x.melt$variable<-gsub( "os_cpu" , "OS CPU Avg" , x.melt$variable)
  
  # get the max vals for each day
  max_vals <- ddply(x.melt, .(variable,inst,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
  max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")
  
  
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(group=variable,color=variable)) +
    geom_line(aes(group=variable,color=variable), size=.2)+
    attr$themeScaleColour+attr$themeScaleFill+
    stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
                aes(group=variable,color=variable,fill=variable))+
    ylab('CPU Percent')+
    ylim(0,115)+
    geom_point(data=max_vals, aes(x=end, y=value, fill=variable), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=value, color=variable,label=label),size=3, vjust=-.8, hjust=1.5)+
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-45,size=1.5,hjust=-.5,alpha=0.2)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1)+
    facet_grid(inst ~ .)+
    theme(axis.title.x  = element_blank())+
    theme(panel.background = element_rect(colour = "#777777"))+
    xlim(min(x.melt$end),max(x.melt$end))+
    labs(title=paste("CPU Avg and Max by Instance for ",main$current_db_name,sep=""))+
    main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    #scale_x_datetime(labels = date_format("%a, %b %d %I %p"),breaks = date_breaks("2 hour"),
    scale_x_datetime(labels = date_format("%a, %b %d %I %p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))+
    main$gg_avg_max_fill+main$gg_avg_max_color
  # theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))
  #theme(panel.grid.major = element_line("#eeeeee", size = 0.2,linetype = "dotted"))+
  #theme(panel.grid.minor = element_line("#efefef", size = 0.1,linetype = "dotted"))
  #main$gg_bottom_panel
  
  #p
  p_gt <- ggplot_gtable(ggplot_build(p))
  flog.debug('plot_cpu - end',name='plot_cpu')
  #   p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  return(p_gt)
  #}
}


plot_main_activity <- function(DF_MAIN_INT){
  flog.debug('plot_main_activity - start',name='plot_main_activity')
  #if(dim(DF_MAIN_INT)[1] > 2){
  #DF_MAIN_INT<-DF_MAIN
  
  DF_MAIN_INT2 <- ddply(DF_MAIN_INT, .(end), summarise, 
                        aas=sum(aas),
                        aas_max=sum(aas_max),
                        sql_res_t_cs=sum(sql_res_t_cs),
                        logons_s=sum(logons_s),
                        logons_total=sum(logons_total),
                        exec_s=sum(exec_s),
                        hard_p_s=sum(hard_p_s),
                        commits_s=sum(commits_s))

  
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end"), measure.var = c("aas", "aas_max","sql_res_t_cs", "logons_s",
                                                                  "logons_total","exec_s","hard_p_s","commits_s"))
  #"db_block_gets_s","db_block_changes_s"))
  
  x.melt$value <- round(x.melt$value,2)
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  x.melt[with(x.melt, grepl("aas_max", variable)),]$variable<-"AAS Max"
  x.melt[with(x.melt, grepl("aas", variable)),]$variable<-"AAS"
  x.melt[with(x.melt, grepl("sql_res_t_cs", variable)),]$variable<-"SQL Resp (cs)"
  x.melt[with(x.melt, grepl("logons_s", variable)),]$variable<-"Logons/s"
  x.melt[with(x.melt, grepl("logons_total", variable)),]$variable<-"Logons Total"
  x.melt[with(x.melt, grepl("exec_s", variable)),]$variable<-"Execs/s"
  x.melt[with(x.melt, grepl("commits_s", variable)),]$variable<-"Commits/s"
  x.melt[with(x.melt, grepl("hard_p_s", variable)),]$variable<-"Hard Parses/s"
  
  x.melt$variable <- factor(x.melt$variable)
  
  x.melt <- data.frame(x.melt,stringsAsFactors =TRUE)
  DF_VAR_INT <- data.frame(unique(x.melt$variable))
  DF_SNAP_ID_SUBSET2 <- merge(DF_VAR_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(DF_SNAP_ID_SUBSET2$end),
                      variable = unique(x.melt$variable))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  # dummy value to adjust scales of aas to match aax_max
  
  
  x.melt <- na.omit(x.melt)
  
  last_level <- as.character(levels(x.melt$variable)[length(levels(x.melt$variable))])
  DF_SNAP_ID_SUBSET2$variable <- last_level
  
  DF_SNAP_ID_SUBSET2$variable <- factor(DF_SNAP_ID_SUBSET2$variable)
  
  if(nrow(subset(x.melt, variable=="AAS")) == 0){
    gg_aas_max_max <- opts()
  }  else {
    df_aas_max_max <- data.frame(end=min(x.melt$end),variable="AAS",value=max(subset(x.melt, variable == "AAS Max")$value),stringsAsFactors =TRUE)
    gg_aas_max_max <- geom_point(data=df_aas_max_max,aes(x=end,y=value),alpha=0)
  }
  
  max_vals <- ddply(x.melt, .(variable,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
  max_vals$label <- formatC(max_vals$value, digits=2,format="fg", big.mark=",")
  
  p <- ggplot() +
    geom_line(data=x.melt,aes(x=end, y=value,color=variable), size=.2)+
    attr$themeScaleColour+attr$themeScaleFill+
    # stat_smooth(data=x.melt,aes(x=end, y=value),method = "loess",n=300,size=.2,linetype="dashed",alpha=0.2)+
    geom_point(data=max_vals, aes(x=end, y=value, fill=variable), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=value, color=variable,label=label),size=2.5, vjust=0.5, hjust=1.6)+
    ylab('')+
    facet_grid(variable ~ .,scales="free_y")+
    scale_y_continuous(labels=comma)+
    xlim(min(x.melt$end),max(x.melt$end))+
    theme(axis.title.x  = element_blank(),legend.position="none")+
    labs(title=paste("Main Activity Variables for ",main$current_db_name,sep=""))+
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.15,vjust=2.8,color="#bbbbbb",alpha=0.1)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0,color="#999999",alpha=0.1)+
    main$gg_hour_bars+
    gg_aas_max_max +
    attr$vertical_line + attr$vertical_text +
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))
  #scale_colour_tableau("colorblind10")+scale_fill_tableau("colorblind10")
  
  
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  flog.debug('plot_main_activity - end',name='plot_main_activity')
  #grid.draw(p_gt)
  return(p_gt)
  
  
}




plot_RAC_activity <- function(DF_MAIN_INT){
  flog.debug('plot_RAC_activity - start',name='plot_RAC_activity')
  #if(dim(DF_MAIN_INT)[1] > 2){
  #DF_MAIN_INT<-main$DF_MAIN
  
  DF_MAIN_INT2 <- ddply(DF_MAIN_INT, .(end,inst), summarise, 
                        gc_cr_rec_s=sum(gc_cr_rec_s),
                        gc_cu_rec_s=sum(gc_cu_rec_s),
                        gc_cr_get_cs=mean(gc_cr_get_cs),
                        gc_cu_get_cs=mean(gc_cu_get_cs),
                        gc_bk_corrupted=sum(gc_bk_corrupted),
                        gc_bk_lost=sum(gc_bk_lost))
  #db_block_gets_s=sum(db_block_gets_s),
  #db_block_changes_s=sum(db_block_changes_s))
  
  flog.trace('DF_MAIN_INT2-1',DF_MAIN_INT2,capture=TRUE,name='plot_RAC_activity')
  if(!(max(DF_MAIN_INT2$gc_cr_rec_s)>10)){
    return(TRUE)
  }
  
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end","inst"), measure.var = c("gc_cr_rec_s", "gc_cu_rec_s","gc_cr_get_cs", "gc_cu_get_cs",
                                                                         "gc_bk_corrupted","gc_bk_lost"))
  #"db_block_gets_s","db_block_changes_s"))
  
  x.melt$value <- round(x.melt$value,2)
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  x.melt[with(x.melt, grepl("gc_cr_rec_s", variable)),]$variable<-"GC CR Block Rec /s"
  x.melt[with(x.melt, grepl("gc_cu_rec_s", variable)),]$variable<-"GC Current Block Rec /s"
  x.melt[with(x.melt, grepl("gc_cr_get_cs", variable)),]$variable<-"GC Avg CR Get cs"
  x.melt[with(x.melt, grepl("gc_cu_get_cs", variable)),]$variable<-"GC Avg Current Get cs"
  x.melt[with(x.melt, grepl("gc_bk_corrupted", variable)),]$variable<-"GC Blocks Corrupted"
  x.melt[with(x.melt, grepl("gc_bk_lost", variable)),]$variable<-"GC Blocks Lost"

  flog.debug(nrow(subset(x.melt,variable == 'GC Blocks Corrupted')),name='plot_RAC_activity')
  flog.debug(nrow(subset(x.melt,variable == 'GC Blocks Corrupted' & value==0)),name='plot_RAC_activity')
  flog.debug(nrow(subset(x.melt,variable == 'GC Blocks Corrupted' & value==NA)),name='plot_RAC_activity')
  
  if(nrow(subset(x.melt,variable == 'GC Blocks Corrupted')) == nrow(subset(x.melt,variable == 'GC Blocks Corrupted' & value==0))){
    x.melt <- subset(x.melt,variable != 'GC Blocks Corrupted')
  }
  
  if(nrow(subset(x.melt,variable == 'GC Blocks Lost')) == nrow(subset(x.melt,variable == 'GC Blocks Lost' & value==0))){
    x.melt <- subset(x.melt,variable != 'GC Blocks Lost')
  }
  
  #x.melt[with(x.melt, grepl("db_block_gets_s", variable)),]$variable<-"Block Gets/s"
  #x.melt[with(x.melt, grepl("db_block_changes_s", variable)),]$variable<-"Block Changes/s"
  
  x.melt$variable <- factor(x.melt$variable)
  x.melt$inst <- factor(x.melt$inst)
  
  x.melt <- data.frame(x.melt,stringsAsFactors =TRUE)
  DF_VAR_INT <- data.frame(unique(x.melt$variable))
  DF_SNAP_ID_SUBSET2 <- merge(DF_VAR_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(DF_SNAP_ID_SUBSET2$end),
                      variable = unique(x.melt$variable))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  # dummy value to adjust scales of aas to match aax_max
  
  
  x.melt <- na.omit(x.melt)
  
  last_level <- as.character(levels(x.melt$variable)[length(levels(x.melt$variable))])
  DF_SNAP_ID_SUBSET2$variable <- last_level
  
  DF_SNAP_ID_SUBSET2$variable <- factor(DF_SNAP_ID_SUBSET2$variable)
  
  #   if(nrow(subset(x.melt, variable=="AAS")) == 0){
  #     gg_aas_max_max <- opts()
  #   }  else {
  #     df_aas_max_max <- data.frame(end=min(x.melt$end),variable="AAS",value=max(subset(x.melt, variable == "AAS Max")$value),stringsAsFactors =TRUE)
  #     gg_aas_max_max <- geom_point(data=df_aas_max_max,aes(x=end,y=value),alpha=0)
  #   }
  
  max_vals <- ddply(x.melt, .(variable,inst,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
  max_vals$label <- formatC(max_vals$value, digits=2,format="fg", big.mark=",")
  
  p <- ggplot() +
    geom_line(data=x.melt,aes(x=end, y=value,color=inst,group=inst), size=.2,alpha=0.8)+
    #     stat_smooth(data=x.melt,aes(x=end, y=value),method = "loess",n=300,size=.2,linetype="dashed",alpha=0.2)+
    geom_point(data=max_vals, aes(x=end, y=value, fill=inst,group=inst), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=value, color=inst,label=label,group=inst),size=2.5, vjust=0.5, hjust=1.6)+
    #     ylab('')+
    facet_grid(variable ~ .,scales="free_y")+
    attr$themeScaleColour+attr$themeScaleFill+
    scale_y_continuous(labels=comma)+
    xlim(min(x.melt$end),max(x.melt$end))+
    #theme(axis.title.x  = element_blank(),legend.position="none")+
    theme(text =               element_text(size = 6))+
    labs(title=paste("Global Cache Attributes for ",main$current_db_name,sep=""))+
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.15,vjust=2.8,color="#bbbbbb",alpha=0.1)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0,color="#999999",alpha=0.1)+
    main$gg_hour_bars+
    #gg_aas_max_max +
    attr$vertical_line + attr$vertical_text +
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))
  #scale_colour_tableau("colorblind10")+scale_fill_tableau("colorblind10")
  
  
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  flog.debug('plot_RAC_activity - end',name='plot_RAC_activity')
  #grid.draw(p_gt)
  return(p_gt)
  
  
}



plot_memory <- function(DF_MEMORY_INT){
  
  flog.debug('plot_memory - start',name='plot_memory')
  #if(dim(DF_MAIN_INT)[1] > 2){
  #DF_MEMORY_INT <- main$DF_MEMORY
  #browser()
  
  DF_MEMORY_INT_SUM <- ddply(DF_MEMORY_INT, .(end,SNAP_ID), summarise, 
                             SGA=sum(SGA),
                             PGA=sum(PGA),
                             TOTAL=sum(TOTAL))
  
  #DF_MEMORY_INT_SUM$end <- as.POSIXlt(DF_MEMORY_INT_SUM$end)
  
  # get the max vals for each day
  max_vals <- ddply(DF_MEMORY_INT_SUM, .(end_new = as.character(end,format="%y/%m/%d",tx="GMT")), transform, 
                    rank = rank(-TOTAL, ties.method = "first"))
  max_vals <- subset(max_vals,rank <=1)
  
  
  max_vals$label <- prettyNum(max_vals$TOTAL, big.mark=",")
  
  line_labels <- data.frame(end=DF_MEMORY_INT_SUM$end[[nrow(DF_MEMORY_INT_SUM)]], 
                            SGA=DF_MEMORY_INT_SUM$SGA[[nrow(DF_MEMORY_INT_SUM)]],
                            PGA=DF_MEMORY_INT_SUM$PGA[[nrow(DF_MEMORY_INT_SUM)]])
  
  p <- ggplot(data=DF_MEMORY_INT_SUM, aes(x=end, y=TOTAL)) +
    geom_area(fill="#3694d1",alpha=0.2)+
    geom_point(data=max_vals, aes(x=end, y=TOTAL), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=TOTAL,label=label),size=3, vjust=-.8, hjust=1.5)+
    geom_line(aes(x=end,y=SGA),color="#c0392b",size=1)+
    geom_text(data=line_labels,aes(x=end, y=SGA,label="SGA"),color="#c0392b",size=3, vjust=-.8, hjust=1.5)+
    geom_line(aes(x=end,y=PGA),color="#47b850",size=1)+
    geom_text(data=line_labels,aes(x=end, y=PGA,label="PGA"),color="#47b850",size=3, vjust=-.8, hjust=1.5)+
    theme(axis.title.x  = element_blank())+
    #opts(plot.margin = unit(c(.1,.1,.1,.1), "cm"),panel.background = theme_rect(colour = "#777777"))+
    labs(title=paste("Total Memory Usage in GB for ",main$current_db_name,sep=""))+
    #main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    scale_x_datetime(labels = date_format("%a, %b %d"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_MEMORY_INT_SUM$end),max(DF_MEMORY_INT_SUM$end)))
  #opts(axis.text.x=theme_text(angle=-30, hjust=-.1,vjust=1,size=6))+
  #opts(panel.grid.major = theme_line("#eeeeee", size = 0.2,linetype = "dotted"))+
  #opts(panel.grid.minor = theme_line("#efefef", size = 0.1,linetype = "dotted"))
  
  p
  flog.debug('plot_memory - end',name='plot_memory')
  return(p)
  #}
}


#====================================================================================================================











# *MAIN LOOP STRUCTURES THAT CALL OTHER FUNCTIONS* ==================================================================


main$overall_summary_df <- NULL
main$overall_combined_df <- NULL
main$DATA_FRAME <- NULL
main$DF_OS <- NULL
main$DF_MEMORY <- NULL
main$DF_MAIN <- NULL
main$DF_SNAP_ID_DATE <- NULL
main$DF_SNAP_ID_SUBSET <- NULL
main$DF_SNAP_ID_DATE2 <- NULL
main$DF_IO_WAIT_HIST <- NULL
main$DF_DB_PARAMETERS <- NULL
main$DF_IO_BY_OBJECT_TYPE <- NULL
main$gg_hour_bars <- NULL
main$cpu_cores <- NULL
main$current_db_name=""
main$attributes_file="attributes.csv"
main$plot_attributes <- NULL
main$current_plot_attributes <- NULL

main$cpu_plot<-NULL
main$RAC_plot<-NULL


main$mainFunction <- function(f){
  
  vector_element = which(main$db_id==f)
  main$current_db_name=main$db_name[which(main$db_id==f)]
  flog.info(paste0('Starting DB: ',main$current_db_name))
  
  #flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
  
  main$DATA_FRAME <- NULL
  main$DF_OS <- NULL
  main$DF_MEMORY <- NULL
  main$DF_MAIN <- NULL
  main$DF_MAIN_BY_SNAP <- NULL
  main$DF_IO_WAIT_HIST <- NULL
  main$current_plot_attributes <- NULL
  
  tryCatch(main$current_plot_attributes <- load_plot_attributes(), 
           error = function(e) {
             #traceback()
             flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
             flog.error(paste0("Error in ",main$current_db_name,": ",e))
             #flog.remove(name=main$current_db_name)
             #browser()
             
           }
           #,finally=print("finished")
  )
  
  attr$filter_snap_min <<- as.numeric(attr$filter_snap_min)
  attr$filter_snap_max <<- as.numeric(attr$filter_snap_max)
  
  c(main$DF_OS, main$DF_MAIN,main$DF_MEMORY,main$DF_SPACE,main$DF_AAS,main$DF_SQL_SUMMARY,main$DF_SQL_BY_SNAPID,main$DF_SNAP_ID_DATE,main$DF_IO_WAIT_HIST,main$DF_DB_PARAMETERS,main$DF_IO_BY_OBJECT_TYPE) := build_data_frames(f,main$current_db_name)
  c(main$DF_MAIN_BY_SNAP) := summarise_dfs_by_snap()
  main$DF_SNAP_ID_DATE2 <- build_snap_to_date_df()
  add_vetical_lines()
  set_date_break_vars(main$DF_MAIN)
  
  main$DF_SNAP_ID_SUBSET <- generate_snap_id_labels(main$DF_SNAP_ID_DATE)
  
  if(is.na(get_os_stat("NUM_CPU_CORES"))) main$num_cpu_cores <- get_os_stat("NUM_CPUS") else main$num_cpu_cores <- get_os_stat("NUM_CPU_CORES")
  
  main$cpu_cores <- main$num_cpu_cores*get_os_stat("INSTANCES")
  
  main$gg_hour_bars <- generate_hours_bars(main$DF_MAIN)
  
  c(main$DF_SUMMARY_OS,main$DF_SUMMARY_MAIN,main$DF_SUMMARY_OVERALL) := gen_summary_data()
  main$overall_summary_df <- rbind(main$overall_summary_df, main$DF_SUMMARY_OVERALL)
  box_plots <- plot_summary_boxplot_main()
  tblText <- tableGrob(main$DF_SUMMARY_OS,show.rownames = FALSE, gpar.coretext = gpar(fontsize=12),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  tblText2 <- tableGrob(main$DF_SUMMARY_MAIN,show.rownames = FALSE, gpar.coretext = gpar(fontsize=10),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  
  flog.trace(nrow(subset(main$DF_OS,STAT_NAME == 'HOSTS')),name='mainFunction')
  DF_HOSTS_INT <- subset(main$DF_OS,STAT_NAME == 'HOSTS')
  if(nrow(DF_HOSTS_INT)==0){
    DF_HOSTS_INT <- data.frame(col1='HOSTS',col2='NA')
  }
  tblText3 <- tableGrob(DF_HOSTS_INT,show.rownames = FALSE, gpar.coretext = gpar(fontsize=8),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = FALSE,col.just = "left")
  flog.trace(str(tblText3),name='mainFunction')
  c(aas_plot, aas_plot2_gt,aas_plot2_line) := plot_aas_chart(main$DF_AAS)
  
  outFileName <- paste(main$current_db_name,min(main$DF_MAIN$snap),max(main$DF_MAIN$snap),outFileSuffix,sep='-')
  
  pdf(paste(outFileName,"-plot.pdf",sep=""), width = 11, height = 8.5,useDingbats=FALSE)
  
  #x <- grid.arrange(box_plots, ncol = 1, heights=c(1))
  #x <- grid.arrange(tblText ,box_plots, ncol = 1, heights=c(1,1))
  #x <- grid.arrange(tblText,tblText2,tblText3, box_plots, ncol = 1, heights=c(1,1,1,8))
  #x <- grid.arrange(tblText,tblText2,tblText3, aas_plot2_line,box_plots, ncol = 1, heights=c(1,1,1,8,8))
  if(debugMode){
    x <- grid.arrange(tblText,tblText2,tblText3, aas_plot2_line,box_plots, ncol = 1, heights=c(1,1,1,8,8))
  }
  else{
    tryCatch(x <- grid.arrange(tblText,tblText2,tblText3, aas_plot2_line,box_plots, ncol = 1, heights=c(1,1,1,8,8)), 
             error = function(e) {
               tryCatch(x <- grid.arrange(tblText,tblText2,tblText3, box_plots, ncol = 1, heights=c(1,1,1,8)), 
                        error = function(e) {
                          x <- grid.arrange(tblText ,tblText2,tblText3, ncol = 1, heights=c(1,1,1))
                        }
               )
             }
    )
  }
  
  #flog.remove(main$current_db_name)
  
  cpu_plot <- NULL
  cpu_plot <- plot_cpu(main$DF_MAIN)
  io_plot <- plot_io(main$DF_MAIN_BY_SNAP)
  
  main_activity_plot <- plot_main_activity(main$DF_MAIN)
  
  memory_plot <- plot_memory(main$DF_MEMORY)
  
  RAC_activity_plot <- NULL
  
  if(("gc_cr_rec_s" %in% names(main$DF_MAIN))){
    tryCatch(RAC_activity_plot <- plot_RAC_activity(main$DF_MAIN), 
             error = function(e) {
               traceback()
               print(paste0("Error in ",main$current_db_name,": ",e))
               #browser()
             }
             #,finally=print("finished")
    )
  }
  
  aas_bars_by_date_plot <- plot_aas_bars_by_date(main$DF_AAS)
  
  #main$cpu_plot <<-cpu_plot
  if( nrow(main$DF_IO_WAIT_HIST)>10){
    c(io_hist_plot, io_hist_area_plot) := plot_io_histograms(main$DF_IO_WAIT_HIST)
  }
 # main$RAC_plot <<-RAC_activity_plot
  
  grid.newpage()
  grid.draw(cpu_plot)
  grid.newpage()
  grid.draw(io_plot)
  
  if( nrow(main$DF_IO_WAIT_HIST)>10){
    x <- grid.arrange(io_hist_plot,io_hist_area_plot , ncol = 1, heights=c(1,4))
  }
  head(main$DF_IO_WAIT_HIST)
  grid.newpage()
  
  grid.draw(aas_plot)
  grid.newpage()
  grid.draw(aas_plot2_gt)
  
  #grid.newpage()
  print(aas_bars_by_date_plot)
  
  grid.newpage()
  grid.draw(main_activity_plot)
  
  
  #if(!is.null(RAC_activity_plot)){
  if(inherits(RAC_activity_plot,what='grob')){
    if(("gc_cr_rec_s" %in% names(main$DF_MAIN))){
      grid.newpage()
      tryCatch(
        grid.draw(RAC_activity_plot), 
        error = function(e) {
          traceback()
          print(paste0("Error in ",main$current_db_name,": ",e))
          #browser()
        }
        #,finally=print("finished")
      )
      
    }
  }
  print(memory_plot)
  
  
  parTable <- function(df){
    df[is.na(df)] <- ""
    return(tableGrob(df,show.rownames = FALSE, gpar.coretext = gpar(fontsize=6),gpar.coltext = gpar(fontsize=4),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,core.just="left", gpar.corefill = gpar(fill=NA,col=NA) ))
  }
  
  dbParameters1 <- parTable(main$DF_DB_PARAMETERS[c(seq(1,80)),])
  dbParameters2 <- parTable(main$DF_DB_PARAMETERS[c(seq(81,160)),])
  dbParameters3 <- parTable(main$DF_DB_PARAMETERS[c(seq(161,240)),])
  #dbParameters <- tableGrob(main$DF_DB_PARAMETERS,show.rownames = FALSE, gpar.coretext = gpar(fontsize=5),gpar.coltext = gpar(fontsize=5),padding.v = unit(1, "mm"),padding.h = unit(1, "mm"),show.colnames = TRUE,col.just = "left")
  
  grid.arrange(dbParameters1,dbParameters2,dbParameters3,ncol = 3, widths=c(1,1,1))
  
  
  main$DF_SQL_SUMMARY$ELAP <-  main$DF_SQL_SUMMARY$ELAP / 1000000 # convert microseconds to seconds
  main$DF_SQL_SUMMARY$AVG_DOP <- main$DF_SQL_SUMMARY$PX_SERVERS_EXECS / main$DF_SQL_SUMMARY$EXECS
  main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M <- (main$DF_SQL_SUMMARY$ELAP / main$DF_SQL_SUMMARY$EXECS)/60
  main$DF_SQL_SUMMARY$logRsGBperExec <- ((main$DF_SQL_SUMMARY$LOG_READS* 8)/main$DF_SQL_SUMMARY$EXECS)/1024/1024
  main$DF_SQL_SUMMARY$ELAP <-  formatC(main$DF_SQL_SUMMARY$ELAP, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$EXECS <-  formatC(main$DF_SQL_SUMMARY$EXECS, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$LOG_READS <-  formatC(main$DF_SQL_SUMMARY$LOG_READS, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M <-  formatC(main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$AVG_DOP <-  formatC(main$DF_SQL_SUMMARY$AVG_DOP, digits=0,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$logRsGBperExec <-  formatC(main$DF_SQL_SUMMARY$logRsGBperExec, digits=2,format="fg", big.mark=",")
  
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="ELAP"] <- "ELAP_S"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PARSING_SCHEMA_NAME"] <- "SCHEMA"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="OPTIMIZER_COST"] <- "COST"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PLAN_COUNT"] <- "PLANS"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PX_SERVERS_EXECS"] <- "PX_EXEC"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="LOG_READS_RANK"] <- "logRsRank"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PHYS_READS_RANK"] <- "physRsRank"
  #subset(df, select=-c(z,u))
  sqlSummaryText1 <- tableGrob(subset(main$DF_SQL_SUMMARY, select=-c(PX_EXEC,LOG_READS)),show.rownames = FALSE, gpar.coretext = gpar(fontsize=5),gpar.coltext = gpar(fontsize=5),padding.v = unit(1, "mm"),padding.h = unit(1, "mm"),show.colnames = TRUE,col.just = "left", gpar.corefill = gpar(fill=NA,col=NA),h.even.alpha = 0 )
  #print(sqlSummaryText1)
  grid.arrange(sqlSummaryText1,ncol = 1, widths=c(1))
  
  dev.off()
  
  main$plot_attributes <- rbind(main$plot_attributes,main$current_plot_attributes)
  flog.info(paste0('Finished DB: ',main$current_db_name))
}

main$mainLoop <- function(){
  for (f in main$db_id) {
    if(debugMode){
      print('Running without tryCatch as we are in debug mode')
      main$mainFunction(f)
    }
    else{
      tryCatch(main$mainFunction(f), 
               error = function(e) {
                 traceback()
                 flog.appender(appender.file(paste0(main$current_db_name,'.err')), name=main$current_db_name)
                 flog.error(paste0("Error in ",main$current_db_name,": ",e),name=main$current_db_name)
                 print(e)
                 flog.error(traceback(),name=main$current_db_name)
                 flog.remove(main$current_db_name)
                 #browser()
                 
               }
               #,finally=print("finished")
      )
      
    } #end else
    
  } #end for loop
  write.csv(main$overall_summary_df,'OverallSummary.csv')
  if(length(main$plot_attributes) > 0){
    write.csv(main$plot_attributes,'attributes.csv',row.names=FALSE)
  }
}

get_db_names()
main$mainLoop()

#====================================================================================================================
