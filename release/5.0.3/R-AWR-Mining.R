#Sys.setenv(http_proxy="http://www-proxy.us.oracle.com:80")


#debugModeOverride <- TRUE  | rm(debugModeOverride)
#debugMoveFiles <- TRUE  | rm(debugMoveFiles)
#dumpCSV <- TRUE  | rm(dumpCSV)
#filePatternOverride <- "^awr-hist.+DB110g.+(\\.out|\\.gz)$" | rm(filePatternOverride)
#plotOverride <- "ALL" [ALL|NONE|SOME|PAGE1|AAS] | rm(plotOverride) | plotOverride <- c("SOME","PAGE1","AAS")
#parseOverride <- "ALL" [ALL|NONE|SOME|PAGE1|AAS] | rm(parseOverride) | c("SOME","aas_facet")
#parseOverride <- c("!TOP-SQL-BY-SNAPID")
#parseOverride <- c("!TOP-SQL-BY-SNAPID","!TOP-SQL-SUMMARY","!TOP-N-TIMED-EVENTS","!IO-WAIT-HISTOGRAM","!IOSTAT-BY-FUNCTION")
#plotOverride <- "NONE"

#parseOverride <- c("SOME","OS-INFORMATION","MEMORY")


list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate","gplots","gtools","dplyr","sjPlot","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})


if(interactive()){
  #WORK_DIR <- 'E:/Portable-AWR-Miner/CSVs'
}else{
  WORK_DIR <- (commandArgs(TRUE)[1])
  setwd(WORK_DIR)
  #print("arg")
  #print(args[1])
}

#====================================================================================================================
# Change Variables in this sections as needed 

MAX_DAYS <- 30

outFileSuffix <- '1'


#====================================================================================================================
options(scipen=999) # disable scientific notation
debugMode <- FALSE
flog.threshold(INFO) #TRACE, DEBUG, INFO, WARN, ERROR, FATAL
#flog.threshold(ERROR,name='getSection') #TRACE, DEBUG, INFO, WARN, ERROR, FATAL 
#flog.threshold(DEBUG,name='print') #TRACE, DEBUG, INFO, WARN, ERROR, FATAL 
# flog.threshold(DEBUG,name='generate_plot_attributes') #TRACE, DEBUG, INFO, WARN, ERROR, FATAL 
#name: build_data_frames, mainFunction,set_date_break_vars,plot_RAC_activity
try(rm(main),silent=TRUE)

if(file.exists('settings.R')){
  source('settings.R')
}

fullNamePattern <- "^awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-([0-9]+)-([0-9]+)(\\.|out|gz)+$"

#debugModeOverride <- TRUE  | rm(debugModeOverride)
if(exists("debugModeOverride")){
  if(!is.null(debugModeOverride)){
    if(debugModeOverride){
      debugMode <- TRUE
      print('In debug mode')
      writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
      flog.threshold(DEBUG)
      flog.threshold(DEBUG,name='build_data_frames')
      debugVars <- new.env()
      
    }
    else{
      debugMode <- FALSE
    }
  }
}



awrMinerPlotVersion <- '5.0.3'

filePattern <- "^awr-hist*.*(\\.out|\\.gz)$"
if(exists("filePatternOverride")){
  if(!is.null(filePatternOverride)){
    if(nchar(filePatternOverride)>1){
      filePattern <- filePatternOverride
    }
  }
}


appender.status.fn <- function(lineIn) {
  strFormat <- function(stringIn){
    stringInternal <- str_replace_all(stringIn,"\\n$","")
    return(stringInternal)
  }
  lineIn <- strFormat(lineIn)
  cat(lineIn,file="status.log",append=TRUE,sep="\n")
  print(lineIn,quote=FALSE,useSource = TRUE)
}

flog.logger(name='status', threshold=INFO, appender=NULL)
flog.appender(appender.status.fn, 'status')
flog.info("\n\n--------------------------------------------------\n\n",name='status')
flog.info("** Starting AWR-Miner ** ",name='status')



flog.info("Looking in this directory:",name='status')
flog.info(getwd(),name='status')
flog.info(paste0('for files that match the pattern: ',filePattern),name='status')
# print('Looking in this directory:')
# print(getwd())
# print(paste0('for files that match the pattern: ',filePattern))
Sys.sleep(2)


main <- new.env()
awrM <- new.env()
awrM$debug <- new.env()

awrM$debug.plotSummary <- data.frame()
awrM$debug.lastFunction <- NULL

awrM$debug.unitTimes <- data.frame()


attr <- new.env()
attr$filter_snap_min <- 1
attr$filter_snap_max <- 1000000000
attr$vertical_line <- theme()
attr$vertical_text <- theme()

attr$date_break_major_var <- date_breaks("1 day")
attr$date_break_minor_var <- date_breaks("12 hour")

attr$barChartWidth <- NULL



main$awrFiles <- list.files(pattern=filePattern)

flog.info(paste0("Files found (",length(main$awrFiles),"): \n",paste(main$awrFiles,collapse="\n"),"\n"), name='status')


# *UTILITY FUNCTIONS* ===============================================================================================

data_frame_col_not_null <- function(df_in, column_in, min_rows_in = 1){
  #return(TRUE)
  #foo <<- df_in
  flog.debug(paste0("data_frame_col_not_null - ",column_in),name="data_frame_col_not_null")
  
  if((column_in %in% names(df_in))){
    # the relevant column is in the data frame
    #print(nrow(df_in[!(is.na(df_in[[column_in]]))]))
    if(nrow(df_in[!(is.na(df_in[[column_in]])),]) >= min_rows_in){
      # there are at least min_rows_in number of rows in this df that are not NA
      return(TRUE)
    }
    else{
      # the column exists, but there are not min_rows_in rows that are not NA (common case would be a column with all NAs)
      return(FALSE)
    }
  }
  else{
    # the relevant column is NOT in the data frame
    return(FALSE)
  }
  # fall through, defaults to false
  return(FALSE)
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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
    return(as.numeric(as.vector(main$DF_OS[with(main$DF_OS,STAT_NAME == SEARCH_VAL),]$STAT_VALUE[1])))
  }
}

get_os_stat_string <- function(SEARCH_VAL){
  if(nrow(subset(main$DF_OS, STAT_NAME==SEARCH_VAL)) == 0)
    return(NULL)
  else
    return(as.character(as.vector(main$DF_OS[with(main$DF_OS,STAT_NAME == SEARCH_VAL),]$STAT_VALUE[[1]])))
}


convert_snap_id_to_posixct <- function(snapID){
  theDateChar <- subset(main$DF_SNAP_ID_DATE2 , snap==snapID)
  theDateReturn <- as.POSIXct(theDateChar$end, format = "%y/%m/%d %H:%M",tz="UTC")
  return(theDateReturn)
}



appender.fn <- function(lineIn) { 
  lineVars <- str_extract_all(lineIn, "\\[.+?\\]")
  
  strFormat <- function(stringIn){
    stringInternal <- str_replace_all(stringIn,"\\[|\\]|\\n","")
    return(stringInternal)
  }
  
  lineVars <- lapply(lineVars,FUN=strFormat)
  
  oppCode <- as.character("")
  
  if(str_detect(lineVars[[1]][3]," - start$")){
    oppCode <- "start"
    lineVars[[1]][3] <- str_replace(lineVars[[1]][3]," - start$","")
    awrM$debug.lastFunction <<- lineVars[[1]][3] 
  }
  
  if(str_detect(lineVars[[1]][3]," - end$")){
    oppCode <- "end"
    lineVars[[1]][3] <- str_replace(lineVars[[1]][3]," - end$","")
    awrM$debug.lastFunction <<- lineVars[[1]][3] 
  }
  
  if(debugMode){
    awrM$debug.unitTimes <<- rbind(awrM$debug.unitTimes,data.frame(db=main$current_db_name,level=lineVars[[1]][1],time=lineVars[[1]][2],message=lineVars[[1]][3],opp=oppCode))
  }
  print(lineIn,quote=FALSE)
}

if(debugMode){
  flog.appender(appender.fn)
  
  layout <- layout.format('[~l] [~t] [~m]')
  flog.layout(layout)
}


okToPrintPlot <- function(plotName){
  plotNameInt <- toupper(plotName)
  if(exists("plotOverride")){
    if(!is.null(plotOverride)){
      plotOverride <- lapply(plotOverride,function(x) toupper(x) )
      
      if(is.element('NONE', plotOverride)){
        return(FALSE)
      }
      
      
      if(is.element('ALL', plotOverride)){
        return(TRUE)
      }
      
      if(is.element('SOME', plotOverride)){
        if(is.element(plotNameInt, plotOverride)){
          return(TRUE)
        }else
        {
          return(FALSE)
        }
      }
    }
    else{
      return(TRUE)
    }
  }
  else{
    return(TRUE)
  }
}

okToParse <- function(sectionName){
  sectionNameInt <- toupper(sectionName)
  if(exists("parseOverride")){
    if(!is.null(parseOverride)){
      parseOverride <- lapply(parseOverride,function(x) toupper(x) )
      
      if(is.element('NONE', parseOverride)){
        return(FALSE)
      }
      
      
      if(is.element('ALL', parseOverride)){
        return(TRUE)
      }
      
      if(is.element(paste0("!",sectionNameInt), parseOverride)){
        return(FALSE)
      }
      
      
      if(is.element('SOME', parseOverride)){
        if(is.element(sectionNameInt, parseOverride)){
          return(TRUE)
        }else
        {
          return(FALSE)
        }
      }
      
      return(TRUE)
    }
    else{
      return(TRUE)
    }
  }
  else{
    return(TRUE)
  }
}





getCPUcores <- function(){
  cpuCoresInt <- NULL
  if(is.na(get_os_stat("NUM_CPU_CORES"))) {
    if(is.na(get_os_stat("!CPU_CORE_COUNT"))) {
      cpuCoresInt <- get_os_stat("NUM_CPUS")
    }
    else{
      cpuCoresInt <- get_os_stat("!CPU_CORE_COUNT")/get_os_stat("INSTANCES")
    }
  }
  else 
  {
    cpuCoresInt <- get_os_stat("NUM_CPU_CORES")
  }
  return(cpuCoresInt)
}



saveUniqueModules <- function(){
  tylersModDir <- 'M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/modules'
  if (file.exists(tylersModDir)){
    mods <- data.frame(module=unique(main$DF_SQL_BY_SNAPID$MODULE))
    out_file <- tempfile(pattern = "modules-", tmpdir = tylersModDir , fileext = ".csv")
    write.csv(mods,file=out_file,row.names=FALSE)
  }
  
}



#====================================================================================================================







main$db_name = vector()
main$db_id = vector()

get_db_names <- function(){
  flog.debug("get_db_name - start")
  
  fullNamePattern <- "^awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-([0-9]+)-([0-9]+)(\\.|out|gz)+$"
  str_detect('awr-hist-3846920754-RAC81P-289-1209.out',namePattern)
  
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

get_db_name <- function(fileName){
  flog.debug("get_db_names - start")
  
  fullNamePattern <- "^awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-([0-9]+)-([0-9]+)(\\.|out|gz)+$"
  
  if(str_detect(fileName,fullNamePattern)){
    namePattern <- "awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-.*"
    main$current_db_name <<- gsub(pattern = namePattern, replacement="\\2", fileName)
    main$current_dbid <<- gsub(pattern = namePattern, replacement="\\1", fileName)
  }
  else{
    theFile <- readLines(fileName,n=20)
    theFileTXT <- paste(theFile, collapse='\n')
    # unlink(theFile)
    
    getInitHeadInfo <- function(headerTxt,attrName){
      strTmp <- str_extract(headerTxt, perl(paste0('\n',attrName,'[[:space:]].+[[:alnum:]]+\n')))
      strTmp <- gsub('\n', '', strTmp)
      attrVal <- unlist(str_split(strTmp, perl("[[:space:]]+")))
      return(as.character(attrVal[2]))
    }
    
    main$current_db_name <<- getInitHeadInfo(theFileTXT,'DB_NAME')
    main$current_dbid <<- getInitHeadInfo(theFileTXT,'DBID')
  }
  
  
  flog.debug("get_db_name - stop")
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
  if(nrow(subset(main$current_plot_attributes, variable==SEARCH_VAL & db==main$current_db_name)) == 0){
    return(NA)
  }
  else{
    return(main$current_plot_attributes[with(main$current_plot_attributes,variable==SEARCH_VAL & db==main$current_db_name),])
  }
}

load_plot_attributes <- function(){
  #main$attributes_file 
  DF_ATTRIBUTES_INT <- data.frame()
  flog.debug('load_plot_attributes - start')
  if(file.exists('attributes.csv')){
    if(debugMode){
      DF_ATTRIBUTES_INT <- read.csv('attributes.csv', head=TRUE,sep=",",stringsAsFactors=FALSE)
      
    }
    else{
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
    
  }
  
  if(nrow(DF_ATTRIBUTES_INT) > 0){
    DF_ATTRIBUTES_INT <- subset(DF_ATTRIBUTES_INT,db == main$current_db_name)
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
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'date_range_filter','','')
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
    
    DF_TEMP_DATE <- NULL
    DF_TEMP_DATE <- get_attrs('date_range_filter')
    
    DATE_FILTER_FOUND <- FALSE
    
    DATE_LOW <- NULL
    DATE_HIGH <- NULL
    if(nchar(as.character(DF_TEMP_DATE[1,]$value1))>1){
      flog.debug('found begin date_range',name='apply_current_attributes')
      attr$filter_snap_min <<- date_to_snap_id(DF_TEMP_DATE$value1,'low')
      DATE_LOW <- DF_TEMP_DATE$value1
      flog.debug(paste0('filter_snap_min - ',attr$filter_snap_min),name='apply_current_attributes')
      #attr$filter_snap_min <<- as.vector(DF_TEMP$value1) 
      DATE_FILTER_FOUND <- TRUE
    }
    
    if(nchar(as.character(DF_TEMP_DATE[1,]$value2))>1){
      flog.debug('found end date_range',name='apply_current_attributes')
      attr$filter_snap_max <<- date_to_snap_id(as.character(DF_TEMP_DATE$value2),'high')
      flog.debug(paste0('filter_snap_max - ',attr$filter_snap_max),name='apply_current_attributes')
      DATE_HIGH <- DF_TEMP_DATE$value2
      #attr$filter_snap_max <<- as.vector(DF_TEMP$value1) 
      DATE_FILTER_FOUND <- TRUE
    }
    
    
    if(DATE_FILTER_FOUND){
      flog.debug('apply_current_attributes - end',name='apply_current_attributes')
      flog.info(paste0("Filtering on DATE range ",DATE_LOW," - ",DATE_HIGH ),name='status')
      return(TRUE)
    }
    
    DF_TEMP <- NULL
    DF_TEMP <- get_attrs('snap_id_filter')
    SNAP_ID_FILTER_FOUND <- FALSE
    
    SNAP_LOW <- NULL
    SNAP_HIGH <- NULL
    if(nchar(as.character(DF_TEMP[1,]$value1))>1){
      flog.debug('found begin snap',name='apply_current_attributes')
      attr$filter_snap_min <<- as.vector(DF_TEMP$value1)
      SNAP_LOW <- attr$filter_snap_min
      SNAP_ID_FILTER_FOUND <- TRUE
    }
    if(nchar(as.character(DF_TEMP[1,]$value2))>1){
      flog.debug('found end snap',name='apply_current_attributes')
      attr$filter_snap_max <<- as.vector(DF_TEMP$value2) 
      SNAP_HIGH <- attr$filter_snap_max
      SNAP_ID_FILTER_FOUND <- TRUE
    }
  }
  
  
  if(SNAP_ID_FILTER_FOUND){
    
    
    flog.debug('apply_current_attributes - end',name='apply_current_attributes')
    flog.info(paste0("Filtering on SNAP ID range ",SNAP_LOW," - ",SNAP_HIGH ),name='status')
    return(TRUE)
  }
  
  
  
  
  ## Filter by date
  
  #attr$filter_snap_min <- 220
  #attr$filter_snap_max <- 228
  min_date <- max(main$DF_MAIN$end) - days(MAX_DAYS)
  
  min_snap_id <- date_to_snap_id(min_date,'low')
  flog.debug(paste0('min_date - ',min_date),name='apply_current_attributes')
  flog.debug(paste0('min_snap_id - ',min_snap_id),name='apply_current_attributes')
  attr$filter_snap_min <<- min_snap_id
  
  flog.debug('apply_current_attributes - end',name='apply_current_attributes')
  return(TRUE)
}

set_date_break_vars <- function(DF_MAIN_IN){
  flog.debug('set_date_break_vars - start',name='set_date_break_vars')
  #numDays <- as.numeric(max(DF_MAIN_IN$end)-min(DF_MAIN_IN$end))
  numDays <- difftime(max(main$DF_MAIN$end), min(main$DF_MAIN$end), unit="days")
  
  flog.trace(paste0('numDays: ',numDays),name='set_date_break_vars')
  
  if(numDays > 60){
    attr$date_break_major_var <<- date_breaks("7 day")
    attr$date_break_minor_var <<- date_breaks("1 day")
  }
  else if (numDays > 31 & numDays <= 60){
    attr$date_break_major_var <<- date_breaks("2 day")
    attr$date_break_minor_var <<- date_breaks("1 day")
  }
  else if (numDays > 4 & numDays <= 31){
    attr$date_break_major_var <<- date_breaks("1 day")
    attr$date_break_minor_var <<- date_breaks("12 hour")
  }
  else if (numDays > 1 & numDays <= 4){
    attr$date_break_major_var <<- date_breaks("3 hour")
    attr$date_break_minor_var <<- date_breaks("1 hour")
  }  
  else{
    attr$date_break_major_var <<- date_breaks("1 hour")
    attr$date_break_minor_var <<- date_breaks("1 hour")
  }
  
  flog.debug('set_date_break_vars - end',name='set_date_break_vars')
  
  return(TRUE)
  
}


date_format_tz <- function(format = "%a, %b %d %I %p", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}

#DF_ATTR <- load_plot_attributes()
#DF_ATTR <- generate_plot_attributes()
#====================================================================================================================





# *DATA LOADING AND MANIPULATION FUNCTIONS* =========================================================================


save_parsed_data <- function(){
  flog.debug('save_parsed_data - start',name='save_parsed_data')
  main.save <- new.env()
  
  for (objName in ls(main)) {
    tmp <- get(objName,envir=main)
    if(inherits(tmp,what='data.frame')){
      if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
       # print(objName)
        main.save[[objName]] <- tmp
      }
    }
    rm(tmp)
  }
  
  if(exists("parseOverride")){
    main.save$parseOverride <- parseOverride
  }
  
  snap_id_min <- as.numeric(min(main.save$DF_MAIN$snap))
  snap_id_max <- as.numeric(max(main.save$DF_MAIN$snap))
  
  out_file_name <- paste0(main$current_db_name,"-",main$current_dbid,"-",snap_id_min,"-",snap_id_max)
  save(main.save,file=paste0(out_file_name,"-parsed.Rda"))
  #head(main.save$DF_AAS)
  #ls(main.save)
  flog.debug('save_parsed_data - end',name='save_parsed_data')
}



parsed_data_exists <- function(fileNameIn){
  flog.debug('parsed_data_exists - start',name='parsed_data_exists')
  if(!(str_detect(fileNameIn,fullNamePattern))){
    flog.debug("File name does not conform to standard")
    return(FALSE)
  }
  else{
    namePattern <- "awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-([0-9]+)-([0-9]+).*"
    fileDBID <- gsub(pattern = namePattern, replacement="\\1", fileNameIn)
    fileDBName <- gsub(pattern = namePattern, replacement="\\2", fileNameIn)
    fileSnapMin <- as.numeric(gsub(pattern = namePattern, replacement="\\3", fileNameIn))
    fileSnapMin.1 <- as.numeric(gsub(pattern = namePattern, replacement="\\3", fileNameIn))+1
    fileSnapMax <- gsub(pattern = namePattern, replacement="\\4", fileNameIn)
    
    #     print(fileDBID)
    #     print(fileDBName)
    #     print(fileSnapMin)
    #     print(fileSnapMax)
    
    # DHLS-1376736500-100-412-parsed.Rda
    parsedFileName <- paste0(fileDBName,"-",fileDBID,"-",fileSnapMin,"-",fileSnapMax,"-parsed.Rda")
    parsedFileName.1 <- paste0(fileDBName,"-",fileDBID,"-",fileSnapMin.1 ,"-",fileSnapMax,"-parsed.Rda")
    #print(parsedFileName)
    flog.debug(paste0("Looking for parsed file: ",parsedFileName),name='parsed_data_exists')
    flog.debug('parsed_data_exists - end',name='parsed_data_exists')
    if (file.exists(parsedFileName)){
      flog.info(paste0("Found parsed data file ",parsedFileName),name='status')
      return(parsedFileName)
      
    }
    else if (file.exists(parsedFileName.1)){
      flog.info(paste0("Found parsed data file ",parsedFileName.1),name='status')
      return(parsedFileName.1)
    }
    else{
      flog.debug("No parsed data found")
      return(FALSE)
    }
  }
}

load_parsed_data <- function(fileNameIn){
  flog.debug('load_parsed_data - start',name='load_parsed_data')
  load(fileNameIn)
  for (objName in ls(main.save)) {
    tmp <- get(objName,envir=main.save)
    if(inherits(tmp,what='data.frame')){
      if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
      
        flog.debug(paste0(objName,' - ',nrow(tmp)),name='load_parsed_data')
      
        main[[objName]] <<- tmp
      }
    }
    rm(tmp)
  }
  flog.debug('load_parsed_data - end',name='load_parsed_data')
  return(TRUE)
}


filter_parsed_data <- function(snapIdMinIn,snapIdMaxIn){
  flog.debug('filter_parsed_data - start',name='filter_parsed_data')
  for (objName in ls(main)) {
    tmp <- get(objName,envir=main)
    if(inherits(tmp,what='data.frame')){
      if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
      #if((objName %in% c('DF_MAIN','DF_AAS'))){        
           
        flog.debug(paste0(objName,' - ',nrow(tmp)),name='filter_parsed_data')
        if(data_frame_col_not_null(tmp,'SNAP_ID')){
          #print('SNAP_ID')
          #tmp <- tmp[SNAP_ID >= snapIdMinIn & SNAP_ID <= snapIdMaxIn]
          tmp <- subset(tmp,SNAP_ID >= snapIdMinIn & SNAP_ID <= snapIdMaxIn)
        } else if(data_frame_col_not_null(tmp,'snap')){
          #print('snap')
          #tmp <- tmp[snap >= snapIdMinIn & snap <= snapIdMaxIn]
          tmp <- subset(tmp,snap >= snapIdMinIn & snap <= snapIdMaxIn)
        }
        else{
          NULL
        }
        
        
        flog.debug(paste0(objName,' - ',nrow(tmp)),name='filter_parsed_data')
        main[[objName]] <<- tmp
      }
    }
    rm(tmp)
  }
  flog.debug('filter_parsed_data - end',name='filter_parsed_data')
}


date_to_snap_id <- function(dateIn,lowHigh = 'low'){
  dateInPOSIXct <- NULL
  
  # try YMD first
  dateInPOSIXct <- try(ymd(paste0(dateIn," UTC"),quiet=TRUE),silent=TRUE)
  
  if((is.na(dateInPOSIXct)) | (!(inherits(dateInPOSIXct,what='POSIXct')))){
    # if it wasn't YMD, try YMD_HMS 
    dateInPOSIXct <- try(ymd_hms(paste0(dateIn," UTC"),quiet=TRUE),silent=TRUE)
    # print("ymd_hms")
  }
  
  if((is.na(dateInPOSIXct)) | (!(inherits(dateInPOSIXct,what='POSIXct')))){
    return(FALSE)
  }
  
  if(lowHigh == 'low'){
    dateInPOSIXct <- floor_date(dateInPOSIXct,"hour")
    snapReturn <- as.numeric(head(subset(main$DF_SNAP_ID_DATE,end >= dateInPOSIXct),n=1)$SNAP_ID)
    return(snapReturn)
    
  }
  else{
    dateInPOSIXct <- ceiling_date(dateInPOSIXct,"hour")
    snapReturn <- as.numeric(tail(subset(main$DF_SNAP_ID_DATE,end <= dateInPOSIXct),n=1)$SNAP_ID)
    return(snapReturn)
  }
}






getSectionInt <- function(inFile,blockName,decSep='.',searchPatternIn=NULL,replacePatternIn=NULL){
  flog.debug(paste0("getSection - ",blockName," - start"),name="getSection")
  beginBlock <- paste0('~~[ ]*BEGIN-',blockName,'[ ]*~~')
  endBlock <- paste0('~~[ ]*END-',blockName,'[ ]*~~')
  thePattern <- paste0(beginBlock,'(.*)',endBlock)
  body <- str_extract(inFile, thePattern)
  #print(str(body))
  if(is.na(body)){
    flog.debug(paste0("getSection - ",blockName," - section not in capture"),name="getSection")
    return(data.frame())
  }
  
  if(str_detect(body, 'table not in this version')){
    flog.trace("table not in this version",name="getSection")
    dfInt <- data.frame()
    #flog.debug(paste0("getSection - ",blockName," - end"),name="getSection")
    return(dfInt)
  }
  
  
  
  body <- gsub('\n\n', '\n', body)
  #
  body <- gsub('~~.+~~\n','\n', body)
  body <- gsub('\n~~.+~~','\n', body)
  body <- gsub('\n\n', '\n', body)
  
  theBodyRaw <<- body
  # Get the dashes which we'll use to get a vector of column lengths
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
  dashesTest <- str_extract(body, '\n[- ]+\n')
  if(is.na(dashesTest)){
    body <- gsub('\n[= ]+\n', '\n', body)
  }
  else{
    body <- gsub('\n[- ]+\n', '\n', body)
  }
  
  
  
  
 
  
  body <- str_replace_all(body,theTitlesOrig,'')
  theBody <<- body
  numRows <- str_count(body,'\n')
  #numRows <- 10
  #theBody <<- body
  theColNames <<- theColNames
  theWidths <<- dashesVectorLengths
  #dfInt = read.table(text=body,header = TRUE,skip=0,nrows=numRows,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,dec=decSep)
  dfInt = read.fwf(file=textConnection(body),skip=0,nrows=numRows,col.names=theColNames,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,widths=unlist(dashesVectorLengths[[1]]),dec=decSep)
  #dfInt = read_fwf(file=textConnection(body),skip=0,n_max=numRows,col_names=theColNames,fill=TRUE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,widths=unlist(dashesVectorLengths[[1]]))
  #dfInt <- read_table(body,col_names=theColNames)
  
  #print(head(dfInt))
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
  
  
  flog.debug(paste0("getSection - ",blockName," - end"),name="getSection")
  #dfInt <- na.omit(dfInt)
  
  #write.csv(dfInt,paste0(main$current_db_name,"-",blockName,"-RAW.csv"))
  #write.csv(summary(dfInt),paste0(main$current_db_name,"-",blockName,"-SUMMARY.csv"))
  return(dfInt)
}


getSection <- function(inFile,blockName,decSep='.',searchPatternIn=NULL,replacePatternIn=NULL){
  df_int_try <- data.frame()
  if(!okToParse(blockName)){
    return(df_int_try)
  }
  
  if(debugMode){
    df_int_try <- getSectionInt(inFile,blockName,decSep,searchPatternIn,replacePatternIn)
  }
  else{
    
    tryCatch(df_int_try <- getSectionInt(inFile,blockName,decSep,searchPatternIn,replacePatternIn), 
             error = function(e) {
               #traceback()
               return(df_int_try)
               #flog.remove(name=main$current_db_name)
               #browser()
               
             }
             #,finally=print("finished")
    )
  }
  return(df_int_try)
}



get_capture_times <- function(inFileTXT){
  
  timingVector <- str_extract_all(inFileTXT, "~~END.+?\nElapsed.+?\n")
  timingVector <- unlist(timingVector)
  
  if(length(timingVector) < 3){
    flog.debug("No capture timing available")
    return(data.frame())
  }
  
  timingVector <- gsub('~~END\\-(.+)~~\n','\\1\n', timingVector)
  timingVector <- gsub('Elapsed: ','', timingVector)
  
  timing_DF <- colsplit(timingVector, "\n", names=c("section", "elapsed"))
  options(digits.secs=2)
  timing_DF$seconds <- second(strptime(timing_DF$elapsed,"%H:%M:%OS"))
  #head(timing_DF)
  
  
  #timing_DF$seconds <- seconds(hms(timing_DF$elapsed))
  
  #foo <- sum(timing_DF$seconds)
  
  return(timing_DF)
}

build_data_frames <- function(fileName) {
  flog.debug("build_data_frames - start",name="build_data_frames")
  flog.info("-",name='status')
  flog.info(paste0("dbname-dbid: ",main$current_db_name,'-',main$current_dbid),name="build_data_frames")
  flog.info(paste0("dbname-dbid: ",main$current_db_name,'-',main$current_dbid),name='status')
  flog.info(fileName,name='status')
  flog.info(paste0("File size (kb): ",round(file.info(fileName)[1,"size"]/1024)),name='status')
  theFile <- readLines(fileName)
  #theFile <- read_lines(fileName)
  theFileTXT <- paste(theFile,collapse="\n")
  flog.debug(length(theFile))
  #unlink(theFile)
  rm(theFile)
  
  
  numSections <- str_count(theFileTXT, "~~[ ]*BEGIN")
  flog.info(paste0("Number of sections: ",numSections),name='status')
  
  oraErrors <- str_count(theFileTXT, perl("\nERROR at line .+\n"))
  
  
  if(oraErrors > 0){
    flog.info(paste0("Capture file has ",oraErrors," errors"),name='status')
    
    #if(debugMode){
    errorDir <- paste0(dirname(fileName),"/error/")
    if(!(file.exists(errorDir))){
      dir.create(errorDir,showWarnings = FALSE)
    }
    
    file.copy(fileName, paste0(dirname(fileName),"/error/",basename(fileName)))
              
              #file.rename(from=fileName,
              #           to=paste0(dirname(fileName),"/error/",basename(fileName))
    #)
    #}
    
    if(debugMode){
      
    }
    else{
      stop(paste0("Capture file has ",oraErrors," errors"))   
    }
    
    
  }
  
  
  main$capture_times <<- get_capture_times(theFileTXT)
  
  
  DF_TEMP <- getSection(theFileTXT,'MEMORY')
  countDecimals <- sum(str_count(DF_TEMP$PGA,'\\.'))+sum(str_count(DF_TEMP$SGA,'\\.'))
  countCommas <- sum(str_count(DF_TEMP$PGA,'\\,'))+sum(str_count(DF_TEMP$SGA,'\\,'))
  
  computedDecSep <- '.'
  if(countCommas>countDecimals){
    computedDecSep <- ','
  }
  
  main$currentComputedDecSep <- computedDecSep
  
  DATA_FRAME_INT <- NULL
  #file_pattern=paste(WORK_DIR,paste(dbname,dbid,sep="-"),sep="/")
  DF_OS_INT <- getSection(theFileTXT,'OS-INFORMATION',computedDecSep)
  
  DF_OS_INT[with(DF_OS_INT, grepl("PHYSICAL_MEMORY_GB", STAT_NAME)),]$STAT_VALUE<-gsub(",", ".",
                                                                                       DF_OS_INT[with(DF_OS_INT, grepl("PHYSICAL_MEMORY_GB", STAT_NAME)),]$STAT_VALUE)
  
  
  flog.trace('DF_OS_INT',DF_OS_INT,name="build_data_frames",capture=TRUE)
  DF_MEMORY_INT <- getSection(theFileTXT,'MEMORY',computedDecSep)
  
  DF_MEMORY_INT <- getSection(theFileTXT,'MEMORY',computedDecSep)
  
  DF_MEMORY_SGA_ADVICE_INT <- getSection(theFileTXT,'MEMORY-SGA-ADVICE',computedDecSep)
  DF_MEMORY_PGA_ADVICE_INT <- getSection(theFileTXT,'MEMORY-PGA-ADVICE',computedDecSep)
  
  #flog.trace('DF_MEMORY_INT',DF_MEMORY_INT,name="build_data_frames",capture=TRUE)
  DF_SPACE_INT <- getSection(theFileTXT,'SIZE-ON-DISK',computedDecSep)
  DF_MAIN_INT <- getSection(theFileTXT,'MAIN-METRICS',computedDecSep)
  
  DF_MAIN_INT <- data.table(DF_MAIN_INT)
  flog.trace("YEP0.1",name="build_data_frames")
  DF_DB_PARAMETERS_INT <- getSection(theFileTXT,'DATABASE-PARAMETERS',computedDecSep)
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
  #DF_AAS_INT <- getSection(theFileTXT,'AVERAGE-ACTIVE-SESSIONS',computedDecSep,searchPattern,replacePattern)
  DF_AAS_INT <- getSection(theFileTXT,'AVERAGE-ACTIVE-SESSIONS',computedDecSep)
  DF_AAS_INT <- data.table(DF_AAS_INT)
  
  flog.trace("YEP2",name="build_data_frames")
  DF_AAS_INT$SNAP_ID <- as.numeric(DF_AAS_INT$SNAP_ID)
  DF_AAS_INT$AVG_SESS <- as.numeric(DF_AAS_INT$AVG_SESS)
  idx_aas_ltz_rm <- with(DF_AAS_INT,AVG_SESS >= 0)
  DF_AAS_INT<- DF_AAS_INT[idx_aas_ltz_rm,]
  
  setkey(DF_AAS_INT, "WAIT_CLASS","AVG_SESS","SNAP_ID") 
  flog.trace("DF_AAS_INT1",DF_AAS_INT,name="build_data_frames",capture=TRUE)
  
  
  DF_TOP_N_EVENTS_INT <- getSection(theFileTXT,'TOP-N-TIMED-EVENTS',computedDecSep)
  
  searchPattern <- "\n([[:digit:] ]{10}) ([[:print:] ]{20}) ([[:print:] ]{37}) ([[:print:] ]{15}) ([[:print:] ]{10})"
  replacePattern <- "\n'\\1' '\\2' '\\3' '\\4' '\\5' "
  DF_IO_WAIT_HIST_INT <- getSection(theFileTXT,'IO-WAIT-HISTOGRAM',computedDecSep,searchPattern,replacePattern)
  DF_IO_WAIT_HIST_INT$SNAP_ID <- as.numeric(DF_IO_WAIT_HIST_INT$SNAP_ID)
  DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI <- as.numeric(DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI)
  DF_IO_WAIT_HIST_INT$WAIT_COUNT <- as.numeric(DF_IO_WAIT_HIST_INT$WAIT_COUNT)
  
  DF_IOSTAT_FUNCTION_INT <- getSection(theFileTXT,'IOSTAT-BY-FUNCTION',computedDecSep)
  
  DF_IO_BY_OBJECT_TYPE_INT <- getSection(theFileTXT,'IO-OBJECT-TYPE',computedDecSep)
  
  
  DF_OSSTAT_INT <- getSection(theFileTXT,'OSSTAT',computedDecSep)
  
  DF_SQL_SUMMARY_INT <- getSection(theFileTXT,'TOP-SQL-SUMMARY',computedDecSep)
  
  DF_SQL_SUMMARY_INT_TMP <<- DF_SQL_SUMMARY_INT
  if(nrow(DF_SQL_SUMMARY_INT)>5){
    if(("MODULE" %in% names(DF_SQL_SUMMARY_INT))){
      #if(data_frame_col_not_null(DF_SQL_SUMMARY_INT,"MODULE")){
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
  }
  
  DF_SQL_BY_SNAPID_INT <- getSection(theFileTXT,'TOP-SQL-BY-SNAPID',computedDecSep)
  
  rm(theFileTXT)
  
  
  # Normalize dates by snap_id
  
  
  options(tz="")
  
  DF_SNAP_ID_DATE_INT <- ddply(DF_MAIN_INT, .(snap), summarise, 
                               end=min(as.POSIXct(end,format="%y/%m/%d %H:%M",tz="UTC")),
                               dur_m=max(dur_m))
  
  DF_SNAP_ID_DATE_INT <- data.table(DF_SNAP_ID_DATE_INT)
  
  #names(DF_SNAP_ID_DATE_INT)[names(DF_SNAP_ID_DATE_INT)=="snap"] <- "SNAP_ID"
  
  setnames(DF_SNAP_ID_DATE_INT,"snap","SNAP_ID")
  DF_SNAP_ID_DATE_INT$SNAP_ID <- as.numeric(DF_SNAP_ID_DATE_INT$SNAP_ID)
  # Tyler just added to fix issue with hours_bars
  #DF_MAIN_INT$end <- as.POSIXct(strptime(DF_MAIN_INT$end,format="%y/%m/%d %H:%M",tz=""),tz="")
  #DF_MAIN_INT$end <- as.POSIXct(strptime(DF_MAIN_INT$end,format="%y/%m/%d %H:%M",tz=""),tz="")
  DF_MAIN_INT$end <- as.POSIXct(DF_MAIN_INT$end,format="%y/%m/%d %H:%M",tz="UTC")
  
  # load / generate attributes (attributes.csv) so we can use them to filter
  
  
  
  
  setkey(DF_SNAP_ID_DATE_INT,SNAP_ID,end)
  setkey(DF_AAS_INT,SNAP_ID)
  setkey(DF_MAIN_INT,snap,end)
  
  

  

  
  #DF_SNAP_ID_DATE_INT<-filter_n_days(DF_SNAP_ID_DATE_INT)
  #DF_SNAP_ID_DATE_INT <- subset(DF_SNAP_ID_DATE_INT, SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max)
  
  numDaysTotal <- difftime(max(DF_MAIN_INT$end), min(DF_MAIN_INT$end), unit="days")

  DF_OS_INT <- rbind(DF_OS_INT,data.frame(STAT_NAME='DAYS',STAT_VALUE=round(numDaysTotal,1)))
  DF_OS_INT <- rbind(DF_OS_INT,data.frame(STAT_NAME='DAYS_FILTERED',STAT_VALUE=NA))
  
  flog.trace("DF_AAS_INT2.1",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  
  #   DF_AAS_INT<-data.frame(DF_AAS_INT)
  #DF_AAS_INT<-filter_n_days(DF_AAS_INT)
  #DF_TOP_N_EVENTS_INT<-filter_n_days(DF_TOP_N_EVENTS_INT)
  flog.trace("DF_AAS_INT2.2",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  #DF_MEMORY_INT<-filter_n_days(DF_MEMORY_INT)
  #DF_MEMORY_SGA_ADVICE_INT<-filter_n_days(DF_MEMORY_SGA_ADVICE_INT)
  #DF_MEMORY_PGA_ADVICE_INT<-filter_n_days(DF_MEMORY_PGA_ADVICE_INT)
  
  
  #DF_SQL_BY_SNAPID_INT<-filter_n_days(DF_SQL_BY_SNAPID_INT)
  
  flog.trace("DF_SNAP_ID_DATE_INT1",head(DF_SNAP_ID_DATE_INT),name="build_data_frames",capture=TRUE)
  flog.trace("DF_AAS_INT2.3",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  
  flog.trace("DF_SNAP_ID_DATE_INT1",str(DF_SNAP_ID_DATE_INT),name="build_data_frames",capture=TRUE)
  flog.trace("DF_AAS_INT2.3",str(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  
  #DF_AAS_INT<-data.frame(DF_AAS_INT)
  #DF_SNAP_ID_DATE_INT<-data.frame(DF_SNAP_ID_DATE_INT)
  flog.trace("DF_AAS_INT2.4",head(DF_AAS_INT),name="build_data_frames",capture=TRUE)
  DF_AAS_INT <- merge(DF_AAS_INT,DF_SNAP_ID_DATE_INT)
  
  DF_MEMORY_INT <- merge(DF_MEMORY_INT,DF_SNAP_ID_DATE_INT)
  DF_MEMORY_SGA_ADVICE_INT <- merge(DF_MEMORY_SGA_ADVICE_INT,DF_SNAP_ID_DATE_INT)
  DF_MEMORY_PGA_ADVICE_INT <- merge(DF_MEMORY_PGA_ADVICE_INT,DF_SNAP_ID_DATE_INT)
  
  
  DF_SQL_BY_SNAPID_INT <- merge(DF_SQL_BY_SNAPID_INT,DF_SNAP_ID_DATE_INT)
  #print(head(DF_AAS_INT))
  
  flog.trace("DF_AAS_INT3",DF_AAS_INT,name="build_data_frames",capture=TRUE)
  
  #if(("WAIT_CLASS" %in% names(DF_AAS_INT))){
  if(data_frame_col_not_null(DF_AAS_INT,"WAIT_CLASS")){
    DF_AAS_INT[with(DF_AAS_INT, grepl("DB CPU", WAIT_CLASS)),]$WAIT_CLASS<-"CPU"
  }
  # due to a bug in the 2.7 sql script
  #DF_AAS_INT[with(DF_AAS_INT, grepl("Administrati", WAIT_CLASS)),]$WAIT_CLASS<-"Administrative"
  
  #min_snap_id <- min(subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))$snap)
  #DF_MAIN_INT <- subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))
  # tyler changed for Pearson
  
  #flog.debug(summary(DF_AAS_INT))
  
  #print(head(DF_OS_INT))
  flog.debug("build_data_frames - end",name="build_data_frames")
  return(list(DF_OS_INT,DF_MAIN_INT,DF_OSSTAT_INT,
              DF_MEMORY_INT,DF_MEMORY_SGA_ADVICE_INT,DF_MEMORY_PGA_ADVICE_INT,
              DF_SPACE_INT,DF_AAS_INT,DF_SQL_SUMMARY_INT,DF_SQL_BY_SNAPID_INT,DF_SNAP_ID_DATE_INT,
              DF_IO_WAIT_HIST_INT,DF_IOSTAT_FUNCTION_INT,
              DF_DB_PARAMETERS_INT,DF_IO_BY_OBJECT_TYPE_INT,DF_TOP_N_EVENTS_INT))
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
                               #read_iops_direct=sum(read_iops_direct),
                               #read_iops_direct_max=sum(read_iops_direct_max),
                               write_iops=sum(write_iops),
                               write_iops_max=sum(write_iops_max),
                               #write_iops_direct=sum(write_iops_direct),
                               #write_iops_direct_max=sum(write_iops_direct_max),
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
                               aas=sum(aas),
                               dur_m=max(dur_m)) 
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
                            cpu    = max(os_cpu),
                            read_iops = sum(read_iops), write_iops = sum(write_iops),
                            read_mb_s = sum(read_mb_s), write_mb_s = sum(write_mb_s),
                            logons_total=sum(logons_total),
                            exec_s=sum(exec_s),commits_s=sum(commits_s),
                            aas=sum(aas))
  
  if(is.na(sum(DF_MAIN_INT_SUM1$aas))){
    DF_MAIN_INT_SUM1 <- subset( DF_MAIN_INT_SUM1, select = -aas )
    
    DF_AAS_TMP <- ddply(main$DF_AAS, .(SNAP_ID), summarise, aas=sum(AVG_SESS) )
    #DF_AAS_TMP <- rename(DF_AAS_TMP, replace=c("SNAP_ID" = "snap" ))
    #names(d) <- sub("^alpha$", "one", names(d))
    names(DF_AAS_TMP) <- sub("^SNAP_ID$", "snap", names(DF_AAS_TMP))
    
    DF_MAIN_INT_SUM1 <- merge(DF_MAIN_INT_SUM1,DF_AAS_TMP)
  }
  
  
  DF_MAIN_INT_SUM <- ddply(DF_MAIN_INT_SUM1,.(), summarise,
                           cpu    = max(cpu),
                           r_iops = max(read_iops), w_iops = max(write_iops),
                           r_mb_s = max(read_mb_s), w_mb_s = max(write_mb_s),
                           logons_total = max(logons_total),
                           "logons/core"=0,
                           exec_s = max(exec_s),
                           commits_s = max(commits_s), aas = max(aas)
  )
  
  DF_MAIN_INT_SUM$"logons/core" <- round(DF_MAIN_INT_SUM$logons_total/main$cpu_cores,0)
  # round the whole data frame
  numVars <- sapply(DF_MAIN_INT_SUM, is.numeric)
  DF_MAIN_INT_SUM[numVars] <- lapply(DF_MAIN_INT_SUM[numVars], round, digits = 1)
  
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
                          #days=get_os_stat("DAYS"),
                          days=main$DAYS_OF_DATA,
                          "days shown"=get_os_stat("DAYS_FILTERED"),
                          snap.min=Mode(main$DF_SNAP_ID_DATE$dur_m),
                          AWR.Miner.Capture=get_os_stat_string("AWR_MINER_VER"),
                          AWR.Miner.Graph=awrMinerPlotVersion
  )
  
  if(nrow(main$capture_times)>0){
    DF_OS_INT$Capture.Sec <- sum(main$capture_times$seconds)
  }
  
  
  # begin 10g fixup for missing data if missing and if we have it
  if((get_os_stat_string("PLATFORM_NAME") == "None") & !is.null(get_os_stat_string("!PLATFORM_NAME"))){
    DF_OS_INT$platform <- get_os_stat_string("!PLATFORM_NAME")
  }
  
  fixupCPUs <- function(metricName,fixupMetricName){
    if(nrow(subset(main$DF_OS,STAT_NAME == metricName)) > 0){
      if(is.na(get_os_stat_string(metricName)) | is.null(get_os_stat_string(metricName))){
        if(!is.null(get_os_stat_string(fixupMetricName)) & !is.na(get_os_stat_string(fixupMetricName))){
          main$DF_SUMMARY_OS[,metricName] <<- get_os_stat_string(fixupMetricName)
        }
      }
    }
  }
  
  
  
  # end 10g fixup
  
  
  
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
  
  flog.debug('gen_summary_data - end')
  return(list(DF_OS_INT,DF_SUM_COMBINED_INT,DF_SUM_COMBINED_INT2))
  
}


computeBarChartWidth <- function(){
  
  
  snapIntervalMode <- Mode(main$DF_SNAP_ID_DATE$dur_m)
  # print(paste0("snapIntervalMode-",snapIntervalMode))
  barChartWidthFudgeFactor <- NULL
  
  if(snapIntervalMode < 20){
    barChartWidthFudgeFactor <- 0.15
  }
  else{
    barChartWidthFudgeFactor <- 0.05
  }
  
  #barWidth <- as.numeric(median(main$DF_SNAP_ID_DATE$dur_m))
  #barWidth <- barWidth + (barWidth*0.1)
  

  barChartWidth <- snapIntervalMode*60
  barChartWidth <- barChartWidth + (barChartWidth * barChartWidthFudgeFactor)
  attr$barChartWidth <<- barChartWidth
  
#   main$DF_SNAP_ID_DATE$barChartWidth <- main$DF_SNAP_ID_DATE$dur_m * 60
#   main$DF_SNAP_ID_DATE$barChartWidth <- ifelse(main$DF_SNAP_ID_DATE$dur_m < 20,
#                                                main$DF_SNAP_ID_DATE$barChartWidth + (main$DF_SNAP_ID_DATE$barChartWidth *  0.15),
#                                                main$DF_SNAP_ID_DATE$barChartWidth + (main$DF_SNAP_ID_DATE$barChartWidth *  0.05))
#   
#   
#   main$DF_SNAP_ID_DATE$barChartWidth <- as.numeric(main$DF_SNAP_ID_DATE$barChartWidth)
  
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
  if(debugMode){
    debugVars$boxplot$melt <- x.melt
  }
  
  # add a "stat" column so we can facet by stat for avg-max
  #   x.melt$stat <- "Avg"  
  #   idx_max <- with(x.melt, grepl("max", variable))
  #   x.melt[idx_max,]$stat <- "Max"
  
  if(is.na(sum(subset(x.melt,variable == 'aas')$value))){
    idx_aas_rm <- !with(x.melt, variable == 'aas')
    x.melt<- x.melt[idx_aas_rm,]
    DF_AAS.melt1 <- ddply(main$DF_AAS, .(end), summarise, value=sum(AVG_SESS) )
    DF_AAS.melt1$variable <- "aas"
    DF_AAS.melt1$L1 <- 1
    x.melt <- rbind(DF_AAS.melt1,x.melt)
  }
  
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
  
  if(is.na(sum(subset(x.melt,variable == 'Avg Act Sessions')$value))){
    idx_aas_rm <- !with(x.melt, variable == 'Avg Act Sessions')
    x.melt<- x.melt[idx_aas_rm,]
  }
  
  
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
    #geom_jitter(alpha=.2,size=1,position = position_jitter(width = .2,height=0),aes(colour="gray"))+
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



#DF_AAS <- data.table()


plot_aas_chart <- function(DF_AAS_INT){
  flog.debug('plot_aas_chart - start',name='plot_aas_chart')
  
  
  DF_AAS_INT$WAIT_CLASS <- str_trim(DF_AAS_INT$WAIT_CLASS)
  DF_AAS_INT$WAIT_CLASS <- factor(DF_AAS_INT$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
  
  
  vals <- expand.grid(end = unique(DF_AAS_INT$end),
                      WAIT_CLASS = unique(DF_AAS_INT$WAIT_CLASS))
  vals <- data.table(vals)
  
  DF_AAS_INT <- merge(vals,DF_AAS_INT,by=c("end","WAIT_CLASS"))
  
  
  rm(vals)
  
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
  
  
  snapIntervalMode <- Mode(main$DF_SNAP_ID_DATE$dur_m)
  # print(paste0("snapIntervalMode-",snapIntervalMode))
  barChartWidthFudgeFactor <- NULL
  
  if(snapIntervalMode < 20){
    barChartWidthFudgeFactor <- 0.15
  }
  else{
    barChartWidthFudgeFactor <- 0.05
  }
  
  #barWidth <- as.numeric(median(main$DF_SNAP_ID_DATE$dur_m))
  #barWidth <- barWidth + (barWidth*0.1)
  
  DF_AAS_INT$barChartWidth <- DF_AAS_INT$dur_m*60
  DF_AAS_INT$barChartWidth <- DF_AAS_INT$barChartWidth + (DF_AAS_INT$barChartWidth * barChartWidthFudgeFactor)
  
  #DF_AAS <<- DF_AAS_INT
  
  plot_aas_wait_class <- ggplot()+
    main$gg_hour_bars+
    geom_area(data=DF_AAS_INT, aes(x = end, y = AVG_SESS,
                                   fill = WAIT_CLASS),stat = "identity", position = "stack",alpha=.95)+
#     geom_bar(data=DF_AAS_INT, aes(x = end, y = AVG_SESS,
#                                   fill = WAIT_CLASS,width=barChartWidth),stat = "identity", position = "stack")+
   # geom_bar(data=DF_AAS_INT, aes(x = end, y = AVG_SESS,
    #                              fill = WAIT_CLASS),stat = "identity", position = "stack")+
    gg_aas_colors+
    theme(axis.title.x  = element_blank(),axis.title.y  = element_blank(),
          legend.key.size =    unit(0.6, "lines"))+
    labs(title=paste("Average Active Sessions (AAS) for ",main$current_db_name,sep=""))+
    
    geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5)+
    geom_text(data=main$DF_SNAP_ID_SUBSET,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,alpha=0.2,hjust=-0.1,vjust=0.7)+                                     
    geom_point(data=main$DF_SNAP_ID_SUBSET,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0)+
    cpu_cores_line+
    attr$vertical_line + attr$vertical_text +
    geom_text(data=df_cpu_cores_label, aes(x=end, y=AVG_SESS,label=paste0("CPU Cores - ",main$cpu_cores)),size=2, vjust=-.8, hjust=.5,color="red",alpha=0.4)+
    #scale_x_datetime(breaks=NULL)+
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_AAS_INT$end),max(DF_AAS_INT$end))
    )
  
  
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
  
  #SUMMARY_DF_TMP <- SUMMARY_DF_TMP[order(SUMMARY_DF_TMP$aas,decreasing = TRUE),]
  
  
  plot_aas_wait_class2_line <- ggplot()+
    #geom_line(data=DF_AAS_INT2, aes(x = end, y = AVG_SESS,color=WAIT_CLASS),stat = "identity",alpha=1,size=.7) +
    geom_bar(data=DF_AAS_INT, aes(x = end, y = AVG_SESS,fill=WAIT_CLASS),stat = "identity") +
    scale_color_manual("", values = aas_colors)+
    #geom_point(data=max_vals2, aes(x=end, y=AVG_SESS), size=2, shape=21)+
    #geom_text(data=max_vals2, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.4, hjust=1)+
    #facet_grid(WAIT_CLASS ~ . )+
    main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    #scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
    scale_x_datetime(breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var
                     #limits = c(min(DF_AAS_INT2$end),max(DF_AAS_INT2$end))
    )+
    theme(axis.title.x  = element_blank(),axis.title.y  = element_blank(),axis.text.x = element_blank())+
    labs(title=paste(paste("Average Active Sessions by Wait Class - ",main$current_db_name,sep=""),"\n Only Values >= 95th Percentile",sep=""))+
    #geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,alpha=0.2,hjust=-0.15,vjust=2.2)+
    #geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0)+
    #ylim(0,ymax2)+
    theme(legend.position="none")
  
  plot_aas_wait_class2 <- plot_aas_wait_class2_line+
    facet_grid(WAIT_CLASS ~ . )+theme(legend.position="none")
  
  plot_aas_wait_class2_line <- plot_aas_wait_class2_line+cpu_cores_line
  
  
  
  
  flog.debug('plot_aas_chart - end',name='plot_aas_chart')
  #return(list(plot_aas_wait_class_gt,plot_aas_wait_class2_gt,plot_aas_wait_class2_line))
  return(list(plot_aas_wait_class,plot_aas_wait_class2,plot_aas_wait_class2_line))
  
  
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
    theme(legend.key.size =    unit(0.6, "lines"))+
    theme(legend.position="right")
  
  
  
  
  
  
  
  
  flog.debug('plot_aas_bars_by_date - end',name='plot_aas_bars_by_date')
  return(aas_bar_plot)
}


plot_aas_percent <- function(DF_AAS_INT){
  flog.debug('plot_aas_percent - start',name='plot_aas_percent')
  
  
  #New AAS Percent Section
  
  avgSessTotal <- sum(DF_AAS_INT$AVG_SESS)
  DF_AAS_AGG_BY_SNAP <- DF_AAS_INT[, list(AVG_SESS = sum(AVG_SESS)), by = list(SNAP_ID)]
  
  DF_AAS_AGG <- DF_AAS_INT[, list(AVG_SESS = sum(AVG_SESS)), by = list(WAIT_CLASS)]
  DF_AAS_AGG$AVG_SESS_PCT <- round(DF_AAS_AGG$AVG_SESS/avgSessTotal,2)
  
  ptile <- 0.75
  aasPTile <- as.vector(quantile(DF_AAS_AGG_BY_SNAP$AVG_SESS,probs=c(ptile),type=4))
  DF_AAS_AGG_BY_SNAP <- data.table(DF_AAS_AGG_BY_SNAP)
  DF_AAS_AGG_BY_SNAP2 <- data.table(DF_AAS_AGG_BY_SNAP[AVG_SESS >= aasPTile])
  DF_AAS_AGG_BY_SNAP3 <- data.table(DF_AAS_AGG_BY_SNAP2[,list(SNAP_ID = SNAP_ID)])
  DF_AAS_TEMP2 <- data.table(DF_AAS_INT[DF_AAS_AGG_BY_SNAP3])
  avgSessTotal2 <- sum(DF_AAS_TEMP2$AVG_SESS)
  DF_AAS_AGG2 <- DF_AAS_TEMP2[, list(AVG_SESS = sum(AVG_SESS)), by = list(WAIT_CLASS)]
  DF_AAS_AGG2$AVG_SESS_PCT <- round(DF_AAS_AGG2$AVG_SESS/avgSessTotal2,2)
  
  aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                  "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                  "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                  "System I/O" = "#0993de","User I/O" = "#054ae1")
  
  gg_aas_colors <- scale_fill_manual("", values = aas_colors)
  
  
  if( nrow(main$DF_TOP_N_EVENTS)>10){
    titleText <- ggtitle(expression(atop("AAS % by Wait Class - All Snapshots"), atop(scriptstyle(" "), "")))
  }
  else{
    titleText <- labs(title=paste("AAS % by Wait Class - All Snapshots - ",main$current_db_name,sep=""))
  }
  
  
  aas_pct_plot1 <- ggplot(data=DF_AAS_AGG, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
    geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
    geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
    gg_aas_colors+
    scale_y_continuous(labels = percent_format())+
    theme(axis.title.y  = element_blank(),axis.title.x  = element_blank(),legend.position =    "none",
          plot.title= element_text(size = 5) )+
    titleText
  #aas_pct_plot1
  
  
  #   
  #   
  #   
  if( nrow(main$DF_TOP_N_EVENTS)>10){
    
    DF_TOP_N_AGG1 <- main$DF_TOP_N_EVENTS %>%
      group_by(WAIT_CLASS,EVENT_NAME) %>%
      summarise(TOTAL_TIME_S = sum(as.numeric(TOTAL_TIME_S))) %>%
      ungroup() %>%
      arrange(desc(TOTAL_TIME_S)) %>%
      head(25) %>%
      arrange(WAIT_CLASS)
    
    
    
    if(nrow(subset(DF_TOP_N_AGG1,WAIT_CLASS == "DB CPU"))>0){
      DF_TOP_N_AGG1[with(DF_TOP_N_AGG1, grepl("DB CPU", WAIT_CLASS)),]$WAIT_CLASS<-"CPU"
    }
    total_time <- sum(DF_TOP_N_AGG1$TOTAL_TIME_S)
    
    DF_TOP_N_AGG1$pct_time <- (DF_TOP_N_AGG1$TOTAL_TIME_S/total_time)
    
    DF_TOP_N_AGG1$EVENT_NAME <- substr(str_trim(DF_TOP_N_AGG1$EVENT_NAME),1,25)
    DF_TOP_N_AGG1$WAIT_CLASS <- as.character(DF_TOP_N_AGG1$WAIT_CLASS)
    DF_TOP_N_AGG1 <- DF_TOP_N_AGG1[order(DF_TOP_N_AGG1$WAIT_CLASS,decreasing = TRUE),]
    
    #DF_TOP_N_AGG1$WAIT_CLASS <- factor(DF_TOP_N_AGG1$WAIT_CLASS)
    
    DF_TOP_N_AGG1 <- subset(DF_TOP_N_AGG1,pct_time > 0.01)
    aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                    "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                    "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                    "System I/O" = "#0993de","User I/O" = "#054ae1")
    gg_aas_colors <- scale_fill_manual("", values = aas_colors)
    
    aas_pct_plot2 <- ggplot(data=DF_TOP_N_AGG1, aes(x=EVENT_NAME, y=pct_time),order = desc(WAIT_CLASS)) +
      geom_bar(stat="identity",aes(fill=WAIT_CLASS,order = desc(WAIT_CLASS)))+
      geom_text(aes(label=paste(round(pct_time * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
      gg_aas_colors+
      scale_y_continuous(labels = percent_format())+
      theme(axis.title.y  = element_blank(),axis.title.x  = element_blank(),legend.position =    "none",
            plot.title= element_text(size = 5),
            axis.text.x=element_text(size=4))+
      ggtitle(expression(atop("Top N Timed Events Aggregated over Snapshot Range", atop(scriptstyle("Colors correspond to wait classes from AAS graph to left"), ""))))
  }
  else{
    aas_pct_plot2 <- ggplot(data=DF_AAS_AGG2, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
      geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
      geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
      gg_aas_colors+
      scale_y_continuous(labels = percent_format())+
      theme(axis.title.y  = element_blank(),axis.title.x  = element_blank(),legend.position =    "none",
            plot.title= element_text(size = 5) )+
      labs(title=paste("AAS % by Wait Class - Only Snapshots Where Total AAS >= 75th Percentile - ",main$current_db_name,sep=""))
  }
  
  
  
  
  #aas_pct_plot2
  
  
  
  
  flog.debug('plot_aas_percent - end',name='plot_aas_percent')
  return(list(aas_pct_plot1,aas_pct_plot2))
  
}


plot_io <- function(DF_MAIN_BY_SNAP_INT){
  flog.debug('plot_io - start',name='plot_io')
  DF_MAIN_INT2 <- DF_MAIN_BY_SNAP_INT
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end"), measure.var = c("read_iops", "read_iops_max","write_iops", "write_iops_max",
                                                                  "read_mb_s","read_mb_s_max","write_mb_s","write_mb_s_max"
                                                                  #"read_iops_direct","read_iops_direct_max","write_iops_direct","write_iops_direct_max"
  ))
  # add a "stat" column so we can facet by stat for avg-max
  x.melt$stat <- "Avg"
  x.melt$alphaSet <- .9  
  idx_max <- with(x.melt, grepl("max", variable))
  x.melt[idx_max,]$stat <- "Max"
  #idx_max <- with(x.melt, grepl("direct", variable))
  #x.melt[idx_max,]$stat <- "Direct" 
  #idx_max <- with(x.melt, grepl("direct_max", variable))
  #x.melt[idx_max,]$stat <- "Direct Max" 
  #x.melt$value <- round(x.melt$value)
  #idx_max <- with(x.melt, grepl("direct", variable))
  #x.melt[idx_max,]$alphaSet <- 0.4 
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  #x.melt[with(x.melt, grepl("read_iops_direct", variable)),]$variable<-"Read IOPs Direct *"
  #x.melt[with(x.melt, grepl("write_iops_direct", variable)),]$variable<-"Write IOPs Direct *"
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
  
  
  
  plot_io_int <- function(df_in){
    
    
    
    max_vals <- ddply(df_in, .(variable,stat,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
    max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")
    
    plot_int <- ggplot(data=df_in, aes(x=end, y=value),aes(color=stat)) +
      geom_line(aes(color=stat), size=.2)+
      #stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
      #      aes(color=stat,fill=stat))+
      
      attr$themeScaleColour+attr$themeScaleFill+
      main$gg_avg_max_fill+main$gg_avg_max_color+
      geom_point(data=max_vals, aes(x=end, y=value, fill=stat), size=2, shape=21)+
      geom_text(data=max_vals, aes(x=end, y=value, color=stat,label=label),size=2.5, vjust=0.5, hjust=1.25,position = position_jitter(width = .2,height=0))+
      geom_text(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.1,vjust=0.7)+
      geom_point(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0),size=1)+
      ylab('')+
      
      facet_grid(variable ~ .,scales="free_y")+
      scale_y_continuous(labels=comma)+
      xlim(min(x.melt$end),max(x.melt$end))+
      labs(title=paste("IO Avg and Max by IO Type for ",main$current_db_name,sep=""))+
      main$gg_hour_bars+
      attr$vertical_line + attr$vertical_text +
      
      scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                       minor_breaks = attr$date_break_minor_var,
                       limits = c(min(x.melt$end),max(x.melt$end))
      )+
      scale_alpha(guide = 'none')+
      theme(legend.key.size = unit(.25, "cm"),
            strip.text.y = element_text(size = 6)
      )
    return(plot_int)
  }
  
  #p <- plot_io_int(subset(x.melt,variable %in% c("Read IOPs Direct *","Write IOPs Direct *","Read IOPs","Write IOPs","Read MB/s","Write MB/s") & stat == "Avg"))
  p <- plot_io_int(subset(x.melt,variable %in% c("Read IOPs Direct *","Write IOPs Direct *","Read IOPs","Write IOPs","Read MB/s","Write MB/s")))
  
  #p_gt <- ggplot_gtable(ggplot_build(p))
  #p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  
  
  
  flog.debug('plot_io - end',name='plot_io')
  return(p)
  #return(list(p_gt,io_hist_area_plot))
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
  
  
  
  # This is to fill in missing data
  # Create a data frame with all possible values of the following columns
  vals <- expand.grid(SNAP_ID = unique(DF_IO_WAIT_HIST_INT$SNAP_ID),
                      WAIT_TIME_MILLI = unique(DF_IO_WAIT_HIST_INT$WAIT_TIME_MILLI),
                      EVENT_NAME =unique(DF_IO_WAIT_HIST_INT$EVENT_NAME)
  )
  # These two data frames are just to add in the 2 missing columns of WAIT_CLASS and end that we elimintaded
  wait.class.fill <- unique(DF_IO_WAIT_HIST_INT %>% select(EVENT_NAME,WAIT_CLASS))
  end.fill <- unique(DF_IO_WAIT_HIST_INT %>% select(SNAP_ID,end))
  
  # Join in WAIT_CLASS and end
  vals <- left_join(tbl_dt(vals),wait.class.fill,copy = TRUE,by=c("EVENT_NAME"))
  vals <- left_join(tbl_dt(vals),end.fill,copy = TRUE,by=c("SNAP_ID"))
  
  # Join vals to our original frame so we have original plus missing rows
  DF_IO_WAIT_HIST_INT <- left_join(tbl_dt(vals),DF_IO_WAIT_HIST_INT)
  # Set value of WAIT_COUNT in new (missing) rows to zero
  DF_IO_WAIT_HIST_INT[is.na(DF_IO_WAIT_HIST_INT$WAIT_COUNT)]$WAIT_COUNT <- 0
  
  
  

  DF_IO_WAIT_HIST_INT <- data.table(DF_IO_WAIT_HIST_INT)
  setkey(DF_IO_WAIT_HIST_INT,end, EVENT_NAME, WAIT_TIME_MILLI)
  DF_IO_WAIT_HIST_INT <- DF_IO_WAIT_HIST_INT[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(end, EVENT_NAME, WAIT_TIME_MILLI)]
  
  DF_IO_WAIT_HIST_INT[with(DF_IO_WAIT_HIST_INT, grepl(64, WAIT_TIME_MILLI)),]$WAIT_TIME_MILLI<-"64+"
  
  DF_IO_WAIT_HIST_INT_GROUP <- DF_IO_WAIT_HIST_INT[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(EVENT_NAME, WAIT_TIME_MILLI)]
  
  DF_IO_WAIT_HIST_INT_GROUP <- DF_IO_WAIT_HIST_INT_GROUP[, list(WAIT_TIME_MILLI=WAIT_TIME_MILLI,WAIT_PCT =WAIT_COUNT/ sum(WAIT_COUNT)), by = list(EVENT_NAME)]
  
  
  
  io_hist_colors2 <- c("1" = "#315280", "2" = "#4575B4", "4" = "#74ADD1", "8" = "#ABD9E9",
                       "16" = "#FDAE61", "32" = "#F46D43", "64+"="#D73027")
  
  gg_io_hist_colors2 <- scale_fill_manual(values = io_hist_colors2,name="wait ms" )
  
  io_hist_plot <- ggplot(DF_IO_WAIT_HIST_INT_GROUP,aes(x=factor(WAIT_TIME_MILLI),fill = WAIT_TIME_MILLI))+
    geom_bar(stat ='identity',aes(y=WAIT_PCT),width=1)+
    geom_text(aes(label=round(WAIT_PCT*100,0),y=WAIT_PCT),size=2)+
    facet_grid(. ~ EVENT_NAME,scales="free_y")+
    gg_io_hist_colors2+
    labs(title=paste0("I/O Wait Event Histogram"))+
    scale_y_continuous(labels = percent_format())+
    theme(axis.title.y  = element_blank(),legend.position =    "right" ,
          legend.key.size = unit(.25, "cm"),
          strip.text.x = element_text(size = 6))+
    xlab("Wait Milliseconds")
  
  barWidth <- (400000/length(unique(main$DF_IO_WAIT_HIST$SNAP_ID)))*2
  
  snapIntervalMode <- Mode(main$DF_SNAP_ID_DATE$dur_m)
  # print(paste0("snapIntervalMode-",snapIntervalMode))
  barChartWidthFudgeFactor <- NULL
  
  if(snapIntervalMode < 20){
    barChartWidthFudgeFactor <- 0.15
  }
  else{
    barChartWidthFudgeFactor <- 0.05
  }
  
  #barWidth <- as.numeric(median(main$DF_SNAP_ID_DATE$dur_m))
  #barWidth <- barWidth + (barWidth*0.1)
  
  DF_IO_WAIT_HIST_INT$barChartWidth <- snapIntervalMode*60
  DF_IO_WAIT_HIST_INT$barChartWidth <- DF_IO_WAIT_HIST_INT$barChartWidth + (DF_IO_WAIT_HIST_INT$barChartWidth * barChartWidthFudgeFactor)
  
  
  flog.debug(paste0("Num snap_id for IO_HIST: ",length(unique(main$DF_IO_WAIT_HIST$SNAP_ID))))
  
  io_hist_area_plot <- ggplot()+
    #ggplot(DF_IO_WAIT_HIST_INT, aes(x = end,
    #                                                       fill = WAIT_TIME_MILLI))+
    #   geom_bar(stat = "identity", position = "stack",right=FALSE,drop=TRUE,
    #             aes(y = WAIT_COUNT)+
    main$gg_hour_bars+
#     geom_bar(data=DF_IO_WAIT_HIST_INT,aes(x = end, y = WAIT_COUNT,
#                                           fill = WAIT_TIME_MILLI,width=barChartWidth),stat = "identity", position = "stack",alpha=1)+
  geom_area(data=DF_IO_WAIT_HIST_INT,aes(x = end, y = WAIT_COUNT,
                                        fill = WAIT_TIME_MILLI),stat = "identity", position = "stack",alpha=1)+  
#   geom_bar(data=DF_IO_WAIT_HIST_INT,aes(x = end, y = WAIT_COUNT,
#                                           fill = WAIT_TIME_MILLI),stat = "identity", position = "stack",alpha=1)+
    facet_grid(EVENT_NAME ~ .,scales="free_y")+
    gg_io_hist_colors2+
    
    attr$vertical_line + attr$vertical_text +
    labs(title=paste0("I/O Wait Event Area Chart"))+
    theme(legend.key.size = unit(.25, "cm"),
          strip.text.y = element_text(size = 4))+
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_IO_WAIT_HIST_INT$end),max(DF_IO_WAIT_HIST_INT$end))
    )+
    #scale_y_continuous(labels = comma)+
    ylab("Wait Count")+
    theme(axis.title.x=element_blank(),legend.position =    "none" )
  
  
  #io_hist_area_plot$grobs[[4]]$children[[2]]$width <- (io_hist_area_plot$grobs[[4]]$children[[2]]$width[[1]]*1.5)
  
  flog.debug('plot_io_histograms - end',name='plot_io_histograms')
  awrM$debug.lastFunction <- 'plot_io_histograms - end'
  return(list(io_hist_plot,io_hist_area_plot))
}




plot_iostat_by_function <- function(DF_IOSTAT_FUNCTION_INT){
  
  iostat.melt <- melt(DF_IOSTAT_FUNCTION_INT,id.var = c("SNAP_ID","FUNCTION_NAME"),measure.var = c("SM_R_REQS","SM_W_REQS", "LG_R_REQS", "LG_W_REQS"))
  iostat.melt<- merge(iostat.melt,main$DF_SNAP_ID_DATE,by="SNAP_ID")
  
  #iostat2.melt.tmp <<- iostat.melt 
  
  vals <- expand.grid(SNAP_ID = unique(iostat.melt$SNAP_ID),
                      FUNCTION_NAME = unique(iostat.melt$FUNCTION_NAME),
                      variable = unique(iostat.melt$variable)
  )
  
  end.fill <- unique(iostat.melt %>% select(SNAP_ID,end))
  vals <- left_join(tbl_dt(vals),end.fill,copy = TRUE,by=c("SNAP_ID"))
  iostat2.melt <- left_join(tbl_dt(vals),iostat.melt,copy = TRUE)
  iostat2.melt[is.na(iostat2.melt$value)]$value <- 0
  

  iostat2.melt <- transform(iostat2.melt, variable = as.character(variable))
  iostat2.melt[with(iostat2.melt, grepl("SM_R_REQS", variable)),]$variable<-"Small Read IOPs"
  iostat2.melt[with(iostat2.melt, grepl("SM_W_REQS", variable)),]$variable<-"Small Write IOPs *"
  iostat2.melt[with(iostat2.melt, grepl("LG_R_REQS", variable)),]$variable<-"Large Read IOPs"
  iostat2.melt[with(iostat2.melt, grepl("LG_W_REQS", variable)),]$variable<-"Large Write IOPs *"
  
  iostat2.melt[with(iostat2.melt, grepl("Buffer Cache Reads", FUNCTION_NAME)),]$FUNCTION_NAME<-"Buffer Cache Reads (from disk)"
  
  #iostat2.melt <- subset(iostat2.melt,value > 0 )
  
  
  #convert absolute values to per-second values
  iostat2.melt$value_per_s <- round(iostat2.melt$value / (iostat2.melt$dur_m*60),0)
  
  # find the FUNCTIONS for which we have no data and remove them
  iostat2.totals <- ddply(iostat2.melt, .(FUNCTION_NAME), summarise, 
                          value=sum(as.numeric(value)))
  #tyler removed for testing
  idx_iostat_rm <- !with(iostat2.melt, FUNCTION_NAME %in% subset(iostat2.totals,value==0)$FUNCTION_NAME)
  iostat2.melt<- iostat2.melt[idx_iostat_rm,]
  
  #create fake max points to normalize the scale of small reads with writes, and the same for large ...
  iostat2.agg1 <- ddply(iostat2.melt, .(SNAP_ID,end,variable), summarise, value_per_s=sum(as.numeric(value_per_s)))
  iostat2.agg1.max <- ddply(iostat2.agg1, .(variable), summarise, value_per_s=max(as.numeric(value_per_s)))
  iostat2.agg1.max$value_per_s <- iostat2.agg1.max$value_per_s + (iostat2.agg1.max$value_per_s*0.05) # add 5% padding
  #rm(iostat2.agg1)
  iostat2.agg1.max.tmp <- iostat2.agg1.max
  # Flip the names with each other
  iostat2.agg1.max$variable<-paste0(iostat2.agg1.max$variable,"_TMP")
  iostat2.agg1.max$variable<-gsub( "Small Read IOPs_TMP" , "Small Write IOPs *" , iostat2.agg1.max$variable)
  iostat2.agg1.max$variable<-gsub( "Small Write IOPs \\*_TMP" , "Small Read IOPs" , iostat2.agg1.max$variable)
  iostat2.agg1.max$variable<-gsub( "Large Read IOPs_TMP" , "Large Write IOPs *" , iostat2.agg1.max$variable)
  iostat2.agg1.max$variable<-gsub( "Large Write IOPs \\*_TMP" , "Large Read IOPs" , iostat2.agg1.max$variable)
  
  iostat2.agg1.max <- rbind(iostat2.agg1.max,iostat2.agg1.max.tmp )
  iostat2.agg1.max$end <- min(iostat2.melt$end)
  
  
  max_vals <- ddply(iostat2.agg1, .(variable,end=format(end,"%y/%m/%d")), subset, subset = rank(-value_per_s) <= 1)
  max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")
  
  
  
  
  #http://docs.oracle.com/cd/E24628_01/server.121/e17635/tdppt_realtime.htm#CBBEFAAC   
  iostat_colors <- c("ARCH" = "#cc6617", "Archive Manager" = "#9ccecc", "Buffer Cache Reads (from disk)" = "#0133ff", "Data Pump" = "#747254",
                     "DBWR" = "#993309","Direct Reads" = "#00cc2e","Direct Writes" = "#9aff9a","LGWR" = "#cc330c",
                     "Others" = "#ff6699","Recovery" = "#5c460c","RMAN" = "#fcfe84",
                     "Smart Scan" = "#800040","Streams AQ" = "#9c9274","XDB" = "#c4b69c")
  
  
  gg_iostat_colors <- scale_fill_manual("", values = iostat_colors)
  
  
  iostat_plot_int <- ggplot()+
    main$gg_hour_bars+
#     geom_bar(stat = "identity", position = "stack")+
#   geom_bar(data=iostat2.melt, aes(x = end, y = value_per_s,
#                                   fill = FUNCTION_NAME),stat = "identity", position = "stack",width=4000)+
#   geom_bar(data=iostat2.melt, aes(x = end, y = value_per_s,
#                                   fill = FUNCTION_NAME),stat = "identity", position = "stack")+
    geom_area(data=iostat2.melt, aes(x = end, y = value_per_s,
                                    fill = FUNCTION_NAME),stat = "identity", position = "stack")+
    geom_point(data=max_vals, aes(x=end, y=value_per_s), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=value_per_s,label=label),size=2, vjust=-.5, hjust=1)+
    gg_iostat_colors+
    
    attr$vertical_line + attr$vertical_text +
    geom_point(data=iostat2.agg1.max,aes(x=end,y=value_per_s),alpha=0)+
    theme(
      strip.text.y = element_text(size = 7),
      legend.key.size = unit(.25, "cm")
    )+
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(iostat2.melt$end),max(iostat2.melt$end))
    )+
    facet_grid(variable ~ . ,scales="free_y")+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank() )+
    labs(title=paste("IO Requests by Size by Function - ",main$current_db_name,sep=""))
  
  return(iostat_plot_int)
  
}



plot_cpu_no_sd <- function(DF_MAIN_INT){
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
  #max_vals <- ddply(x.melt, .(variable,inst,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
  #max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")
  
  max_vals <- data.frame()
  gg_maxvals_point <- theme()
  gg_maxvals_text <- theme()
  
  get_maxvals_int <- function(df_in){
    max_vals_int <- ddply(df_in, .(variable,inst,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
    max_vals_int$label <- formatC(max_vals_int$value, format="d", big.mark=",")
    
    #gg_maxvals_point <<- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    gg_maxvals_point_int <- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    
    gg_maxvals_text_int <- geom_text(data=max_vals_int, aes(x=end, y=value, color=variable,label=label),size=3, vjust=-.8, hjust=1.5)
    return(list(max_vals_int,gg_maxvals_point_int,gg_maxvals_text_int))
  }
  
  
  tryCatch(c(max_vals,gg_maxvals_point,gg_maxvals_text) :=  get_maxvals_int(x.melt),
           error = function(e) {
             #traceback()
             gg_maxvals_point <- theme()
             gg_maxvals_text <- theme()
             
           }
  )
  
  
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(group=variable,color=variable)) +
    geom_line(aes(group=variable,color=variable), size=.2)+
    attr$themeScaleColour+attr$themeScaleFill+
    stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
                aes(group=variable,color=variable,fill=variable))+
    ylab('CPU Percent')+
    ylim(0,115)+
    gg_maxvals_point+gg_maxvals_text+
    
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
  #p_gt <- ggplot_gtable(ggplot_build(p))
  flog.debug('plot_cpu - end',name='plot_cpu')
  #   p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  return(p)
  #}
}

plot_cpu_sd <- function(DF_MAIN_INT){
  flog.debug('plot_cpu - start',name='plot_cpu')
  
  
  
  DF_MAIN_INT$cpu_per_s_pct <- round((DF_MAIN_INT$cpu_per_s / get_os_stat("NUM_CPU_CORES"))*100,1)
  DF_MAIN_INT$cpu_per_s_sd_pct <- round((DF_MAIN_INT$cpu_per_s_sd / get_os_stat("NUM_CPU_CORES"))*100,1)
  
  DF_INST_INT <- data.frame(unique(DF_MAIN_INT$inst))
  DF_SNAP_ID_SUBSET2 <- merge(DF_INST_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(main$DF_SNAP_ID_SUBSET$end),
                      inst = unique(DF_MAIN_INT$inst))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  x.melt <- melt(DF_MAIN_INT, id.var = c("end","inst","os_cpu_sd","cpu_per_s_sd_pct"), measure.var = c("os_cpu", "cpu_per_s_pct"))
  
  x.melt$sd <- ifelse(x.melt$variable=="os_cpu",x.melt$os_cpu_sd,x.melt$cpu_per_s_sd_pct)
  
  x.melt$variable<-gsub( "cpu_per_s_pct" , "DB CPU Avg" , x.melt$variable)
  
  x.melt$variable<-gsub( "os_cpu" , "OS CPU Avg" , x.melt$variable)
  # add 2 minutes to the DB CPU Avg rows to keep the lines for std dev from over-plotting.
  x.melt[with(x.melt, grepl("DB CPU Avg", variable)),]$end<-x.melt[with(x.melt, grepl("DB CPU Avg", variable)),]$end + minutes(2)
  
  
  max_vals <- data.frame()
  gg_maxvals_point <- theme()
  gg_maxvals_text <- theme()
  
  get_maxvals_int <- function(df_in){
    max_vals_int <- ddply(df_in, .(variable,inst,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
    max_vals_int$label <- formatC(max_vals_int$value, format="d", big.mark=",")
    
    #gg_maxvals_point <<- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    gg_maxvals_point_int <- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    
    gg_maxvals_text_int <- geom_text(data=max_vals_int, aes(x=end, y=value, color=variable,label=label),size=3, vjust=-.8, hjust=1.5)
    return(list(max_vals_int,gg_maxvals_point_int,gg_maxvals_text_int))
  }
  
  
  tryCatch(c(max_vals,gg_maxvals_point,gg_maxvals_text) :=  get_maxvals_int(x.melt),
           error = function(e) {
             #traceback()
             gg_maxvals_point <- theme()
             gg_maxvals_text <- theme()
             
           }
  )
  
  DF_ANNOTATE_INT <- data.frame(x=quantile(x.melt$end,0.10),y=115,inst=min(x.melt$inst),
                                labs="Standard Deviation shown as vertical lines over points")
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(group=variable,color=variable)) +
    main$gg_hour_bars+
    geom_point(aes(group=variable,color=variable), size=0.6,shape=1,alpha=0.8)+
    geom_linerange(aes(ymin=value-1.96*sd,ymax=value+1.96*sd,color=variable), alpha=0.5,size=0.1)+
    attr$themeScaleColour+attr$themeScaleFill+
    stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",formula=y~poly(x,2),
                aes(group=variable,color=variable,fill=variable))+
    ylab('CPU Percent')+
    ylim(0,115)+
    gg_maxvals_point+gg_maxvals_text+
    
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-45,size=1.5,hjust=-.5,alpha=0.2)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1)+
    facet_grid(inst ~ .)+
    theme(axis.title.x  = element_blank())+
    theme(panel.background = element_rect(colour = "#777777"))+
    xlim(min(x.melt$end),max(x.melt$end))+
    labs(title=paste("OS CPU % and Database CPU % for ",main$current_db_name,sep=""))+
    
    attr$vertical_line + attr$vertical_text +
    #scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = date_breaks("2 hour"),
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end))
    )+
    main$gg_avg_max_fill+main$gg_avg_max_color+
    geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4)
  #annotate("text", x = median(x.melt$end), y = 105, label = "Standard Deviation shown as vertical lines over points",size=2,alpha=.4)
  # theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))
  #theme(panel.grid.major = element_line("#eeeeee", size = 0.2,linetype = "dotted"))+
  #theme(panel.grid.minor = element_line("#efefef", size = 0.1,linetype = "dotted"))
  #main$gg_bottom_panel
  
  #p
  #p_gt <- ggplot_gtable(ggplot_build(p))
  flog.debug('plot_cpu - end',name='plot_cpu')
  
  return(p)
  #}
}

#cpu_plot <- plot_cpu(main$DF_MAIN)


plot_cpu_host_db <- function(DF_MAIN_INT){
  flog.debug('plot_cpu_host_db - start',name='plot_cpu_host_db')
  
  
  
  DF_MAIN_INT$cpu_per_s_pct <- round((DF_MAIN_INT$cpu_per_s / get_os_stat("NUM_CPU_CORES"))*100,1)
  DF_MAIN_INT$cpu_per_s_sd_pct <- round((DF_MAIN_INT$cpu_per_s_sd / get_os_stat("NUM_CPU_CORES"))*100,1)
  
  DF_INST_INT <- data.frame(unique(DF_MAIN_INT$inst))
  DF_SNAP_ID_SUBSET2 <- merge(DF_INST_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(main$DF_SNAP_ID_SUBSET$end),
                      inst = unique(DF_MAIN_INT$inst))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  x.melt <- melt(DF_MAIN_INT, id.var = c("end","inst","os_cpu_sd","cpu_per_s_sd_pct"), measure.var = c("os_cpu", "cpu_per_s_pct"))
  
  x.melt$sd <- ifelse(x.melt$variable=="os_cpu",x.melt$os_cpu_sd,x.melt$cpu_per_s_sd_pct)
  
  x.melt$variable<-gsub( "cpu_per_s_pct" , "DB CPU Avg" , x.melt$variable)
  
  x.melt$variable<-gsub( "os_cpu" , "OS CPU Avg" , x.melt$variable)
  # add 2 minutes to the DB CPU Avg rows to keep the lines for std dev from over-plotting.
  x.melt[with(x.melt, grepl("DB CPU Avg", variable)),]$end<-x.melt[with(x.melt, grepl("DB CPU Avg", variable)),]$end + minutes(2)
  
  
  max_vals <- data.frame()
  gg_maxvals_point <- theme()
  gg_maxvals_text <- theme()
  
  get_maxvals_int <- function(df_in){
    max_vals_int <- ddply(df_in, .(variable,inst,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
    max_vals_int$label <- formatC(max_vals_int$value, format="d", big.mark=",")
    
    #gg_maxvals_point <<- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    gg_maxvals_point_int <- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    
    gg_maxvals_text_int <- geom_text(data=max_vals_int, aes(x=end, y=value, color=variable,label=label),size=3, vjust=-.8, hjust=1.5)
    return(list(max_vals_int,gg_maxvals_point_int,gg_maxvals_text_int))
  }
  
  
  tryCatch(c(max_vals,gg_maxvals_point,gg_maxvals_text) :=  get_maxvals_int(x.melt),
           error = function(e) {
             #traceback()
             gg_maxvals_point <- theme()
             gg_maxvals_text <- theme()
             
           }
  )
  
  DF_ANNOTATE_INT <- data.frame(x=quantile(x.melt$end,0.10),y=115,inst=min(x.melt$inst),
                                labs="Standard Deviation shown as vertical lines over points")
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(group=variable,color=variable)) +
    main$gg_hour_bars+
    geom_point(aes(group=variable,color=variable), size=0.6,shape=1,alpha=0.8)+
    geom_linerange(aes(ymin=value-1.96*sd,ymax=value+1.96*sd,color=variable), alpha=0.5,size=0.1)+
    attr$themeScaleColour+attr$themeScaleFill+
    stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",formula=y~poly(x,2),
                aes(group=variable,color=variable,fill=variable))+
    ylab('CPU Percent')+
    ylim(0,115)+
    gg_maxvals_point+gg_maxvals_text+
    
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-45,size=1.5,hjust=-.5,alpha=0.2)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1)+
    facet_grid(inst ~ .)+
    theme(axis.title.x  = element_blank())+
    theme(panel.background = element_rect(colour = "#777777"))+
    xlim(min(x.melt$end),max(x.melt$end))+
    labs(title=paste("OS CPU % and Database CPU % for ",main$current_db_name,sep=""))+
    
    attr$vertical_line + attr$vertical_text +
    #scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = date_breaks("2 hour"),
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end))
    )+
    main$gg_avg_max_fill+main$gg_avg_max_color+
    geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4)
  #annotate("text", x = median(x.melt$end), y = 105, label = "Standard Deviation shown as vertical lines over points",size=2,alpha=.4)
  # theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))
  #theme(panel.grid.major = element_line("#eeeeee", size = 0.2,linetype = "dotted"))+
  #theme(panel.grid.minor = element_line("#efefef", size = 0.1,linetype = "dotted"))
  #main$gg_bottom_panel
  
  #p
  #p_gt <- ggplot_gtable(ggplot_build(p))
  flog.debug('plot_cpu_host_db - end',name='plot_cpu_host_db')
  
  return(p)
  #}
}


plot_cpu <- function(DF_MAIN_INT){
  
  # tyler made a 1-0ff change for army ERP
  #p_gt <- plot_cpu_no_sd(DF_MAIN_INT)
  #return(p_gt)
  
  if(data_frame_col_not_null(DF_MAIN_INT,'h_cpu_per_s')){
    
    p_gt <- plot_cpu_host_db(DF_MAIN_INT)
  }
  else if("cpu_per_s" %in% colnames(DF_MAIN_INT)){
    if(sum(DF_MAIN_INT$os_cpu)>0){
      p_gt <- plot_cpu_sd(DF_MAIN_INT)
    }
    else{
      p_gt <- NULL
    }
  }
  else{
    if(sum(DF_MAIN_INT$os_cpu)>0){
      p_gt <- plot_cpu_no_sd(DF_MAIN_INT)
    }
    else{
      p_gt <- NULL
    }
  }
  
  return(p_gt)
}


plot_main_activity <- function(DF_MAIN_INT){
  flog.debug('plot_main_activity - start',name='plot_main_activity')
  #if(dim(DF_MAIN_INT)[1] > 2){
  #DF_MAIN_INT<-DF_MAIN
  
  DF_MAIN_INT2 <- ddply(DF_MAIN_INT, .(end), summarise, 
                        #aas=sum(aas),
                        #aas_max=sum(aas_max),
                        sql_res_t_cs=sum(sql_res_t_cs),
                        logons_s=sum(logons_s),
                        logons_total=sum(logons_total),
                        exec_s=sum(exec_s),
                        hard_p_s=sum(hard_p_s),
                        commits_s=sum(commits_s),
                        se_sess=sum(se_sess),
                        px_sess=sum(px_sess),
                        redo_mb_s=sum(redo_mb_s)
                        #                         ,
                        #                         se_sess=sum(se_sess),
                        #                        px_sess=sum(px_sess)
  )
  
  
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end"), measure.var = c("se_sess", "px_sess","sql_res_t_cs", "logons_s",
                                                                  "logons_total","exec_s","hard_p_s","commits_s","redo_mb_s"
                                                                  #,
                                                                  #"se_sess","px_sess"
  ))
  #"db_block_gets_s","db_block_changes_s"))
  
  x.melt$value <- round(x.melt$value,2)
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  x.melt[with(x.melt, grepl("se_sess", variable)),]$variable<-"Sessions Serial"
  x.melt[with(x.melt, grepl("px_sess", variable)),]$variable<-"Sessions Parallel"
  x.melt[with(x.melt, grepl("sql_res_t_cs", variable)),]$variable<-"SQL Resp (cs)"
  x.melt[with(x.melt, grepl("logons_s", variable)),]$variable<-"Logons/s"
  x.melt[with(x.melt, grepl("logons_total", variable)),]$variable<-"Logons Total"
  #x.melt[with(x.melt, grepl("se_sess", variable)),]$variable<-"Serial Sessions"
  #x.melt[with(x.melt, grepl("px_sess", variable)),]$variable<-"Parallel Sessions"
  x.melt[with(x.melt, grepl("exec_s", variable)),]$variable<-"Execs/s"
  x.melt[with(x.melt, grepl("commits_s", variable)),]$variable<-"Commits/s"
  x.melt[with(x.melt, grepl("hard_p_s", variable)),]$variable<-"Hard Parses/s"
  x.melt[with(x.melt, grepl("redo_mb_s", variable)),]$variable<-"Redo MB/s"
  
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
    gg_aas_max_max <- theme()
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
    theme(
      strip.text.y = element_text(size = 6)
    )+
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),
                     breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end))
    )
  #scale_colour_tableau("colorblind10")+scale_fill_tableau("colorblind10")
  
  
  #p_gt <- ggplot_gtable(ggplot_build(p))
  #p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  flog.debug('plot_main_activity - end',name='plot_main_activity')
  #grid.draw(p_gt)
  return(p)
  
  
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
    flog.debug('plot_RAC_activity - end',name='plot_RAC_activity')
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
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end))
    )
  #scale_colour_tableau("colorblind10")+scale_fill_tableau("colorblind10")
  
  
  #p_gt <- ggplot_gtable(ggplot_build(p))
  #p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  flog.debug('plot_RAC_activity - end',name='plot_RAC_activity')
  #grid.draw(p_gt)
  return(p)
  
  
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
    labs(title=paste("Total Memory Usage in GB for ",main$current_db_name,sep=""))+
    #main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_MEMORY_INT_SUM$end),max(DF_MEMORY_INT_SUM$end))
    )
  
  p
  flog.debug('plot_memory - end',name='plot_memory')
  return(p)
  #}
}



plot_memory_sga_advise <- function(df_in){
  flog.debug('plot_memory_sga_advise - start',name='plot_memory_sga_advise')
  DF_MEMORY_SGA_ADVICE_TMP <- df_in 
  
  if(is.na(sum(main$DF_MEMORY_SGA_ADVICE$ESTD_PHYSICAL_READS))){
  #  return(NULL)
  }
  
  DF_MEMORY_SGA_ADVICE_TMP <- subset(DF_MEMORY_SGA_ADVICE_TMP,ESTD_PHYSICAL_READS > 0)
  
  if(nrow(DF_MEMORY_SGA_ADVICE_TMP) ==0){
    return(FALSE)
  }
  
  
  DF_MEMORY_SGA_ADVICE_1_TMP <- subset(DF_MEMORY_SGA_ADVICE_TMP,SIZE_FACTOR==1)
  DF_MEMORY_SGA_ADVICE_1_TMP <- DF_MEMORY_SGA_ADVICE_1_TMP[c("end","SNAP_ID","INSTANCE_NUMBER","ESTD_PHYSICAL_READS")]
  
  
  setnames(DF_MEMORY_SGA_ADVICE_1_TMP, "ESTD_PHYSICAL_READS", "CURRENT_PHYSICAL_READS")
  
  DF_MEMORY_SGA_ADVICE_TMP <- merge(DF_MEMORY_SGA_ADVICE_TMP,DF_MEMORY_SGA_ADVICE_1_TMP,by=c('SNAP_ID','INSTANCE_NUMBER'))
  rm(DF_MEMORY_SGA_ADVICE_1_TMP)
  DF_MEMORY_SGA_ADVICE_TMP$pct_change <- (DF_MEMORY_SGA_ADVICE_TMP$ESTD_PHYSICAL_READS-DF_MEMORY_SGA_ADVICE_TMP$CURRENT_PHYSICAL_READS)/DF_MEMORY_SGA_ADVICE_TMP$CURRENT_PHYSICAL_READS
  
  
  DF_MEMORY_SGA_ADVICE_TMP_AGG <- ddply(DF_MEMORY_SGA_ADVICE_TMP,.(INSTANCE_NUMBER,SGA_TARGET_GB),summarise,
                                        pct_change=mean(pct_change),
                                        SIZE_FACTOR=mean(SIZE_FACTOR))
  
  DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change>=2,2,DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change)
  
  DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$SIZE_FACTOR==1,"Current Value","Minor Change")
  DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change>=0.05,"Increased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP_AGG$color)
  DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change<=-0.05,"Decreased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP_AGG$color)
  
  DF_MEMORY_SGA_ADVICE_TMP_AGG$SGA_TARGET_GB <- factor(DF_MEMORY_SGA_ADVICE_TMP_AGG$SGA_TARGET_GB)
  
  #DF_MEMORY_SGA_ADVICE_TMP_AGG$INSTANCE_NUMBER <- paste0("Node ",DF_MEMORY_SGA_ADVICE_TMP_AGG$INSTANCE_NUMBER)
  
  
  sga_colors <- c("Current Value" = "#000000", "Minor Change" = "#1a466e", 
                  "Increased Physical Reads" = "#c10534",
                  "Decreased Physical Reads" = "#54752e")
  
  gg_sga_colors <- scale_colour_manual("", values = sga_colors)
  gg_sga_fill<- scale_fill_manual("", values = sga_colors)
  
  
  p <- ggplot(data=DF_MEMORY_SGA_ADVICE_TMP_AGG,aes(x=SGA_TARGET_GB,y=pct_change*100,colour=color,fill=color))+
    geom_bar(stat="identity",alpha=.2)+
    geom_text(aes(label=paste(round(pct_change * 100, 1), "%", sep = "")),size=1.5,colour="black",vjust=0,angle=-90)+
    facet_grid(INSTANCE_NUMBER ~ . )+
    gg_sga_colors+gg_sga_fill+
    scale_x_discrete(breaks=unique(DF_MEMORY_SGA_ADVICE_TMP_AGG$SGA_TARGET_GB))+
    labs(title=paste("Average Percent Change in Physical Reads \nRelative To SGA_TARGET from SGA Target Advisory for ",main$current_db_name,sep=""))+
    xlab("SGA_TARGET (GB)")+
    ylab("Percent Change in Physical Reads (Negative is better) \nValues over 200% are set to 200%")+
    theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
          strip.text.y = element_text(size = 7),
          legend.key.size = unit(.25, "cm")
    )
  
  flog.debug('plot_memory_sga_advise - end',name='plot_memory_sga_advise')
  
  return(p)
}


plot_memory_pga_advise <- function(df_in){
  flog.debug('plot_memory_PGA_advise - start',name='plot_memory_PGA_advise')
  
  DF_MEMORY_PGA_ADVICE_TMP <- df_in 
  #DF_MEMORY_PGA_ADVICE_TMP <- main$DF_MEMORY_PGA_ADVICE 
  
  
  
  
  DF_MEMORY_PGA_ADVICE_1_TMP <- subset(DF_MEMORY_PGA_ADVICE_TMP,SIZE_FACTOR==1)
  
  
  Mode <- function(x) names(which.max(table(x)))  
  DF_MEMORY_PGA_ADVICE_TMP_MODE <- ddply(DF_MEMORY_PGA_ADVICE_1_TMP,.(INSTANCE_NUMBER),summarise,
                                         PGA_TARGET_GB=Mode(PGA_TARGET_GB)
  )
  
  DF_MEMORY_PGA_ADVICE_1_TMP <- DF_MEMORY_PGA_ADVICE_1_TMP[c("end","SNAP_ID","INSTANCE_NUMBER","ESTD_EXTRA_MB_RW")]
  
  setnames(DF_MEMORY_PGA_ADVICE_1_TMP, "ESTD_EXTRA_MB_RW", "CURRENT_MB_RW")
  
  DF_MEMORY_PGA_ADVICE_1_TMP$CURRENT_MB_RW <- ifelse(DF_MEMORY_PGA_ADVICE_1_TMP$CURRENT_MB_RW==0,0.1,DF_MEMORY_PGA_ADVICE_1_TMP$CURRENT_MB_RW) 
  DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW <- ifelse(DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW==0,0.1,DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW) 
  
  DF_MEMORY_PGA_ADVICE_TMP <- merge(DF_MEMORY_PGA_ADVICE_TMP,DF_MEMORY_PGA_ADVICE_1_TMP,by=c('end','SNAP_ID','INSTANCE_NUMBER'))
  rm(DF_MEMORY_PGA_ADVICE_1_TMP)
  DF_MEMORY_PGA_ADVICE_TMP$pct_change <- (DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW-DF_MEMORY_PGA_ADVICE_TMP$CURRENT_MB_RW)/DF_MEMORY_PGA_ADVICE_TMP$CURRENT_MB_RW
  
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG <- ddply(DF_MEMORY_PGA_ADVICE_TMP,.(INSTANCE_NUMBER,PGA_TARGET_GB),summarise,
                                        pct_change=mean(pct_change),
                                        SIZE_FACTOR=mean(SIZE_FACTOR))
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG$pct_change <- ifelse(DF_MEMORY_PGA_ADVICE_TMP_AGG$pct_change>=2,2,DF_MEMORY_PGA_ADVICE_TMP_AGG$pct_change)
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_PGA_ADVICE_TMP_AGG$SIZE_FACTOR==1,"Current Value","Minor Change")
  DF_MEMORY_PGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_PGA_ADVICE_TMP_AGG$pct_change>=0.05,"Increased W/A MB Read/Written",DF_MEMORY_PGA_ADVICE_TMP_AGG$color)
  DF_MEMORY_PGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_PGA_ADVICE_TMP_AGG$pct_change<=-0.05,"Decreased W/A MB Read/Written",DF_MEMORY_PGA_ADVICE_TMP_AGG$color)
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG$PGA_TARGET_GB <- factor(DF_MEMORY_PGA_ADVICE_TMP_AGG$PGA_TARGET_GB)
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG$INSTANCE_NUMBER <- paste0("Node ",DF_MEMORY_PGA_ADVICE_TMP_AGG$INSTANCE_NUMBER)
  
  pga_colors <- c("Current Value" = "#000000", "Minor Change" = "#1a466e", 
                  "Increased W/A MB Read/Written" = "#c10534",
                  "Decreased W/A MB Read/Written" = "#54752e")
  
  gg_PGA_colors <- scale_colour_manual("", values = pga_colors)
  gg_PGA_fill<- scale_fill_manual("", values = pga_colors)
  
  
  p <- ggplot(data=DF_MEMORY_PGA_ADVICE_TMP_AGG,aes(x=PGA_TARGET_GB,y=pct_change*100,colour=color,fill=color))+
    geom_bar(stat="identity",alpha=.2)+
    geom_text(aes(label=paste(round(pct_change * 100, 1), "%", sep = "")),size=1,colour="black",vjust=0,angle=-90)+
    facet_grid(INSTANCE_NUMBER ~ . )+
    gg_PGA_colors+gg_PGA_fill+
    scale_x_discrete(breaks=unique(DF_MEMORY_PGA_ADVICE_TMP_AGG$PGA_TARGET_GB))+
    labs(title=paste("Average Percent Change in W/A MB Read/Written to Disk \nRelative To PGA_AGGREGATE_TARGET from PGA Target Advisory for ",main$current_db_name,sep=""))+
    xlab("PGA_AGGREGATE_TARGET (GB)")+
    ylab("Percent Change in W/A MB Read/Written to Disk (Negative is better) \nValues over 200% are set to 200%")+
    theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
          strip.text.y = element_text(size = 7),
          legend.key.size = unit(.25, "cm")
    )
  #  p
  flog.debug('plot_memory_PGA_advise - end',name='plot_memory_PGA_advise')
  # str(p)
  return(p)
}






plot_db_parameters <- function(){
  flog.debug('plot_db_parameters - start')
  
  
  dbParametersCombined <- cbind(main$DF_DB_PARAMETERS[c(seq(1,80)),],main$DF_DB_PARAMETERS[c(seq(81,160)),],main$DF_DB_PARAMETERS[c(seq(161,240)),])
  
  
  textplot(dbParametersCombined,cex=.5,lspace=.01,show.colnames=TRUE,show.rownames=FALSE,cmar=3,rmar=.5,family="mono",hadj=1,mar=c(0,.001,0,0),fixed.width=TRUE,halign="left")
  flog.debug('plot_db_parameters - end')
}


plot_sql_text <- function(){
  flog.debug('plot_sql_text - start')
  
  main$DF_SQL_SUMMARY$ELAP <-  main$DF_SQL_SUMMARY$ELAP / 1000000 # convert microseconds to seconds
  main$DF_SQL_SUMMARY$AVG_DOP <- main$DF_SQL_SUMMARY$PX_SERVERS_EXECS / main$DF_SQL_SUMMARY$EXECS
  #main$DF_SQL_SUMMARY$ELAP_PER_EXEC_S <- (main$DF_SQL_SUMMARY$ELAP / (ifelse(main$DF_SQL_SUMMARY$EXECS > 0, main$DF_SQL_SUMMARY$EXECS ,1)))
  main$DF_SQL_SUMMARY$ELAP_PER_EXEC_S <- ifelse(main$DF_SQL_SUMMARY$EXECS==0, 
                                                main$DF_SQL_SUMMARY$ELAP, 
                                                (main$DF_SQL_SUMMARY$ELAP / main$DF_SQL_SUMMARY$EXECS ))
  main$DF_SQL_SUMMARY$logRsGBperExec <- ((main$DF_SQL_SUMMARY$LOG_READS* 8)/main$DF_SQL_SUMMARY$EXECS)/1024/1024
  main$DF_SQL_SUMMARY$PhysRsGBperExec <- ((main$DF_SQL_SUMMARY$PHY_READ_GB)/main$DF_SQL_SUMMARY$EXECS)
  numVars <- sapply(main$DF_SQL_SUMMARY, is.numeric)
  main$DF_SQL_SUMMARY[numVars] <- lapply(main$DF_SQL_SUMMARY[numVars], round, digits = 1)
  
  main$DF_SQL_SUMMARY$ELAP_PER_EXEC_S <- ifelse(main$DF_SQL_SUMMARY$EXECS==0, 
                                                paste0('[', main$DF_SQL_SUMMARY$ELAP,']'), 
                                                main$DF_SQL_SUMMARY$ELAP_PER_EXEC_S)
  
  main$DF_SQL_SUMMARY$ELAP <-  formatC(main$DF_SQL_SUMMARY$ELAP, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$OPTIMIZER_COST <-  formatC(main$DF_SQL_SUMMARY$OPTIMIZER_COST, digits=0,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$EXECS <-  formatC(main$DF_SQL_SUMMARY$EXECS, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$LOG_READS <-  formatC(main$DF_SQL_SUMMARY$LOG_READS, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M <-  formatC(main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$AVG_DOP <-  formatC(main$DF_SQL_SUMMARY$AVG_DOP, digits=0,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$logRsGBperExec <-  formatC(main$DF_SQL_SUMMARY$logRsGBperExec, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$PhysRsGBperExec <-  formatC(main$DF_SQL_SUMMARY$PhysRsGBperExec, digits=1,format="fg", big.mark=",")
  
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="ACTION"] <- ".          ACTION          ."
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="ELAP"] <- "ELAP_S"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PARSING_SCHEMA_NAME"] <- "SCHEMA"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="OPTIMIZER_COST"] <- "COST"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PLAN_COUNT"] <- "PLANS"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PX_SERVERS_EXECS"] <- "PX_EXEC"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="LOG_READS_RANK"] <- "logRsRank"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PHYS_READS_RANK"] <- "physRsRank"
  
  #subset(df, select=-c(z,u))
  #sqlSummaryText1 <- tableGrob(head(subset(main$DF_SQL_SUMMARY, select=-c(PX_EXEC,LOG_READS)),75),show.rownames = FALSE, gpar.coretext = gpar(fontsize=5),gpar.coltext = gpar(fontsize=5),padding.v = unit(1, "mm"),padding.h = unit(1, "mm"),show.colnames = TRUE,col.just = "left", gpar.corefill = gpar(fill=NA,col=NA),h.even.alpha = 0 )
  #print(sqlSummaryText1)
  #grid.arrange(sqlSummaryText1,ncol = 1, widths=c(1))
  
  
  #textplot(head(subset(main$DF_SQL_SUMMARY, select=-c(PX_EXEC,LOG_READS)),75),cex=.5,cspace=0,lspace=.01,show.colnames=TRUE,cmar=.4,rmar=.5)
  
  textplot(head(subset(main$DF_SQL_SUMMARY, select=-c(PX_EXEC,LOG_READS,ELAP_S,ELAP_PER_EXEC_M,COST)),75),cex=.4,lspace=.01,show.colnames=TRUE,show.rownames=TRUE,cmar=.55,rmar=1.2,hadj=0.5,mar=c(0,.001,0,0),fixed.width=FALSE,halign="left")
  
  flog.debug('plot_sql_text - end')
}

#====================================================================================================================











# *MAIN LOOP STRUCTURES THAT CALL OTHER FUNCTIONS* ==================================================================


main$overall_summary_df <- NULL
main$overall_combined_df <- NULL
main$DATA_FRAME <- NULL
main$DF_OS <- NULL
main$DF_OSSTAT <- NULL
main$DF_MEMORY <- NULL
main$DF_MEMORY_SGA_ADVICE <- NULL
main$DF_MEMORY_PGA_ADVICE <- NULL
main$DF_MAIN <- NULL
main$DF_SNAP_ID_DATE <- NULL
main$DF_SNAP_ID_SUBSET <- NULL
main$DF_SNAP_ID_DATE2 <- NULL
main$DF_IO_WAIT_HIST <- NULL
main$DF_DB_PARAMETERS <- NULL
main$DF_IO_BY_OBJECT_TYPE <- NULL
main$DF_TOP_N_EVENTS <- NULL

main$gg_hour_bars <- NULL
main$cpu_cores <- NULL
main$current_db_name=""
main$attributes_file="attributes.csv"
main$plot_attributes <- NULL
main$current_plot_attributes <- NULL
main$capture_times <- data.frame()

main$cpu_plot<-NULL
main$RAC_plot<-NULL
main$DAYS_OF_DATA<-NULL










#####################################################################


main$mainFunction <- function(f){
  
  get_db_name(f)
  
  #main$current_db_name=main$db_name[which(main$db_id==f)]
  flog.info(paste0('Starting DB: ',main$current_db_name))
  flog.debug(paste0('Database - ',main$current_db_name," - start"))
  
  #flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
  
  main$DATA_FRAME <- NULL
  main$DF_OS <- NULL
  main$DF_MEMORY <- NULL
  main$DF_MAIN <- NULL
  main$DF_MAIN_BY_SNAP <- NULL
  main$DF_IO_WAIT_HIST <- NULL
  main$current_plot_attributes <- NULL
  main$capture_times <<- data.frame()
  
  if(debugMode){
    main$current_plot_attributes <- load_plot_attributes()
  }
  else{
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
  }
  
  attr$filter_snap_min <<- as.numeric(attr$filter_snap_min)
  attr$filter_snap_max <<- as.numeric(attr$filter_snap_max)
  
  parsedFile <- NULL
  parsedFile <- parsed_data_exists(f)
  
  if(parsedFile==FALSE){
    c(main$DF_OS, main$DF_MAIN,
      main$DF_OSSTAT,
      main$DF_MEMORY,main$DF_MEMORY_SGA_ADVICE,main$DF_MEMORY_PGA_ADVICE,
      main$DF_SPACE,main$DF_AAS,main$DF_SQL_SUMMARY,main$DF_SQL_BY_SNAPID,
      main$DF_SNAP_ID_DATE,
      main$DF_IO_WAIT_HIST,main$DF_IOSTAT_FUNCTION,
      main$DF_DB_PARAMETERS,main$DF_IO_BY_OBJECT_TYPE,
      #main$DF_TOP_N_EVENTS) := build_data_frames(f,main$current_db_name)
      main$DF_TOP_N_EVENTS) := build_data_frames(f)
      save_parsed_data()  
    
  }
  else{
      load_parsed_data(parsedFile)
  }
  
  main$current_plot_attributes <<- load_plot_attributes()
  # print(head(main$current_plot_attributes ,n=20))
  if(nrow(main$current_plot_attributes)<2){
    
    main$current_plot_attributes <<- generate_plot_attributes(main$DF_SNAP_ID_DATE )
    # print(head(main$current_plot_attributes ,n=20))
  }
  apply_current_attributes()
  flog.trace(paste0('Snaps:',attr$filter_snap_min,' - ',attr$filter_snap_max))
  attr$filter_snap_min <<- as.numeric(attr$filter_snap_min)
  attr$filter_snap_max <<- as.numeric(attr$filter_snap_max)
  
  
  main$DAYS_OF_DATA<-get_os_stat("DAYS")

  filter_parsed_data(attr$filter_snap_min,attr$filter_snap_max)
  
  numDaysFiltered <- round(difftime(max(main$DF_MAIN$end), min(main$DF_MAIN$end), unit="days"),1)
  main$DF_OS$STAT_VALUE[main$DF_OS$STAT_NAME=='DAYS_FILTERED'] <<- numDaysFiltered
  computeBarChartWidth()
  
  
  # call write to .Rda
  
  c(main$DF_MAIN_BY_SNAP) := summarise_dfs_by_snap()
  
  main$DF_SNAP_ID_DATE2 <- build_snap_to_date_df()
  
  saveUniqueModules()
  
  outFileName <- paste(main$current_db_name,min(main$DF_MAIN$snap),max(main$DF_MAIN$snap),outFileSuffix,sep='-')
  
  if(debugMode){
    debugVars$main <- main
    save(debugVars,file=paste(outFileName,"-debugVars.Rda",sep=""))
    if(okToPrintPlot('snapshots_html')){
      sjt.df(main$DF_SNAP_ID_DATE,file=paste0(outFileName,"-snapshots.html"),describe=FALSE,alternateRowColors=TRUE)
    }
    
    if(exists("dumpCSV")){
      if(!is.null(dumpCSV)){
        if(dumpCSV){
          
          for (objName in ls(main)) {
            tmp <- get(objName,envir=main)
            #print(class(tmp))
            #print(paste0(objName," - ",class(tmp)))
            if(inherits(tmp,what='data.frame')){
              write.csv(x=tmp,file=paste0(main$current_db_name, "-",objName,".csv"),row.names=FALSE)
            }
            rm(tmp)
          }
        }
      }
    }
  }
  
  add_vetical_lines()
  set_date_break_vars(main$DF_MAIN)
  
  main$DF_SNAP_ID_SUBSET <- generate_snap_id_labels(main$DF_SNAP_ID_DATE)
  
  
  # if(is.na(get_os_stat("NUM_CPU_CORES"))) main$num_cpu_cores <- get_os_stat("NUM_CPUS") else main$num_cpu_cores <- get_os_stat("NUM_CPU_CORES")
  
  # CPU_CORES Fixup should go here
  main$node_cpu_cores  <- getCPUcores()
  
  main$cpu_cores <- main$node_cpu_cores*get_os_stat("INSTANCES")
  
  
  
  tryCatch(main$gg_hour_bars <- generate_hours_bars(main$DF_MAIN), 
           error = function(e) {
             #traceback()
             flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
             flog.error(paste0("Error in ",main$current_db_name,": ",e))
             main$gg_hour_bars <- theme()
             #flog.remove(name=main$current_db_name)
             #browser()
             
           }
           #,finally=print("finished")
  )
  
  
  c(main$DF_SUMMARY_OS,main$DF_SUMMARY_MAIN,main$DF_SUMMARY_OVERALL) := gen_summary_data()
  main$DF_SUMMARY_OVERALL$outFileName <- paste0(outFileName,"-plot.pdf")
  main$overall_summary_df <- rbind(main$overall_summary_df, main$DF_SUMMARY_OVERALL)
  
  flog.trace(nrow(subset(main$DF_OS,STAT_NAME == 'HOSTS')),name='mainFunction')
  DF_HOSTS_INT <- subset(main$DF_OS,STAT_NAME == 'HOSTS')
  if(nrow(DF_HOSTS_INT)==0){
    DF_HOSTS_INT <- data.frame(col1='HOSTS',col2='NA')
  }
  
  flog.trace(str(tblText3),name='mainFunction')
  
  if(okToPrintPlot('aas') | okToPrintPlot('page1')){
  #if(okToPrintPlot('aas1')){
    c(aas_pct1, aas_pct2) := plot_aas_percent(main$DF_AAS)
    
    c(aas_plot, aas_plot2_gt,aas_plot2_line) := plot_aas_chart(main$DF_AAS)
    #l <<- lineprof(c(aas_plot, aas_plot2_gt,aas_plot2_line) := plot_aas_chart(main$DF_AAS))
  }
  
  
  removeIfExists <- function(...) {
    xchar <- deparse(substitute(...))
    if(exists(xchar)){
      rm(list=xchar,envir=sys.frame(-1))
    }
  }
  
  
  
  plotPDF <- TRUE
  
  pdfFileSuffix <- NULL
  
  if(exists("plotOverride")){
    if(!is.null(plotOverride) & is.element('NONE', plotOverride)){
      flog.debug("PDF Output is Disabled")
      plotPDF <- FALSE
    }
    else{
      pdfFileSuffix <- "-partial"
    }
      
  }
  
  if(plotPDF){
    flog.debug("PDF Output is Enabled")
    pdf(paste(outFileName,pdfFileSuffix,"-plot.pdf",sep=""), width = 11, height = 8.5,useDingbats=FALSE)
  }
  
  
  if(okToPrintPlot('page1')){ 
    box_plots <- plot_summary_boxplot_main()
    tblText <- tableGrob(main$DF_SUMMARY_OS,show.rownames = FALSE, gpar.coretext = gpar(fontsize=12),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
    tblText2 <- tableGrob(main$DF_SUMMARY_MAIN,show.rownames = FALSE, gpar.coretext = gpar(fontsize=10),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
    
    tblText3 <- tableGrob(DF_HOSTS_INT,show.rownames = FALSE, gpar.coretext = gpar(fontsize=8),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = FALSE,col.just = "left")
  }
    
  
  #x <- grid.arrange(box_plots, ncol = 1, heights=c(1))
  #x <- grid.arrange(tblText ,box_plots, ncol = 1, heights=c(1,1))
  #x <- grid.arrange(tblText,tblText2,tblText3, box_plots, ncol = 1, heights=c(1,1,1,8))
  #x <- grid.arrange(tblText,tblText2,tblText3, aas_plot2_line,box_plots, ncol = 1, heights=c(1,1,1,8,8))
  if(debugMode){
    if(plotPDF){
      #debugVars$tblText <- tblText
      #debugVars$tblText2 <- tblText2
      #debugVars$tblText3 <- tblText3
      #debugVars$aas_plot2_line <- aas_plot2_line
      #debugVars$box_plots <- box_plots
    }
    
    
    save(debugVars,file=paste(outFileName,"-debugVars.Rda",sep=""))
    if(okToPrintPlot('page1')){ 
      flog.debug('print_page1 - start')
      if(exists("aas_pct1")){
        x <- grid.arrange(tblText,tblText2,tblText3, arrangeGrob(aas_pct1, aas_pct2, ncol=2),box_plots, ncol = 1, heights=c(1,1,1,8,8))
      }
      else{
        x <- grid.arrange(tblText ,tblText2,tblText3, ncol = 1, heights=c(1,1,1))
      }
      flog.debug('print_page1 - end')
    }
    #x <- grid.arrange(tblText,tblText2,tblText3, box_plots, ncol = 1, heights=c(1,1,1,8))
    #x <- grid.arrange(tblText ,tblText2,tblText3, ncol = 1, heights=c(1,1,1))
  }
  else{
    if(okToPrintPlot('page1')){
      flog.debug('print_page1 - start')
      #tryCatch(x <- grid.arrange(tblText,tblText2,tblText3, aas_plot2_line,box_plots, ncol = 1, heights=c(1,1,1,8,8)), 
      tryCatch(x <- grid.arrange(tblText,tblText2,tblText3, arrangeGrob(aas_pct1, aas_pct2, ncol=2),box_plots, ncol = 1, heights=c(1,1,1,8,8)), 
               error = function(e) {
                 tryCatch(x <- grid.arrange(tblText,tblText2,tblText3, box_plots, ncol = 1, heights=c(1,1,1,8)), 
                          error = function(e) {
                            x <- grid.arrange(tblText ,tblText2,tblText3, ncol = 1, heights=c(1,1,1))
                          }
                 )
               }
      )
      removeIfExists(tblText)
      removeIfExists(tblText2)
      removeIfExists(tblText3)
      removeIfExists(aas_pct1)
      removeIfExists(aas_pct2)
      removeIfExists(x)
      
      flog.debug('print_page1 - end')
    }
  }
  
  #flog.remove(main$current_db_name)
  if(okToPrintPlot('aas')){ 
    
    #grid.newpage()
    #grid.draw(aas_plot)
    print(aas_plot)
    removeIfExists(aas_plot)
  }
  
  
  #tyler remove
  #cpu_plot_tmp <<- cpu_plot
  #!!!!!!!!!!!!!!!!!!!!!!!
  
  

  
  
  
  

  if(okToPrintPlot('cpu')){ 
    cpu_plot <- NULL
    cpu_plot <- plot_cpu(main$DF_MAIN)
    if(inherits(cpu_plot,what='ggplot')){
      print(cpu_plot)
      #print(plot_cpu(main$DF_MAIN))
    }
    removeIfExists(cpu_plot)
  }
  
  if(okToPrintPlot('io')){ 
    io_plot <- plot_io(main$DF_MAIN_BY_SNAP)
    print(io_plot)
    removeIfExists(io_plot)
  }
  
  if(okToPrintPlot('iostat_function')){ 
    if( nrow(main$DF_IOSTAT_FUNCTION)>10){
        iostat_by_function_plot <-  plot_iostat_by_function(main$DF_IOSTAT_FUNCTION)
        flog.debug('print_iostat_function - start',name='print')
      print(iostat_by_function_plot)
      removeIfExists(iostat_by_function_plot)
      flog.debug('print_iostat_function - end',name='print')
    }
  }
  
  
  
  if(okToPrintPlot('io_histogram')){ 
    if( nrow(main$DF_IO_WAIT_HIST)>10){
      c(io_hist_plot, io_hist_area_plot) := plot_io_histograms(main$DF_IO_WAIT_HIST)
      flog.debug('io_histogram - start',name='print')
      #x <- grid.arrange(io_hist_plot, ncol = 1)
      #x <- grid.arrange(io_hist_area_plot, ncol = 1)
      x <- grid.arrange(io_hist_plot,io_hist_area_plot , ncol = 1, heights=c(1,4),as.table=FALSE)
      #print(io_hist_plot)
      flog.debug('io_histogram - end',name='print')
    }
  }
  #head(main$DF_IO_WAIT_HIST)
  
  
  if(okToPrintPlot('aas_facet')){ 
    aas_bars_by_date_plot <- plot_aas_bars_by_date(main$DF_AAS)
    print(aas_bars_by_date_plot)
  }
  

  if(okToPrintPlot('main_activity')){
    main_activity_plot <- plot_main_activity(main$DF_MAIN)
    
    flog.debug('print_main_activity - start',name='print')
    print(main_activity_plot)
    flog.debug('print_main_activity - stop',name='print')
  }
  
  
  #if(!is.null(RAC_activity_plot)){
  if(okToPrintPlot('rac')){ 
    
    RAC_activity_plot <- NULL
    
    
    
    if(data_frame_col_not_null(main$DF_MAIN,"gc_cr_rec_s")){
      
      tryCatch(RAC_activity_plot <- plot_RAC_activity(main$DF_MAIN), 
               error = function(e) {
                 traceback()
                 print(paste0("Error in ",main$current_db_name,": ",e))
                 #browser()
               }
               #,finally=print("finished")
      )
      
      #grid.newpage() 
      tryCatch(
        #grid.draw(RAC_activity_plot), 
        print(RAC_activity_plot), 
        error = function(e) {
          traceback()
          print(paste0("Error in ",main$current_db_name,": ",e))
          #browser()
        }
        #,finally=print("finished")
      )
      
    }
  }
  
  if(okToPrintPlot('memory_plot')){
    flog.debug('generate_memory_plot - start',name='generate')
    tryCatch(memory_plot <- plot_memory(main$DF_MEMORY), 
             error = function(e) {
               
               #browser()
             }
             #,finally=print("finished")
    )
    
    flog.debug('generate_memory_plot - end',name='generate')
    
    flog.debug('print_memory_plot - start',name='print')
    
    tryCatch(print(memory_plot), 
             error = function(e) {
               
               #browser()
             }
             #,finally=print("finished")
    )
    flog.debug('print_memory_plot - stop',name='print')
  }
  
  #memory_sga_advise_plot_tmp <<- memory_sga_advise_plot
  
  if(okToPrintPlot('memory_plot_sga_advise') && okToPrintPlot('memory_plot_pga_advise')){ 
  #if(1==2){
    
    memory_sga_advise_plot <- NULL
    
    if( nrow(main$DF_MEMORY_SGA_ADVICE)>10){
      
      tryCatch(memory_sga_advise_plot <- plot_memory_sga_advise(main$DF_MEMORY_SGA_ADVICE), 
               error = function(e) {
                 memory_sga_advise_plot <- NULL
                 #browser()
               }
               #,finally=print("finished")
      )
    }
    
    memory_pga_advise_plot <- NULL
    
    if( nrow(main$DF_MEMORY_PGA_ADVICE)>10){
      
      tryCatch(memory_pga_advise_plot <- plot_memory_pga_advise(main$DF_MEMORY_PGA_ADVICE), 
               error = function(e) {
                 memory_pga_advise_plot <- NULL
                 #browser()
               }
               #,finally=print("finished")
      )
    }
    
    
    tryCatch({
      if( nrow(main$DF_MEMORY_SGA_ADVICE)>10 && nrow(main$DF_MEMORY_PGA_ADVICE)>10){
        #print(memory_sga_advise_plot)
        if(inherits(memory_sga_advise_plot,what='ggplot') && inherits(memory_pga_advise_plot,what='ggplot')){
          flog.debug('print_memory_advice_plot - start',name='print')
          
          x <- grid.arrange(memory_sga_advise_plot ,memory_pga_advise_plot, ncol = 2, widths=c(1,1))
          
          flog.debug('print_memory_advice_plot - stop',name='print')
        }
        else if(inherits(memory_sga_advise_plot,what='ggplot')){
          x <- grid.arrange(memory_sga_advise_plot , ncol = 1, widths=c(1))
        }
        else if(inherits(memory_pga_advise_plot,what='ggplot')){
          x <- grid.arrange(memory_pga_advise_plot , ncol = 1, widths=c(1))
        }
      }
      },
      error = function(e) {
        #memory_pga_advise_plot <- NULL
        #browser()
      }
    )
    
    
  }
  
  if(okToPrintPlot('db_parameters')){
    tryCatch(plot_db_parameters(), 
             error = function(e) {
               
               #browser()
             }
             #,finally=print("finished")
    )
  }
  
  if(okToPrintPlot('sql_text')){
    if(nrow(main$DF_SQL_SUMMARY)> 5){
      
      tryCatch(plot_sql_text(), 
               error = function(e) {
                 
                 #browser()
               }
               #,finally=print("finished")
      )
    }
  }
  
  if(plotPDF){
    dev.off()
  }
  
  main$plot_attributes <- rbind(main$plot_attributes,main$current_plot_attributes)
  
  
  
  
  if(debugMode){
    if(exists("debugMoveFiles")){
      if(debugMoveFiles){
        # tyler added to move files to a done dir
        doneDir <- paste0(dirname(f),"/done/")
        if (!file.exists(doneDir)){
          dir.create(doneDir)
        }
        
        file.rename(from=f,
                    to=paste0(dirname(f),"/done/",basename(f))
        )
      }
    }
    
  }
  
  
  # improved list of objects
  .ls.objects <- function (pos = 1, pattern, order.by,
                           decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
      fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
      capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
      as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
      out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
      out <- head(out, n)
    out
  }
  
  # shorthand
  lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
  }
  
  #print(lsos(pos = environment()))
  
  flog.debug(paste0('Database - ',main$current_db_name," - end"))
  flog.info(paste0('Finished DB: ',main$current_db_name))
  flog.info(paste0('Finished DB: ',main$current_db_name," - ",main$current_dbid),name='status')
}

main$mainLoop <- function(){
  #for (f in main$db_id) {
  for (f in main$awrFiles) {
    if(debugMode){
      print('Running without tryCatch as we are in debug mode')
      #l <<- lineprof(main$mainFunction(f))
      main$mainFunction(f)
    }
    else{
      tryCatch(
        #l <<- lineprof(main$mainFunction(f)),
        main$mainFunction(f), 
               error = function(e) {
                 traceback()
                 flog.appender(appender.file(paste0(f,'.err')), name=f)
                 flog.error(paste0("Error in ",f,": ",e),name=f)
                 print(e)
                 flog.error(traceback(),name=f)
                 flog.remove(f)
                 #browser()
                 
               }
               #,finally=print("finished")
      )
      
    } #end else
    
  } #end for loop
  
  #TEST_DF <<- main$overall_summary_df
  
 
  
  if(okToPrintPlot('summary_html')){
    SUMMARY_DF_TMP <- main$overall_summary_df
    SUMMARY_DF_TMP$link <- paste0('<a href="',SUMMARY_DF_TMP$outFileName,'">',SUMMARY_DF_TMP$outFileName,'</a>')
    #TEST_DF <<- SUMMARY_DF_TMP
    #SUMMARY_DF_TMP <- subset(SUMMARY_DF_TMP,select=c(name,nodes,platform,cores,version,memused,sizegb,aas,link))
    
    SUMMARY_DF_TMP <- SUMMARY_DF_TMP[order(SUMMARY_DF_TMP$aas,decreasing = TRUE),]
    sjt.df(SUMMARY_DF_TMP,file=paste0("summary.html"),describe=FALSE,alternateRowColors=TRUE)
    rm(SUMMARY_DF_TMP)
  }
  
  
  
  write.csv(main$overall_summary_df,'OverallSummary.csv')
  #print(head(main$plot_attributes,n=20))
  if(length(main$plot_attributes) > 0){
    if(file.exists('attributes.csv')){
      DF_OLD_ATTRIBUTES_INT <- read.csv('attributes.csv', head=TRUE,sep=",",stringsAsFactors=FALSE)
      #print(head(DF_OLD_ATTRIBUTES_INT,n=20))
    
      if(!(identical(DF_OLD_ATTRIBUTES_INT,main$plot_attributes))){
        for (i in 1:100)
        {
          newName <- paste("attributes","old",i,"csv",sep=".")
          if(!(file.exists(newName))){
            file.rename(from='attributes.csv',
                        to=newName)
            break
          }
        }
        
        
      }
    }
    
    write.csv(main$plot_attributes,'attributes.csv',row.names=FALSE)
  }
  
  
  
  
  
  
  
  
  if(debugMode){
    awrM$debug.unitTimes[awrM$debug.unitTimes == ""] <<- NA
    
    
    awrM$debug.unitTimesWide <<- reshape(subset(na.omit(awrM$debug.unitTimes),length(awrM$debug.unitTimes$opp)>2), 
                                         timevar = "opp",
                                         idvar = c("db", "level", "message"),
                                         direction = "wide")
    
    awrM$debug.unitTimesWide$time.start <- as.POSIXct(awrM$debug.unitTimesWide$time.start, format = "%Y-%m-%d %H:%M:%S",tz="UTC")
    awrM$debug.unitTimesWide$time.end <- as.POSIXct(awrM$debug.unitTimesWide$time.end, format = "%Y-%m-%d %H:%M:%S",tz="UTC")
    awrM$debug.unitTimesWide$duration <- difftime(awrM$debug.unitTimesWide$time.end , awrM$debug.unitTimesWide$time.start , unit="secs")
    print(head(awrM$debug.unitTimesWide,30))
    save(awrM,file="awrM.Rda")
    
    
    
  }
  
}

#get_db_names()
#library(lineprof)
#l <- NULL
main$mainLoop()



 #library(lineprof)
 #l <- lineprof(main$mainLoop())
# print(l, depth = 3)
#shine(l)

#====================================================================================================================









