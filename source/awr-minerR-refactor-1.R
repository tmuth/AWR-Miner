library(futile.logger)
#====================================================================================================================
# Change Variables in this sections as needed 

WORK_DIR <- 'E:/Portable-AWR-Miner/CSVs'
#WORK_DIR <- 'E:/Portable-AWR-Miner/CSVs/LAD-Cust-Help'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/NC/Public Schools/Pearson/Pearson-POV/Results/Test-H/CSVs'

#os_files <- list.files(pattern="EBS-2045810607-os.csv")
os_files <- list.files(pattern="^*.*os.csv$")

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

main <- new.env()

attr <- new.env()
attr$filter_snap_min <- 1
attr$filter_snap_max <- 1000000000
attr$vertical_line <- theme()
attr$vertical_text <- theme()

setwd(WORK_DIR)


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
  for (f in os_files) {
    #list(c(unlist(LL),5:9))
    #db_name <- list(c(unlist(db_name),gsub(pattern = "awr-wl-([a-zA-Z0-9_]+).*", replacement="\\1", f)))
    main$db_name <- c(main$db_name,gsub(pattern = "([a-zA-Z0-9_]+)-.*", replacement="\\1", f))
    main$db_id <- c(main$db_id,gsub(pattern = "([a-zA-Z0-9_]+)-([0-9]+).*", replacement="\\2", f))
    log_it.info(paste0('Found file for: ',main$db_name))
    flog.trace(f)
  }
  main$db_id <- unique(main$db_id)
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


build_data_frames <- function(dbid,dbname) {
  log_it.debug("build_data_frames - start")
  log_it.info(paste0("dbid-dbname: ",dbid,'-',dbname))
  #   dbid <- 1499932796
  #   dbname <- main$current_db_name
  
  DATA_FRAME_INT <- NULL
  file_pattern=paste(WORK_DIR,paste(dbname,dbid,sep="-"),sep="/")
  DF_OS_INT <- read.csv(paste(file_pattern,"-os.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=FALSE)
  DF_MAIN_INT <- read.csv(paste(file_pattern,"-main.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  DF_MEMORY_INT <- read.csv(paste(file_pattern,"-memory.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  DF_SPACE_INT <- read.csv(paste(file_pattern,"-space.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  DF_AAS_INT <- read.csv(paste(file_pattern,"-aas.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=FALSE)
  
  #flog.trace('Summary',summary(DF_AAS_INT),capture=TRUE)
  
  DF_SQL_SUMMARY_INT <- read.csv(paste(file_pattern,"-sql-summary.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  DF_SQL_BY_SNAPID_INT <- read.csv(paste(file_pattern,"-sql-by-snap.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  
  #DF_MAIN_INT$end <- strptime(DF_MAIN_INT$end, "%y/%m/%d %H:%M")
  DF_MAIN_INT$end <- as.POSIXct(DF_MAIN_INT$end, format = "%y/%m/%d %H:%M")
  
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
               flog.appender(appender.file(paste0(main$current_db_name,'.log')), name=main$current_db_name)
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
