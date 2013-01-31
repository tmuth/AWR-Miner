library(plyr)

library(ggplot2)
library(gridExtra)
library(scales)
library(reshape)
library(xtable)
library(ggthemes)
# 
# 
 myTheme <- theme_stata() +
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

setwd("M:/Dropbox/MyFiles/Projects/AWR-Mining-Reboot/Versions/2.9")
# source(file="custom-packages.R")




main <- new.env()


#date_break_major_var <- date_breaks("1 hour")
date_break_major_var <- date_breaks("1 day")
#date_break_minor_var <- date_breaks("1 hour")
date_break_minor_var <- date_breaks("12 hour")





main$avg_max_colors <- c("Avg" = "#1F77B4", "Max" = "#D62728", 
                          "OS CPU Avg" = "#1F77B4", "OS CPU Max" = "#D62728")

main$gg_avg_max_fill <- scale_fill_manual("", values = main$avg_max_colors) 
main$gg_avg_max_color <- scale_colour_manual("", values = main$avg_max_colors)

#vert_line_snap_ids <- data.frame(db_name="PSPROD",snap_id=240)
#vert_line_snap_ids <- rbind(vert_line_snap_ids, data.frame(db_name="PSPRODDB",snap_id=4798))
#vert_line_snap_ids <- rbind(vert_line_snap_ids, data.frame(db_name="PSPRODDB",snap_id=479))


#vert_line <- theme()
# vert_line <- geom_vline(xintercept=c(as.vector(as.POSIXct('12/11/23 10:38', format = "%y/%m/%d %H:%M")),
#                                      as.vector(as.POSIXct('12/11/23 14:52', format = "%y/%m/%d %H:%M"))
#                                      ), linetype="dotted",color="#ff513a",size=0.5,alpha=0.5)

vert_line <- geom_vline(xintercept=c(as.vector(as.POSIXct('13/01/15 19:59', format = "%y/%m/%d %H:%M")),
                                     as.vector(as.POSIXct('13/01/16 16:00', format = "%y/%m/%d %H:%M")),
                                     as.vector(as.POSIXct('13/01/17 06:00', format = "%y/%m/%d %H:%M"))
                                     ), linetype="dotted",color="#ff513a",size=0.5,alpha=0.5)

#geom_vline(xintercept=as.vector(DF_SNAP_ID_DATE[with(DF_SNAP_ID_DATE,SNAP_ID == 10795),]$end), linetype="dotted",color="#ff513a",size=0.5,alpha=0.5)

# vert_line <- geom_vline(xintercept=c(as.vector(main$DF_SNAP_ID_DATE[with(main$DF_SNAP_ID_DATE,snap == 10795),]$end),
#                                      as.vector(main$DF_SNAP_ID_DATE[with(main$DF_SNAP_ID_DATE,snap == 10815),]$end),
#                                      as.vector(main$DF_SNAP_ID_DATE[with(main$DF_SNAP_ID_DATE,snap == 10829),]$end)
#                                      ), linetype="dotted",color="#ff513a",size=0.5,alpha=0.5)


#WORK_DIR <- 'M:/Dropbox/MyFiles/Code Samples/SQL Tuning/AWR Mining/AWR-Mining-Reboot/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/NC/Public Schools/Pearson/AWRs/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/OH/Jobs and Family Services/AWR-Miner/Dev/CSVs'
WORK_DIR <- 'E:/Portable-AWR-Miner/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/OH/State Teachers Retirement/AWR-Mining/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/Federal/DLA/Accenture-SAP/AWR-Mining/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/Federal/DLA/Accenture-SAP/AWR-Mining/awr/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/NC/Public Schools/Pearson/Pearson-POV/Results/Test-C/raw-data/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/NC/Public Schools/Pearson/Pearson-POV/Results/Test-H/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/Commercial/Unknown/Kurt Vogel/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/MN/State of Minnesota MMB/Peoplesoft-POV/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/S&L/MI/MI Dept of Technology, Mgt and Budget/QA-DR/CSVs'
#WORK_DIR <- 'E:/Temp/Hichwa-Exadata-Tests/CSVs'
#WORK_DIR <- 'M:/Dropbox/MyFiles/Accounts/Federal/Census/Decennial/Decennial-POV/AWR-Miner/35-State-Run-Test2/CSVs'





MAX_DAYS <- 32
#remove(INSTANCE_FILTER)
#INSTANCE_FILTER <- c(3,4)

setwd(WORK_DIR)


#os_files <- list.files(pattern="^ZG1-.*os.csv$")
#os_files <- list.files(pattern="^APPSPROD.*os.csv$")

#os_files <- list.files(pattern="EBS-2045810607-os.csv")
os_files <- list.files(pattern="^*.*os.csv$")
#os_files <- list.files(pattern="^PM1.+-os.csv")
#os_files <- list.files(pattern="^EFNP.+-os.csv")

# colors from Color Inspiration book, p30, "coal tar"
colours <- c("#2F295F","#FC4723", "#288CF8", "#FC2346", "#F7DE28","#BBB509","#C03360")



# main$gg_bottom_panel <- opts(legend.position = "bottom",legend.direction = "horizontal",legend.width=unit(20,"cm"),
#                         panel.background = theme_rect(colour = "#aaaaaa"))

main$db_name = vector()
main$db_id = vector()
for (f in os_files) {
  #list(c(unlist(LL),5:9))
  #db_name <- list(c(unlist(db_name),gsub(pattern = "awr-wl-([a-zA-Z0-9_]+).*", replacement="\\1", f)))
  main$db_name <- c(main$db_name,gsub(pattern = "([a-zA-Z0-9_]+)-.*", replacement="\\1", f))
  main$db_id <- c(main$db_id,gsub(pattern = "([a-zA-Z0-9_]+)-([0-9]+).*", replacement="\\2", f))
  #print(main$db_name)
}

#main$db_name <- unique(main$db_name)
main$db_id <- unique(main$db_id)

#print(db_name)
#print(db_id)

log_it <- function(x){
  print(x)
}

convert_snap_id_to_posixct <- function(snapID){
  theDateChar <- subset(main$DF_SNAP_ID_DATE2 , snap==snapID)
  theDateReturn <- as.POSIXct(theDateChar$end, format = "%y/%m/%d %H:%M")
  return(theDateReturn)
}


get_attrs <- function(SEARCH_VAL){
  if(nrow(subset(main$current_plot_attributes, variable==SEARCH_VAL)) == 0){
    return(NA)
  }
  else{
    return(main$current_plot_attributes[with(main$current_plot_attributes,variable==SEARCH_VAL),])
  }
}

attr <- new.env()
attr$filter_snap_min <- 1
attr$filter_snap_max <- 1000000000
attr$vertical_line <- theme()
attr$vertical_text <- theme()

apply_current_attributes <- function(){

  ## Filter by snap ID
  if(nrow(main$current_plot_attributes) > 0){
    DF_TEMP <- NULL
    DF_TEMP <- get_attrs('snap_id_filter')
    if(length(DF_TEMP)>0){
      if(length(DF_TEMP$value1)>0){ 
        attr$filter_snap_min <- as.vector(DF_TEMP$value1) }
      if(length(DF_TEMP$value2)>0){ 
        attr$filter_snap_max <- as.vector(DF_TEMP$value2) }
      #attr$filter_snap_max <- as.vector(DF_TEMP$value2)
    }
  }
  
  ## Filter by date
  
  
  
  return(TRUE)
}

add_vetical_lines <- function(){
  if(nrow(main$current_plot_attributes) > 0){
    DF_TEMP <- NULL
    DF_TEMP <- get_attrs('annotated_line')
    V_VERT_VECT <- numeric(0)
    DF_VERT_TEXT <- data.frame()
    for (i in 1:nrow(DF_TEMP)){
      if(length(DF_TEMP[i,]$value1)>0){
        theDate <- DF_TEMP[i,]$value1
        theNumbers <- grepl("^[[:digit:]]+$", theDate) 
        print(theNumbers)
        #if(as.numeric(theDate) > 0){
        if(theNumbers){
          theDate <- convert_snap_id_to_posixct(theDate)
        }
        else{
          #theDate <- c(as.vector(as.POSIXct(theDate, format = "%y/%m/%d %H:%M")))
          theDate <- as.POSIXct(theDate, format = "%y/%m/%d %H:%M")
        }
        
        V_VERT_VECT <- c(V_VERT_VECT, theDate)
        
        if(length(DF_TEMP[i,]$value2)>0){
          DF_VERT_TEXT <- rbind(DF_VERT_TEXT,data.frame(end=theDate,label=DF_TEMP[i,]$value2))
        }
      }
    }
    
    
    if(length(V_VERT_VECT)>0){
      attr$vertical_line <- geom_vline(xintercept=V_VERT_VECT, linetype="dotted",color="#555555",size=0.2,alpha=0.5)}
    
    if(length(DF_VERT_TEXT)>0){
      attr$vertical_text <- geom_text(aes(x=end,label=label,y=0), data=DF_VERT_TEXT,angle=90,size=1.5,hjust=0,vjust=-0.2,alpha=0.5,color="#555555")}
  }
}

build_data_frames <- function(dbid,dbname) {
  log_it("build_data_frames - start")
  log_it(paste0("dbid-dbname: ",dbid,dbname))
#   dbid <- 1499932796
#   dbname <- main$current_db_name
  
  DATA_FRAME_INT <- NULL
  file_pattern=paste(WORK_DIR,paste(dbname,dbid,sep="-"),sep="/")
  DF_OS_INT <- read.csv(paste(file_pattern,"-os.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=FALSE)
  DF_MAIN_INT <- read.csv(paste(file_pattern,"-main.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  DF_MEMORY_INT <- read.csv(paste(file_pattern,"-memory.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  DF_SPACE_INT <- read.csv(paste(file_pattern,"-space.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=TRUE)
  DF_AAS_INT <- read.csv(paste(file_pattern,"-aas.csv",sep=""), head=TRUE,sep=",",stringsAsFactors=FALSE)
  
  print(summary(DF_AAS_INT))
  
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

  min_snap_id <- min(subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))$snap)
  #DF_MAIN_INT <- subset(DF_MAIN_INT, end >= max(DF_MAIN_INT$end)-as.difftime(MAX_DAYS, unit="days"))
  # tyler changed for Pearson
  
  print(summary(DF_AAS_INT))
  
  #print(head(DF_OS_INT))
  log_it("build_data_frames - end")
  return(list(DF_OS_INT,DF_MAIN_INT,DF_MEMORY_INT,DF_SPACE_INT,DF_AAS_INT,DF_SQL_SUMMARY_INT,DF_SQL_BY_SNAPID_INT,DF_SNAP_ID_DATE_INT))
}

#foo<- build_data_frames(main$db_id,main$db_name)

generate_hours_bars <- function(DF_MAIN_INT){
  log_it('generate_hours_bars - start')
  #DF_MAIN_INT <- main$DF_MAIN
  DF_NIGHT_HOURS_INT <- data.frame("end"= unique(as.POSIXlt(strptime(format(DF_MAIN_INT$end,"%y/%m/%d"),format="%y/%m/%d"))))
  
  DF_NIGHT_HOURS_INT_A <- DF_NIGHT_HOURS_INT
  DF_NIGHT_HOURS_INT_A$work_start <- DF_NIGHT_HOURS_INT_A$end + 1
  DF_NIGHT_HOURS_INT_A$work_stop <- DF_NIGHT_HOURS_INT_A$work_start + ((3600*7)-1)
  
  DF_NIGHT_HOURS_INT_B <- DF_NIGHT_HOURS_INT
  DF_NIGHT_HOURS_INT_B$work_start <- DF_NIGHT_HOURS_INT_B$end + (3600*19)
  DF_NIGHT_HOURS_INT_B$work_stop <- DF_NIGHT_HOURS_INT_B$end + ((3600*24)-1)
  
  DF_NIGHT_HOURS_INT_A <- rbind(DF_NIGHT_HOURS_INT_A, DF_NIGHT_HOURS_INT_B)
  
  # find date ranges outside of the filtered min and max and REMOVE them
  idx_date_min_rm <- !with(DF_NIGHT_HOURS_INT_A, work_stop < min(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A<- DF_NIGHT_HOURS_INT_A[idx_date_min_rm,]
  idx_date_max_rm <- !with(DF_NIGHT_HOURS_INT_A, work_start > max(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A<- DF_NIGHT_HOURS_INT_A[idx_date_max_rm,]
  # find remaining date ranges outside of the filtered min and max and CHANGE them
  idx_date_min <- with(DF_NIGHT_HOURS_INT_A, work_start < min(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A$work_start[idx_date_min] <- min(main$DF_SNAP_ID_DATE$end)
  idx_date_max <- with(DF_NIGHT_HOURS_INT_A, work_stop > max(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A$work_stop[idx_date_max] <- max(main$DF_SNAP_ID_DATE$end)
  
  DF_NIGHT_HOURS_INT_A$value <- 0
  
  hour_bars <- geom_rect(data=DF_NIGHT_HOURS_INT_A,aes(xmin=work_start,xmax=work_stop,ymin=-Inf,ymax=Inf),alpha=0.1,fill="#bdd1e6")
  log_it('generate_hours_bars - end')
  return(hour_bars)
}

generate_snap_id_labels <- function(DF_SNAP_ID_DATE_INT){
  #DF_SNAP_ID_DATE_INT <- DF_SNAP_ID_DATE
  log_it('generate_snap_id_labels - start')
  num_dates <- as.numeric(nrow(DF_SNAP_ID_DATE_INT))
  num_snap_ids <- min(num_dates,40)
  
  mod_num <- round(num_dates/num_snap_ids)
  
  DF_SNAP_ID_SUBSET_INT <- subset(DF_SNAP_ID_DATE_INT,(SNAP_ID %% mod_num)==0,rownames=FALSE)
  log_it('generate_snap_id_labels - end')
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


load_plot_attributes <- function(){
  #main$attributes_file 
  DF_ATTRIBUTES_INT <- data.frame()
  log_it('load_plot_attributes - start')
  if(file.exists('attributes.csv')){
    DF_ATTRIBUTES_INT <- read.csv('attributes.csv', head=TRUE,sep=",",stringsAsFactors=FALSE)
    DF_ATTRIBUTES_INT <- subset(DF_ATTRIBUTES_INT, db == main$current_db_name)
    print(head(DF_ATTRIBUTES_INT))
    
  }
  
  log_it('load_plot_attributes - end')
  
  return(DF_ATTRIBUTES_INT)
}


generate_plot_attributes <- function(){
  
  log_it('generate_plot_attributes - start')
  
  df_plot_attr_int <- NULL
  
  add_df_row <- function(df,attr,val1,val2){
    df<-rbind(df,data.frame(db=main$current_db_name,variable=attr,value1=val1,value2=val2))
    return(df)
  }
  

  df_plot_attr_int <- add_df_row(df_plot_attr_int,'$date_range',format(min(main$DF_SNAP_ID_DATE$end),"%Y-%m-%d %H:%M:%S"),format(max(main$DF_SNAP_ID_DATE$end),"%Y-%m-%d %H:%M:%S"))
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'$snap_id_range',as.character(min(main$DF_SNAP_ID_DATE$SNAP_ID)),as.character(max(main$DF_SNAP_ID_DATE$SNAP_ID)))
  #df_plot_attr_int <- add_df_row(df_plot_attr_int,'date_filter','','')
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'snap_id_filter','','')
  
  #df_plot_attr_int <- add_df_row(df_plot_attr_int,'comments','','')
  df_plot_attr_int <- add_df_row(df_plot_attr_int,'annotated_line','','')
  #print(head(df_plot_attr_int))
  
  log_it('generate_plot_attributes - end')
  return(df_plot_attr_int)
}



save_plot_attributes <- function(){
  #main$attributes_file
  #file.exists(file_name)
  
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
  log_it('summarise_dfs_by_snap - start')
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
  log_it('summarise_dfs_by_snap - end')
  return(list(DF_MAIN_BY_SNAP_INT))
}


plot_io <- function(DF_MAIN_BY_SNAP_INT){
  log_it('plot_io - start')
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
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(color=stat),alpha=0.5) +
    geom_line(aes(color=stat), size=.2,alpha=0.5)+
    #stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
          #      aes(color=stat,fill=stat))+
                  #scale_colour_few()+
                  #scale_fill_few()+
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
                  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_break_major_var,
                                   minor_breaks = date_break_minor_var,
                                   limits = c(min(x.melt$end),max(x.melt$end)))
                                     #main$gg_bottom_panel
  
  #p
  
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  log_it('plot_io - end')
  return(p_gt)
  #}
}



plot_cpu <- function(DF_MAIN_INT){
  log_it('plot_cpu - start')
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
  
  
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(group=variable,color=variable),alpha=0.3) +
    geom_line(aes(group=variable,color=variable), size=.2)+
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
                   scale_x_datetime(labels = date_format("%a, %b %d %I %p"),breaks = date_break_major_var,
                                    minor_breaks = date_break_minor_var,
                                    limits = c(min(x.melt$end),max(x.melt$end)))+
                                      main$gg_avg_max_fill+main$gg_avg_max_color
                                    # theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))
                                     #theme(panel.grid.major = element_line("#eeeeee", size = 0.2,linetype = "dotted"))+
                                     #theme(panel.grid.minor = element_line("#efefef", size = 0.1,linetype = "dotted"))
                                     #main$gg_bottom_panel
  
  #p
   p_gt <- ggplot_gtable(ggplot_build(p))
  log_it('plot_cpu - end')
#   p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
   return(p_gt)
  #}
}



plot_aas_chart <- function(DF_AAS_INT){
  log_it('plot_aas_chart - start')
  log_it(min(DF_AAS_INT$end))
  log_it(max(DF_AAS_INT$end))
  
  log_it(min(main$DF_SNAP_ID_SUBSET$end))
  log_it(max(main$DF_SNAP_ID_SUBSET$end))
  #browser()
  #DF_AAS_INT <- DF_AAS
  
  #print(unique(main$DF_AAS$WAIT_CLASS))
  DF_AAS_INT$WAIT_CLASS <- factor(DF_AAS_INT$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
  
  
  vals <- expand.grid(end = unique(DF_AAS_INT$end),
                      WAIT_CLASS = unique(DF_AAS_INT$WAIT_CLASS))
  DF_AAS_INT <- merge(vals,DF_AAS_INT)
  
  
  DF_AAS_INT[is.na(DF_AAS_INT)] <- 0
  
  
  
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
  scale_x_datetime(breaks=NULL) 
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
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_break_major_var,
                     minor_breaks = date_break_minor_var
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
  log_it('plot_aas_chart - end')
  return(list(plot_aas_wait_class_gt,plot_aas_wait_class2_gt,plot_aas_wait_class2_line))
  
  
}



plot_main_activity <- function(DF_MAIN_INT){
  log_it('plot_main_activity - start')
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
  #db_block_gets_s=sum(db_block_gets_s),
  #db_block_changes_s=sum(db_block_changes_s))
  
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
  #x.melt[with(x.melt, grepl("db_block_gets_s", variable)),]$variable<-"Block Gets/s"
  #x.melt[with(x.melt, grepl("db_block_changes_s", variable)),]$variable<-"Block Changes/s"
  
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
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_break_major_var,
                     minor_breaks = date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))
                       #scale_colour_tableau("colorblind10")+scale_fill_tableau("colorblind10")
                      
  
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  log_it('plot_main_activity - end')
  #grid.draw(p_gt)
  return(p_gt)
  
  
}




plot_RAC_activity <- function(DF_MAIN_INT){
  log_it('plot_RAC_activity - start')
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
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_break_major_var,
                     minor_breaks = date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))
  #scale_colour_tableau("colorblind10")+scale_fill_tableau("colorblind10")
  
  
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  log_it('plot_RAC_activity - end')
  #grid.draw(p_gt)
  return(p_gt)
  
  
}

#foo<-plot_RAC_activity(main$DF_MAIN)



plot_memory <- function(DF_MEMORY_INT){
  log_it('plot_memory - start')
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
    scale_x_datetime(labels = date_format("%a, %b %d"),breaks = date_break_major_var,
                     minor_breaks = date_break_minor_var,
                     limits = c(min(DF_MEMORY_INT_SUM$end),max(DF_MEMORY_INT_SUM$end)))
                       #opts(axis.text.x=theme_text(angle=-30, hjust=-.1,vjust=1,size=6))+
                       #opts(panel.grid.major = theme_line("#eeeeee", size = 0.2,linetype = "dotted"))+
                       #opts(panel.grid.minor = theme_line("#efefef", size = 0.1,linetype = "dotted"))
  
  p
  log_it('plot_memory - end')
  return(p)
  #}
}

gen_summary_data <- function(){
  log_it('gen_summary_data - start')
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
  print(paste('instances: ',num_instances,sep=''))
  #DF_OS_INT <- data.frame(name=main$current_db_name,nodes=num_instances)
  DF_OS_INT <- data.frame(name=main$current_db_name,nodes=num_instances,
                 platform=get_os_stat_string("PLATFORM_NAME"),version=get_os_stat_string("VERSION"),
                 sockets=ifelse(is.na(get_os_stat("NUM_CPU_SOCKETS")) == TRUE, NA, get_os_stat("NUM_CPU_SOCKETS")*num_instances),
                          #sockets=(get_os_stat("NUM_CPU_SOCKETS")*get_os_stat("INSTANCES")),
                  #sockets=(get_os_stat("NUM_CPU_SOCKETS")*get_os_stat("INSTANCES")),
                  cores=(get_os_stat("NUM_CPU_CORES")*num_instances),
                  threads=(get_os_stat("NUM_CPUS")*num_instances),
                  mem=(get_os_stat("PHYSICAL_MEMORY_GB") * num_instances)
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
  log_it('gen_summary_data - end')
  return(list(DF_OS_INT,DF_SUM_COMBINED_INT,DF_SUM_COMBINED_INT2))
  
}

# plot_sql_summary <- function(DF_SQL_SUMMARY_INT){
#   
#   ggplot(data=DF_SQL_SUMMARY,aes(x=ELAP_RANK,y=EXEC_RANK))+geom_point()+geom_smooth()
#   ggplot(data=DF_SQL_SUMMARY,aes(x=ELAP_RANK,y=LOG_READS_RANK))+geom_point()+geom_smooth()
#   ggplot(data=DF_SQL_SUMMARY,aes(x=ELAP_RANK,y=PHYS_READS_RANK))+geom_point()+geom_smooth()
#   ggplot(data=DF_SQL_SUMMARY,aes(x=ELAP_RANK,y=EXEC_RANK))+geom_point()+geom_smooth()
#   cor(DF_SQL_SUMMARY[4:7] )
#   plotmatrix(with(DF_SQL_SUMMARY, data.frame(ELAP_RANK, EXEC_RANK, LOG_READS_RANK, PHYS_READS_RANK)))
# }

# ==========================================================================



plot_summary_boxplot_main <- function(){
  log_it('plot_summary_boxplot_main - start')
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
  
  summary_vals <- rbind(median_vals,mean_vals,quant_low,quant_high)
  summary_vals$id <- 1
  
  #print(summary(x.melt))
  #print(head(x.melt))
  
  
  p <- ggplot(data=x.melt, aes(x=id, y=value),aes(fill=variable),position="dodge")+
    geom_violin(aes(),fill="#4DAF4A",colour="#000000",size=0.05,alpha=0.6,adjust=0.5) +
    geom_boxplot(aes(),colour="#000000",alpha=.6,show_guide=FALSE,notch = FALSE,outlier.colour = "orange", outlier.size = 2,outlier.alpha=.5,outlier.shape=5)+
    geom_jitter(alpha=.3,size=1,position = position_jitter(width = .2,height=0),aes(colour="gray"))+
    geom_text(data=summary_vals,aes(y=value,label=round(value,1)),size=2,vjust=-0.8,hjust=0)+
    facet_wrap( ~ variable,scales="free",nrow=1)+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4,alpha=0.7,colour="#D62728")+
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
  log_it('plot_summary_boxplot_main - end')
  return(p)
}

build_snap_to_date_df <- function(){
  DF_SNAP_ID_DATE_INT <- ddply(main$DF_MAIN, .(snap), summarise, 
                               end=min(as.POSIXct(end)),
                               aas=sum(aas)) 
  DF_SNAP_ID_DATE_INT$end <- format(DF_SNAP_ID_DATE_INT$end,"%y/%m/%d %H:%M")
  return(DF_SNAP_ID_DATE_INT)
}


plot_snap_id_list <- function(){
  dateTable <- function(df){
    df[is.na(df)] <- ""
    return(tableGrob(df,show.rownames = FALSE, gpar.coretext = gpar(fontsize=6),gpar.coltext = gpar(fontsize=4),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left", gpar.corefill = gpar(fill=NA,col=NA) ))
    
  }
  
  
  snapIdDateText1 <- dateTable(main$DF_SNAP_ID_DATE2[c(seq(1,80)),])
  snapIdDateText2 <- dateTable(main$DF_SNAP_ID_DATE2[c(seq(81,160)),])
  snapIdDateText3 <- dateTable(main$DF_SNAP_ID_DATE2[c(seq(161,240)),])
  snapIdDateText4 <- dateTable(main$DF_SNAP_ID_DATE2[c(seq(241,320)),])
  snapIdDateText5 <- dateTable(main$DF_SNAP_ID_DATE2[c(seq(321,400)),])
  snapIdDateText6 <- dateTable(main$DF_SNAP_ID_DATE2[c(seq(401,480)),])
  snapIdDateText7 <- dateTable(main$DF_SNAP_ID_DATE2[c(seq(481,560)),])
  #grid.newpage()
  grid.arrange(snapIdDateText1,snapIdDateText2,snapIdDateText3,snapIdDateText4,snapIdDateText5,snapIdDateText6,snapIdDateText7,ncol = 7, widths=c(1,1,1,1,1,1,1))
  
}

plot_sql_summary <- function(){
  main$DF_SQL_SUMMARY$AVG_DOP <- main$DF_SQL_SUMMARY$PX_SERVERS_EXECS / main$DF_SQL_SUMMARY$EXECS
  main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M <- (main$DF_SQL_SUMMARY$ELAP / main$DF_SQL_SUMMARY$EXECS)/60
  main$DF_SQL_SUMMARY$logRsGBperExec <- ((main$DF_SQL_SUMMARY$LOG_READS* 8)/main$DF_SQL_SUMMARY$EXECS)/1024/1024
  main$DF_SQL_SUMMARY$ELAP <-  formatC(main$DF_SQL_SUMMARY$ELAP, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$EXECS <-  formatC(main$DF_SQL_SUMMARY$EXECS, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$LOG_READS <-  formatC(main$DF_SQL_SUMMARY$LOG_READS, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M <-  formatC(main$DF_SQL_SUMMARY$ELAP_PER_EXEC_M, digits=2,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$AVG_DOP <-  formatC(main$DF_SQL_SUMMARY$AVG_DOP, digits=0,format="fg", big.mark=",")
  main$DF_SQL_SUMMARY$logRsGBperExec <-  formatC(main$DF_SQL_SUMMARY$logRsGBperExec, digits=2,format="fg", big.mark=",")
  
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PARSING_SCHEMA_NAME"] <- "PARSING_SCHEMA"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="LOG_READS_RANK"] <- "logRsRank"
  names(main$DF_SQL_SUMMARY)[names(main$DF_SQL_SUMMARY)=="PHYS_READS_RANK"] <- "physRsRank"
  #subset(df, select=-c(z,u))
  sqlSummaryText1 <- tableGrob(subset(main$DF_SQL_SUMMARY, select=-c(PX_SERVERS_EXECS,LOG_READS)),show.rownames = FALSE, gpar.coretext = gpar(fontsize=7),gpar.coltext = gpar(fontsize=5),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left", gpar.corefill = gpar(fill=NA,col=NA),h.even.alpha = 0 )
  #print(sqlSummaryText1)
  grid.arrange(sqlSummaryText1,ncol = 1, widths=c(1))
}

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

main$foo <- vector()

main$mainFunction <- function(f){
  
  vector_element = which(main$db_id==f)
  main$current_db_name=main$db_name[which(main$db_id==f)]
  
  main$DATA_FRAME <- NULL
  main$DF_OS <- NULL
  main$DF_MEMORY <- NULL
  main$DF_MAIN <- NULL
  main$DF_MAIN_BY_SNAP <- NULL
  main$current_plot_attributes <- NULL
  
  main$current_plot_attributes <- load_plot_attributes()
  apply_current_attributes()
  
  c(main$DF_OS, main$DF_MAIN,main$DF_MEMORY,main$DF_SPACE,main$DF_AAS,main$DF_SQL_SUMMARY,main$DF_SQL_BY_SNAPID,main$DF_SNAP_ID_DATE) := build_data_frames(f,main$current_db_name)
  c(main$DF_MAIN_BY_SNAP) := summarise_dfs_by_snap()
  main$DF_SNAP_ID_DATE2 <- build_snap_to_date_df()
  
  add_vetical_lines()
  
  if(length(main$current_plot_attributes)<2)
    main$current_plot_attributes <- generate_plot_attributes()
  
  #print(head(main$DF_MAIN))
  print(main$current_db_name)
  #break

  
  main$DF_SNAP_ID_SUBSET <- generate_snap_id_labels(main$DF_SNAP_ID_DATE)
  
  if(is.na(get_os_stat("NUM_CPU_CORES"))) main$num_cpu_cores <- get_os_stat("NUM_CPUS") else main$num_cpu_cores <- get_os_stat("NUM_CPU_CORES")
  
  main$cpu_cores <- main$num_cpu_cores*get_os_stat("INSTANCES")
  
  main$gg_hour_bars <- generate_hours_bars(main$DF_MAIN)
  
  c(main$DF_SUMMARY_OS,main$DF_SUMMARY_MAIN,main$DF_SUMMARY_OVERALL) := gen_summary_data()
  main$overall_summary_df <- rbind(main$overall_summary_df, main$DF_SUMMARY_OVERALL)
  

  #median_max_cpu <- median(main$DF_MAIN$os_cpu_max)
  #print(plot_summary_boxplot_main(main$DF_MAIN,"os_cpu_max",median_max_cpu,"OS CPU"))
  #box_cpu <- plot_summary_boxplot_main(main$DF_MAIN,"os_cpu_max",median_max_cpu,"OS CPU")
  box_plots <- plot_summary_boxplot_main()
  #print(box_plots)
  c(aas_plot, aas_plot2_gt,aas_plot2_line) := plot_aas_chart(main$DF_AAS)
  pdf(paste(main$current_db_name,"-plot.pdf",sep=""), width = 11, height = 8.5,useDingbats=FALSE)
  #tblText <- tableGrob(main$DF_OS,show.rownames = FALSE, gpar.coretext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = FALSE,col.just = "left")
  log_it('tblText - start')
  tblText <- tableGrob(main$DF_SUMMARY_OS,show.rownames = FALSE, gpar.coretext = gpar(fontsize=12),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  #tblText <- tableGrob(main$DF_SUMMARY_MAIN,show.rownames = FALSE, gpar.coretext = gpar(fontsize=12),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  log_it('tblText - end')
  log_it('tblText2 - start')
  tblText2 <- tableGrob(main$DF_SUMMARY_MAIN,show.rownames = FALSE, gpar.coretext = gpar(fontsize=10),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  #tblText2 <- tableGrob(main$DF_SUMMARY_OS,show.rownames = FALSE, gpar.coretext = gpar(fontsize=10),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  log_it('tblText2 - end')
  
  tblText3 <- tableGrob(subset(main$DF_OS,STAT_NAME == 'HOSTS'),show.rownames = FALSE, gpar.coretext = gpar(fontsize=8),gpar.coltext = gpar(fontsize=8),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = FALSE,col.just = "left")
  #spaceText <- tableGrob(main$DF_SPACE,show.rownames = FALSE, gpar.coretext = gpar(fontsize=8),padding.v = unit(2, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  #x <- grid.arrange(tblText, spaceText, ncol = 1, heights=c(2,5))
  #str(tblText2)
  dummy_df <- data.frame()
    tryCatch(x <- grid.arrange(tblText,tblText2,tblText3, aas_plot2_line,box_plots, ncol = 1, heights=c(1,1,1,8,8)), 
             error = function(e) {
               tryCatch(x <- grid.arrange(tblText,tblText2,tblText3, box_plots, ncol = 1, heights=c(1,1,1,8)), 
                        error = function(e) {
                          x <- grid.arrange(tblText ,tblText2,tblText3, ncol = 1, heights=c(1,1,1))
                        }
               )
             }
    )
  
  #x <- grid.arrange(tblText,tblText2, aas_plot2_line,tblText, ncol = 1, heights=c(1,1,4,4))
  #x <- grid.arrange(tblText, ncol = 1, heights=c(1))
  #print(x)
 
  log_it('printed x')
  
  cpu_plot <- plot_cpu(main$DF_MAIN)
   io_plot <- plot_io(main$DF_MAIN_BY_SNAP)
   
  main_activity_plot <- plot_main_activity(main$DF_MAIN)
  
      tryCatch(RAC_activity_plot <- plot_RAC_activity(main$DF_MAIN), 
               error = function(e) {
                #traceback()
                 #print(paste0("Error in ",main$current_db_name,": ",e))
                 #browser()
               }
               #,finally=print("finished")
      )
 
  
   memory_plot <- plot_memory(main$DF_MEMORY)
  
  grid.newpage()
  grid.draw(cpu_plot)
  grid.newpage()
  grid.draw(io_plot)
  grid.newpage()
  grid.draw(aas_plot)
  grid.newpage()
  grid.draw(aas_plot2_gt)
  grid.newpage()
  grid.draw(main_activity_plot)
  grid.newpage()
  tryCatch(
    grid.draw(RAC_activity_plot), 
           error = function(e) {
             #traceback()
             #print(paste0("Error in ",main$current_db_name,": ",e))
             #browser()
           }
           #,finally=print("finished")
  )
  print(memory_plot)
  
  #snapIdDateText1 <- tableGrob(main$DF_SNAP_ID_DATE2,show.rownames = FALSE, gpar.coretext = gpar(fontsize=6),gpar.coltext = gpar(fontsize=6),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
  
 # plot_snap_id_list()
  
#  plot_sql_summary()
  
  dev.off()
  main$plot_attributes <- rbind(main$plot_attributes,main$current_plot_attributes)
  #print(main$foo)
}



main$mainLoop <- function(){
  for (f in main$db_id) {
    main$mainFunction(f)
#     tryCatch(main$mainFunction(f), 
#              error = function(e) {
#               #traceback()
#                print(paste0("Error in ",main$current_db_name,": ",e))
#                #browser()
#               
#              }
#              #,finally=print("finished")
#     )
  }
  write.csv(main$overall_summary_df,'OverallSummary.csv')
  write.csv(main$plot_attributes,'attributes.csv',row.names=FALSE)
}

main$mainLoop()

