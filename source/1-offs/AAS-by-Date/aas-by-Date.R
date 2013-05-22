library(microbenchmark)
rm(DF_AAS_INT)
setwd("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/1-offs/AAS-by-Date")
#save(DF_IO_HIST,file="DF_IO_HIST.Rda")
#load(file="DF_IO_HIST.Rda")
#load(file="DF_AAS_INT_mainframe.Rda")
load(file="DF_AAS_INT_DLA.Rda")
#load(file="DF_IO_HIST_DLA_PR1.Rda")
options(scipen=10)
list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})



#DF_AAS_INT[SNAP_ID >= 70480,]
DF_AAS_INT <- data.table(DF_AAS_INT)
setkey(DF_AAS_INT,end, WAIT_CLASS)
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


#--------------------------------------------------------------------------
#DT_IO_HIST_SMALL <- DT_IO_HIST_SMALL[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(SNAP_ID, EVENT_NAME, WAIT_TIME_MILLI)]
DF_AAS_INT_AGG <- DF_AAS_INT[, list(AVG_SESS = sum(AVG_SESS)), by = list(end)]
head(DF_AAS_INT_AGG)
DF_AAS_INT_AGG$hour <- as.numeric(format(DF_AAS_INT_AGG$end,"%H"))

DF_AAS_INT_AGG$dayName <- format(DF_AAS_INT_AGG$end,"%A")
head(DF_AAS_INT_AGG)
#head(format(DF_AAS_INT_AGG$end,"%H"))
#head(format(DF_AAS_INT_AGG$end,"%a"))

DF_AAS_INT_AGG$day_night <- "night" 
DF_AAS_INT_AGG[hour >= 6 & hour < 18]$day_night <- "day"
DF_AAS_INT_AGG$hour2 <- as.numeric(format(DF_AAS_INT_AGG$end,"%I"))



 ggplot(DF_AAS_INT_AGG,aes(y=AVG_SESS,x=factor(hour2)))+
  geom_boxplot( trim = FALSE)+
  facet_grid(day_night ~ dayName)



#--------------------------------------------------------------------------
nrow(DF_AAS_INT)
head(DF_AAS_INT)
DF_AAS_INT2 <- data.table(DF_AAS_INT)

DF_AAS_INT2$hour <- as.numeric(format(DF_AAS_INT2$end,"%H"))
DF_AAS_INT2$minute <- as.numeric(format(DF_AAS_INT_AGG$end,"%M"))
DF_AAS_INT2$dayName <- format(DF_AAS_INT2$end,"%A")

DF_AAS_INT2[minute >= 31]$hour <- DF_AAS_INT2[minute >= 31]$hour+1
DF_AAS_INT2[hour >= 24]$hour <- 0
DF_AAS_INT2$day_night <- "night" 
DF_AAS_INT2[hour >= 8 & hour < 18]$day_night <- "day"
DF_AAS_INT2$hour <- as.numeric(format(DF_AAS_INT_AGG$end,"%I"))
DF_AAS_INT2[minute >= 31]$hour <- DF_AAS_INT2[minute >= 31]$hour+1
DF_AAS_INT2[hour > 12]$hour <- 1
head(DF_AAS_INT2)

aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "System I/O" = "#0993de","User I/O" = "#054ae1")

gg_aas_colors <- scale_fill_manual("", values = aas_colors)
DF_AAS_INT3 <- DF_AAS_INT2
#DF_AAS_INT3 <- DF_AAS_INT2[(hour >= 7 & hour <= 11)  & dayName == 'Thursday']
nrow(DF_AAS_INT3)


DF_AAS_INT3_AGG <- data.table(DF_AAS_INT3[, list(AVG_SESS = mean(AVG_SESS)), by = list(dayName,day_night,hour,WAIT_CLASS)])
setkey(DF_AAS_INT3_AGG,dayName,hour,WAIT_CLASS)

DF_AAS_INT3_AGG<-data.frame(DF_AAS_INT3_AGG)
DF_AAS_INT3_AGG$WAIT_CLASS <- factor(DF_AAS_INT3_AGG$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))

DF_AAS_INT3_AGG$hour <- factor(DF_AAS_INT3_AGG$hour,c(8,9,10,11,12,1,2,3,4,5,6,7))
DF_AAS_INT3_AGG$dayName <- factor(DF_AAS_INT3_AGG$dayName,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(DF_AAS_INT3_AGG)+
  geom_bar(aes(x=hour,y=AVG_SESS,fill=WAIT_CLASS),stat='identity',position='stack')+
  gg_aas_colors+
  facet_grid(day_night ~ dayName)

#--------------------------------------------------------------------------










summary(DT_IO_HIST)
unique(DT_IO_HIST$EVENT_NAME)
DT_IO_HIST$EVENT_NAME <- str_trim(DT_IO_HIST$EVENT_NAME)
DT_IO_HIST_SMALL <- DT_IO_HIST
#DT_IO_HIST_SMALL <- DT_IO_HIST[EVENT_NAME %in% 'db file scattered read',]
#DT_IO_HIST_SMALL <- DT_IO_HIST_SMALL[SNAP_ID<= 71181 & SNAP_ID>= 70000,]
#DT_IO_HIST_SMALL <- DT_IO_HIST_SMALL[SNAP_ID>= 70000,]
DT_IO_HIST_SMALL <- DT_IO_HIST_SMALL[SNAP_ID>= 900,]
summary(DT_IO_HIST_SMALL)
unique(DT_IO_HIST_SMALL$EVENT_NAME)
unique(DT_IO_HIST_SMALL$WAIT_TIME_MILLI2)



DT_IO_HIST_SMALL$WAIT_TIME_MILLI <- as.numeric(as.character(DT_IO_HIST_SMALL$WAIT_TIME_MILLI))
DT_IO_HIST_SMALL[WAIT_TIME_MILLI>=64, WAIT_TIME_MILLI := 64]
DT_IO_HIST_SMALL$WAIT_TIME_MILLI <- factor(DT_IO_HIST_SMALL$WAIT_TIME_MILLI)



vals <- expand.grid(SNAP_ID = unique(DT_IO_HIST_SMALL$SNAP_ID),
                    WAIT_TIME_MILLI = unique(DT_IO_HIST_SMALL$WAIT_TIME_MILLI))

nrow(DT_IO_HIST_SMALL)

DT_IO_HIST_SMALL <- merge(vals,DT_IO_HIST_SMALL)

nrow(DT_IO_HIST_SMALL)


#microbenchmark( 
  
#DT_IO_HIST_SMALL2 <- ddply(DT_IO_HIST_SMALL, .(SNAP_ID,EVENT_NAME,WAIT_TIME_MILLI), summarise, 
#                          WAIT_COUNT = sum(WAIT_COUNT)),
DT_IO_HIST_SMALL <- data.table(DT_IO_HIST_SMALL)
setkey(DT_IO_HIST_SMALL,SNAP_ID, EVENT_NAME, WAIT_TIME_MILLI)
DT_IO_HIST_SMALL <- DT_IO_HIST_SMALL[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(SNAP_ID, EVENT_NAME, WAIT_TIME_MILLI)]


#)

DT_IO_HIST_SMALL[with(DT_IO_HIST_SMALL, grepl(64, WAIT_TIME_MILLI)),]$WAIT_TIME_MILLI<-"64+"

DT_IO_HIST_SMALL_GROUP <- DT_IO_HIST_SMALL[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(EVENT_NAME, WAIT_TIME_MILLI)]

DT_IO_HIST_SMALL_GROUP <- DT_IO_HIST_SMALL_GROUP[, list(WAIT_TIME_MILLI=WAIT_TIME_MILLI,WAIT_PCT =WAIT_COUNT/ sum(WAIT_COUNT)), by = list(EVENT_NAME)]
#x.melt[with(x.melt, grepl("read_iops", variable)),]$variable<-"Read IOPs"

#diamondsDT[, list(price = mean(price)), by = cut]

#io_hist_colors <- c("1" = "#4575B4", "2" = "#74ADD1", "4" = "#ABD9E9", "8" = "#E0F3F8",
io_hist_colors2 <- c("1" = "#315280", "2" = "#4575B4", "4" = "#74ADD1", "8" = "#ABD9E9",
                "16" = "#FDAE61", "32" = "#F46D43", "64+"="#D73027")

gg_io_hist_colors2 <- scale_fill_manual(values = io_hist_colors2,name="wait ms" )

io_hist_plot <- ggplot(DT_IO_HIST_SMALL_GROUP,aes(x=factor(WAIT_TIME_MILLI),fill = WAIT_TIME_MILLI,))+
geom_bar(stat ='identity',aes(y=WAIT_PCT))+
  facet_grid(. ~ EVENT_NAME,scales="free_y")+
  #facet_grid(EVENT_NAME ~ .,scales="free_y")+
  gg_io_hist_colors2+
  labs(title=paste0("I/O Wait Event Histogram"))+
  opts(legend.key.size = unit(.25, "cm"))+
  scale_y_continuous(labels = percent_format())+
  theme(axis.title.y  = element_blank(),legend.position =    "right" )+
  xlab("Wait Milliseconds")

#theme(axis.title.x  = element_blank(),axis.title.y  = element_blank())+

#io_hist_plot

io_hist_area_plot <- ggplot()+
  geom_area(data=DT_IO_HIST_SMALL, aes(x = SNAP_ID, y = WAIT_COUNT,
                                 fill = WAIT_TIME_MILLI),stat = "identity", position = "stack",alpha=1)+
  facet_grid(EVENT_NAME ~ .,scales="free_y")+
  gg_io_hist_colors2+
  labs(title=paste0("I/O Wait Event Area Chart"))+
  theme(legend.key.size = unit(.25, "cm"))+
  scale_y_continuous(labels = comma)+
  ylab("Wait Count")+
  theme(axis.title.x=element_blank(),legend.position =    "none" )



#x <- grid.arrange(io_hist_area_plot ,io_hist_plot, ncol = 2, widths=c(3,1))
x <- grid.arrange(io_hist_plot,io_hist_area_plot , ncol = 1, heights=c(1,4))








