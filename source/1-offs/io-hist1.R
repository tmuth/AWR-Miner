library(microbenchmark)
rm(DF_IO_HIST)
setwd("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/1-offs")
#save(DF_IO_HIST,file="DF_IO_HIST.Rda")
#load(file="DF_IO_HIST.Rda")
load(file="DF_IO_HIST_USPS_POV.Rda")
#load(file="DF_IO_HIST_PEARSON_H.Rda")
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




DT_IO_HIST <- data.table(DF_IO_HIST)

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








