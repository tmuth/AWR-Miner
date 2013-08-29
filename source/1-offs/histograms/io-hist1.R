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
#DT_IO_HIST_SMALL <- DT_IO_HIST_SMALL[SNAP_ID>= 900,]
summary(DT_IO_HIST_SMALL)
unique(DT_IO_HIST_SMALL$EVENT_NAME)
unique(DT_IO_HIST_SMALL$WAIT_TIME_MILLI2)


DT_IO_HIST_SMALL <- data.table(DT_IO_HIST_SMALL)
DT_IO_HIST_SMALL$WAIT_TIME_MILLI <- as.numeric(as.character(DT_IO_HIST_SMALL$WAIT_TIME_MILLI))
DT_IO_HIST_SMALL[WAIT_TIME_MILLI>=64, WAIT_TIME_MILLI := 64]
DT_IO_HIST_SMALL <- DT_IO_HIST_SMALL[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(SNAP_ID,WAIT_CLASS,EVENT_NAME,WAIT_TIME_MILLI)]
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
DT_IO_HIST_SMALL3 <- DT_IO_HIST_SMALL[, list(WAIT_COUNT2 = sum(WAIT_COUNT),WAIT_COUNT=WAIT_COUNT,WAIT_PCT=WAIT_COUNT/sum(WAIT_COUNT),WAIT_TIME_MILLI=WAIT_TIME_MILLI), by = list(SNAP_ID,EVENT_NAME)]


#)

DT_IO_HIST_SMALL[with(DT_IO_HIST_SMALL, grepl(64, WAIT_TIME_MILLI)),]$WAIT_TIME_MILLI<-"64+"

DT_IO_HIST_SMALL_GROUP <- DT_IO_HIST_SMALL[, list(WAIT_COUNT = sum(WAIT_COUNT)), by = list(SNAP_ID,EVENT_NAME, WAIT_TIME_MILLI)]

DT_IO_HIST_SMALL_GROUP <- DT_IO_HIST_SMALL_GROUP[, list(WAIT_TIME_MILLI=WAIT_TIME_MILLI,WAIT_PCT =WAIT_COUNT/ sum(WAIT_COUNT)), by = list(EVENT_NAME)]
#x.melt[with(x.melt, grepl("read_iops", variable)),]$variable<-"Read IOPs"

#diamondsDT[, list(price = mean(price)), by = cut]

#io_hist_colors <- c("1" = "#4575B4", "2" = "#74ADD1", "4" = "#ABD9E9", "8" = "#E0F3F8",
io_hist_colors2 <- c("1" = "#315280", "2" = "#4575B4", "4" = "#74ADD1", "8" = "#ABD9E9",
                "16" = "#FDAE61", "32" = "#F46D43", "64"="#D73027")

gg_io_hist_colors2 <- scale_fill_manual(values = io_hist_colors2)

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


DT_IO_HIST_SMALL4 <- DT_IO_HIST_SMALL3
  DT_IO_HIST_SMALL4  <- ddply(DT_IO_HIST_SMALL3 , .(SNAP_ID), transform,  
             rescale = round(rescale(WAIT_COUNT2,to=c(1,64))))

#binned <-cut(x,cutpoints,include.lowest=TRUE)

DT_IO_HIST_SMALL4$rescale2 <- with(DT_IO_HIST_SMALL4,cut(rescale,breaks =c(1,2,4,8,16,32,64,Inf),labels = c(1,2,4,8,16,32,64),right = FALSE,include.lowest = TRUE,))
DT_IO_HIST_SMALL4$rescale <- factor(round(DT_IO_HIST_SMALL4$rescale),levels=c(1,2,4,8,16,32,64))
DT_IO_HIST_SMALL4$WAIT_TIME_MILLI <- factor(DT_IO_HIST_SMALL4$WAIT_TIME_MILLI)
DT_IO_HIST_SMALL4<- data.table(DT_IO_HIST_SMALL4)
DT_IO_HIST_SMALL4[WAIT_PCT>=0.05 & WAIT_PCT<0.1 , WAIT_PCT := 0.1]

DT_IO_HIST_SMALL4$pct2 <- with(DT_IO_HIST_SMALL4,cut(WAIT_PCT,breaks = seq(0, 1, by=0.1),labels = seq(0, 0.9, by=0.1),right = FALSE,include.lowest = TRUE,))

#x <- grid.arrange(io_hist_area_plot ,io_hist_plot, ncol = 2, widths=c(3,1))
x <- grid.arrange(io_hist_plot,io_hist_area_plot , ncol = 1, heights=c(1,4))


io_hist_colors2 <- c("0" = "#ffffff","0.1" = "#ABD9E9", "0.2" = "#4575B4", "0.3" = "#315280", "0.4" = "#407b31",
                     "0.5" = "#79c267", "0.6" = "#c5d647",
                     "0.7" = "#FDAE61", "0.8" = "#F46D43", "0.9"="#D73027","1"="#D73027")
gg_io_hist_colors2 <- scale_fill_manual(values = io_hist_colors2)

ggplot(data=DT_IO_HIST_SMALL4,aes(x=SNAP_ID,y=WAIT_TIME_MILLI,fill=pct2))+
  geom_tile(aes(),colour = "white",size=0)+
  
  geom_point(data=DT_IO_HIST_SMALL4,aes(x=SNAP_ID,y=rescale2),alpha=0.02)+
  #scale_size_manual(values=c(1,2,4,8,16,32,64),guide="none")+
  #scale_y_discrete( breaks=c(1,2,4,8,16,32,64))+
  #geom_text(data=DT_IO_HIST_SMALL4,aes(x=SNAP_ID,y=rescale,label=rescale))+
 #scale_fill_gradient(low="#fdefae", high="red")+
  #scale_y_discrete()+
  gg_io_hist_colors2+
  facet_grid(EVENT_NAME ~ .)
  #scale_fill_gradient(low = "white",high = "steelblue")


summary(DT_IO_HIST_SMALL4)
DT_IO_HIST_SMALL4$WAIT_TIME_MILLI <- as.numeric(DT_IO_HIST_SMALL4$WAIT_TIME_MILLI)
str(DT_IO_HIST_SMALL4)

head(DT_IO_HIST_SMALL4,n=20)







