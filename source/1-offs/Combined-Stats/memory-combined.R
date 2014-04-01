list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate","gplots")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})

#setwd("M:/Dropbox/MyFiles/Accounts/Federal/FAA/AWR-Sizing-Sept-2013")
setwd("M:/Dropbox/MyFiles/Accounts/Federal/DOE/KAPP-Bettis/awr-miner-march-2014")
#load(file="RAC01P-23369-24089-1-debugVars.Rda")
rm(debugVars)

DF_TEMP <- data.frame()

DF_MEMORY_COMBINED <- data.frame()


getIOPsDF <- function(file){
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <- gsub(pattern = namePattern, replacement="\\1", file)
  DF_TEMP <<- NULL
  
  load(file=file)
  debugVars$main$DF_MEMORY$db <- dbName
  DF_TEMP <<- debugVars$main$DF_MEMORY
  

  DF_TEMP$end <<- round_date(DF_TEMP$end,"hour")
  DF_MEMORY_COMBINED <<- rbind(DF_MEMORY_COMBINED,DF_TEMP)
  
  
}
rm(main)

rdaFiles <- list.files(pattern="*debugVars.Rda")

for (f in rdaFiles) {
  getIOPsDF(f)
}


DF_MEMORY_COMBINED <- DF_MEMORY_COMBINED_KEEP

DF_MEMORY_COMBINED_KEEP <- DF_MEMORY_COMBINED

DF_MEMORY_COMBINED <- subset(DF_MEMORY_COMBINED,INSTANCE_NUMBER == 1)


DF_MEMORY_COMBINED_MAX <- ddply(DF_MEMORY_COMBINED, .(INSTANCE_NUMBER,end), summarise,TOTAL=sum(TOTAL))
max_vals <- ddply(DF_MEMORY_COMBINED_MAX, .(INSTANCE_NUMBER,format(end,"%y/%m/%d")), subset, subset = rank(-TOTAL) <= 1)

#max_vals <- ddply(DF_MEMORY_COMBINED, .(INSTANCE_NUMBER,format(end,"%y/%m/%d")), subset, subset = rank(-TOTAL) <= 1)

ggplot(data=DF_MEMORY_COMBINED,aes(x=end,y=TOTAL))+
  #geom_line(aes(color=db), size=.2)total_iops
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  #geom_bar(aes(),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=TOTAL), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=TOTAL,label=TOTAL),size=3, vjust=-.1, hjust=1.5,alpha=0.7)+
  #scale_fill_stata()+
  facet_grid(INSTANCE_NUMBER ~ . )+
  labs(title="Bar Chart for all Nodes, Facet by Node")+
  ylab("Combined PGA+SGA in Gigabytes")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
  theme(legend.key.size =    unit(0.2, "lines"))



DF_MEMORY_COMBINED_MAX2 <- DF_MEMORY_COMBINED %.%
  group_by(INSTANCE_NUMBER,db) %.%
  summarise(TOTAL= max(TOTAL)) %.%
  arrange(desc(TOTAL))

d <- within(DF_MEMORY_COMBINED, db <- factor(db, levels=DF_MEMORY_COMBINED_MAX2$db))

ggplot(data=d,aes(x='0',y=TOTAL))+
  geom_boxplot(aes(order = desc(TOTAL)),colour="#000000",alpha=.6,show_guide=FALSE,notch = FALSE,outlier.colour = "orange", outlier.size = 1,outlier.alpha=.4,outlier.shape=5)+
  geom_jitter(alpha=.2,size=1,position = position_jitter(width = .2,height=0))+
  labs(title="Boxplots for Node 1 Only, Facet by DB")+
  ylab("Combined PGA+SGA in Gigabytes")+
  theme(axis.title.x  = element_blank())+
  facet_wrap( ~ db,nrow=3)
  

DF_MEMORY_COMBINED_MAX2 <- 
  
DF_MEMORY_COMBINED %.%
  group_by(INSTANCE_NUMBER) %.%
  summarise(DBs= length(unique(db)))
 