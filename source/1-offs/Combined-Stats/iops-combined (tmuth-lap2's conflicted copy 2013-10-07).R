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
setwd("M:/Dropbox/MyFiles/Accounts/Federal/World Bank/WB-share/AWR-Miner-Oct-2-2013")
#load(file="RAC01P-23369-24089-1-debugVars.Rda")
rm(debugVars)

DF_TEMP <- data.frame()
DF_TEMP2 <- data.frame()
DF_IOPS_COMBINED <- data.frame()


getIOPsDF <- function(file){
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <- gsub(pattern = namePattern, replacement="\\1", file)
  DF_TEMP <<- NULL
  
  load(file=file)
  debugVars$main$DF_MAIN$db <- dbName
  DF_TEMP2 <<- debugVars$main$DF_MAIN
  DF_TEMP <<- ddply(debugVars$main$DF_MAIN, .(snap,db), summarise, 
                              end=min(as.POSIXct(end,tz="UTC")),
                              read_iops=sum(read_iops),
                              read_iops_max=sum(read_iops_max),
                              read_iops_direct=sum(read_iops_direct),
                              read_iops_direct_max=sum(read_iops_direct_max),
                              write_iops=sum(write_iops)*2,
                              write_iops_max=sum(write_iops_max)*2,
                              write_iops_direct=sum(write_iops_direct)*2,
                              write_iops_direct_max=sum(write_iops_direct_max)*2
                   
                   ) 
  DF_TEMP$total_iops <<- DF_TEMP$read_iops + DF_TEMP$write_iops
  DF_TEMP$total_iops_max <<- DF_TEMP$read_iops_max + DF_TEMP$write_iops_max
  DF_TEMP$total_iops_direct <<- DF_TEMP$read_iops_direct + DF_TEMP$write_iops_direct
  DF_TEMP$total_iops_direct_max <<- DF_TEMP$read_iops_direct_max + DF_TEMP$write_iops_direct_max
  DF_TEMP$end <<- round_date(DF_TEMP$end,"hour")
  DF_IOPS_COMBINED <<- rbind(DF_IOPS_COMBINED,DF_TEMP)
  
  
}
rm(main)

rdaFiles <- list.files(pattern="*debugVars.Rda")

for (f in rdaFiles) {
  getIOPsDF(f)
}

# getIOPsDF("ETMSB1-28487-29206-1-debugVars.Rda")
# getIOPsDF("ETMSE1-28408-29127-1-debugVars.Rda")
# getIOPsDF("ETMSR1-28265-28984-1-debugVars.Rda")
# getIOPsDF("METRIC1-27706-28425-1-debugVars.Rda")
# getIOPsDF("OFFLDB1-20030-20749-1-debugVars.Rda")
# getIOPsDF("OFFLDR1-28267-28987-1-debugVars.Rda")


DF_IOPS_COMBINED$db <- factor(DF_IOPS_COMBINED$db)

#subsets for World Bank
DF_IOPS_COMBINED <- subset(DF_IOPS_COMBINED,db != "RAC07P")
DF_IOPS_COMBINED <- subset(DF_IOPS_COMBINED,db != "RAC07T")
DF_IOPS_COMBINED <- subset(DF_IOPS_COMBINED,end >= as.POSIXct("2013-09-23 00:00:00",format="%Y-%m-%d %H:%M:%S",tz="UTC"))

DF_IOPS_COMBINED_PCT <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops=sum(total_iops),total_iops_direct=sum(total_iops_direct))

DF_IOPS_COMBINED_PCT$direct_iops_pct <- round(DF_IOPS_COMBINED_PCT$total_iops_direct /  DF_IOPS_COMBINED_PCT$total_iops,2)
DF_IOPS_COMBINED_PCT$non_direct_iops_pct <- (1-DF_IOPS_COMBINED_PCT$direct_iops_pct)
DF_IOPS_COMBINED_PCT.melt <- melt(DF_IOPS_COMBINED_PCT, id.var = c("end"), measure.var = c("direct_iops_pct","non_direct_iops_pct"))

pdf("Combined-IOPs4.pdf", width = 11, height = 8.5,useDingbats=FALSE)

DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops=sum(total_iops))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Average IOPs - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

max_vals <- ddply(DF_IOPS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-total_iops) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops,group=db))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  facet_grid(db ~ . )+
  labs(title="Combined Average IOPs - Facet by Database - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

## *********************************************************
DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops_direct=sum(total_iops_direct))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_direct) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_direct))+
  #geom_line(aes(color=db), size=.2)total_iops
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops_direct), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_direct,label=total_iops_direct),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Average DIRECT-PATH IOPs - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

max_vals <- ddply(DF_IOPS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_direct) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_direct,group=db))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops_direct), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_direct,label=total_iops_direct),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  facet_grid(db ~ . )+
  labs(title="Combined Average DIRECT-PATH IOPs - Facet by Database - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

## *********************************************************
rm(DF_IOPS_COMBINED_MAX)
DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops_max=sum(total_iops_max))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_max) <= 1)


ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_max))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack')+
  geom_point(data=max_vals, aes(x=end, y=total_iops_max), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_max,label=total_iops_max),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Maximum IOPs - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

rm(max_vals)
max_vals <- ddply(DF_IOPS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_max) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_max,group=db))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack')+
  geom_point(data=max_vals, aes(x=end, y=total_iops_max), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_max,label=total_iops_max),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  facet_grid(db ~ . )+
  labs(title="Combined Maximum IOPs - Facet by Database - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))




max_vals <- ddply(DF_IOPS_COMBINED_PCT.melt, .(end=format(end,"%y/%m/%d"),variable), subset, subset = rank(-value) <= 1)

ggplot(data=DF_IOPS_COMBINED_PCT.melt,aes(x=end,y=value))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=variable),stat='identity',position='stack',alpha=.8)+
  #geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  #geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  geom_text(data=max_vals, aes(x=end, y=value,label= (paste0(round(value*100,0),"%"))),size=3, vjust=-.8, hjust=0,alpha=0.7)+
  facet_grid(variable ~ . )+
  scale_y_continuous(labels = percent_format())+
  labs(title="Combined Average IOPs Percent - Facet by Type - 7P Removed")+
  #stat_smooth(method = "loess",size=.2,alpha=.1,linetype="dashed",formula=y ~ poly(x, 2),aes())+
  stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",aes())+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("12 hour"),minor_breaks=date_breaks("2 hour"))


dev.off()


DF_IOPS_COMBINED$total_iops_non_direct <- DF_IOPS_COMBINED$total_iops-DF_IOPS_COMBINED$total_iops_direct

pdf("Combined-IOPs4.pdf", width = 11, height = 8.5,useDingbats=FALSE)

DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops_non_direct=sum(total_iops_non_direct))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_non_direct) <= 1)
max_vals_hidden1 <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_non_direct) <= 1)

DF_IOPS_COMBINED_MAX2 <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops_direct=sum(total_iops_direct))
max_vals_hidden2 <- ddply(DF_IOPS_COMBINED_MAX2, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_direct) <= 1)
max_vals2 <- ddply(DF_IOPS_COMBINED_MAX2, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_direct) <= 1)

p1 <- ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_non_direct))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops_non_direct), size=2, shape=21)+
  geom_point(data=max_vals_hidden2, aes(x=end, y=total_iops_direct), size=0, shape=21,alpha=0)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_non_direct,label=total_iops_non_direct),size=3, vjust=-.5, hjust=1.2,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Average Non-DIRECT-PATH IOPs - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
  theme(legend.key.size =    unit(0.4, "lines") ,legend.position="top")





p2 <- ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_direct))+
  #geom_line(aes(color=db), size=.2)total_iops
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals2, aes(x=end, y=total_iops_direct), size=2, shape=21)+
  geom_point(data=max_vals_hidden1, aes(x=end, y=total_iops_non_direct), size=0, shape=21,alpha=0)+
  geom_text(data=max_vals2, aes(x=end, y=total_iops_direct,label=total_iops_direct),size=3, vjust=-.5, hjust=1.2,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Average DIRECT-PATH IOPs - 7P Removed")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("12 hour"),minor_breaks=date_breaks("2 hour"))+
  theme(legend.position="none",axis.text.x= element_blank())
  

grid.arrange(p2,p1 , ncol = 1, heights=c(1,1.4))


max_vals <- ddply(DF_IOPS_COMBINED_PCT.melt, .(end=format(end,"%y/%m/%d"),variable), subset, subset = rank(-value) <= 1)

ggplot(data=DF_IOPS_COMBINED_PCT.melt,aes(x=end,y=value))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=variable),stat='identity',position='stack',alpha=.8)+
  #geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  #geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  geom_text(data=max_vals, aes(x=end, y=value,label= (paste0(round(value*100,0),"%"))),size=3, vjust=-.8, hjust=0,alpha=0.7)+
  facet_grid(variable ~ . )+
  scale_y_continuous(labels = percent_format())+
  labs(title="Combined Average IOPs Percent - Facet by Type - 7P Removed")+
  #stat_smooth(method = "loess",size=.2,alpha=.1,linetype="dashed",formula=y ~ poly(x, 2),aes())+
  stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",aes())+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("12 hour"),minor_breaks=date_breaks("2 hour"))


dev.off()

summary(DF_IOPS_COMBINED)
