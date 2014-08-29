source("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/commonFunctions.R")

#setwd("M:/Dropbox/MyFiles/Accounts/Federal/FAA/AWR-Sizing-Sept-2013")
#setwd("M:/Dropbox/MyFiles/Accounts/Federal/World Bank/WB-share/AWR-Miner-Oct-2-2013")
#setwd("M:/Dropbox/MyFiles/Accounts/Federal/World Bank/WB-share/AWR-Miner-Oct-22")


DF_TEMP <- data.frame()
DF_TEMP2 <- data.frame()
#DF_TEMP3 <- data.frame()
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
                              #read_iops_direct=sum(read_iops_direct),
                              #read_iops_direct_max=sum(read_iops_direct_max),
                              write_iops=sum(write_iops)*2,
                              write_iops_max=sum(write_iops_max)*2
                              #write_iops_direct=sum(write_iops_direct)*2,
                              #write_iops_direct_max=sum(write_iops_direct_max)*2
                   
                   ) 
  # round to the nearest hour
  DF_TEMP$end <<- round_date(DF_TEMP$end,"hour")
  
  # group by snap,db,end, max(iops): to get only max iops per hour (in case of 15 min snapshots for example)
  DF_TEMP <<- ddply(DF_TEMP, .(snap,db,end), summarise, 
                     read_iops=max(read_iops),
                     read_iops_max=max(read_iops_max),
                     write_iops=max(write_iops)*2,
                     write_iops_max=max(write_iops_max)*2
  )
  
  
  DF_TEMP$total_iops <<- DF_TEMP$read_iops + DF_TEMP$write_iops
  DF_TEMP$total_iops_max <<- DF_TEMP$read_iops_max + DF_TEMP$write_iops_max
  #DF_TEMP$total_iops_direct <<- DF_TEMP$read_iops_direct + DF_TEMP$write_iops_direct
  #DF_TEMP$total_iops_direct_max <<- DF_TEMP$read_iops_direct_max + DF_TEMP$write_iops_direct_max
  
  
  # need to avg by(snap,db,end) for all metrics to account for 15 min snapshots
  
  DF_IOPS_COMBINED <<- rbind(DF_IOPS_COMBINED,DF_TEMP)
  
  
}
rm(main)

rdaFiles <- list.files(pattern="*debugVars.Rda")

for (f in rdaFiles) {
  print(f)
  getIOPsDF(f)
}

rm(DF_TEMP)
rm(DF_TEMP2)

# getIOPsDF("ETMSB1-28487-29206-1-debugVars.Rda")
# getIOPsDF("ETMSE1-28408-29127-1-debugVars.Rda")
# getIOPsDF("ETMSR1-28265-28984-1-debugVars.Rda")
# getIOPsDF("METRIC1-27706-28425-1-debugVars.Rda")
# getIOPsDF("OFFLDB1-20030-20749-1-debugVars.Rda")
# getIOPsDF("OFFLDR1-28267-28987-1-debugVars.Rda")

# get the mean CPU per sec by db
DF_IOPS_COMBINED_MEAN<- ddply(DF_IOPS_COMBINED, .(db), summarise,total_iops=mean(total_iops))
# reorder the DBs by their mean CPU per sec
DF_IOPS_COMBINED_MEAN <- DF_IOPS_COMBINED_MEAN[ order(DF_IOPS_COMBINED_MEAN$total_iops, decreasing=TRUE), ]
# reorder the DB factors by the reordered DF
DF_IOPS_COMBINED$db <- factor(as.character(DF_IOPS_COMBINED$db),levels=DF_IOPS_COMBINED_MEAN$db,ordered=TRUE)




#DF_IOPS_COMBINED$db <- factor(DF_IOPS_COMBINED$db)

#DF_IOPS_COMBINED <- subset(DF_IOPS_COMBINED,db != "RAC07P")
#DF_IOPS_COMBINED <- subset(DF_IOPS_COMBINED,db != "RAC07T")
#DF_IOPS_COMBINED <- subset(DF_IOPS_COMBINED,end >= as.POSIXct("2013-09-23 00:00:00",format="%Y-%m-%d %H:%M:%S",tz="UTC"))




pdf("Combined-IOPs4.pdf", width = 11, height = 8.5,useDingbats=FALSE)

DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops=sum(total_iops))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Average IOPs")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

max_vals <- ddply(DF_IOPS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-total_iops) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops,group=db))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  facet_grid(db ~ . )+
  labs(title="Combined Average IOPs - Facet by Database")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

## *********************************************************
# DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops_direct=sum(total_iops_direct))
# max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_direct) <= 1)
# 
# ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_direct))+
#   #geom_line(aes(color=db), size=.2)total_iops
#   geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
#   geom_point(data=max_vals, aes(x=end, y=total_iops_direct), size=2, shape=21)+
#   geom_text(data=max_vals, aes(x=end, y=total_iops_direct,label=total_iops_direct),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
#   scale_fill_stata()+
#   labs(title="Combined Average DIRECT-PATH IOPs")+
#   scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))
# 
# max_vals <- ddply(DF_IOPS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_direct) <= 1)
# 
# ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_direct,group=db))+
#   #geom_line(aes(color=db), size=.2)
#   geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
#   geom_point(data=max_vals, aes(x=end, y=total_iops_direct), size=2, shape=21)+
#   geom_text(data=max_vals, aes(x=end, y=total_iops_direct,label=total_iops_direct),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
#   scale_fill_stata()+
#   facet_grid(db ~ . )+
#   labs(title="Combined Average DIRECT-PATH IOPs - Facet by Database")+
#   scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

## *********************************************************
rm(DF_IOPS_COMBINED_MAX)
DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops_max=sum(total_iops_max))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_max) <= 1)


ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_max))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack')+
  geom_bar(aes(fill=db),stat='identity',position='stack')+
  geom_point(data=max_vals, aes(x=end, y=total_iops_max), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_max,label=total_iops_max),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Maximum IOPs")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

rm(max_vals)
max_vals <- ddply(DF_IOPS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_max) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_max,group=db))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack')+
  geom_bar(aes(fill=db),stat='identity',position='stack')+
  geom_point(data=max_vals, aes(x=end, y=total_iops_max), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_max,label=total_iops_max),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  facet_grid(db ~ . )+
  labs(title="Combined Maximum IOPs - Facet by Database")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

dev.off()



summary(DF_IOPS_COMBINED)
