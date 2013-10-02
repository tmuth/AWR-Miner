setwd("M:/Dropbox/MyFiles/Accounts/Federal/FAA/AWR-Sizing-Sept-2013")


DF_TEMP <- data.frame()
DF_IOPS_COMBINED <- data.frame()


getIOPsDF <- function(file){
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <- gsub(pattern = namePattern, replacement="\\1", file)
  DF_TEMP <<- NULL
  
  load(file=file)
  debugVars$main$DF_MAIN$db <- dbName
  DF_TEMP <<- ddply(debugVars$main$DF_MAIN, .(snap,db), summarise, 
                              end=min(as.POSIXct(end,tz="UTC")),
                              read_iops=sum(read_iops),
                              read_iops_max=sum(read_iops_max),
                              write_iops=sum(write_iops)*2,
                              write_iops_max=sum(write_iops_max)*2
                   
                   ) 
  DF_TEMP$total_iops <<- DF_TEMP$read_iops + DF_TEMP$write_iops
  DF_TEMP$total_iops_max <<- DF_TEMP$read_iops_max + DF_TEMP$write_iops_max
  DF_TEMP$end <<- round_date(DF_TEMP$end,"hour")
  DF_IOPS_COMBINED <<- rbind(DF_IOPS_COMBINED,DF_TEMP)
  
  
}
rm(main)
getIOPsDF("ETMSB1-28487-29206-1-debugVars.Rda")
getIOPsDF("ETMSE1-28408-29127-1-debugVars.Rda")
getIOPsDF("ETMSR1-28265-28984-1-debugVars.Rda")
getIOPsDF("METRIC1-27706-28425-1-debugVars.Rda")
getIOPsDF("OFFLDB1-20030-20749-1-debugVars.Rda")
getIOPsDF("OFFLDR1-28267-28987-1-debugVars.Rda")
DF_IOPS_COMBINED$db <- factor(DF_IOPS_COMBINED$db)

pdf("Combined-IOPs.pdf", width = 11, height = 8.5,useDingbats=FALSE)

DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops=sum(total_iops))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Average IOPs")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

max_vals <- ddply(DF_IOPS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-total_iops) <= 1)

ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops,group=db))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=total_iops), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops,label=total_iops),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  facet_grid(db ~ . )+
  labs(title="Combined Average IOPs - Facet by Database")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))
  
rm(DF_IOPS_COMBINED_MAX)
DF_IOPS_COMBINED_MAX <- ddply(DF_IOPS_COMBINED, .(end), summarise,total_iops_max=sum(total_iops_max))
max_vals <- ddply(DF_IOPS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-total_iops_max) <= 1)


ggplot(data=DF_IOPS_COMBINED,aes(x=end,y=total_iops_max))+
  #geom_line(aes(color=db), size=.2)
  geom_area(aes(fill=db),stat='identity',position='stack')+
  geom_point(data=max_vals, aes(x=end, y=total_iops_max), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=total_iops_max,label=total_iops_max),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Maximum IOPs")+
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
  labs(title="Combined Maximum IOPs - Facet by Database")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

dev.off()



summary(DF_IOPS_COMBINED)
