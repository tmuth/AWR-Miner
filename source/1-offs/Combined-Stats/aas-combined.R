source("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/commonFunctions.R")

#setwd("M:/Dropbox/MyFiles/Accounts/Federal/FAA/AWR-Sizing-Sept-2013")
#setwd("M:/Dropbox/MyFiles/Accounts/Federal/World Bank/WB-share/AWR-Miner-Oct-2-2013")
#setwd("M:/Dropbox/MyFiles/Accounts/Federal/World Bank/WB-share/AWR-Miner-Oct-22")


DF_AAS_TEMP <- data.frame()
DF_AAS_TEMP2 <- data.frame()
#DF_TEMP <- data.frame()
#DF_TEMP2 <- data.frame()
#DF_TEMP3 <- data.frame()
DF_AAS_COMBINED <- data.frame()


getAASdf <- function(file){
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <- gsub(pattern = namePattern, replacement="\\1", file)
  DF_AAS_TEMP <<- NULL
  
  load(file=file)
  debugVars$main$DF_AAS$db <- dbName
  
  DF_AAS_TEMP2 <<- debugVars$main$DF_AAS
  
  DF_AAS_TEMP <<- ddply(debugVars$main$DF_AAS, .(db,SNAP_ID), summarise, 
                    end=min(as.POSIXct(end,tz="UTC")),
                    AVG_SESS=sum(AVG_SESS)
                    
  ) 
  # round to the nearest hour
  DF_AAS_TEMP$end <<- round_date(DF_AAS_TEMP$end,"hour")
  
  # group by snap,db,end, max(iops): to get only max iops per hour (in case of 15 min snapshots for example)
  DF_AAS_TEMP <<- ddply(DF_AAS_TEMP, .(db,end), summarise, 
                        AVG_SESS=max(AVG_SESS)
  )
  
  
  
  
  #DF_TEMP$total_iops_direct <<- DF_TEMP$read_iops_direct + DF_TEMP$write_iops_direct
  #DF_TEMP$total_iops_direct_max <<- DF_TEMP$read_iops_direct_max + DF_TEMP$write_iops_direct_max
  
  
  # need to avg by(snap,db,end) for all metrics to account for 15 min snapshots
  
  DF_AAS_COMBINED <<- rbind(DF_AAS_COMBINED,DF_AAS_TEMP)
  
  
}
rm(main)

rdaFiles <- list.files(pattern="*debugVars.Rda")

for (f in rdaFiles) {
  print(f)
  getAASdf(f)
}

rm(DF_AAS_TEMP)
rm(DF_AAS_TEMP2)

# DF_AAS_COMBINED_SAVE <- DF_AAS_COMBINED
# filter results above a certain quantile to keep outliers from skewing the plots
quant_high <- quantile(DF_AAS_COMBINED$AVG_SESS,probs=c(0.99),type=4)
DF_AAS_COMBINED <- subset(DF_AAS_COMBINED,AVG_SESS < quant_high[[1]],rownames=FALSE,stringsasfactors=TRUE)

pdf("Combined-AAS.pdf", width = 11, height = 8.5,useDingbats=FALSE)

DF_AAS_COMBINED_MAX <- ddply(DF_AAS_COMBINED, .(end), summarise,AVG_SESS=sum(AVG_SESS))
max_vals <- ddply(DF_AAS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)

ggplot(data=DF_AAS_COMBINED,aes(x=end,y=AVG_SESS))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  labs(title="Combined Average Active Sessions (AAS)")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

max_vals <- ddply(DF_AAS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)

ggplot(data=DF_AAS_COMBINED,aes(x=end,y=AVG_SESS,group=db))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  scale_fill_stata()+
  facet_grid(db ~ . )+
  labs(title="Combined Average Active Sessions (AAS) - Facet by Database")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))

dev.off()