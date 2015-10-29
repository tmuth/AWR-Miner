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
  
  DF_AAS_TEMP_MAX <- ddply(DF_AAS_TEMP, .(db,end), subset, subset = rank(-AVG_SESS) <= 1)
  
  DF_AAS_TEMP_MAX <- DF_AAS_TEMP_MAX[c("db","SNAP_ID")]
  
  
  
  
  
  DF_AAS_TEMP <- merge(DF_AAS_TEMP_MAX,DF_AAS_TEMP2,by=c("db","SNAP_ID"))
  
  DF_AAS_TEMP <- DF_AAS_TEMP[c("db","end","WAIT_CLASS","AVG_SESS")]
  
  

  
  
  
  
  #DF_TEMP$total_iops_direct <<- DF_TEMP$read_iops_direct + DF_TEMP$write_iops_direct
  #DF_TEMP$total_iops_direct_max <<- DF_TEMP$read_iops_direct_max + DF_TEMP$write_iops_direct_max
  
  
  # need to avg by(snap,db,end) for all metrics to account for 15 min snapshots
  
  DF_AAS_COMBINED <<- rbind(DF_AAS_COMBINED,DF_AAS_TEMP)
  
  
}
#rm(main)

rdaFiles <- list.files(pattern="*debugVars.Rda")

for (f in rdaFiles) {
  print(f)
  getAASdf(f)
}

rm(DF_AAS_TEMP)
rm(DF_AAS_TEMP2)

DF_AAS_COMBINED_SUM <<- ddply(DF_AAS_COMBINED, .(end), summarise, 
                              AVG_SESS=sum(AVG_SESS)
)

quant_val <- 0.99

quant_high_sum <- quantile(DF_AAS_COMBINED_SUM$AVG_SESS,probs=c(quant_val[[1]]),type=4)

# DF_AAS_COMBINED_SAVE <- DF_AAS_COMBINED
# filter results above a certain quantile to keep outliers from skewing the plots

quant_high <- quantile(DF_AAS_COMBINED$AVG_SESS,probs=c(quant_val[[1]]),type=4)


pdf("Combined-AAS.pdf", width = 11, height = 8.5,useDingbats=FALSE)

DF_AAS_COMBINED_MAX <- ddply(DF_AAS_COMBINED, .(end), summarise,AVG_SESS=sum(AVG_SESS))
max_vals <- ddply(DF_AAS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)



#DF_ANNOTATE_INT <- data.frame(x=quantile(DF_AAS_COMBINED$end,0.10),y=quantile(DF_AAS_COMBINED$AVG_SESS,probs=(0.99),type=4),
DF_ANNOTATE_INT <- data.frame(x=quantile(DF_AAS_COMBINED$end,0.05),y=quant_high_sum[[1]],
                              labs=paste0("Data with total values over ",round(quant_val[[1]]*100),"th percentile (",quant_high_sum[[1]],") removed"))

aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "System I/O" = "#0993de","User I/O" = "#054ae1")

gg_aas_colors <- scale_fill_manual("", values = aas_colors)
#colorRampPalette(pal12)(26) 



ggplot(data=DF_AAS_COMBINED,aes(x = end, y = AVG_SESS,
           fill = WAIT_CLASS))+
  #geom_area(stat = "identity", position = "stack",alpha=.95)+
  geom_bar(aes(fill=WAIT_CLASS),stat='identity',position='stack',alpha=0.9,width=2000)+
  gg_aas_colors+
  theme(axis.title.x  = element_blank(),axis.title.y  = element_blank(),
        legend.key.size =    unit(0.6, "lines"))+
  labs("Average Active Sessions (AAS) ")
 
#  geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
#  geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5)+

  #geom_text(data=df_cpu_cores_label, aes(x=end, y=AVG_SESS,label=paste0("CPU Cores - ",main$cpu_cores)),size=2, vjust=-.8, hjust=.5,color="red",alpha=0.4)+
  #scale_x_datetime(breaks=NULL)+
  #scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),
                   #breaks = attr$date_break_major_var,
                   #minor_breaks = attr$date_break_minor_var,
   #                limits = c(min(DF_AAS_COMBINED$end),max(DF_AAS_COMBINED$end))
  



ggplot(data=DF_AAS_COMBINED,aes(x=end,y=AVG_SESS))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  #scale_fill_stata()+
  scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_AAS_COMBINED$db))) )+
  labs(title="Combined Average Active Sessions (AAS)")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
  ylim(0,quant_high_sum[[1]])+
  geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4)+
  theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
        strip.text.y = element_text(size = 7),
        legend.key.size = unit(.15, "cm"),
        legend.text=element_text(size=5)
  )

max_vals <- ddply(DF_AAS_COMBINED, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)


ggplot(data=DF_AAS_COMBINED,aes(x=end,y=AVG_SESS,group=db))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  #scale_fill_stata()+
  scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_AAS_COMBINED$db))) )+
  facet_grid(db ~ . )+
  labs(title="Combined Average Active Sessions (AAS) - Facet by Database")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
  ylim(0,quant_high[[1]])+
  theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
        strip.text.y = element_text(size = 4),
        legend.key.size = unit(.15, "cm"),
        legend.text=element_text(size=5)
  )

dev.off()









DF_AAS_TEMP3 <- DF_AAS_TEMP2


DF_AAS_TEMP4 <- ddply(DF_AAS_TEMP2, .(db,SNAP_ID), summarise, 
                      end=min(as.POSIXct(end,tz="UTC")),
                      AVG_SESS=sum(AVG_SESS))

DF_AAS_TEMP4_MAX <- ddply(DF_AAS_TEMP4, .(db,end), subset, subset = rank(-AVG_SESS) <= 1)

DF_AAS_TEMP4_MAX <- DF_AAS_TEMP4_MAX[c("db","SNAP_ID")]

DF_AAS_TEMP4_MAX <- subset(DF_AAS_TEMP4_MAX,SNAP_ID != 7242)



DF_AAS_TEMP3 <- merge(DF_AAS_TEMP2,DF_AAS_TEMP4_MAX,by=c("db","SNAP_ID"))

                      
max_vals <- ddply(DF_AAS_SUM_INT, .(format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)




dput(head(DF_AAS_TEMP4,n=10))


DF_TEMP5_MAX <- ddply(DF_TEMP5, .(db,end), subset, subset = rank(-AVG_SESS) <= 1)



