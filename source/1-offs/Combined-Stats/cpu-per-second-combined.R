source("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/commonFunctions.R")

#setwd("M:/Dropbox/MyFiles/Accounts/Federal/FAA/AWR-Sizing-Sept-2013")
#setwd("M:/Dropbox/MyFiles/Accounts/Federal/World Bank/WB-share/AWR-Miner-Oct-2-2013")
#setwd("M:/Dropbox/MyFiles/Accounts/Federal/World Bank/WB-share/AWR-Miner-Oct-22")


DF_CPU_TEMP <- data.frame()
DF_CPU_TEMP2 <- data.frame()
#DF_TEMP <- data.frame()
#DF_TEMP2 <- data.frame()
#DF_TEMP3 <- data.frame()
DF_CPU_COMBINED <- data.frame()


getCPUdf <- function(file){
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <- gsub(pattern = namePattern, replacement="\\1", file)
  DF_CPU_TEMP <<- NULL
  
  load(file=file)
  debugVars$main$DF_MAIN$db <- dbName
  
  
  
  DF_CPU_TEMP2 <<- debugVars$main$DF_MAIN
  DF_CPU_TEMP2$cpu_per_s.cpu_per_s_sd <- DF_CPU_TEMP2$cpu_per_s+DF_CPU_TEMP2$cpu_per_s_sd
  
  DF_CPU_TEMP <<- ddply(DF_CPU_TEMP2, .(snap,db), summarise, 
                    end=min(as.POSIXct(end,tz="UTC")),
                    cpu_per_s=sum(cpu_per_s),
                    cpu_per_s.cpu_per_s_sd=sum(cpu_per_s.cpu_per_s_sd),
                    cpu_per_s_sd_mean=mean(cpu_per_s_sd)
                    
  ) 
  
  # round to the nearest hour
  DF_CPU_TEMP$end <<- round_date(DF_CPU_TEMP$end,"hour")
  
  # group by snap,db,end, max(iops): to get only max iops per hour (in case of 15 min snapshots for example)
  DF_CPU_TEMP <<- ddply(DF_CPU_TEMP, .(db,end), summarise, 
                        cpu_per_s=max(cpu_per_s),
                        cpu_per_s.cpu_per_s_sd=max(cpu_per_s.cpu_per_s_sd),
                        cpu_per_s_sd_mean=max(cpu_per_s_sd_mean)
  )
  
  
  
  
  #DF_TEMP$total_iops_direct <<- DF_TEMP$read_iops_direct + DF_TEMP$write_iops_direct
  #DF_TEMP$total_iops_direct_max <<- DF_TEMP$read_iops_direct_max + DF_TEMP$write_iops_direct_max
  
  
  # need to avg by(snap,db,end) for all metrics to account for 15 min snapshots
  
  DF_CPU_COMBINED <<- rbind(DF_CPU_COMBINED,DF_CPU_TEMP)
  
  
}
#rm(main)

#rdaFiles <- list.files(pattern="*debugVars.Rda")
rdaFiles <- list.files(pattern="EDWDBA-7239-7447-1-debugVars.Rda")

for (f in rdaFiles) {
  print(f)
  getCPUdf(f)
}

#rm(DF_CPU_TEMP)
#rm(DF_CPU_TEMP2)

DF_CPU_COMBINED_SUM <<- ddply(DF_CPU_COMBINED, .(end), summarise, 
                              cpu_per_s.cpu_per_s_sd=sum(cpu_per_s.cpu_per_s_sd)
)

quant_val <- 0.99

quant_high_sum <- quantile(DF_CPU_COMBINED_SUM$cpu_per_s.cpu_per_s_sd,probs=c(quant_val[[1]]),type=4)

# DF_CPU_COMBINED_SAVE <- DF_CPU_COMBINED
# filter results above a certain quantile to keep outliers from skewing the plots

quant_high <- quantile(DF_CPU_COMBINED$cpu_per_s.cpu_per_s_sd,probs=c(quant_val[[1]]),type=4)


pdf("Combined-DB-CPU-per-Second.pdf", width = 11, height = 8.5,useDingbats=FALSE)

DF_CPU_COMBINED_MAX <- ddply(DF_CPU_COMBINED, .(end), summarise,cpu_per_s.cpu_per_s_sd=sum(cpu_per_s.cpu_per_s_sd))
max_vals <- ddply(DF_CPU_COMBINED_MAX, .(end=format(end,"%y/%m/%d")), subset, subset = rank(-cpu_per_s.cpu_per_s_sd) <= 1)



#DF_ANNOTATE_INT <- data.frame(x=quantile(DF_CPU_COMBINED$end,0.10),y=quantile(DF_CPU_COMBINED$AVG_SESS,probs=(0.99),type=4),
DF_ANNOTATE_INT <- data.frame(x=quantile(DF_CPU_COMBINED$end,0.05),y=quant_high_sum[[1]],
                              labs=paste0("Data with total values over ",round(quant_val[[1]]*100),"th percentile (",quant_high_sum[[1]],") removed"))

pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
          "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", 
          "#FFFF99", "#B15928") 

pal12 = c("#b61818","#ea910b","#e4ea0f","#54752e","#06879d","#044a9f","#75049f") 

#colorRampPalette(pal12)(26) 

# get the mean CPU per sec by db
DF_CPU_COMBINED_MEAN<- ddply(DF_CPU_COMBINED, .(db), summarise,cpu_per_s.cpu_per_s_sd=mean(cpu_per_s.cpu_per_s_sd))
# reorder the DBs by their mean CPU per sec
DF_CPU_COMBINED_MEAN <- DF_CPU_COMBINED_MEAN[ order(DF_CPU_COMBINED_MEAN$cpu_per_s.cpu_per_s_sd, decreasing=TRUE), ]
# reorder the DB factors by the reordered DF
DF_CPU_COMBINED$db <- factor(as.character(DF_CPU_COMBINED$db),levels=DF_CPU_COMBINED_MEAN$db,ordered=TRUE)



ggplot(data=DF_CPU_COMBINED,aes(x=end,y=cpu_per_s.cpu_per_s_sd))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=cpu_per_s.cpu_per_s_sd), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=cpu_per_s.cpu_per_s_sd,label=cpu_per_s.cpu_per_s_sd),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  #scale_fill_stata()+
  scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_CPU_COMBINED$db))) )+
  labs(title="Combined Database CPU Seconds Per Second")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
  ylim(0,quant_high_sum[[1]])+
  ylab("DB CPU Seconds Per Second")+
  geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4)+
  theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
        strip.text.y = element_text(size = 7),
        legend.key.size = unit(.15, "cm"),
        legend.text=element_text(size=5)
  )

#max_vals <- ddply(DF_CPU_COMBINED, .(db,end=format(end,"%y/%m/%d")), subset, subset = rank(-cpu_per_s.cpu_per_s_sd) <= 1)


ggplot(data=DF_CPU_COMBINED,aes(x=end,y=cpu_per_s.cpu_per_s_sd,group=db))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  #geom_point(data=max_vals, aes(x=end, y=cpu_per_s.cpu_per_s_sd), size=2, shape=21)+
  #geom_text(data=max_vals, aes(x=end, y=cpu_per_s.cpu_per_s_sd,label=cpu_per_s.cpu_per_s_sd),size=2, vjust=-.8, hjust=1.5,alpha=0.7)+
  #scale_fill_stata()+
  scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_CPU_COMBINED$db))) )+
  facet_grid(db ~ . )+
  labs(title="Combined Database CPU Seconds Per Second - Facet by Database")+
  ylab("DB CPU Seconds Per Second")+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
  ylim(0,quant_high[[1]])+
  theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
        strip.text.y = element_text(size = 4),
        legend.position="none",
        axis.text.y = element_text(size=5)
  )



# # get the mean CPU per sec by db
# DF_CPU_COMBINED_MEAN<- ddply(DF_CPU_COMBINED2, .(db), summarise,cpu_per_s.cpu_per_s_sd=mean(cpu_per_s.cpu_per_s_sd))
# # reorder the DBs by their mean CPU per sec
# DF_CPU_COMBINED_MEAN <- DF_CPU_COMBINED_MEAN[ order(DF_CPU_COMBINED_MEAN$cpu_per_s.cpu_per_s_sd, decreasing=TRUE), ]
# # reorder the DB factors by the reordered DF
# DF_CPU_COMBINED2$db <- factor(as.character(DF_CPU_COMBINED2$db),levels=DF_CPU_COMBINED_MEAN$db,ordered=TRUE)
#   
#   
  
  
  
  
  
  
  
ggplot(data=DF_CPU_COMBINED, aes(x=db, y=cpu_per_s.cpu_per_s_sd),aes(fill=db))+
  #geom_violin(aes(fill="red"),colour="#ff0000",size=0.5,alpha=0.6) +
  #geom_violin(colour="#000000") +
  geom_boxplot(aes(fill=db),alpha=.6,show_guide=FALSE,notch = FALSE,outlier.colour = "orange", outlier.size = 1,outlier.alpha=.4,outlier.shape=5)+
  geom_jitter(alpha=.2,size=1,position = position_jitter(width = .2,height=0),aes(colour="gray"))+
  scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_CPU_COMBINED$db))) )+
  labs(title="Boxplot of Database CPU Seconds Per Second")+
  ylab("DB CPU Seconds Per Second")+
  xlab("Database Name")+
  theme(legend.position="none")
  




dev.off()
