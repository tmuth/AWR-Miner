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



load_parsed_data <- function(fileNameIn){
  flog.debug('load_parsed_data - start',name='load_parsed_data')
  load(fileNameIn)
  for (objName in ls(main.save)) {
    tmp <- get(objName,envir=main.save)
    if(inherits(tmp,what='data.frame')){
      if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
        
        flog.debug(paste0(objName,' - ',nrow(tmp)),name='load_parsed_data')
        
        main[[objName]] <<- tmp
      }
    }
    rm(tmp)
  }
  flog.debug('load_parsed_data - end',name='load_parsed_data')
  return(TRUE)
}


getAASdf <- function(file){
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <- gsub(pattern = namePattern, replacement="\\1", file)
  DF_AAS_TEMP <<- NULL
  
  load(file=file)
  #debugVars$main$DF_AAS$db <- dbName
  main.save$DF_AAS$db <- dbName
  
  #DF_AAS_TEMP2 <<- debugVars$main$DF_AAS
  DF_AAS_TEMP2 <<- main.save$DF_AAS
  
  #DF_AAS_TEMP <<- ddply(debugVars$main$DF_AAS, .(db,SNAP_ID), summarise, 
  DF_AAS_TEMP <<- ddply(main.save$DF_AAS, .(db,SNAP_ID), summarise, 
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
#rm(main)




get_value_by_key<- function(DF_IN,SEARCH_COL,SEARCH_VAL,RETURN_COL){
    return(as.character(as.vector(DF_IN[DF_IN[SEARCH_COL]==SEARCH_VAL,][[RETURN_COL]][[1]])))
}

#get_value_by_key(main.save$DF_OS,"STAT_NAME","DB_NAME","STAT_VALUE")







summarize.df.dynamic<- function(df_in, sum_metric_in){
  temp_df <- df_in %>%
    group_by(cyl) %>%
    summarize_(qsec = ~mean(qsec), 
               xyz = interp(~mean(var), var = as.name(sum_metric_in))) 
  
  names(temp_df)[names(temp_df) == "xyz"] <- sum_metric_in  
  return(temp_df)
}


loadParsedDF <- function(fileName,DF_NAME,sumMetric){
  load(file=fileName)
  TMP_DF <- data.frame(main.save[[DF_NAME]])
  #setkey(TMP_DF,NULL)
  TMP_DF$db <- get_value_by_key(main.save$DF_OS,"STAT_NAME","DB_NAME","STAT_VALUE")
  rm(main.save)
  #print(head(TMP_DF))
  #print(str(TMP_DF))
  

  #      select_("SNAP_ID","db","end",interp(~var,var = as.name(sumMetric))) %>%
  TMP_DF_AGG <- TMP_DF %>%
      group_by(db,SNAP_ID) %>%
      summarize_(
      #"end"=~min(as.POSIXct(end),tz="UTC"),
      "end"=~min(end),
      sum_col = interp(~sum(var), var = as.name(sumMetric)))


  names(TMP_DF_AGG)[names(TMP_DF_AGG) == "sum_col"] <- sumMetric  
  
  #setnames(TMP_DF_AGG,"sum_col",sumMetric) # if using data.table
  
  return(TMP_DF_AGG)
}


#foo <- loadParsedDF("FINPROD-274063292-64870-65052-parsed.Rda","DF_AAS","AVG_SESS")

#head(foo)


getAllDataFrames <- function(fileList,dfName,sumMetric){
  
  COMBINED_DF_INT <- data.frame()
  
  for (f in fileList) {
    print(f)
    TMP_DF <- loadParsedDF(f,dfName,sumMetric)
    COMBINED_DF_INT <- rbind(COMBINED_DF_INT,TMP_DF)
  }
  return(COMBINED_DF_INT)
}




rdaFiles <- list.files(pattern="*-parsed.Rda")

DF_AAS2 <- getAllDataFrames(rdaFiles,"DF_AAS","AVG_SESS")




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


# get the mean CPU per sec by db
DF_AAS_COMBINED_MEAN<- ddply(DF_AAS_COMBINED, .(db), summarise,AVG_SESS=mean(AVG_SESS))
# reorder the DBs by their mean CPU per sec
DF_AAS_COMBINED_MEAN <- DF_AAS_COMBINED_MEAN[ order(DF_AAS_COMBINED_MEAN$AVG_SESS, decreasing=TRUE), ]
# reorder the DB factors by the reordered DF
DF_AAS_COMBINED$db <- factor(as.character(DF_AAS_COMBINED$db),levels=DF_AAS_COMBINED_MEAN$db,ordered=TRUE)



pdf("Combined-AAS.pdf", width = 11, height = 8.5,useDingbats=FALSE)


#DF_ANNOTATE_INT <- data.frame(x=quantile(DF_AAS_COMBINED$end,0.10),y=quantile(DF_AAS_COMBINED$AVG_SESS,probs=(0.99),type=4),


pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
          "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", 
          "#FFFF99", "#B15928") 

#colorRampPalette(pal12)(26) 
pal12 = c("#b61818","#ea910b","#e4ea0f","#54752e","#06879d","#044a9f","#75049f") 


DF_AAS_COMBINED_MAX <- ddply(DF_AAS_COMBINED, .(end), summarise,AVG_SESS=sum(AVG_SESS))
max_vals <- ddply(DF_AAS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)



DF_ANNOTATE_INT <- data.frame(x=quantile(DF_AAS_COMBINED$end,0.1),y=quant_high_sum[[1]],
                              labs=paste0("Data with total values over ",round(quant_val[[1]]*100),"th percentile (",quant_high_sum[[1]],") removed"))
numberOfDBs <- length(unique(DF_AAS_COMBINED$db))

ggplot(data=DF_AAS_COMBINED,aes(x=end,y=AVG_SESS))+
  #geom_line(aes(color=db), size=.2)
  #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
  geom_bar(aes(fill="#A6CEE3"),stat='identity',alpha=0.9)+
  geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
  #scale_fill_stata()+
  scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_AAS_COMBINED$db))) )+
  labs(title=paste0("Combined Average Active Sessions (AAS) for all DBs") )+
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
  geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4,vjust=-(quant_high_sum[[1]]*0.1))+
  theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
        strip.text.y = element_text(size = 7),
        legend.key.size = unit(.15, "cm"),
        legend.text=element_text(size=3)
  )+
  scale_y_continuous(breaks=seq(0, quant_high_sum[[1]], 2))+
  ylim(0,quant_high_sum[[1]])






plotStackedBar <- function(DF_IN){
  
  DF_AAS_COMBINED_MAX <- ddply(DF_IN, .(end), summarise,AVG_SESS=sum(AVG_SESS))
  max_vals <- ddply(DF_AAS_COMBINED_MAX, .(format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)
  
  
  
  DF_ANNOTATE_INT <- data.frame(x=quantile(DF_IN$end,0.1),y=quant_high_sum[[1]],
                                labs=paste0("Data with total values over ",round(quant_val[[1]]*100),"th percentile (",quant_high_sum[[1]],") removed"))
  numberOfDBs <- length(unique(DF_IN$db))
  
  ggplot(data=DF_IN,aes(x=end,y=AVG_SESS))+
    #geom_line(aes(color=db), size=.2)
    #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
    geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
    geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5,alpha=0.7)+
    #scale_fill_stata()+
    scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_IN$db))) )+
    labs(title=paste0("Combined Average Active Sessions (AAS) for top ",numberOfDBs," DBs") )+
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
    geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4,vjust=-(quant_high_sum[[1]]*0.1))+
    theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
          strip.text.y = element_text(size = 7),
          legend.key.size = unit(.15, "cm"),
          legend.text=element_text(size=3)
    )+
    scale_y_continuous(breaks=seq(0, quant_high_sum[[1]], 2))+
    ylim(0,quant_high_sum[[1]])
  
  
}

#plotStackedBar(DF_AAS_COMBINED)





plotFacetedBar <- function(DF_IN){

  max_vals <- ddply(DF_IN, .(db,format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)
  
  numberOfDBs <- length(unique(DF_IN$db))
  
  ggplot(data=DF_IN,aes(x=end,y=AVG_SESS,group=db))+
    #geom_line(aes(color=db), size=.2)
    #geom_area(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
    geom_bar(aes(fill=db),stat='identity',position='stack',alpha=0.9)+
    geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=2, vjust=-(quant_high[[1]]*0.07), hjust=1.2,alpha=0.7)+
    #scale_fill_stata()+
    scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_IN$db))) )+
    facet_grid(db ~ . )+
    #labs(title="Combined Average Active Sessions (AAS) - Facet by Database")+
    labs(title=paste0("Combined Average Active Sessions (AAS) - Facet by Database for top ",numberOfDBs," DBs") )+
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 day"))+
    ylim(0,(quant_high[[1]]+(quant_high[[1]]*0.25)))+
   theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"),
         strip.text.y = element_text(size = 3),
         legend.position="none",
         axis.text.y = element_text(size=5)
   )
}

#plotFacetedBar(DF_AAS_COMBINED)
 
plotBoxPlots <- function(DF_IN){
  
  numberOfDBs <- length(unique(DF_IN$db))

 ggplot(data=DF_IN, aes(x=db, y=AVG_SESS),aes(fill=db))+
   #geom_violin(aes(fill="red"),colour="#ff0000",size=0.5,alpha=0.6) +
   #geom_violin(colour="#000000") +
   geom_boxplot(aes(fill=db),alpha=.6,show_guide=FALSE,notch = FALSE,outlier.colour = "orange", outlier.size = 1,outlier.alpha=.4,outlier.shape=5)+
   geom_jitter(alpha=.2,size=1,position = position_jitter(width = .2,height=0),aes(colour="gray"))+
   scale_fill_manual( values = colorRampPalette(pal12)(length(unique(DF_IN$db))) )+
   #labs(title="Boxplot of Average Active Sessions")+
   labs(title=paste0("Boxplot of Average Active Sessions for top ",numberOfDBs," DBs") )+
   ylab("Average Active Sessions")+
   xlab("Database Name")+
   theme(legend.position="none")+
    scale_y_continuous(breaks=seq(0, max(DF_AAS_COMBINED_SUM$AVG_SESS)+(max(DF_AAS_COMBINED_SUM$AVG_SESS)*.10), 2))

}

#plotBoxPlots(DF_AAS_COMBINED)



DF_AAS_COMBINED_MEAN_MEDIUM <- head(DF_AAS_COMBINED_MEAN,n=30)
DF_AAS_COMBINED_MEDIUM <- subset(DF_AAS_COMBINED,db %in% DF_AAS_COMBINED_MEAN_MEDIUM$db)
plotStackedBar(DF_AAS_COMBINED_MEDIUM)
plotFacetedBar(DF_AAS_COMBINED_MEDIUM)
plotBoxPlots(DF_AAS_COMBINED_MEDIUM)


DF_AAS_COMBINED_MEAN_small <- head(DF_AAS_COMBINED_MEAN,n=5)
DF_AAS_COMBINED_SMALL <- subset(DF_AAS_COMBINED,db %in% DF_AAS_COMBINED_MEAN_small$db)
plotStackedBar(DF_AAS_COMBINED_SMALL)
plotFacetedBar(DF_AAS_COMBINED_SMALL)
plotBoxPlots(DF_AAS_COMBINED_SMALL)
 
dev.off()