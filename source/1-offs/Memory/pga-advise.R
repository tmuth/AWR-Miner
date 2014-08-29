plot_memory_pga_advise <- function(df_in){
  flog.debug('plot_memory_PGA_advise - start',name='plot_memory_PGA_advise')
  
DF_MEMORY_PGA_ADVICE_TMP <- df_in 
  #DF_MEMORY_PGA_ADVICE_TMP <- main$DF_MEMORY_PGA_ADVICE 
  
  
  
  
  DF_MEMORY_PGA_ADVICE_1_TMP <- subset(DF_MEMORY_PGA_ADVICE_TMP,SIZE_FACTOR==1)
  

  Mode <- function(x) names(which.max(table(x)))  
  DF_MEMORY_PGA_ADVICE_TMP_MODE <- ddply(DF_MEMORY_PGA_ADVICE_1_TMP,.(INSTANCE_NUMBER),summarise,
                                         PGA_TARGET_GB=Mode(PGA_TARGET_GB)
                                      )

  DF_MEMORY_PGA_ADVICE_1_TMP <- DF_MEMORY_PGA_ADVICE_1_TMP[c("end","SNAP_ID","INSTANCE_NUMBER","ESTD_EXTRA_MB_RW")]
  
  setnames(DF_MEMORY_PGA_ADVICE_1_TMP, "ESTD_EXTRA_MB_RW", "CURRENT_MB_RW")

  DF_MEMORY_PGA_ADVICE_1_TMP$CURRENT_MB_RW <- ifelse(DF_MEMORY_PGA_ADVICE_1_TMP$CURRENT_MB_RW==0,0.1,DF_MEMORY_PGA_ADVICE_1_TMP$CURRENT_MB_RW) 
  DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW <- ifelse(DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW==0,0.1,DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW) 
  
  DF_MEMORY_PGA_ADVICE_TMP <- merge(DF_MEMORY_PGA_ADVICE_TMP,DF_MEMORY_PGA_ADVICE_1_TMP,by=c('end','SNAP_ID','INSTANCE_NUMBER'))
  rm(DF_MEMORY_PGA_ADVICE_1_TMP)
  DF_MEMORY_PGA_ADVICE_TMP$pct_change <- (DF_MEMORY_PGA_ADVICE_TMP$ESTD_EXTRA_MB_RW-DF_MEMORY_PGA_ADVICE_TMP$CURRENT_MB_RW)/DF_MEMORY_PGA_ADVICE_TMP$CURRENT_MB_RW
  
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG <- ddply(DF_MEMORY_PGA_ADVICE_TMP,.(INSTANCE_NUMBER,PGA_TARGET_GB),summarise,
                                        pct_change=mean(pct_change),
                                        SIZE_FACTOR=mean(SIZE_FACTOR))
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_PGA_ADVICE_TMP_AGG$SIZE_FACTOR==1,"Current Value","Minor Change")
  DF_MEMORY_PGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_PGA_ADVICE_TMP_AGG$pct_change>=0.05,"Increased Physical Reads",DF_MEMORY_PGA_ADVICE_TMP_AGG$color)
  DF_MEMORY_PGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_PGA_ADVICE_TMP_AGG$pct_change<=-0.05,"Decreased Physical Reads",DF_MEMORY_PGA_ADVICE_TMP_AGG$color)
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG$PGA_TARGET_GB <- factor(DF_MEMORY_PGA_ADVICE_TMP_AGG$PGA_TARGET_GB)
  
  DF_MEMORY_PGA_ADVICE_TMP_AGG$INSTANCE_NUMBER <- paste0("Node ",DF_MEMORY_PGA_ADVICE_TMP_AGG$INSTANCE_NUMBER)
  
  sga_colors <- c("Current Value" = "#000000", "Minor Change" = "#777777", 
                  "Increased Physical Reads" = "#a60000",
                  "Decreased Physical Reads" = "#008000")
  
  gg_PGA_colors <- scale_colour_manual("", values = sga_colors)
  gg_PGA_fill<- scale_fill_manual("", values = sga_colors)
  
  
  p <- ggplot(data=DF_MEMORY_PGA_ADVICE_TMP_AGG,aes(x=PGA_TARGET_GB,y=pct_change*100,colour=color,fill=color))+
    geom_bar(stat="identity",alpha=.2)+
    geom_text(aes(label=paste(round(pct_change * 100, 1), "%", sep = "")),size=2,colour="black",vjust=.5)+
    facet_grid(INSTANCE_NUMBER ~ . )+
    gg_PGA_colors+gg_PGA_fill+
    scale_x_discrete(breaks=unique(DF_MEMORY_PGA_ADVICE_TMP_AGG$PGA_TARGET_GB))+
    labs(title=paste("Average Percent Change in W/A MB Read/Written to Disk Relative To PGA_AGGREGATE_TARGET from PGA Target Advisory for ",main$current_db_name,sep=""))+
    xlab("PGA_AGGREGATE_TARGET (GB)")+
    ylab("Percent Change in W/A MB Read/Written to Disk (Negative is better)")+
    theme(panel.grid.major.x = element_line("#aaaaaa", size = .1,linetype = "dotted"))
#  p
  flog.debug('plot_memory_PGA_advise - end',name='plot_memory_PGA_advise')
 # str(p)
  return(p)
}
  

  