plot_aas_percent2 <- function(DF_AAS_INT){
  flog.debug('plot_aas_percent - start',name='plot_aas_percent')
  
  
  #New AAS Percent Section
  
  avgSessTotal <- sum(DF_AAS_INT$AVG_SESS)
  DF_AAS_AGG_BY_SNAP <- DF_AAS_INT[, list(AVG_SESS = sum(AVG_SESS)), by = list(SNAP_ID)]
  
  DF_AAS_AGG <- DF_AAS_INT[, list(AVG_SESS = sum(AVG_SESS)), by = list(WAIT_CLASS)]
  DF_AAS_AGG$AVG_SESS_PCT <- round(DF_AAS_AGG$AVG_SESS/avgSessTotal,2)
  
  ptile <- 0.75
  aasPTile <- as.vector(quantile(DF_AAS_AGG_BY_SNAP$AVG_SESS,probs=c(ptile),type=4))
  DF_AAS_AGG_BY_SNAP <- data.table(DF_AAS_AGG_BY_SNAP)
  DF_AAS_AGG_BY_SNAP2 <- data.table(DF_AAS_AGG_BY_SNAP[AVG_SESS >= aasPTile])
  DF_AAS_AGG_BY_SNAP3 <- data.table(DF_AAS_AGG_BY_SNAP2[,list(SNAP_ID = SNAP_ID)])
  DF_AAS_TEMP2 <- data.table(DF_AAS_INT[DF_AAS_AGG_BY_SNAP3])
  avgSessTotal2 <- sum(DF_AAS_TEMP2$AVG_SESS)
  DF_AAS_AGG2 <- DF_AAS_TEMP2[, list(AVG_SESS = sum(AVG_SESS)), by = list(WAIT_CLASS)]
  DF_AAS_AGG2$AVG_SESS_PCT <- round(DF_AAS_AGG2$AVG_SESS/avgSessTotal2,2)
  
  aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                  "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                  "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                  "System I/O" = "#0993de","User I/O" = "#054ae1")
  
  gg_aas_colors <- scale_fill_manual("", values = aas_colors)
  
  
  if( nrow(main$DF_TOP_N_EVENTS)>10){
    titleText <- ggtitle(expression(atop("AAS % by Wait Class - All Snapshots"), atop(scriptstyle(" "), "")))
  }
  else{
    titleText <- labs(title=paste("AAS % by Wait Class - All Snapshots - ",main$current_db_name,sep=""))
  }
  
  
  aas_pct_plot1 <- ggplot(data=DF_AAS_AGG, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
    geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
    geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
    gg_aas_colors+
    scale_y_continuous(labels = percent_format())+
    theme(axis.title.y  = element_blank(),axis.title.x  = element_blank(),legend.position =    "none",
          plot.title= element_text(size = 5) )+
    titleText
  #aas_pct_plot1
  
  
  #   
  #   
  #   
  if( nrow(main$DF_TOP_N_EVENTS)>10){
    
    DF_TOP_N_AGG1 <- main$DF_TOP_N_EVENTS %>%
      group_by(WAIT_CLASS,EVENT_NAME) %>%
      summarise(TOTAL_TIME_S = sum(as.numeric(TOTAL_TIME_S))) %>%
      arrange(desc(TOTAL_TIME_S)) %>%
      head(25) %>%
      arrange(WAIT_CLASS)
    
    
    
    if(nrow(subset(DF_TOP_N_AGG1,WAIT_CLASS == "DB CPU"))>0){
      DF_TOP_N_AGG1[with(DF_TOP_N_AGG1, grepl("DB CPU", WAIT_CLASS)),]$WAIT_CLASS<-"CPU"
    }
    total_time <- sum(DF_TOP_N_AGG1$TOTAL_TIME_S)
    
    DF_TOP_N_AGG1$pct_time <- (DF_TOP_N_AGG1$TOTAL_TIME_S/total_time)
    
    DF_TOP_N_AGG1$EVENT_NAME <- substr(str_trim(DF_TOP_N_AGG1$EVENT_NAME),1,25)
    
    DF_TOP_N_AGG1$WAIT_CLASS <- factor(DF_TOP_N_AGG1$WAIT_CLASS)
    
    DF_TOP_N_AGG1 <- subset(DF_TOP_N_AGG1,pct_time > 0.01)
    aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                    "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                    "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                    "System I/O" = "#0993de","User I/O" = "#054ae1")
    gg_aas_colors <- scale_fill_manual("", values = aas_colors)
    
    aas_pct_plot2 <- ggplot(data=DF_TOP_N_AGG1, aes(x=EVENT_NAME, y=pct_time),aes(color=WAIT_CLASS)) +
      geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
      geom_text(aes(label=paste(round(pct_time * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
      gg_aas_colors+
      scale_y_continuous(labels = percent_format())+
      theme(axis.title.y  = element_blank(),axis.title.x  = element_blank(),legend.position =    "none",
            plot.title= element_text(size = 5),
            axis.text.x=element_text(size=4))+
      ggtitle(expression(atop("Top N Timed Events Aggregated over Snapshot Range", atop(scriptstyle("Colors correspond to wait classes from AAS graph to left"), ""))))
  }
  else{
    aas_pct_plot2 <- ggplot(data=DF_AAS_AGG2, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
      geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
      geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
      gg_aas_colors+
      scale_y_continuous(labels = percent_format())+
      theme(axis.title.y  = element_blank(),axis.title.x  = element_blank(),legend.position =    "none",
            plot.title= element_text(size = 5) )+
      labs(title=paste("AAS % by Wait Class - Only Snapshots Where Total AAS >= 75th Percentile - ",main$current_db_name,sep=""))
  }
  
  
  
  
  #aas_pct_plot2
  
  
  
  
  flog.debug('plot_aas_percent - end',name='plot_aas_percent')
  return(list(aas_pct_plot1,aas_pct_plot2))
  
}



c(aas_pct1, aas_pct2) := plot_aas_percent2(main$DF_AAS)

print(aas_pct2)




tmPlot(GNI2010
       , index = c("continent", "iso3")
       , vSize = "population"
       , vColor = "GNI"
       , type = "value")



DF_TOP_N_AGG1 <- main$DF_TOP_N_EVENTS %>%
  group_by(WAIT_CLASS,EVENT_NAME) %>%
  summarise(TOTAL_TIME_S = sum(as.numeric(TOTAL_TIME_S))) %>%
  ungroup() %>%
  arrange(desc(TOTAL_TIME_S)) 
  #head(25) %>%
  #arrange(WAIT_CLASS)



DF_TOP_N_AGG1 <- DF_TOP_N_AGG1 %>%
  arrange(desc(TOTAL_TIME_S))

library(treemap)

DF_TOP_N_AGG2 <- data.frame(DF_TOP_N_AGG1)


aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","DB CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "System I/O" = "#0993de","User I/O" = "#054ae1")

color_DF <- data.frame("WAIT_CLASS"=names(aas_colors),"color"=aas_colors,row.names=NULL)

DF_TOP_N_AGG3 <- merge(DF_TOP_N_AGG2,color_DF)

main_vp <- viewport(x=0.5, y=0.5, width=1, height=1)
pushViewport(main_vp)


treemap(DF_TOP_N_AGG3
       , index = c("WAIT_CLASS", "EVENT_NAME")
       , vSize = "TOTAL_TIME_S"
       , vColor = "color"
       , type = "color"
       ,vp=main_vp)

x <- grid.arrange( ncol = 1, heights=c(1))

grid.show.layout(x)




print(tm)
