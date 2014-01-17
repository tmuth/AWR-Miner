plot_aas_chart2 <- function(DF_AAS_INT){
  flog.debug('plot_aas_chart - start',name='plot_aas_chart')
  #   log_it('plot_aas_chart - start')
  #   log_it(min(DF_AAS_INT$end))
  #   log_it(max(DF_AAS_INT$end))
  #   
  #   log_it(min(main$DF_SNAP_ID_SUBSET$end))
  #   log_it(max(main$DF_SNAP_ID_SUBSET$end))
  #browser()
  #DF_AAS_INT <- DF_AAS
  
  #print(unique(main$DF_AAS$WAIT_CLASS))
  
  DF_AAS_INT$WAIT_CLASS <- str_trim(DF_AAS_INT$WAIT_CLASS)
  DF_AAS_INT$WAIT_CLASS <- factor(DF_AAS_INT$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
  
  
  vals <- expand.grid(end = unique(DF_AAS_INT$end),
                      WAIT_CLASS = unique(DF_AAS_INT$WAIT_CLASS))
  
  DF_AAS_INT <- merge(vals,DF_AAS_INT)
  rm(vals)
  #print(is.na(DF_AAS_INT))
  #print(head(DF_AAS_INT))
  
  #DF_AAS_INT[is.na(DF_AAS_INT)] <- 0
  #df[df$column_name,]
  #DF_AAS_INT <- DF_AAS_INT[DF_AAS_INT$end,]
  DF_AAS_INT <- subset(DF_AAS_INT,end != TRUE)
  DF_AAS_INT <- subset(DF_AAS_INT,end != FALSE)
  DF_AAS_INT$AVG_SESS[is.na(DF_AAS_INT$AVG_SESS)] <- 0
  #print(head(is.na(DF_AAS_INT)))
  
  
  aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                  "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                  "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                  "System I/O" = "#0993de","User I/O" = "#054ae1")
  
  gg_aas_colors <- scale_fill_manual("", values = aas_colors)
  
  
  DF_AAS_SUM_INT <- ddply(DF_AAS_INT, .(SNAP_ID,end), summarise,AVG_SESS=sum(AVG_SESS))
  
  # get the max vals for each day
  max_vals <- ddply(DF_AAS_SUM_INT, .(format(end,"%y/%m/%d")), subset, subset = rank(-AVG_SESS) <= 1)
  
  sess_quantile <- quantile(DF_AAS_SUM_INT$AVG_SESS,0.99)
  cpu_cores_line <- geom_hline(yintercept=as.numeric(main$cpu_cores), linetype="dotted",color="red",size=0.7,alpha=0.4)
  df_cpu_cores_label <- data.frame(end=min(min(DF_AAS_INT$end)),AVG_SESS=main$cpu_cores)
  
  ymax <- max(c(main$cpu_cores+(main$cpu_cores*0.2),(max(DF_AAS_SUM_INT$AVG_SESS)+(max(DF_AAS_SUM_INT$AVG_SESS)*0.2))))
  
  plot_aas_wait_class <- ggplot()+
    geom_bar(data=DF_AAS_INT, aes(x = end, y = AVG_SESS,
                                   fill = WAIT_CLASS),stat = "identity", position = "stack")+
    gg_aas_colors+
    theme(axis.title.x  = element_blank(),axis.title.y  = element_blank(),
          legend.key.size =    unit(0.6, "lines"))+
    labs(title=paste("Average Active Sessions (AAS) for ",main$current_db_name,sep=""))+
    main$gg_hour_bars+
    geom_point(data=max_vals, aes(x=end, y=AVG_SESS), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.8, hjust=1.5)+
    geom_text(data=main$DF_SNAP_ID_SUBSET,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,alpha=0.2,hjust=-0.1,vjust=0.7)+                                     
    geom_point(data=main$DF_SNAP_ID_SUBSET,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0)+
    cpu_cores_line+
    attr$vertical_line + attr$vertical_text +
    geom_text(data=df_cpu_cores_label, aes(x=end, y=AVG_SESS,label=paste0("CPU Cores - ",main$cpu_cores)),size=2, vjust=-.8, hjust=.5,color="red",alpha=0.4)+
    #scale_x_datetime(breaks=NULL)+
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_AAS_INT$end),max(DF_AAS_INT$end)))
  #                                      scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_breaks("1 days"),
  #                                                       minor_breaks = date_breaks("12 hour"),
  #                                                       limits = c(min(DF_AAS_INT$end),max(DF_AAS_INT$end))
  #                                                       )
  #ylim(-(ymax*0.025),ymax)
  
  
  plot_aas_wait_class_gt <- ggplot_gtable(ggplot_build(plot_aas_wait_class))
  
#   sess_quantile2 <- quantile(DF_AAS_INT$AVG_SESS,probs=c(0.95),type=4)
#   
#   DF_AAS_INT2 <- subset(DF_AAS_INT,AVG_SESS >= sess_quantile2[[1]],rownames=FALSE,stringsasfactors=TRUE)
#   DF_AAS_INT2$WAIT_CLASS <- factor(DF_AAS_INT2$WAIT_CLASS,c("Other","Cluster","Queueing","Network","Administrative","Configuration","Commit","Application","Concurrency","System I/O","User I/O","Scheduler","CPU"))
#   
#   
#   DF_AAS_INT2 <- droplevels(DF_AAS_INT2)
#   # get the max vals for each day
#   max_vals2 <- ddply(DF_AAS_INT2, .(format(end,"%y/%m/%d"),WAIT_CLASS), subset, subset = rank(-AVG_SESS) <= 1)
#   DF_SNAP_ID_SUBSET2 <- main$DF_SNAP_ID_SUBSET
#   #DF_AAS_INT2$WAIT_CLASS <- factor(DF_AAS_INT2$WAIT_CLASS)
#   last_level <- as.character(levels(DF_AAS_INT2$WAIT_CLASS)[length(levels(DF_AAS_INT2$WAIT_CLASS))])
#   DF_SNAP_ID_SUBSET2$WAIT_CLASS <- last_level
#   ymax2 <- max(c(main$cpu_cores+(main$cpu_cores*0.2),(max(DF_AAS_INT2$AVG_SESS)+(max(DF_AAS_INT2$AVG_SESS)*0.2))))
#   
#   
#   plot_aas_wait_class2_line <- ggplot()+
#     geom_line(data=DF_AAS_INT2, aes(x = end, y = AVG_SESS,color=WAIT_CLASS),stat = "identity",alpha=1,size=.7) +
#     scale_color_manual("", values = aas_colors)+
#     geom_point(data=max_vals2, aes(x=end, y=AVG_SESS), size=2, shape=21)+
#     geom_text(data=max_vals2, aes(x=end, y=AVG_SESS,label=AVG_SESS),size=3, vjust=-.4, hjust=1)+
#     #facet_grid(WAIT_CLASS ~ . )+
#     main$gg_hour_bars+
#     attr$vertical_line + attr$vertical_text +
#     scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
#                      minor_breaks = attr$date_break_minor_var
#                      #limits = c(min(DF_AAS_INT2$end),max(DF_AAS_INT2$end))
#     )+
#     theme(axis.title.x  = element_blank(),axis.title.y  = element_blank())+
#     labs(title=paste(paste("Average Active Sessions by Wait Class - ",main$current_db_name,sep=""),"\n Only Values >= 95th Percentile",sep=""))+
#     geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,alpha=0.2,hjust=-0.15,vjust=2.2)+
#     geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0)+
#     #ylim(0,ymax2)+
#     theme(legend.position="right")
#   
#   plot_aas_wait_class2 <- plot_aas_wait_class2_line+
#     facet_grid(WAIT_CLASS ~ . )+theme(legend.position="none")
#   
#   plot_aas_wait_class2_line <- plot_aas_wait_class2_line+cpu_cores_line
#   
#   plot_aas_wait_class2_gt <- ggplot_gtable(ggplot_build(plot_aas_wait_class2))
#   plot_aas_wait_class2_gt$layout$clip[plot_aas_wait_class2_gt$layout$name=="panel"] <- "off"
#   #print(plot_aas_wait_class_gt)
#   #grid.draw(plot_aas_wait_class2_gt)
#   
#   
  
  
  
  
  
  
  flog.debug('plot_aas_chart - end',name='plot_aas_chart')
  return(plot_aas_wait_class)
  #return(plot_aas_wait_class_gt)
  
  
}


c(aas_plot) := plot_aas_chart2(main$DF_AAS)
#grid.draw(aas_plot)
str(aas_plot)