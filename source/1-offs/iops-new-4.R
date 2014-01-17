#plot_io4 <- function(DF_MAIN_BY_SNAP_INT){
  flog.debug('plot_io - start',name='plot_io')
  #DF_MAIN_INT2 <- DF_MAIN_BY_SNAP_INT
  DF_MAIN_INT2 <- data.frame(main$DF_MAIN_BY_SNAP)
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end"), measure.var = c("read_iops", "read_iops_max","write_iops", "write_iops_max",
                                                                  "read_mb_s","read_mb_s_max","write_mb_s","write_mb_s_max"
                                                                  #"read_iops_direct","read_iops_direct_max","write_iops_direct","write_iops_direct_max"
  ))
  # add a "stat" column so we can facet by stat for avg-max
  x.melt$stat <- "Avg"
  x.melt$alphaSet <- .9  
  idx_max <- with(x.melt, grepl("max", variable))
  x.melt[idx_max,]$stat <- "Max"
  #idx_max <- with(x.melt, grepl("direct", variable))
  #x.melt[idx_max,]$stat <- "Direct" 
  #idx_max <- with(x.melt, grepl("direct_max", variable))
  #x.melt[idx_max,]$stat <- "Direct Max" 
  #x.melt$value <- round(x.melt$value)
  #idx_max <- with(x.melt, grepl("direct", variable))
  #x.melt[idx_max,]$alphaSet <- 0.4 
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  #x.melt[with(x.melt, grepl("read_iops_direct", variable)),]$variable<-"Read IOPs Direct *"
  #x.melt[with(x.melt, grepl("write_iops_direct", variable)),]$variable<-"Write IOPs Direct *"
  x.melt[with(x.melt, grepl("read_iops", variable)),]$variable<-"Read IOPs"
  x.melt[with(x.melt, grepl("write_iops", variable)),]$variable<-"Write IOPs"
  x.melt[with(x.melt, grepl("read_mb_s", variable)),]$variable<-"Read MB/s"
  x.melt[with(x.melt, grepl("write_mb_s", variable)),]$variable<-"Write MB/s"
  
  
  DF_VAR_INT <- data.frame(unique(x.melt$variable))
  DF_SNAP_ID_SUBSET2 <- merge(DF_VAR_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(DF_SNAP_ID_SUBSET2$end),
                      variable = unique(x.melt$variable))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  rm(DF_SNAP_ID_SUBSET2)
  
  DF_SNAP_ID_SUBSET3 <- main$DF_SNAP_ID_SUBSET
  x.melt$variable <- factor(x.melt$variable)
  #DF_AAS_INT2$WAIT_CLASS <- factor(DF_AAS_INT2$WAIT_CLASS)
  last_level <- as.character(levels(x.melt$variable)[length(levels(x.melt$variable))])
  DF_SNAP_ID_SUBSET3$variable <- last_level
  
  
  # get the max vals for each day
  
  
  
  plot_io_int <- function(df_in){
    
    
    
    max_vals <- ddply(df_in, .(variable,stat,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
    max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")
    
    plot_int <- ggplot(data=df_in, aes(x=end, y=value),aes(color=stat)) +
      geom_line(aes(color=stat), size=.2)+
      #stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
      #      aes(color=stat,fill=stat))+
      
      attr$themeScaleColour+attr$themeScaleFill+
      main$gg_avg_max_fill+main$gg_avg_max_color+
      geom_point(data=max_vals, aes(x=end, y=value, fill=stat), size=2, shape=21)+
      geom_text(data=max_vals, aes(x=end, y=value, color=stat,label=label),size=2.5, vjust=0.5, hjust=1.25,position = position_jitter(width = .2,height=0))+
      geom_text(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.1,vjust=0.7)+
      geom_point(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0),size=1)+
      ylab('')+
      
      facet_grid(variable ~ .,scales="free_y")+
      scale_y_continuous(labels=comma)+
      xlim(min(x.melt$end),max(x.melt$end))+
      labs(title=paste("IO Avg and Max by IO Type for ",main$current_db_name,sep=""))+
      main$gg_hour_bars+
      attr$vertical_line + attr$vertical_text +
      
      scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                       minor_breaks = attr$date_break_minor_var,
                       limits = c(min(x.melt$end),max(x.melt$end)))+
      scale_alpha(guide = 'none')+
      theme(legend.key.size = unit(.25, "cm"),
            strip.text.y = element_text(size = 6)
      )
    return(plot_int)
  }
  
  p <- plot_io_int(subset(x.melt,variable %in% c("Read IOPs Direct *","Write IOPs Direct *","Read IOPs","Write IOPs","Read MB/s","Write MB/s") & stat == "Avg"))
  
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  
  
  grid.draw(p_gt)
  flog.debug('plot_io - end',name='plot_io')
 # return(p_gt)
  #return(list(p_gt,io_hist_area_plot))
  #}
#}



#io_plot <- plot_io4(main$DF_MAIN_BY_SNAP)
#grid.draw(io_plot)
