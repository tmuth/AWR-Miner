plot_cpu_per_second <- function(DF_MAIN_INT){
  flog.debug('plot_cpu_per_sec - start',name='plot_cpu_per_sec')
  
  DF_MAIN_INT <- main$DF_MAIN
  
  #DF_MAIN_INT$cpu_per_s_pct <- round((DF_MAIN_INT$cpu_per_s / get_os_stat("NUM_CPU_CORES"))*100,1)
  #DF_MAIN_INT$cpu_per_s_sd_pct <- round((DF_MAIN_INT$cpu_per_s_sd / get_os_stat("NUM_CPU_CORES"))*100,1)
  
  DF_INST_INT <- data.frame(unique(DF_MAIN_INT$inst))
  DF_SNAP_ID_SUBSET2 <- merge(DF_INST_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(main$DF_SNAP_ID_SUBSET$end),
                      inst = unique(DF_MAIN_INT$inst))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  x.melt <- melt(DF_MAIN_INT, id.var = c("end","inst","h_cpu_per_s_sd","cpu_per_s_sd"), measure.var = c("h_cpu_per_s", "cpu_per_s"))
  
  x.melt$sd <- ifelse(x.melt$variable=="h_cpu_per_s",x.melt$h_cpu_per_s_sd,x.melt$cpu_per_s_sd)
  
  x.melt$variable<-gsub( "^cpu_per_s$" , "DB CPU/sec Avg" , x.melt$variable)
  
  x.melt$variable<-gsub( "h_cpu_per_s" , "OS CPU/sec Avg" , x.melt$variable)
  # add 2 minutes to the DB CPU Avg rows to keep the lines for std dev from over-plotting.
  x.melt[with(x.melt, grepl("DB CPU/sec Avg", variable)),]$end<-x.melt[with(x.melt, grepl("DB CPU/sec Avg", variable)),]$end + minutes(2)
  
  
  max_vals <- data.frame()
  gg_maxvals_point <- theme()
  gg_maxvals_text <- theme()
  
  get_maxvals_int <- function(df_in){
    max_vals_int <- ddply(df_in, .(variable,inst,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
    max_vals_int$label <- formatC(max_vals_int$value, format="d", big.mark=",")
    
    #gg_maxvals_point <<- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    gg_maxvals_point_int <- geom_point(data=max_vals_int, aes(x=end, y=value, fill=variable), size=2, shape=21)
    
    gg_maxvals_text_int <- geom_text(data=max_vals_int, aes(x=end, y=value, color=variable,label=label),size=3, vjust=-.8, hjust=1.5)
    return(list(max_vals_int,gg_maxvals_point_int,gg_maxvals_text_int))
  }
  
  
  tryCatch(c(max_vals,gg_maxvals_point,gg_maxvals_text) :=  get_maxvals_int(x.melt),
           error = function(e) {
             #traceback()
             gg_maxvals_point <- theme()
             gg_maxvals_text <- theme()
             
           }
  )
  
  
  cores_per_node <- get_os_stat("NUM_CPU_CORES")/get_os_stat("INSTANCES")
  df_cpu_cores_label <- data.frame(end=min(min(x.melt$end)),inst=unique(x.melt$inst),value=cores_per_node)
  cpu_cores_line <- geom_hline(yintercept=as.numeric(cores_per_node), linetype="dotted",color="red",size=0.7,alpha=0.4)
  DF_ANNOTATE_INT <- data.frame(x=quantile(x.melt$end,0.10),y=cores_per_node+(cores_per_node*.1),inst=min(x.melt$inst),
                                labs="Standard Deviation shown as vertical lines over points")
  
  cpu_colors <- c("OS CPU/sec Avg"="#90353b","DB CPU/sec Avg"="#3a6185")
  gg_cpu_colors_fill <- scale_fill_manual("", values = cpu_colors)
  gg_cpu_colors_color <- scale_color_manual("", values = cpu_colors)
  
  
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(group=variable,color=variable)) +
    main$gg_hour_bars+
    geom_point(aes(group=variable,color=variable), size=0.6,shape=1,alpha=0.8)+
    geom_linerange(aes(ymin=value-sd,ymax=value+sd,color=variable), alpha=0.5,size=0.1)+
    attr$themeScaleColour+attr$themeScaleFill+
    #stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",formula=y~poly(x,2),
    #            aes(group=variable,color=variable,fill=variable))+
    ylab('CPU Seconds Per Second')+
    ylim(0,60)+
    gg_maxvals_point+gg_maxvals_text+
    
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-45,size=1.5,hjust=-.5,alpha=0.2)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1)+
    facet_grid(inst ~ .)+
    theme(axis.title.x  = element_blank())+
    theme(panel.background = element_rect(colour = "#777777"))+
    xlim(min(x.melt$end),max(x.melt$end))+
    labs(title=paste("OS CPU % and Database CPU % for ",main$current_db_name,sep=""))+
    
    attr$vertical_line + attr$vertical_text +
    #scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = date_breaks("2 hour"),
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end))
    )+
    gg_cpu_colors_fill+gg_cpu_colors_color+
    geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4)+
    cpu_cores_line+
    geom_text(data=df_cpu_cores_label, aes(x=end, y=value,label=paste0("CPU Cores - ",df_cpu_cores_label$value)),size=2, vjust=-.8, hjust=.5,color="red",alpha=0.4)
  #annotate("text", x = median(x.melt$end), y = 105, label = "Standard Deviation shown as vertical lines over points",size=2,alpha=.4)
  # theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))
  #theme(panel.grid.major = element_line("#eeeeee", size = 0.2,linetype = "dotted"))+
  #theme(panel.grid.minor = element_line("#efefef", size = 0.1,linetype = "dotted"))
  #main$gg_bottom_panel
  
  p
  p_gt <- ggplot_gtable(ggplot_build(p))
  flog.debug('plot_cpu - end',name='plot_cpu')
  
  return(plot_cpu_per_sec)
  #}
}