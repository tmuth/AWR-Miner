


plot_cpu <- function(DF_MAIN_INT){
  flog.debug('plot_cpu - start',name='plot_cpu')
  
  DF_MAIN_INT$cpu_per_s_pct <- round((DF_MAIN_INT$cpu_per_s / get_os_stat("NUM_CPU_CORES"))*100,1)
  DF_MAIN_INT$cpu_per_s_sd_pct <- round((DF_MAIN_INT$cpu_per_s_sd / get_os_stat("NUM_CPU_CORES"))*100,1)
  
  #if(dim(DF_MAIN_INT)[1] > 2){
  #DF_MAIN_INT <- DF_MAIN
  #browser()
  DF_INST_INT <- data.frame(unique(DF_MAIN_INT$inst))
  DF_SNAP_ID_SUBSET2 <- merge(DF_INST_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(main$DF_SNAP_ID_SUBSET$end),
                      inst = unique(DF_MAIN_INT$inst))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  x.melt <- melt(DF_MAIN_INT, id.var = c("end","inst","os_cpu_sd","cpu_per_s_sd_pct"), measure.var = c("os_cpu", "cpu_per_s_pct"))
  print(str(x.melt))
  x.melt$sd <- ifelse(x.melt$variable=="os_cpu",x.melt$os_cpu_sd,x.melt$cpu_per_s_sd_pct)
  x.melt$variable<-gsub( "cpu_per_s_pct" , "DB CPU Avg" , x.melt$variable)
  x.melt$variable<-gsub( "os_cpu" , "OS CPU Avg" , x.melt$variable)
  # add 2 minutes to the DB CPU Avg rows to keep the lines for std dev from over-plotting.
  x.melt[with(x.melt, grepl("DB CPU Avg", variable)),]$end<-x.melt[with(x.melt, grepl("DB CPU Avg", variable)),]$end + minutes(2)


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
  
  DF_ANNOTATE_INT <- data.frame(x=quantile(x.melt$end,0.10),y=115,inst=min(x.melt$inst),
                                labs="Standard Deviation shown as vertical lines over points")
  
  p <- ggplot(data=x.melt, aes(x=end, y=value),aes(group=variable,color=variable)) +
    main$gg_hour_bars+
    geom_point(aes(group=variable,color=variable), size=2,shape=1,alpha=0.8)+
    geom_linerange(aes(ymin=value-1.96*sd,ymax=value+1.96*sd,color=variable), alpha=0.5,size=0.1)+
    attr$themeScaleColour+attr$themeScaleFill+
    stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
                aes(group=variable,color=variable,fill=variable))+
    ylab('CPU Percent')+
    ylim(0,115)+
    gg_maxvals_point+gg_maxvals_text+
    
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-45,size=1.5,hjust=-.5,alpha=0.2)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1)+
    facet_grid(inst ~ .)+
    theme(axis.title.x  = element_blank())+
    theme(panel.background = element_rect(colour = "#777777"))+
    xlim(min(x.melt$end),max(x.melt$end))+
    labs(title=paste("OS CPU % and Database CPU % for ",main$current_db_name,sep=""))+
    
    attr$vertical_line + attr$vertical_text +
    #scale_x_datetime(labels = date_format("%a, %b %d %I %p"),breaks = date_breaks("2 hour"),
    scale_x_datetime(labels = date_format("%a, %b %d %I %p"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))+
    main$gg_avg_max_fill+main$gg_avg_max_color+
    geom_text(data=DF_ANNOTATE_INT,aes(x=x,y=y,label=labs),size=2,alpha=.4)
  #annotate("text", x = median(x.melt$end), y = 105, label = "Standard Deviation shown as vertical lines over points",size=2,alpha=.4)
  # theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))
  #theme(panel.grid.major = element_line("#eeeeee", size = 0.2,linetype = "dotted"))+
  #theme(panel.grid.minor = element_line("#efefef", size = 0.1,linetype = "dotted"))
  #main$gg_bottom_panel
  
  #p
  p_gt <- ggplot_gtable(ggplot_build(p))
  flog.debug('plot_cpu - end',name='plot_cpu')
  #   p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  return(p_gt)
  #}
}

cpu_plot <- plot_cpu(main$DF_MAIN)
grid.newpage()
grid.draw(cpu_plot)