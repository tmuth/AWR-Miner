#plot_main_activity <- function(DF_MAIN_INT){
 # log_it('plot_main_activity - start')
  #if(dim(DF_MAIN_INT)[1] > 2){
  DF_MAIN_INT<-main$DF_MAIN
  
  DF_MAIN_INT2 <- ddply(DF_MAIN_INT, .(end), summarise, 
                        aas=sum(aas),
                        aas_max=sum(aas_max),
                        sql_res_t_cs=sum(sql_res_t_cs),
                        logons_s=sum(logons_s),
                        logons_total=sum(logons_total),
                        exec_s=sum(exec_s),
                        hard_p_s=sum(hard_p_s),
                        commits_s=sum(commits_s))
  #db_block_gets_s=sum(db_block_gets_s),
  #db_block_changes_s=sum(db_block_changes_s))
  
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end"), measure.var = c("aas", "aas_max","sql_res_t_cs", "logons_s",
                                                                  "logons_total","exec_s","hard_p_s","commits_s"))
  #"db_block_gets_s","db_block_changes_s"))
  
  x.melt$value <- round(x.melt$value,2)
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  x.melt[with(x.melt, grepl("aas_max", variable)),]$variable<-"AAS Max"
  x.melt[with(x.melt, grepl("aas", variable)),]$variable<-"AAS"
  x.melt[with(x.melt, grepl("sql_res_t_cs", variable)),]$variable<-"SQL Resp (cs)"
  x.melt[with(x.melt, grepl("logons_s", variable)),]$variable<-"Logons/s"
  x.melt[with(x.melt, grepl("logons_total", variable)),]$variable<-"Logons Total"
  x.melt[with(x.melt, grepl("exec_s", variable)),]$variable<-"Execs/s"
  x.melt[with(x.melt, grepl("commits_s", variable)),]$variable<-"Commits/s"
  x.melt[with(x.melt, grepl("hard_p_s", variable)),]$variable<-"Hard Parses/s"
  #x.melt[with(x.melt, grepl("db_block_gets_s", variable)),]$variable<-"Block Gets/s"
  #x.melt[with(x.melt, grepl("db_block_changes_s", variable)),]$variable<-"Block Changes/s"
  
  x.melt <- subset(x.melt,variable == 'SQL Resp (cs)')
  #x.melt <- subset(x.melt,value < 10)
  sess_quantile2 <- quantile(x.melt$value,probs=c(0.95),type=4)
  #x.melt <- subset(x.melt,value < sess_quantile2[[1]])
  
  
  x.melt$variable <- factor(x.melt$variable)
  
  x.melt <- data.frame(x.melt,stringsAsFactors =TRUE)
  DF_VAR_INT <- data.frame(unique(x.melt$variable))
  DF_SNAP_ID_SUBSET2 <- merge(DF_VAR_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(DF_SNAP_ID_SUBSET2$end),
                      variable = unique(x.melt$variable))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  # dummy value to adjust scales of aas to match aax_max
  
  
  x.melt <- na.omit(x.melt)
  
  last_level <- as.character(levels(x.melt$variable)[length(levels(x.melt$variable))])
  DF_SNAP_ID_SUBSET2$variable <- last_level
  
  DF_SNAP_ID_SUBSET2$variable <- factor(DF_SNAP_ID_SUBSET2$variable)
  
  if(nrow(subset(x.melt, variable=="AAS")) == 0){
    gg_aas_max_max <- opts()
  }  else {
    df_aas_max_max <- data.frame(end=min(x.melt$end),variable="AAS",value=max(subset(x.melt, variable == "AAS Max")$value),stringsAsFactors =TRUE)
    gg_aas_max_max <- geom_point(data=df_aas_max_max,aes(x=end,y=value),alpha=0)
  }
  
  max_vals <- ddply(x.melt, .(variable,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
  max_vals$label <- formatC(max_vals$value, digits=2,format="fg", big.mark=",")
  
  p <- ggplot() +
    geom_line(data=x.melt,aes(x=end, y=value,color=variable), size=.2)+
    # stat_smooth(data=x.melt,aes(x=end, y=value),method = "loess",n=300,size=.2,linetype="dashed",alpha=0.2)+
    geom_point(data=max_vals, aes(x=end, y=value, fill=variable), size=2, shape=21)+
    geom_text(data=max_vals, aes(x=end, y=value, color=variable,label=label),size=2.5, vjust=0.5, hjust=1.6)+
    ylab('')+
    facet_grid(variable ~ .,scales="free_y")+
    #scale_y_continuous(labels=comma,breaks = seq(min(x.melt$value), max(x.melt$value),by=(max(x.melt$value)/10)))+
    
    scale_y_log10(breaks = seq(min(x.melt$value), max(x.melt$value),by=(max(x.melt$value)/10)))+
    
    xlim(min(x.melt$end),max(x.melt$end))+
    theme(axis.title.x  = element_blank(),legend.position="none")+
    labs(title=paste("Main Activity Variables for ",main$current_db_name,sep=""))+
    geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.15,vjust=2.8,color="#bbbbbb",alpha=0.1)+
    geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0,color="#999999",alpha=0.1)+
    main$gg_hour_bars+
    gg_aas_max_max +
    attr$vertical_line + attr$vertical_text +
    scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = date_break_major_var,
                     minor_breaks = date_break_minor_var,
                     limits = c(min(x.melt$end),max(x.melt$end)))
  #scale_colour_tableau("colorblind10")+scale_fill_tableau("colorblind10")
  
  p
#   p_gt <- ggplot_gtable(ggplot_build(p))
#   p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
#   log_it('plot_main_activity - end')
  #grid.draw(p_gt)
#  return(p_gt)
  
  
#}