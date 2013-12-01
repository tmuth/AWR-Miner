save(main,file="main.Rda")
load("main.Rda")
mbrc <- as.numeric(subset(main$DF_DB_PARAMETERS,PARAMETER_NAME=='db_file_multiblock_read_count')$VALUE)


DF_MAIN_IOPS_BY_SNAP_INT <- ddply(main$DF_MAIN, .(end), summarise, 
                             #cpu=max(os_cpu_max),
                             read_iops=sum(read_iops),
                             read_bks=sum(read_bks),
                             read_bks_direct=sum(read_bks_direct),
                             read_iops_est=sum(read_bks)-sum(read_bks_direct),
                                  
                                  write_iops=sum(write_iops),
                                  write_bks=sum(write_bks),
                                  write_bks_direct=sum(write_bks_direct),
                                  write_iops_est=sum(write_bks)-sum(write_bks_direct)

                             #read_iops_max=sum(read_iops_max),
                             #read_iops_direct_max=sum(read_iops_direct_max),
                             #write_iops=sum(write_iops),
                             #write_iops_max=sum(write_iops_max),
                             #write_iops_direct=sum(write_iops_direct)
                                  
                             #write_iops_direct_max=sum(write_iops_direct_max),
                             #read_mb_s=sum(read_mb_s),
                             #read_mb_s_max=sum(read_mb_s_max),
                             #write_mb_s=sum(write_mb_s),
                             #write_mb_s_max=sum(write_mb_s_max),
                             #aas=sum(aas),
                             #logons_total=sum(logons_total),
                             #exec_s=sum(exec_s),
                             #sql_res_t_cs=sum(sql_res_t_cs)
)

DF_MAIN_IOPS_BY_SNAP_INT$read_iops_direct_est <- round(DF_MAIN_IOPS_BY_SNAP_INT$read_bks_direct/mbrc,1)
DF_MAIN_IOPS_BY_SNAP_INT$write_iops_direct_est <- round(DF_MAIN_IOPS_BY_SNAP_INT$write_bks_direct/mbrc,1)



DF_OUT <- data.frame()

plot_io <- function(DF_MAIN_BY_SNAP_INT){
  flog.debug('plot_io - start',name='plot_io')
  DF_MAIN_INT2 <- DF_MAIN_BY_SNAP_INT
  x.melt <- melt(DF_MAIN_INT2, id.var = c("end"), measure.var = c("read_iops", "read_iops_direct_est",
                                                                  "write_iops","write_iops_direct_est"
                                                                  #"read_iops_direct","read_iops_direct_max","write_iops_direct","write_iops_direct_max"
  ))
  # add a "stat" column so we can facet by stat for avg-max
  x.melt$stat <- "Avg"
  #x.melt$alphaSet <- .9  
  #idx_max <- with(x.melt, grepl("max", variable))
  #x.melt[idx_max,]$stat <- "Max"
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
  
  x.melt[with(x.melt, grepl("read_iops_direct_est", variable)),]$variable<-"Read IOPs Direct"
  #x.melt[with(x.melt, grepl("read_iops_est", variable)),]$variable<-"Read IOPs Est"
  x.melt[with(x.melt, grepl("^read_iops$", variable)),]$variable<-"Read IOPs"
  x.melt[with(x.melt, grepl("^write_iops$", variable)),]$variable<-"Write IOPs"
  x.melt[with(x.melt, grepl("^write_iops_direct_est$", variable)),]$variable<-"Write IOPs Direct"
  #x.melt[with(x.melt, grepl("write_iops_est", variable)),]$variable<-"Write IOPs Est"
  print(head(x.melt))
  print(unique(x.melt$variable))
  #x.melt[with(x.melt, grepl("read_mb_s", variable)),]$variable<-"Read MB/s"
  #x.melt[with(x.melt, grepl("write_mb_s", variable)),]$variable<-"Write MB/s"
  
  
  DF_VAR_INT <- data.frame(unique(x.melt$variable))
  DF_SNAP_ID_SUBSET2 <- merge(DF_VAR_INT,main$DF_SNAP_ID_SUBSET)
  
  vals <- expand.grid(end = unique(DF_SNAP_ID_SUBSET2$end),
                      variable = unique(x.melt$variable))
  
  DF_SNAP_ID_SUBSET2 <- merge(vals,main$DF_SNAP_ID_SUBSET)
  
  DF_SNAP_ID_SUBSET3 <- main$DF_SNAP_ID_SUBSET
  x.melt$variable <- factor(x.melt$variable)
  #DF_AAS_INT2$WAIT_CLASS <- factor(DF_AAS_INT2$WAIT_CLASS)
  last_level <- as.character(levels(x.melt$variable)[length(levels(x.melt$variable))])
  DF_SNAP_ID_SUBSET3$variable <- last_level
  
  # get the max vals for each day
  
  
  
  plot_io_int <- function(df_in){
    
    
    
    max_vals <- ddply(df_in, .(variable,stat,format(end,"%y/%m/%d")), subset, subset = rank(-value) <= 1)
    max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")
    
 
    
    # create a "fake" data.frame(), flip the variable names, use to make scales the same between like variables
    max_vals_fake <- max_vals
    max_vals_fake <- transform(max_vals_fake, variable = as.character(variable)) # can't replace factors
    max_vals_fake[with(max_vals_fake, grepl("^Read IOPs$", variable)),]$variable<-"Read IOPs-old"   
    max_vals_fake[with(max_vals_fake, grepl("^Read IOPs Direct$", variable)),]$variable<-"Read IOPs"
    max_vals_fake[with(max_vals_fake, grepl("^Read IOPs-old$", variable)),]$variable<-"Read IOPs Direct"
        
    max_vals_fake[with(max_vals_fake, grepl("^Write IOPs$", variable)),]$variable<-"Write IOPs-old"
    max_vals_fake[with(max_vals_fake, grepl("^Write IOPs Direct$", variable)),]$variable<-"Write IOPs"
    max_vals_fake[with(max_vals_fake, grepl("^Write IOPs-old$", variable)),]$variable<-"Write IOPs Direct"
    
    
    plot_int <- ggplot(data=df_in, aes(x=end, y=value),aes(color=stat)) +
      geom_line(aes(color=stat), size=.2)+
      #stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
      #      aes(color=stat,fill=stat))+
      
      attr$themeScaleColour+attr$themeScaleFill+
      main$gg_avg_max_fill+main$gg_avg_max_color+
      geom_point(data=max_vals, aes(x=end, y=value, fill=stat), size=2, shape=21)+
      geom_point(data=max_vals_fake, aes(x=end, y=value, fill=stat), size=2, shape=21,alpha=0)+
      geom_text(data=max_vals, aes(x=end, y=value, color=stat,label=label),size=2.5, vjust=0.5, hjust=1.25,position = position_jitter(width = .2,height=0))+
      geom_text(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.1,vjust=0.7)+
      geom_point(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0),size=1)+
      ylab('')+
      
      facet_grid(variable ~ .,scales="free_y")+
      scale_y_continuous(labels=comma)+
      xlim(min(x.melt$end),max(x.melt$end))+
      labs(title=paste("IO Avg by Type for ",main$current_db_name,sep=""))+
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
  
  p <- plot_io_int(subset(x.melt,variable %in% c("Read IOPs Direct *","Write IOPs Direct *","Read IOPs","Write IOPs","Read MB/s","Write MB/s","Read IOPs Direct","Write IOPs Direct")))
  print(p)
  p_gt <- ggplot_gtable(ggplot_build(p))
  p_gt$layout$clip[p_gt$layout$name=="panel"] <- "off"
  
  
  
  flog.debug('plot_io - end',name='plot_io')
  return(p_gt)
  #return(list(p_gt,io_hist_area_plot))
  #}
}

p <- plot_io(DF_MAIN_IOPS_BY_SNAP_INT)
print(p)
pdf(paste(main$current_db_name,"-iops.pdf",sep=""), width = 11, height = 8.5,useDingbats=FALSE)
grid.draw(p)


DF_MAIN_IOPS_BY_SNAP_INT

DF_MAIN_IOPS_BY_SNAP_INT$read_iops_indirect <- DF_MAIN_IOPS_BY_SNAP_INT$read_iops - DF_MAIN_IOPS_BY_SNAP_INT$read_iops_direct_est 
DF_MAIN_IOPS_BY_SNAP_INT$write_iops_indirect <- DF_MAIN_IOPS_BY_SNAP_INT$write_iops - DF_MAIN_IOPS_BY_SNAP_INT$write_iops_direct_est 


DF_MAIN_IOPS_BY_SNAP_INT$read_iops_indirect_pct <- round(DF_MAIN_IOPS_BY_SNAP_INT$read_iops_indirect/DF_MAIN_IOPS_BY_SNAP_INT$read_iops,2)
DF_MAIN_IOPS_BY_SNAP_INT$write_iops_indirect_pct <- round(DF_MAIN_IOPS_BY_SNAP_INT$write_iops_indirect/DF_MAIN_IOPS_BY_SNAP_INT$write_iops,2)

DF_MAIN_IOPS_BY_SNAP_INT <- subset(DF_MAIN_IOPS_BY_SNAP_INT,read_iops_indirect > 0 & write_iops_indirect > 0)




DF_MAIN_IOPS_BY_SNAP_INT.melt <- melt(DF_MAIN_IOPS_BY_SNAP_INT, id.var = c("end"), measure.var = c("read_iops_indirect_pct", "write_iops_indirect_pct"
                                                                #"read_iops_direct","read_iops_direct_max","write_iops_direct","write_iops_direct_max"
))


DF_MAIN_IOPS_BY_SNAP_INT.melt <- transform(DF_MAIN_IOPS_BY_SNAP_INT.melt, variable = as.character(variable)) # can't replace factors
DF_MAIN_IOPS_BY_SNAP_INT.melt[with(DF_MAIN_IOPS_BY_SNAP_INT.melt, grepl("^read_iops_indirect_pct$", variable)),]$variable<-"Read IOPs Flash %"   
DF_MAIN_IOPS_BY_SNAP_INT.melt[with(DF_MAIN_IOPS_BY_SNAP_INT.melt, grepl("^write_iops_indirect_pct$", variable)),]$variable<-"Write IOPs Flash %"   



ggplot(data=DF_MAIN_IOPS_BY_SNAP_INT.melt, aes(x=end, y=value)) +
  geom_line(aes(), size=.2,color="#3a6185")+
  #stat_smooth(method = "loess",n=300,size=.2,alpha=.1,linetype="dashed",
  #      aes(color=stat,fill=stat))+
  
  attr$themeScaleColour+attr$themeScaleFill+
  main$gg_avg_max_fill+main$gg_avg_max_color+
  #geom_point(data=max_vals, aes(x=end, y=value, fill=stat), size=2, shape=21)+
  #geom_point(data=max_vals_fake, aes(x=end, y=value, fill=stat), size=2, shape=21,alpha=0)+
  #geom_text(data=max_vals, aes(x=end, y=value, color=stat,label=label),size=2.5, vjust=0.5, hjust=1.25,position = position_jitter(width = .2,height=0))+
  #geom_text(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.1,vjust=0.7)+
  #geom_point(data=DF_SNAP_ID_SUBSET3,aes(x=end,y=0),size=1)+
  ylab('')+
  
  facet_grid(variable ~ .,scales="free_y")+
  scale_y_continuous(labels = percent_format())+
  xlim(min(DF_MAIN_IOPS_BY_SNAP_INT.melt$end),max(DF_MAIN_IOPS_BY_SNAP_INT.melt$end))+
  labs(title=paste("Flash IOPs Percentage for ",main$current_db_name,sep=""))+
  main$gg_hour_bars+
  attr$vertical_line + attr$vertical_text +
  
  scale_x_datetime(labels = date_format("%a, %b %d %I%p"),breaks = attr$date_break_major_var,
                   minor_breaks = attr$date_break_minor_var,
                   limits = c(min(DF_MAIN_IOPS_BY_SNAP_INT.melt$end),max(DF_MAIN_IOPS_BY_SNAP_INT.melt$end)))+
  scale_alpha(guide = 'none')+
  theme(legend.key.size = unit(.25, "cm"),
        strip.text.y = element_text(size = 6)
  )


ggplot(data=DF_MAIN_IOPS_BY_SNAP_INT.melt, aes(x=value)) +
  geom_histogram(color="#20364a",fill="#3a6185")+
  labs(title=paste("Flash IOPs Histogram for ",main$current_db_name,sep=""))+
  scale_x_continuous(labels = percent_format())+
  facet_grid(variable ~ .)

dev.off()
