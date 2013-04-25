#plot_summary_boxplot_main <- function(){
  log_it.debug('plot_summary_boxplot_main - start')
  x.melt <- melt(main$DF_MAIN_BY_SNAP, id.var = c("end"), measure.var = c("cpu","read_iops_max", "write_iops_max",
                                                                          "read_mb_s_max","write_mb_s_max","aas","logons_total","exec_s","sql_res_t_cs"))
  # add a "stat" column so we can facet by stat for avg-max
  #   x.melt$stat <- "Avg"  
  #   idx_max <- with(x.melt, grepl("max", variable))
  #   x.melt[idx_max,]$stat <- "Max"
  
  x.melt$value <- round(x.melt$value,2)
  # We need to change these names and they are "factors" which we can't change
  x.melt <- transform(x.melt, variable = as.character(variable))
  
  x.melt[with(x.melt, grepl("cpu", variable)),]$variable<-"CPU"
  x.melt[with(x.melt, grepl("read_iops", variable)),]$variable<-"Read IOPs"
  x.melt[with(x.melt, grepl("write_iops", variable)),]$variable<-"Write IOPs"
  x.melt[with(x.melt, grepl("read_mb_s", variable)),]$variable<-"Read MB/s"
  x.melt[with(x.melt, grepl("write_mb_s", variable)),]$variable<-"Write MB/s"
  x.melt[with(x.melt, grepl("aas", variable)),]$variable<-"Avg Act Sessions"
  x.melt[with(x.melt, grepl("logons_total", variable)),]$variable<-"Sessions"
  x.melt[with(x.melt, grepl("exec_s", variable)),]$variable<-"Execs/s"
  x.melt[with(x.melt, grepl("sql_res_t_cs", variable)),]$variable<-"SQL Resp Time (cs)"
  x.melt$variable <- factor(x.melt$variable)
  x.melt$id <- 1
  
  median_vals <- ddply(x.melt,.(variable),summarise, value=median(value))
  mean_vals <- ddply(x.melt,.(variable),summarise,value=mean(value))
  quant_low <- ddply(x.melt,.(variable),summarise,value=quantile(value,0.25,na.rm=TRUE))
  quant_high <- ddply(x.melt,.(variable),summarise,value=quantile(value,0.75,na.rm=TRUE))
  
  summary_vals <- rbind(quant_low,quant_high)
  summary_vals$id <- 1
  mean_vals$id <- 1
  median_vals$id <- 1
  
  
  #print(summary(x.melt))
  #print(head(x.melt))
  
  
  p <- ggplot(data=x.melt, aes(x=id, y=value),aes(fill=variable),position="dodge")+
    geom_violin(aes(),fill="#4DAF4A",colour="#000000",size=0.05,alpha=0.6,adjust=0.5) +
    geom_boxplot(aes(),colour="#000000",alpha=.6,show_guide=FALSE,notch = FALSE,outlier.colour = "orange", outlier.size = 2,outlier.alpha=.5,outlier.shape=5)+
    geom_jitter(alpha=.3,size=1,position = position_jitter(width = .2,height=0),aes(colour="gray"))+
    geom_text(data=median_vals,aes(y=value,label=round(value,1)),alpha=0.8,size=2,vjust=-0.8,hjust=0)+
    geom_text(data=summary_vals,aes(y=value,label=round(value,1)),alpha=0.8,size=2,vjust=-0.8,hjust=0,colour="#666666")+
    geom_text(data=mean_vals,aes(y=value,label=round(value,1)),alpha=0.8,size=2,vjust=-0.8,hjust=1.2,colour="#D62728")+
    facet_wrap( ~ variable,scales="free",nrow=1)+
    stat_summary(fun.y=mean, geom="point", shape=5, size=2,alpha=0.7,colour="#D62728")+
    #scale_colour_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")+
    #scale_y_continuous(breaks=seq(0, 8000, 500),minor=seq(0, 8000, 100))+
    theme(text =               element_text(size=5),
          axis.title.x  = element_blank(),axis.title.y  = element_blank(),
          legend.position="none",axis.text.x= element_blank(),
          plot.background = element_rect(fill = "#8EB3BD"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(colour="#436974"),axis.text.y = element_text(colour="#436974"),
          strip.background = element_rect(colour="#436974", linetype=1),
          line = element_line(colour = "#436974", linetype=1))
  
  
  #print(p)
  log_it.debug('plot_summary_boxplot_main - end')
 # return(p)
#}