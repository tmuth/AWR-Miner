DF_OSSTAT_INT <- main$DF_OSSTAT
DF_MAIN_CPU_INT <- main$DF_MAIN %>%
  select(SNAP_ID=snap,INSTANCE_NUMBER=inst,os_cpu) %>%
  mutate(os_cpu = (os_cpu/100))

DF_OSSTAT_INT2 <- left_join(DF_OSSTAT_INT,tbl_dt(DF_MAIN_CPU_INT),copy = TRUE,by=c("SNAP_ID","INSTANCE_NUMBER"))

DF_OSSTAT_INT2 <- left_join(DF_OSSTAT_INT2,tbl_dt(main$DF_SNAP_ID_DATE),copy = TRUE,by=c("SNAP_ID"))


# DF_TOP_N_AGG1 <- main$DF_TOP_N_EVENTS %>%
#   group_by(WAIT_CLASS,EVENT_NAME) %>%
#   summarise(TOTAL_TIME_S = sum(as.numeric(TOTAL_TIME_S))) %>%
#   ungroup() %>%
#   arrange(desc(TOTAL_TIME_S)) %>%
#   head(25) %>%
#   arrange(WAIT_CLASS)

if(!data_frame_col_not_null(DF_OSSTAT_INT2,"cpu_count")){
  DF_OSSTAT_INT2$cpu_count <- NA
}

DF_OS_LOAD.melt <- melt(DF_OSSTAT_INT2, id.var = c("end","INSTANCE_NUMBER"), measure.var = c("load", "cpus","cpu_count"))

DF_OS_LOAD.melt$dynLineSize <- as.numeric(ifelse(DF_OS_LOAD.melt$variable=="cpu_count",.8,1))
DF_OS_LOAD.melt$dynLinetype  <- ifelse(DF_OS_LOAD.melt$variable=="cpu_count","dotted","solid")
DF_OS_LOAD.melt$dynLinetype  <- ifelse(DF_OS_LOAD.melt$variable=="cpus","twodash",DF_OS_LOAD.melt$dynLinetype)



#DF_OS_LOAD.melt$end  <- ifelse(DF_OS_LOAD.melt$variable=="cpus",DF_OS_LOAD.melt$end+600,DF_OS_LOAD.melt$end)

DF_OS_LOAD.melt$value  <- ifelse(DF_OS_LOAD.melt$variable=="cpu_count",(DF_OS_LOAD.melt$value+(DF_OS_LOAD.melt$value * 0.01)),DF_OS_LOAD.melt$value)


DF_OS_LOAD.melt$variable<-gsub( "^cpus$" , "CPUs (threads)" , DF_OS_LOAD.melt$variable)
DF_OS_LOAD.melt$variable<-gsub( "^cpu_count$" , "cpu_count (DB rsrc mgr)" , DF_OS_LOAD.melt$variable)
#DF_OS_LOAD.melt$linetype <- "solid"




if(data_frame_col_not_null(DF_OSSTAT_INT2,"vm_in")){
  
    DF_OS_LOAD.swap<- melt(DF_OSSTAT_INT2, id.var = c("end","INSTANCE_NUMBER"), measure.var = c("vm_in","vm_out"))
    DF_OS_LOAD.swap$value <- round(DF_OS_LOAD.swap$value/1024/1024)

}


outFileName <- paste(main$current_db_name,min(main$DF_MAIN$snap),max(main$DF_MAIN$snap),outFileSuffix,sep='-')
outFileName <- paste0(outFileName,'-osstat.pdf')
pdf(outFileName, width = 11, height = 8.5,useDingbats=FALSE)

p <- ggplot() +
  geom_line(data=DF_OS_LOAD.melt,aes(x=end, y=value,color=variable,linetype=dynLinetype,size=(dynLineSize)),alpha=0.8)+
  scale_linetype_identity() +
  scale_size_identity(guide=FALSE)+
  #geom_line(data=subset(DF_OS_LOAD.melt,variable == 'cpus'),aes(x=end, y=value+(value*.1)),linetype="dotted",color="#00ff00",alpha=0.9,size=)+
  attr$themeScaleColour+attr$themeScaleFill+
  facet_grid(INSTANCE_NUMBER ~ .)+
  ylab('')+
  #scale_y_discrete()+
  #scale_y_continuous(breaks=breaks=pretty_breaks(n=10))+
  scale_y_continuous(breaks=(breaks=pretty(DF_OS_LOAD.melt$value, n=10)))+
  main$gg_hour_bars+
  attr$vertical_line + attr$vertical_text +
  theme(
    strip.text.y = element_text(size = 6)
  )+
  scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),
                   breaks = attr$date_break_major_var,
                   minor_breaks = attr$date_break_minor_var,
                   limits = c(min(DF_OS_LOAD.melt$end),max(DF_OS_LOAD.melt$end))
  )+
  labs(title=paste("CPU Load by Instance for ",main$current_db_name,sep=""))

p


if(data_frame_col_not_null(DF_OSSTAT_INT2,"vm_in")){
  p <- ggplot() +
    geom_line(data=DF_OS_LOAD.swap,aes(x=end, y=value,color=variable),alpha=0.8)+
    #scale_linetype_identity() +
    scale_size_identity(guide=FALSE)+
    #geom_line(data=subset(DF_OS_LOAD.melt,variable == 'cpus'),aes(x=end, y=value+(value*.1)),linetype="dotted",color="#00ff00",alpha=0.9,size=)+
    attr$themeScaleColour+attr$themeScaleFill+
    facet_grid(INSTANCE_NUMBER ~ .)+
    ylab('MB')+
    #scale_y_discrete()+
    #scale_y_continuous(breaks=breaks=pretty_breaks(n=10))+
    #scale_y_continuous(breaks=(breaks=pretty(DF_OS_LOAD.melt$value, n=10)))+
    main$gg_hour_bars+
    attr$vertical_line + attr$vertical_text +
    theme(
      strip.text.y = element_text(size = 6)
    )+
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),
                     breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var,
                     limits = c(min(DF_OS_LOAD.melt$end),max(DF_OS_LOAD.melt$end))
    )+
    labs(title=paste("Swap MB In/Out by Instance for ",main$current_db_name,sep=""))
  
  p
}

dev.off()

# melt it to tall format to diff the snapshots
DF_OS_CPU.melt <- melt(DF_OSSTAT_INT2, id.var = c("end","INSTANCE_NUMBER","os_cpu"), 
                       #measure.var = c("busy","user","sys","cpu_wait" ))
                       measure.var = c("idle","busy","user","sys","cpu_wait","iowait"))

DF_OS_CPU.melt.diff <- DF_OS_CPU.melt %>%
  arrange(variable,INSTANCE_NUMBER,end) %>%
  group_by(variable,INSTANCE_NUMBER) %>%
  mutate(val2 = value - lag(value)) %>%
  filter(!is.na(val2)) %>%
  filter(val2 > 0) %>%
  ungroup()

DF_OS_CPU.melt.diff <- data.table(DF_OS_CPU.melt.diff)
# cast it to wide format to compute some of the aggregates
DF_OS_CPU.cast <- cast(DF_OS_CPU.melt.diff,end + INSTANCE_NUMBER + os_cpu ~ variable,value.var=val2)

DF_OS_CPU.cast$idle.busy <- DF_OS_CPU.cast$idle + DF_OS_CPU.cast$busy
DF_OS_CPU.cast$idle.busy.wait <- DF_OS_CPU.cast$idle + DF_OS_CPU.cast$busy+DF_OS_CPU.cast$cpu_wait
DF_OS_CPU.cast$idle.busy.iowait <- DF_OS_CPU.cast$idle + DF_OS_CPU.cast$busy + DF_OS_CPU.cast$iowait

DF_OS_CPU.cast$user.sys <- DF_OS_CPU.cast$user + DF_OS_CPU.cast$sys
DF_OS_CPU.cast$busy.pct <- round(DF_OS_CPU.cast$busy / (DF_OS_CPU.cast$idle.busy.iowait),3)


DF_OS_CPU.cast$pct.sys <- round(DF_OS_CPU.cast$os_cpu * (DF_OS_CPU.cast$sys / (DF_OS_CPU.cast$user+DF_OS_CPU.cast$sys)),3)
DF_OS_CPU.cast$pct.user <- DF_OS_CPU.cast$os_cpu - DF_OS_CPU.cast$pct.sys 



DF_OS_CPU.melt2 <- melt(as.data.frame(DF_OS_CPU.cast), id.var = c("end","INSTANCE_NUMBER"), 
                       measure.var = c("pct.sys","pct.user"))

#DF_OS_CPU.cast$pct.user <- round(DF_OS_CPU.cast$os_cpu * (DF_OS_CPU.cast$user / (DF_OS_CPU.cast$user+DF_OS_CPU.cast$sys)),3)

#DF_OS_CPU.cast$busy.pct <- round(DF_OS_CPU.cast$busy / DF_OS_CPU.cast$idle.busy,2)


#sDF_OS_CPU.melt.diff$end <- factor(DF_OS_CPU.melt.diff$end)
#DF_OS_CPU.melt.diff$INSTANCE_NUMBER <- factor(DF_OS_CPU.melt.diff$INSTANCE_NUMBER)


p <- ggplot() +
  main$gg_hour_bars+
  geom_line(data=DF_OS_CPU.melt.diff,aes(x=end, y=val2,color=variable), size=2)+
  attr$themeScaleColour+attr$themeScaleFill+
  facet_grid(INSTANCE_NUMBER ~ .,scales="free_y")+
  ylab('')+
  scale_y_continuous()+
  attr$vertical_line + attr$vertical_text +
  theme(
    strip.text.y = element_text(size = 12)
  )+
  scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),
                   breaks = attr$date_break_major_var,
                   minor_breaks = attr$date_break_minor_var,
                   limits = c(min(DF_OS_CPU.melt.diff$end),max(DF_OS_CPU.melt.diff$end))
  )+
  labs(title=paste("CPU Load by Instance for ",main$current_db_name,sep=""))

p




p <- ggplot() +
  main$gg_hour_bars+
  geom_line(data=DF_OS_CPU.melt.diff,aes(x=end, y=val2,color=variable), size=.5)+
  attr$themeScaleColour+attr$themeScaleFill+
  facet_grid(INSTANCE_NUMBER ~ .)
p





p <- ggplot() +
  main$gg_hour_bars+
  geom_area(data=DF_OS_CPU.melt2,aes(x=end, y=value,fill=variable), size=.5,position="stack")+
  attr$themeScaleColour+attr$themeScaleFill+
  scale_y_continuous(labels = percent_format())+
  facet_grid(INSTANCE_NUMBER ~ .)
p




p <- ggplot() +
  geom_line(data=DF_OS_LOAD.melt,aes(x=end, y=value,color=variable), size=.2)+
  attr$themeScaleColour+attr$themeScaleFill+
  # stat_smooth(data=x.melt,aes(x=end, y=value),method = "loess",n=300,size=.2,linetype="dashed",alpha=0.2)+
  geom_point(data=max_vals, aes(x=end, y=value, fill=variable), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=value, color=variable,label=label),size=2.5, vjust=0.5, hjust=1.6)+
  ylab('')+
  facet_grid(variable ~ .,scales="free_y")+
  scale_y_continuous(labels=comma)+
  xlim(min(x.melt$end),max(x.melt$end))+
  theme(axis.title.x  = element_blank(),legend.position="none")+
  labs(title=paste("Main Activity Variables for ",main$current_db_name,sep=""))+
  geom_text(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0,label=SNAP_ID),angle=-20,size=1.5,hjust=-0.15,vjust=2.8,color="#bbbbbb",alpha=0.1)+
  geom_point(data=DF_SNAP_ID_SUBSET2,aes(x=end,y=0),alpha=0.2,size=1,vjust=1,hjust=0,color="#999999",alpha=0.1)+
  main$gg_hour_bars+
  gg_aas_max_max +
  attr$vertical_line + attr$vertical_text +
  theme(
    strip.text.y = element_text(size = 6)
  )+
  scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),
                   breaks = attr$date_break_major_var,
                   minor_breaks = attr$date_break_minor_var,
                   limits = c(min(x.melt$end),max(x.melt$end))
  )