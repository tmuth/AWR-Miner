

DF_IOSTAT_FUNCTION_INT <- main$DF_IOSTAT_FUNCTION
# multiply writes by 2
DF_IOSTAT_FUNCTION_INT$SM_W_REQS <- 2*DF_IOSTAT_FUNCTION_INT$SM_W_REQS
DF_IOSTAT_FUNCTION_INT$LG_W_REQS <- 2*DF_IOSTAT_FUNCTION_INT$LG_W_REQS
iostat.melt <- melt(DF_IOSTAT_FUNCTION_INT,id.var = c("SNAP_ID","FUNCTION_NAME"),measure.var = c("SM_R_REQS","SM_W_REQS", "LG_R_REQS", "LG_W_REQS"))
expanded_vals <- expand.grid(SNAP_ID = unique(main$DF_IOSTAT_FUNCTION$SNAP_ID),
                             FUNCTION_NAME = unique(main$DF_IOSTAT_FUNCTION$FUNCTION_NAME))

iostat.melt <- merge(iostat.melt,expanded_vals)
iostat2.melt<- merge(iostat.melt,main$DF_SNAP_ID_DATE,by="SNAP_ID")
iostat2.melt <- transform(iostat2.melt, variable = as.character(variable))
iostat2.melt[with(iostat2.melt, grepl("SM_R_REQS", variable)),]$variable<-"Small Read IOPs"
iostat2.melt[with(iostat2.melt, grepl("SM_W_REQS", variable)),]$variable<-"Small Write IOPs *"
iostat2.melt[with(iostat2.melt, grepl("LG_R_REQS", variable)),]$variable<-"Large Read IOPs"
iostat2.melt[with(iostat2.melt, grepl("LG_W_REQS", variable)),]$variable<-"Large Write IOPs *"

#convert absolute values to per-second values
iostat2.melt$value_per_s <- round(iostat2.melt$value / (iostat2.melt$dur_m*60),0)

# find the FUNCTIONS for which we have no data and remove them
iostat2.totals <- ddply(iostat2.melt, .(FUNCTION_NAME), summarise, 
      value=sum(as.numeric(value)))
idx_iostat_rm <- !with(iostat2.melt, FUNCTION_NAME %in% subset(iostat2.totals,value==0)$FUNCTION_NAME)
iostat2.melt<- iostat2.melt[idx_iostat_rm,]

#create fake max points to normalize the scale of small reads with writes, and the same for large ...
iostat2.agg1 <- ddply(iostat2.melt, .(SNAP_ID,end,variable), summarise, value_per_s=sum(as.numeric(value_per_s)))
iostat2.agg1.max <- ddply(iostat2.agg1, .(variable), summarise, value_per_s=max(as.numeric(value_per_s)))
iostat2.agg1.max$value_per_s <- iostat2.agg1.max$value_per_s + (iostat2.agg1.max$value_per_s*0.05) # add 5% padding
#rm(iostat2.agg1)
iostat2.agg1.max.tmp <- iostat2.agg1.max
# Flip the names with each other
iostat2.agg1.max$variable<-paste0(iostat2.agg1.max$variable,"_TMP")
iostat2.agg1.max$variable<-gsub( "Small Read IOPs_TMP" , "Small Write IOPs *" , iostat2.agg1.max$variable)
iostat2.agg1.max$variable<-gsub( "Small Write IOPs \\*_TMP" , "Small Read IOPs" , iostat2.agg1.max$variable)
iostat2.agg1.max$variable<-gsub( "Large Read IOPs_TMP" , "Large Write IOPs *" , iostat2.agg1.max$variable)
iostat2.agg1.max$variable<-gsub( "Large Write IOPs \\*_TMP" , "Large Read IOPs" , iostat2.agg1.max$variable)

iostat2.agg1.max <- rbind(iostat2.agg1.max,iostat2.agg1.max.tmp )
iostat2.agg1.max$end <- min(iostat2.melt$end)


max_vals <- ddply(iostat2.agg1, .(variable,end=format(end,"%y/%m/%d")), subset, subset = rank(-value_per_s) <= 1)
max_vals$label <- formatC(max_vals$value, format="d", big.mark=",")




#http://docs.oracle.com/cd/E24628_01/server.121/e17635/tdppt_realtime.htm#CBBEFAAC   
iostat_colors <- c("ARCH" = "#cc6617", "Archive Manager" = "#9ccecc", "Buffer Cache Reads" = "#0133ff", "Data Pump" = "#747254",
                "DBWR" = "#993309","Direct Reads" = "#00cc2e","Direct Writes" = "#9aff9a","LGWR" = "#cc330c",
                "Others" = "#ff6699","Recovery" = "#5c460c","RMAN" = "#fcfe84",
                "Smart Scan" = "#800040","Streams AQ" = "#9c9274","XDB" = "#c4b69c")


gg_iostat_colors <- scale_fill_manual("", values = iostat_colors)


ggplot()+
  geom_area(data=iostat2.melt, aes(x = end, y = value_per_s,
                                 fill = FUNCTION_NAME),stat = "identity", position = "stack",alpha=.95)+
  geom_point(data=max_vals, aes(x=end, y=value_per_s), size=2, shape=21)+
  geom_text(data=max_vals, aes(x=end, y=value_per_s,label=label),size=2, vjust=-.5, hjust=1)+
  gg_iostat_colors+
  geom_point(data=iostat2.agg1.max,aes(x=end,y=value_per_s),alpha=0)+
  theme(
    strip.text.y = element_text(size = 7),
    legend.key.size = unit(.25, "cm")
  )+
  scale_x_datetime(labels = date_format("%a, %b %d %I %p"),breaks = attr$date_break_major_var,
                   minor_breaks = attr$date_break_minor_var,
                   limits = c(min(iostat2.melt$end),max(iostat2.melt$end)))+
  facet_grid(variable ~ . ,scales="free_y")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank() )