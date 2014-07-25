options(scipen=999)
DF_MEMORY_SGA_ADVICE_TMP <- main$DF_MEMORY_SGA_ADVICE


DF_MEMORY_SGA_ADVICE_1_TMP <- subset(DF_MEMORY_SGA_ADVICE_TMP,SIZE_FACTOR==1)
DF_MEMORY_SGA_ADVICE_1_TMP <- DF_MEMORY_SGA_ADVICE_1_TMP[c("end","SNAP_ID","INSTANCE_NUMBER","ESTD_PHYSICAL_READS")]


setnames(DF_MEMORY_SGA_ADVICE_1_TMP, "ESTD_PHYSICAL_READS", "CURRENT_PHYSICAL_READS")

DF_MEMORY_SGA_ADVICE_TMP <- merge(DF_MEMORY_SGA_ADVICE_TMP,DF_MEMORY_SGA_ADVICE_1_TMP,by=c('SNAP_ID','INSTANCE_NUMBER'))
rm(DF_MEMORY_SGA_ADVICE_1_TMP)
DF_MEMORY_SGA_ADVICE_TMP$pct_change <- (DF_MEMORY_SGA_ADVICE_TMP$ESTD_PHYSICAL_READS-DF_MEMORY_SGA_ADVICE_TMP$CURRENT_PHYSICAL_READS)/DF_MEMORY_SGA_ADVICE_TMP$CURRENT_PHYSICAL_READS


DF_MEMORY_SGA_ADVICE_TMP_AGG <- ddply(DF_MEMORY_SGA_ADVICE_TMP,.(INSTANCE_NUMBER,SGA_TARGET_GB),summarise,
                                      pct_change=mean(pct_change),
                                      SIZE_FACTOR=mean(SIZE_FACTOR))

DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$SIZE_FACTOR==1,"Current Value","Minor Change")
DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change>=0.05,"Increased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP_AGG$color)
DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change<=-0.05,"Decreased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP_AGG$color)



sga_colors <- c("Current Value" = "#000000", "Minor Change" = "#777777", 
                "Increased Physical Reads" = "#a60000",
                "Decreased Physical Reads" = "#008000")

gg_sga_colors <- scale_colour_manual("", values = sga_colors)
gg_sga_fill<- scale_fill_manual("", values = sga_colors)


ggplot(data=DF_MEMORY_SGA_ADVICE_TMP_AGG,aes(x=SGA_TARGET_GB,y=pct_change*100,colour=color,fill=color))+
  geom_bar(stat="identity",alpha=.2)+
  geom_text(aes(label=paste(round(pct_change * 100, 1), "%", sep = "")),size=2,colour="black")+
  facet_grid(INSTANCE_NUMBER ~ . )+
  gg_sga_colors+gg_sga_fill+
  labs(title=paste("Average Percent Change in Physical Reads Relative To SGA_TARGET from SGA Target Advisory for ",main$current_db_name,sep=""))+
  xlab("SGA_TARGET (GB)")+
  ylab("Percent Change in Physical Reads (Negative is better)")














DF_MEMORY_SGA_ADVICE_TMP$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP$SIZE_FACTOR==1,"Current Value","Minor Change")
DF_MEMORY_SGA_ADVICE_TMP$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP$pct_change>=0.05,"Increased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP$color)
DF_MEMORY_SGA_ADVICE_TMP$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP$pct_change<=-0.05,"Decreased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP$color)
DF_MEMORY_SGA_ADVICE_TMP2 <- merge(DF_MEMORY_SGA_ADVICE_TMP,main$DF_SNAP_ID_SUBSET,by=c("SNAP_ID"))
DF_SNAP_ID_SUBSET2 <- merge(DF_VAR_INT,main$DF_SNAP_ID_SUBSET)

sga_colors <- c("Current Value" = "#000000", "Minor Change" = "#777777", 
                "Increased Physical Reads" = "#a60000",
                "Decreased Physical Reads" = "#008000")

gg_sga_colors <- scale_colour_manual("", values = sga_colors)


DF_MEMORY_SGA_ADVICE_TMP$SGA_TARGET_GB <- factor(DF_MEMORY_SGA_ADVICE_TMP$SGA_TARGET_GB)
DF_MEMORY_SGA_ADVICE_TMP$color <- factor(DF_MEMORY_SGA_ADVICE_TMP$color)

ggplot(data=DF_MEMORY_SGA_ADVICE_TMP,aes(x=SNAP_ID,y=ESTD_PHYSICAL_READS,group=SGA_TARGET_GB))+
  geom_point(aes(color=SGA_TARGET_GB),position = position_jitter(width = .2,height=0))

ggplot(data=DF_MEMORY_SGA_ADVICE_TMP,aes(x=SNAP_ID,y=SGA_TARGET_GB,group=SGA_TARGET_GB))+
  geom_point(aes(color=SGA_TARGET_GB),position = position_jitter(width = .2,height=0))

ggplot(data=DF_MEMORY_SGA_ADVICE_TMP,aes(x=SNAP_ID,y=SGA_TARGET_GB,colour=color))+
  #geom_point(aes(),position = position_jitter(width = .2,height=0))+
  geom_point(aes(),size=1)+
  geom_text(aes(label=paste(round(pct_change * 100, 1), "%", sep = "")),angle=45,size=3, vjust=-.2, hjust=-.2)+
  facet_grid(INSTANCE_NUMBER ~ . )+
  gg_sga_colors+
  labs(title=paste("Percent Change in Physical Reads from SGA Target Advisory for ",main$current_db_name,sep=""))+
  ylab("SGA_TARGET (GB)")+
  xlab("Snapshot ID")

ggplot(data=DF_MEMORY_SGA_ADVICE_TMP,aes(x=SGA_TARGET_GB,y=pct_change))+
geom_histogram()+
facet_grid(INSTANCE_NUMBER ~ . )


p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
                                                    +     colour = "white") + scale_fill_gradient(low = "white",
                                                                                                  +     high = "steelblue"))
DF_MEMORY_SGA_ADVICE_TMP$SGA_TARGET_GB2 <- as.numeric(DF_MEMORY_SGA_ADVICE_TMP$SGA_TARGET_GB)

DF_MEMORY_SGA_ADVICE_TMP$pct_change2 <- rescale(DF_MEMORY_SGA_ADVICE_TMP$pct_change)

nba.s <- ddply(nba.m, .(variable), transform,
               +     rescale = scale(value))

DF_MEMORY_SGA_ADVICE_TMP3 <- ddply(DF_MEMORY_SGA_ADVICE_TMP,.(SGA_TARGET_GB),transform,
                                   rescale=scale(pct_change))



ggplot(data=DF_MEMORY_SGA_ADVICE_TMP,aes(x=SGA_TARGET_GB2,y=pct_change))+
  geom_tile(aes(fill = rescale),     colour = "white") +
 scale_fill_gradient(low = "white", high = "steelblue")+
  facet_grid(INSTANCE_NUMBER ~ . )

ggplot(data=DF_MEMORY_SGA_ADVICE_TMP3,aes(y=SGA_TARGET_GB,x=pct_change,fill=pct_change,colour=pct_change))+
  geom_tile()+
  scale_fill_gradient(low = "red", high = "green")+
  scale_colour_gradient(low = "red", high = "green")



DF_MEMORY_SGA_ADVICE_TMP_AGG <- ddply(DF_MEMORY_SGA_ADVICE_TMP,.(INSTANCE_NUMBER,SGA_TARGET_GB),summarise,
                                      pct_change=mean(pct_change),
                                      SIZE_FACTOR=mean(SIZE_FACTOR))

DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$SIZE_FACTOR==1,"Current Value","Minor Change")
DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change>=0.05,"Increased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP_AGG$color)
DF_MEMORY_SGA_ADVICE_TMP_AGG$color <- ifelse(DF_MEMORY_SGA_ADVICE_TMP_AGG$pct_change<=-0.05,"Decreased Physical Reads",DF_MEMORY_SGA_ADVICE_TMP_AGG$color)



sga_colors <- c("Current Value" = "#000000", "Minor Change" = "#777777", 
                "Increased Physical Reads" = "#a60000",
                "Decreased Physical Reads" = "#008000")

gg_sga_colors <- scale_colour_manual("", values = sga_colors)
gg_sga_fill<- scale_fill_manual("", values = sga_colors)




ggplot(data=DF_MEMORY_SGA_ADVICE_TMP_AGG,aes(x=SGA_TARGET_GB,y=pct_change*100,colour=color,fill=color))+
  geom_bar(stat="identity",alpha=.2)+
  geom_text(aes(label=paste(round(pct_change * 100, 1), "%", sep = "")),size=2,colour="black")+
  facet_grid(INSTANCE_NUMBER ~ . )+
  gg_sga_colors+gg_sga_fill+
  labs(title=paste("Average Percent Change in Physical Reads Relative To SGA_TARGET from SGA Target Advisory for ",main$current_db_name,sep=""))+
  xlab("SGA_TARGET (GB)")+
  ylab("Percent Change in Physical Reads (Negative is better)")


str(DF_MEMORY_SGA_ADVICE_TMP)
  