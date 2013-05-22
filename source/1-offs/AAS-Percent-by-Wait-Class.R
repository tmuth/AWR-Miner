DF_AAS_TEMP <- main$DF_AAS

summary(DF_AAS_TEMP)
head(DF_AAS_TEMP)

avgSessTotal <- sum(DF_AAS_TEMP$AVG_SESS)
DF_AAS_AGG_BY_SNAP <- DF_AAS_TEMP[, list(AVG_SESS = sum(AVG_SESS)), by = list(SNAP_ID)]

DF_AAS_AGG <- DF_AAS_TEMP[, list(AVG_SESS = sum(AVG_SESS)), by = list(WAIT_CLASS)]
DF_AAS_AGG$AVG_SESS_PCT <- round(DF_AAS_AGG$AVG_SESS/avgSessTotal,2)

ptile <- 0.75
aasPTile <- as.vector(quantile(DF_AAS_AGG_BY_SNAP$AVG_SESS,probs=c(ptile),type=4))
DF_AAS_AGG_BY_SNAP <- data.table(DF_AAS_AGG_BY_SNAP)
DF_AAS_AGG_BY_SNAP2 <- data.table(DF_AAS_AGG_BY_SNAP[AVG_SESS >= aasPTile])
DF_AAS_AGG_BY_SNAP3 <- data.table(DF_AAS_AGG_BY_SNAP2[,list(SNAP_ID = SNAP_ID)])
DF_AAS_TEMP2 <- data.table(DF_AAS_TEMP[DF_AAS_AGG_BY_SNAP3])
avgSessTotal2 <- sum(DF_AAS_TEMP2$AVG_SESS)
DF_AAS_AGG2 <- DF_AAS_TEMP2[, list(AVG_SESS = sum(AVG_SESS)), by = list(WAIT_CLASS)]
DF_AAS_AGG2$AVG_SESS_PCT <- round(DF_AAS_AGG2$AVG_SESS/avgSessTotal2,2)


ggplot(data=DF_SQL_BY_SNAPID_AGG_SCHEMA.melt, aes(x=PARSING_SCHEMA_NAME, y=value),aes(color=PARSING_SCHEMA_NAME)) +
  geom_bar(stat="identity",aes(fill=PARSING_SCHEMA_NAME))+
  geom_text(aes(label=value),size=2.5, vjust=0.5, hjust=1.25)+
  facet_grid(variable ~ .,scales="free_y")


aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "System I/O" = "#0993de","User I/O" = "#054ae1")

gg_aas_colors <- scale_fill_manual("", values = aas_colors)

aas_pct_plot1 <- ggplot(data=DF_AAS_AGG, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
  geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
  geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
  gg_aas_colors+
  scale_y_continuous(labels = percent_format())+
  theme(axis.title.y  = element_blank(),legend.position =    "bottom" ,
        legend.key.size = unit(.25, "cm"))+
  labs(title=paste0("AAS Percent by Wait Class - All Snapshots - HP-DL980"))


aas_pct_plot2 <- ggplot(data=DF_AAS_AGG2, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
  geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
  geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
  gg_aas_colors+
  scale_y_continuous(labels = percent_format())+
  theme(axis.title.y  = element_blank(),legend.position =    "none" ,
        legend.key.size = unit(.25, "cm"))+
  labs(title=paste0("AAS Percent by Wait Class - Only Snapshots Where Total AAS >= 75th Percentile - HP-DL980"))


aas_pct_plot3 <- ggplot(data=DF_AAS_AGG, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
  geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
  geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
  gg_aas_colors+
  scale_y_continuous(labels = percent_format())+
  theme(axis.title.y  = element_blank(),legend.position =    "bottom" ,
        legend.key.size = unit(.25, "cm"))+
  labs(title=paste0("AAS Percent by Wait Class - All Snapshots - Mainframe"))


aas_pct_plot4 <- ggplot(data=DF_AAS_AGG2, aes(x=WAIT_CLASS, y=AVG_SESS_PCT),aes(color=WAIT_CLASS)) +
  geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
  geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
  gg_aas_colors+
  scale_y_continuous(labels = percent_format())+
  theme(axis.title.y  = element_blank(),legend.position =    "none" ,
        legend.key.size = unit(.25, "cm"))+
  labs(title=paste0("AAS Percent by Wait Class - Snapshots AAS >= 75th Percentile - Mainframe"))


x <- grid.arrange(aas_pct_plot1,aas_pct_plot2 , ncol = 1, heights=c(1,1))
x <- grid.arrange(aas_pct_plot3,aas_pct_plot1 ,aas_pct_plot4,aas_pct_plot2, ncol = 2,nrow=2, heights=c(1,1),widths=c(1,1))
x <- grid.arrange(aas_pct_plot3,aas_pct_plot1 , ncol = 1, heights=c(1,1))

ggsave(file="aas-by-percent.pdf",width=11, height=8)
dev.off()



#############################################################################################


