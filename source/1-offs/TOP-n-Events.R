# Load Rda from :
#"M:/Dropbox/MyFiles/Accounts/S&L/TX/TIERs/AWR-Miner-Feb24-2014"

library(dplyr)

head(main$DF_TOP_N_EVENTS)


DF_TOP_N_AGG1 <- main$DF_TOP_N_EVENTS %.%
  group_by(WAIT_CLASS,EVENT_NAME) %.%
  summarise(TOTAL_TIME_S = sum(TOTAL_TIME_S)) %.%
  arrange(desc(TOTAL_TIME_S)) %.%
  head(25)


total_time <- sum(DF_TOP_N_AGG1$TOTAL_TIME_S)

DF_TOP_N_AGG1$pct_time <- (DF_TOP_N_AGG1$TOTAL_TIME_S/total_time)

DF_TOP_N_AGG1 <- subset(DF_TOP_N_AGG1,pct_time > 0.01)
aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "System I/O" = "#0993de","User I/O" = "#054ae1")
gg_aas_colors <- scale_fill_manual("", values = aas_colors)

aas_pct_plot1 <- ggplot(data=DF_TOP_N_AGG1, aes(x=EVENT_NAME, y=pct_time),aes(color=WAIT_CLASS)) +
  geom_bar(stat="identity",aes(fill=WAIT_CLASS))+
  geom_text(aes(label=paste(round(pct_time * 100, 2), "%", sep = "")),size=2.5, vjust=-0.2)+
  gg_aas_colors+
  scale_y_continuous(labels = percent_format())+
  theme(axis.title.y  = element_blank(),axis.title.x  = element_blank(),legend.position =    "none",
        plot.title= element_text(size = 5) )+
  labs(title="Top N Timed Events")

aas_pct_plot1
