setwd("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/1-offs/SQL")
#save(DF_IO_HIST,file="DF_IO_HIST.Rda")
#DF_SQL_SUMMARY <- main$DF_SQL_SUMMARY
#DF_SQL_BY_SNAPID <- main$DF_SQL_BY_SNAPID
#save(DF_SQL_SUMMARY,file="DF_SQL_SUMMARY-CALTRANS.Rda")
#save(DF_SQL_BY_SNAPID,file="DF_SQL_BY_SNAPID-CALTRANS.Rda")
#load(file="DF_IO_HIST.Rda")
rm(main)
load(file="M:/Dropbox/USPS/March-27th/main-PSASP2.Rda")
load(file="M:/Dropbox/USPS/AWR-Miner/HP-DL980/PSASP0-main.Rda")
rm(DF_SQL_SUMMARY)
rm(DF_SQL_BY_SNAPID)
DF_SQL_SUMMARY <- main$DF_SQL_SUMMARY
DF_SQL_BY_SNAPID <- main$DF_SQL_BY_SNAPID

summary(DF_SQL_SUMMARY)
summary(DF_SQL_BY_SNAPID)


#load(file="DF_SQL_SUMMARY-CALTRANS.Rda")
#load(file="DF_SQL_BY_SNAPID-CALTRANS.Rda")

options(scipen=10)


list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})


cleanStringNumbers <- function(x){
  y <- str_replace_all(x,',','')
  y <- str_trim(y)
  y <- as.numeric(y)
  return(y)
}


DT_SQL_BY_SNAPID <- data.table(DF_SQL_BY_SNAPID)

DT_SQL_BY_SNAPID$EXECS <- cleanStringNumbers(DT_SQL_BY_SNAPID$EXECS)
DT_SQL_BY_SNAPID$ELAP_S <- cleanStringNumbers(DT_SQL_BY_SNAPID$ELAP_S)
#DT_SQL_BY_SNAPID$PHY_READ_GB <- cleanStringNumbers(DT_SQL_BY_SNAPID$PHY_READ_GB)
DT_SQL_BY_SNAPID$LOG_READS <- cleanStringNumbers(DT_SQL_BY_SNAPID$LOG_READS)

#DT_SQL_BY_SNAPID$EXECS2 <- DT_SQL_BY_SNAPID$EXECS
#modify execs from 0 to 0.1 if work was done but not completed
DT_SQL_BY_SNAPID <- DT_SQL_BY_SNAPID[ELAP_S > 0]
DT_SQL_BY_SNAPID[EXECS == 0 & ELAP_S > 0, EXECS := 0.1]
DT_SQL_BY_SNAPID$ELAP_P_EXEC <- round(DT_SQL_BY_SNAPID$ELAP_S/DT_SQL_BY_SNAPID$EXECS,2)


nrow(DT_SQL_BY_SNAPID)
summary(DT_SQL_BY_SNAPID)

setnames(DT_SQL_BY_SNAPID, "COMMAND_NAME", "COMMAND")
setnames(DT_SQL_BY_SNAPID, "PARSING_SCHEMA_NAME", "SCHEMA")
DT_SQL_BY_SNAPID[with(DT_SQL_BY_SNAPID, grepl("PL/SQLEXECUTE", COMMAND)),]$COMMAND<-"PL/SQL"

# DT_SQL_BY_SNAPID_GRP_BY_SQLID <- data.table(DT_SQL_BY_SNAPID[, list(ELAP_S = sum(ELAP_S),EXECS=sum(EXECS),
#                                                        cost=max(OPTIMIZER_COST),IO_WAIT=sum(IO_WAIT),READ_MB=sum(READ_MB),CPU_T_S=sum(CPU_T_S)
# ),                                               
#                                                 by = list(MODULE,ACTION,SQL_ID,COMMAND,SCHEMA)])
# 


DT_SQL_BY_SNAPID_GRP_BY_COMMAND <- data.table(DT_SQL_BY_SNAPID[, list(ELAP_S = sum(ELAP_S),EXECS=sum(EXECS),
                                                                    cost=max(OPTIMIZER_COST),IO_WAIT=sum(IO_WAIT),READ_MB=sum(READ_MB),CPU_T_S=sum(CPU_T_S),
                                                                ELAP_P_EXEC=(sum(ELAP_S)/sum(EXECS))
                                                                      ),                                               
                                                             by = list(COMMAND)])

DT_SQL_BY_SNAPID_GRP_BY_COMMAND$Executions <- DT_SQL_BY_SNAPID_GRP_BY_COMMAND$EXECS / sum(DT_SQL_BY_SNAPID_GRP_BY_COMMAND$EXECS)
DT_SQL_BY_SNAPID_GRP_BY_COMMAND$"Elapsed Time" <- DT_SQL_BY_SNAPID_GRP_BY_COMMAND$ELAP_S / sum(DT_SQL_BY_SNAPID_GRP_BY_COMMAND$ELAP_S)
DT_SQL_BY_SNAPID_GRP_BY_COMMAND$"I/O Wait" <- DT_SQL_BY_SNAPID_GRP_BY_COMMAND$IO_WAIT / sum(DT_SQL_BY_SNAPID_GRP_BY_COMMAND$IO_WAIT)
DT_SQL_BY_SNAPID_GRP_BY_COMMAND$READ_MB_PCT <- DT_SQL_BY_SNAPID_GRP_BY_COMMAND$READ_MB / sum(DT_SQL_BY_SNAPID_GRP_BY_COMMAND$READ_MB)
DT_SQL_BY_SNAPID_GRP_BY_COMMAND$"CPU Time" <- DT_SQL_BY_SNAPID_GRP_BY_COMMAND$CPU_T_S / sum(DT_SQL_BY_SNAPID_GRP_BY_COMMAND$CPU_T_S)
DT_SQL_BY_SNAPID_GRP_BY_COMMAND$"Elap Per Exec" <- DT_SQL_BY_SNAPID_GRP_BY_COMMAND$ELAP_P_EXEC / sum(DT_SQL_BY_SNAPID_GRP_BY_COMMAND$ELAP_P_EXEC)




DT_SQL_BY_SNAPID_GRP_BY_COMMAND.melt <- melt(DT_SQL_BY_SNAPID_GRP_BY_COMMAND, id.var = c("COMMAND"), 
                                             measure.var = c("Executions", "Elapsed Time","I/O Wait", "CPU Time","Elap Per Exec"))


ggplot(data=DT_SQL_BY_SNAPID_GRP_BY_COMMAND.melt, aes(x=COMMAND, y=value),aes(color=COMMAND)) +
  geom_bar(stat="identity",aes(fill=COMMAND))+
  geom_text(aes(label=round(value*100,3)),size=2.5, vjust=0.5, hjust=0)+
  facet_grid(variable ~ .,scales="free_y")+
  scale_y_continuous(labels = percent_format())+
  theme(legend.position="none",axis.title.x  = element_blank(),axis.title.y  = element_blank())+
  labs(title="SQL Percent by Command for All SQL")


ggsave(file="sql-by-command-all-sql.pdf",width=11, height=8)
dev.off()


summary(DT_SQL_BY_SNAPID)


nrow(DT_SQL_BY_SNAPID[COMMAND=='SELECT' & ELAP_P_EXEC >= 2])
nrow(DT_SQL_BY_SNAPID[COMMAND=='SELECT' & ELAP_P_EXEC < 2])
sum(DT_SQL_BY_SNAPID[COMMAND=='SELECT' & ELAP_P_EXEC < 10]$ELAP_S)
sum(DT_SQL_BY_SNAPID[COMMAND=='SELECT' & ELAP_P_EXEC >= 10]$ELAP_S)
DT_SQL_BY_SNAPID2 <- DT_SQL_BY_SNAPID

DT_SQL_BY_SNAPID2$seconds<- as.factor(ifelse(DT_SQL_BY_SNAPID2$ELAP_P_EXEC > 10,"10+","<10"))
#DT_SQL_BY_SNAPID2$seconds<- as.factor(ifelse(DT_SQL_BY_SNAPID2$ELAP_P_EXEC > 120,"120+","<120"))


DT_SQL_BY_SNAPID2_GRP <- ddply(DT_SQL_BY_SNAPID2, .(COMMAND,seconds), summarise, 
      ELAP_S=sum(ELAP_S))



DT_SQL_BY_SNAPID2_GRP <- data.table(DT_SQL_BY_SNAPID2_GRP)
DT_SQL_BY_SNAPID2_GRP[,ELAP_PCT := ELAP_S/sum(ELAP_S),by=COMMAND]


ggplot(data=DT_SQL_BY_SNAPID2_GRP, aes(x=seconds, y=ELAP_PCT),aes(fill=seconds))+
  geom_bar(stat="identity",aes(fill=seconds))+
  geom_text(aes(label=round(ELAP_PCT*100,1)),size=2.5, vjust=0, hjust=0)+
  facet_grid(COMMAND ~ .)+
  scale_y_continuous(labels = percent_format())+
  labs(title="SQL Percent of Elapsed Time by Command for All SQL - Binned at 10 Seconds")

ggsave(file="sql-pct-by-command-all-sql-bin-10-second.pdf",width=11, height=8)
dev.off()


# Group by command, show percent wait

DT_SQL_BY_SNAPID3 <- data.table(DT_SQL_BY_SNAPID)
DT_SQL_BY_SNAPID3 <- DT_SQL_BY_SNAPID3[ELAP_P_EXEC >= 10]
summary(DT_SQL_BY_SNAPID3)
DT_SQL_BY_SNAPID3_GRP <- ddply(DT_SQL_BY_SNAPID3, .(COMMAND), summarise, 
                               ELAP_S=sum(ELAP_S),
                               CPU_T_S=sum(CPU_T_S),
                               IO_WAIT_S=sum(IO_WAIT)
                               )


DT_SQL_BY_SNAPID4_GRP <- ddply(DT_SQL_BY_SNAPID3, .(), summarise, 
                               ELAP_S=sum(ELAP_S),
                               CPU_T_S=sum(CPU_T_S),
                               IO_WAIT_S=sum(IO_WAIT)
)
DT_SQL_BY_SNAPID4_GRP <- data.table(DT_SQL_BY_SNAPID4_GRP)
setnames(DT_SQL_BY_SNAPID4_GRP, ".id", "COMMAND")
DT_SQL_BY_SNAPID4_GRP$COMMAND <- "Combined"
DT_SQL_BY_SNAPID3_GRP <- rbind(DT_SQL_BY_SNAPID3_GRP,DT_SQL_BY_SNAPID4_GRP)

DT_SQL_BY_SNAPID3_GRP$OTHER_S <- DT_SQL_BY_SNAPID3_GRP$ELAP_S-DT_SQL_BY_SNAPID3_GRP$CPU_T_S-DT_SQL_BY_SNAPID3_GRP$IO_WAIT_S
DT_SQL_BY_SNAPID3_GRP$CPU <- round(DT_SQL_BY_SNAPID3_GRP$CPU_T_S/DT_SQL_BY_SNAPID3_GRP$ELAP_S,2)
DT_SQL_BY_SNAPID3_GRP$"I/O" <- round(DT_SQL_BY_SNAPID3_GRP$IO_WAIT_S/DT_SQL_BY_SNAPID3_GRP$ELAP_S,2)
DT_SQL_BY_SNAPID3_GRP$Other <- round(DT_SQL_BY_SNAPID3_GRP$OTHER_S/DT_SQL_BY_SNAPID3_GRP$ELAP_S,2)
DT_SQL_BY_SNAPID3_GRP<-data.table(DT_SQL_BY_SNAPID3_GRP)


DT_SQL_BY_SNAPID3_GRP.melt <- melt(DT_SQL_BY_SNAPID3_GRP, id.var = c("COMMAND"), 
                                             measure.var = c("CPU", "I/O","Other"))


ggplot(data=DT_SQL_BY_SNAPID3_GRP.melt , aes(x=variable, y=value),aes(fill=variable))+
  geom_bar(stat="identity",aes(fill=variable))+
  geom_text(aes(label=round(value*100,1)),size=2.5, vjust=0, hjust=0)+
  facet_grid(COMMAND ~ .)+
  scale_y_continuous(labels = percent_format())+
  labs(title="Percent Waited by Wait Class by Command for SQL With Elap/Exec >= 10")+
  theme(legend.position="none",axis.title.y  = element_blank())+
  xlab("Wait Class")

ggsave(file="sql-pct-wait-by-waitclass-by-command-all-sql-over-10-second.pdf",width=11, height=8)
dev.off()




