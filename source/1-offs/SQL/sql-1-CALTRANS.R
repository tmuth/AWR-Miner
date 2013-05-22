setwd("M:/Dropbox/MyFiles/GitHub/AWR-Miner/source/1-offs/SQL")
#save(DF_IO_HIST,file="DF_IO_HIST.Rda")
#DF_SQL_SUMMARY <- main$DF_SQL_SUMMARY
#DF_SQL_BY_SNAPID <- main$DF_SQL_BY_SNAPID
#save(DF_SQL_SUMMARY,file="DF_SQL_SUMMARY-CALTRANS.Rda")
#save(DF_SQL_BY_SNAPID,file="DF_SQL_BY_SNAPID-CALTRANS.Rda")
#load(file="DF_IO_HIST.Rda")
rm(main)
load(file="M:/Dropbox/USPS/AWR-Miner/HP-DL980/PSASP0-main.Rda")
rm(DF_SQL_SUMMARY)
rm(DF_SQL_BY_SNAPID)
DF_SQL_SUMMARY <- main$DF_SQL_SUMMARY
DF_SQL_BY_SNAPID <- main$DF_SQL_BY_SNAPID


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

DF_SQL_BY_SNAPID$end <-  round_date(DF_SQL_BY_SNAPID$end , "hour")
DF_SQL_BY_SNAPID$hour <- as.numeric(format(DF_SQL_BY_SNAPID$end,"%H"))
DF_SQL_BY_SNAPID$dayName <- format(DF_SQL_BY_SNAPID$end,"%A")
DT_SQL_BY_SNAPID <- data.table(DF_SQL_BY_SNAPID)

# filter down to Monday and Wednesday between 6pm and 6am
nrow(DT_SQL_BY_SNAPID)
#DT_SQL_BY_SNAPID2 <- DT_SQL_BY_SNAPID[dayName %in% list('Monday','Wednesday')]
#DT_SQL_BY_SNAPID2 <- DT_SQL_BY_SNAPID2[hour >= 18 | hour <= 6 ]
DT_SQL_BY_SNAPID2 <- DT_SQL_BY_SNAPID
head(DT_SQL_BY_SNAPID2)
summary(DT_SQL_BY_SNAPID2)
nrow(DT_SQL_BY_SNAPID2)

DT_SQL_BY_SNAPID2 <- DT_SQL_BY_SNAPID2[ELAP_S > 0,]






DT_SQL_BY_SNAPID2$EXECS2 <- as.numeric(DT_SQL_BY_SNAPID2$EXECS)
DT_SQL_BY_SNAPID2[EXECS == 0 & ELAP_S > 0, EXECS2 := 0.1]
DT_SQL_BY_SNAPID2$ELAP_PER_EXEC <- round(DT_SQL_BY_SNAPID2$ELAP_S / DT_SQL_BY_SNAPID2$EXECS2,1) 


DT_SQL_BY_SNAPID_AGG_SCHEMA <- data.table(DT_SQL_BY_SNAPID2[, list(ELAP_S = sum(ELAP_S),IO_WAIT = sum(IO_WAIT),EXECS=sum(as.numeric(EXECS)),CPU_T_S=sum(CPU_T_S)), by = list(PARSING_SCHEMA_NAME)])
DF_SQL_BY_SNAPID_AGG_SCHEMA.melt <- melt(DT_SQL_BY_SNAPID_AGG_SCHEMA, id.var = c("PARSING_SCHEMA_NAME"), measure.var = c("ELAP_S", "IO_WAIT","EXECS", "CPU_T_S"))

ggplot(data=DF_SQL_BY_SNAPID_AGG_SCHEMA.melt, aes(x=PARSING_SCHEMA_NAME, y=value),aes(color=PARSING_SCHEMA_NAME)) +
  geom_bar(stat="identity",aes(fill=PARSING_SCHEMA_NAME))+
  geom_text(aes(label=value),size=2.5, vjust=0.5, hjust=1.25)+
  facet_grid(variable ~ .,scales="free_y")


DT_SQL_BY_SNAPID_AGG_COMMAND <- data.table(DT_SQL_BY_SNAPID2[, list(ELAP_S = sum(ELAP_S),IO_WAIT = sum(IO_WAIT),EXECS=sum(as.numeric(EXECS2)),CPU_T_S=sum(CPU_T_S),"Avg Sec per Exec"=round(mean(ELAP_PER_EXEC)),1), by = list(COMMAND_NAME)])
DF_SQL_BY_SNAPID_AGG_COMMAND.melt <- melt(DT_SQL_BY_SNAPID_AGG_COMMAND, id.var = c("COMMAND_NAME"), measure.var = c("ELAP_S", "Avg Sec per Exec","IO_WAIT","EXECS", "CPU_T_S"))

DF_SQL_BY_SNAPID_AGG_COMMAND.melt2 <- data.table(DF_SQL_BY_SNAPID_AGG_COMMAND.melt[, list(value = sum(value)), by = list(COMMAND_NAME,variable)])

ggplot(data=DF_SQL_BY_SNAPID_AGG_COMMAND.melt, aes(x=COMMAND_NAME,y=value),aes(color=COMMAND_NAME)) +
  geom_bar(stat="identity",aes(fill=COMMAND_NAME))+
  #geom_bar(aes(y = (..sum..)/sum(..count..)))+
  #geom_bar(aes(fill=COMMAND_NAME))+
  geom_text(aes(label=value),size=2.5,vjust=1.5)+
  facet_grid(variable ~ .,scales="free_y")
  #scale_y_continuous(labels = percent_format())

ggsave(file="sql-by-command2.pdf",width=11, height=8)
dev.off()













cleanStringNumbers <- function(x){
  y <- str_replace_all(x,',','')
  y <- str_trim(y)
  y <- as.numeric(y)
  return(y)
}

DT_SQL_SUMMARY <- data.table(DF_SQL_SUMMARY)

DT_SQL_SUMMARY$EXECS <- cleanStringNumbers(DT_SQL_SUMMARY$EXECS)
DT_SQL_SUMMARY$ELAP_S <- cleanStringNumbers(DT_SQL_SUMMARY$ELAP_S)
DT_SQL_SUMMARY$PHY_READ_GB <- cleanStringNumbers(DT_SQL_SUMMARY$PHY_READ_GB)
DT_SQL_SUMMARY$LOG_READS <- cleanStringNumbers(DT_SQL_SUMMARY$LOG_READS)

summary(DT_SQL_SUMMARY)
DT_SQL_SUMMARY[, EXECS := apply(DT_SQL_SUMMARY[,EXECS,with=FALSE], 
                                 cleanStringNumbers(x)), 
   with=FALSE]


DT_SQL_SUMMARY_GRP <- data.table(DT_SQL_SUMMARY[, list(ELAP_S = sum(as.numeric(ELAP_S)),EXECS=sum(as.numeric(EXECS)),
                                                          cost=max(OPTIMIZER_COST),PHY_READ_GB=sum(PHY_READ_GB)
                                                          ),                                               
                                                   by = list(MODULE,ACTION,SQL_ID,OPTIMIZER_COST,COMMAND_NAME,PARSING_SCHEMA)])

DT_SQL_SUMMARY_GRP$EXECS <- str_trim(DT_SQL_SUMMARY_GRP$EXECS)
DT_SQL_SUMMARY_GRP$EXECS <- str_replace_all(DT_SQL_SUMMARY_GRP$EXECS,',','')
DT_SQL_SUMMARY_GRP$ELAP_S <- str_trim(DT_SQL_SUMMARY_GRP$ELAP_S)
DT_SQL_SUMMARY_GRP$ELAP_S <- str_replace_all(DT_SQL_SUMMARY_GRP$ELAP_S,',','')
DT_SQL_SUMMARY_GRP$EXECS <- as.numeric(DT_SQL_SUMMARY_GRP$EXECS)
DT_SQL_SUMMARY_GRP$ELAP_S <- as.numeric(DT_SQL_SUMMARY_GRP$ELAP_S)
DT_SQL_SUMMARY_GRP<-data.table(DT_SQL_SUMMARY_GRP)
DT_SQL_SUMMARY_GRP <- DT_SQL_SUMMARY_GRP[ELAP_S > 0,]
DT_SQL_SUMMARY_GRP$EXECS2 <- as.numeric(DT_SQL_SUMMARY_GRP$EXECS)
DT_SQL_SUMMARY_GRP[EXECS == 0, EXECS2 := 0.1]
DT_SQL_SUMMARY_GRP$ELAP_PER_EXEC <- round(DT_SQL_SUMMARY_GRP$ELAP_S / DT_SQL_SUMMARY_GRP$EXECS2,1)
DT_SQL_SUMMARY_GRP$ELAP_M_PER_EXEC <- round(DT_SQL_SUMMARY_GRP$ELAP_PER_EXEC/60,1)
DT_SQL_SUMMARY_GRP <- data.table(DT_SQL_SUMMARY_GRP)
setkey(DT_SQL_SUMMARY_GRP,ELAP_M_PER_EXEC)
summary(DT_SQL_SUMMARY_GRP)
head(DT_SQL_SUMMARY_GRP)
print(DT_SQL_SUMMARY_GRP)

DT_SQL_SUMMARY_GRP2 <- DT_SQL_SUMMARY_GRP[ELAP_M_PER_EXEC >=1]


df3[,foo:=NULL]
DF_SQL_SUMMARY <- data.table(DF_SQL_SUMMARY)
DF_SQL_SUMMARY3 <- DF_SQL_SUMMARY[SQL_ID=='dcmmkhku57c3h']