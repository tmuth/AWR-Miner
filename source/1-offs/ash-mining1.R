list.of.packages <- c("futile.logger","ggplot2", "plyr","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate","gplots","gtools","dplyr","sjPlot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})

library("readr")
ash_1day.narrow <- read_csv("D:/CloudStation/MyFiles/Accounts/Federal/Amtrak/Down DB - October 2015/AWR-ASH-ADDM/ash_1day.narrow.csv.gz")
ash_full <- read_csv("D:/CloudStation/MyFiles/Accounts/Federal/Amtrak/Down DB - October 2015/AWR-ASH-ADDM/ash_full.csv")
rm(ash_1day.narrow)
ash.small <- head(ash_full,n=1000)



ash.small$SAMPLE_TIME2 <- dmy_hms(ash.small$SAMPLE_TIME)
ash.small$SQL_EXEC_START2 <- ymd_hms(ash.small$SQL_EXEC_START)

str(ash.small)



ash_full$SAMPLE_TIME <- dmy_hms(ash_full$SAMPLE_TIME)
ash_full$SQL_EXEC_START <- ymd_hms(ash_full$SQL_EXEC_START)

ash_full.sub <- filter(ash_full,SAMPLE_TIME >= ymd_hm('2015-10-12 01:00') & SAMPLE_TIME < ymd_hm('2015-10-12 08:00'))
attr$date_break_major_var <- date_breaks("1 hour")
attr$date_break_minor_var <- date_breaks("15 min")



ash_full.sub <- filter(ash_full,SAMPLE_TIME >= ymd_hm('2015-10-05 01:00') & SAMPLE_TIME < ymd_hm('2015-10-12 06:00'))
attr$date_break_major_var <- date_breaks("1 day")
attr$date_break_minor_var <- date_breaks("12 hour")

ash_full.sub <- filter(ash_full,SAMPLE_TIME >= ymd_hm('2015-10-12 01:00') & SAMPLE_TIME < ymd_hm('2015-10-12 05:00'))

unique(ash_full.sub$MODULE,ash_full.sub$MACHINE)
unique(ash_full.sub$MODULE)


ash_full.sub.aas <- ash_full.sub %>%
  group_by(SAMPLE_TIME) %>%
  summarize(aas=length(SQL_ID))



ash_full.sub.aas.sqlop <- ash_full.sub %>%
  group_by(SAMPLE_TIME,SQL_OPNAME) %>%
  summarize(aas=length(SQL_ID)) %>%
  ungroup()


ash_full.sub.aas.module <- ash_full.sub %>%
  group_by(SAMPLE_TIME,MODULE) %>%
  summarize(aas=length(SQL_ID)) %>%
  ungroup()


ash_full.sub.aas.machine <- ash_full.sub %>%
  group_by(SAMPLE_TIME,MACHINE) %>%
  summarize(aas=length(SQL_ID)) %>%
  ungroup()



pdf("ash-mining.long.pdf", width = 11, height = 8.5,useDingbats=FALSE)

ggplot(data=ash_full.sub.aas,aes(x=SAMPLE_TIME,y=aas))+
  geom_bar(aes(),stat='identity',alpha=1.0,position='dodge')+
  scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                   minor_breaks = attr$date_break_minor_var
  )
  
  
  ggplot(data=ash_full.sub.aas.sqlop,aes(x=SAMPLE_TIME,y=aas))+
    geom_bar(aes(),stat='identity',alpha=1.0,position='dodge')+ 
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var
    )+
    facet_grid(SQL_OPNAME ~ . )+
    theme(text =               element_text(size=5))
  
  ggplot(data=ash_full.sub.aas.sqlop,aes(x=SAMPLE_TIME,y=aas))+
    geom_bar(aes(),stat='identity',alpha=1.0,position='dodge')+ 
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var
    )+
    facet_grid(SQL_OPNAME ~ .  ,scales="free")+
    theme(text =               element_text(size=5))
  
  
  p <- ggplot(data=ash_full.sub.aas.module,aes(x=SAMPLE_TIME,y=aas))+
    geom_bar(aes(),stat='identity',alpha=1.0,position='dodge')+ 
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var
    )+
    theme(text =               element_text(size=5))
  
  p+facet_grid(MODULE ~ . )
  p+facet_grid(MODULE ~ .,scales="free" )
  
  
  p <- ggplot(data=ash_full.sub.aas.machine,aes(x=SAMPLE_TIME,y=aas))+
    geom_bar(aes(),stat='identity',alpha=1.0,position='dodge')+ 
    scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                     minor_breaks = attr$date_break_minor_var
    )+
    theme(text =               element_text(size=5))
  
  p+facet_grid(MACHINE ~ . )
  p+facet_grid(MACHINE ~ .,scales="free" )

dev.off()



ash_full.sub2 <- filter(ash_full.sub,MODULE == 'SQL*Plus')

ash_full.sub2.grp <- ash_full.sub2 %>%
  group_by(SESSION_ID,BLOCKING_SESSION,`BLOCKING_SESSION_SERIAL#`) %>%
  summarize(count=length(SESSION_ID))






ash_full.sub2.blockers <- semi_join(ash_full.sub,ash_full.sub2.grp,by=c("SESSION_ID"="BLOCKING_SESSION","SESSION_SERIAL#"="BLOCKING_SESSION_SERIAL#"))

ash_full.sub2.677 <- filter(ash_full,SESSION_ID==1743 & `SESSION_SERIAL#` == 231)
ash_full.sub2.677 <- filter(ash_full,BLOCKING_SESSION==1743 & `BLOCKING_SESSION_SERIAL#` == 231)

unique(ash_full.sub2$SQL_ID)
unique(ash_full.sub2$SQL_OPNAME)


ash_full.sub2.blockers2 <- semi_join(ash_full,ash_full.sub2,by=c("BLOCKING_SESSION" = "SESSION_ID","BLOCKING_SESSION_SERIAL#"="SESSION_SERIAL#"))





ash_full.sub2.sqlid <- ash_full.sub2 %>%
  group_by(SAMPLE_TIME,SQL_ID) %>%
  summarize(aas=length(SQL_ID)) %>%
  ungroup()


p <- ggplot(data=ash_full.sub2.sqlid,aes(x=SAMPLE_TIME,y=aas))+
  geom_bar(aes(),stat='identity',alpha=1.0,position='dodge')+ 
  scale_x_datetime(labels = date_format_tz("%a, %b %d %I %p", tz="UTC"),breaks = attr$date_break_major_var,
                   minor_breaks = attr$date_break_minor_var
  )+
  theme(text =               element_text(size=5))

p+facet_grid(SQL_ID ~ . )
p+facet_grid(SQL_ID ~ .,scales="free" )




ash_full.sub2.sqlid2 <- semi_join(ash_full,ash_full.sub2.sqlid,by=c("SQL_ID"))

summary(ash_full.sub2.sqlid2)


ash_full.sub3 <- filter(ash_full.sub,MODULE == 'sqlplus@ussbpec038amtsa (TNS V1-V3)')

ash_full.sub4 <- filter(ash_full,SQL_ID == 'aszhyyht5f0d4')
ash_full.sub4 <- filter(ash_full,TOP_LEVEL_SQL_ID == 'aszhyyht5f0d4')
summary(ash_full.sub4)




##########################

ash_full.batch <- ash_full %>%
  mutate(hr = as.numeric(format(SAMPLE_TIME, "%H"))) %>%
  filter(hr %in% c(2,3)) %>%
  mutate(SAMPLE_HOUR = floor_date(SAMPLE_TIME,unit = c('hour')))




ash_full.sub.aas.sqlop <- ash_full.batch %>%
  group_by(SAMPLE_HOUR,SQL_OPNAME,SQL_ID) %>%
  summarize(aas=length(SQL_ID),SAMPLE_TIME=max(SAMPLE_TIME),execs=length(unique(SQL_EXEC_ID))) %>%
  ungroup() %>%
  filter(SQL_OPNAME == 'DELETE') %>%
  arrange(SAMPLE_TIME)



ash_full.sub.aas.sqlop <- ash_full.batch %>%
  group_by(SAMPLE_HOUR,SQL_OPNAME,SQL_ID,MODULE) %>%
  summarize(aas=length(SQL_ID),SAMPLE_TIME=min(SAMPLE_TIME),execs=length(unique(SQL_EXEC_ID))) %>%
  ungroup() %>%
  filter(SQL_OPNAME == 'DELETE') %>%
  arrange(SAMPLE_TIME)
  
write.csv(ash_full.sub.aas.sqlop,file="deletes_2-3am.csv",row.names=FALSE)


ash_full.sub.aas.sqlop2 <- ash_full.sub.aas.sqlop %>%
  group_by(SQL_ID) %>%
  summarise(length(SQL_ID))


ash_full.sub.module <- ash_full.batch %>%
  group_by(MODULE,MACHINE) %>%
  summarize(samples=length(SQL_ID),SAMPLE_TIME=max(SAMPLE_TIME),module.count=length(unique(MODULE))) %>%
  ungroup() %>%
  #filter(SQL_OPNAME == 'DELETE') %>%
  arrange(SAMPLE_TIME)

write.csv(ash_full.sub.module,file="modules_2-3am.csv",row.names=FALSE)

ash_full.sub.module <- ash_full.batch %>%
  group_by(MODULE,MACHINE,SQL_OPNAME) %>%
  summarize(aas=length(SQL_ID),SAMPLE_TIME=max(SAMPLE_TIME),module.count=length(unique(MODULE))) %>%
  ungroup() %>%
  #filter(SQL_OPNAME == 'DELETE') %>%
  arrange(SAMPLE_TIME)

