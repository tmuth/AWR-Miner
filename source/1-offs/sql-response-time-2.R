setwd("M:/Dropbox/MyFiles/Accounts/S&L/NC/Public Schools/Pearson/Pearson-POV/Results/Test-H")
load(file="PSPRD.main.Rda")

DF_MAIN <- subset(main$DF_MAIN, snap >= 304)
DF_MAIN2 <- ddply(DF_MAIN, .(end), summarise,  aas=sum(aas),sql_res_t_cs=sum(sql_res_t_cs))

DF_MAIN2$db <- "EXADATA"


setwd("M:/Dropbox/MyFiles/Accounts/Canada/Queens University/Tyler/AWR-Miner")

load(file="FNPRD.main.Rda")
DF_MAIN_TMP <- ddply(main$DF_MAIN, .(end), summarise,  aas=sum(aas),sql_res_t_cs=sum(sql_res_t_cs))
DF_MAIN_TMP$db <- "FNPRD"

DF_MAIN2 <- rbind(DF_MAIN2,DF_MAIN_TMP)


rm(DF_MAIN_TMP)

load(file="CSPRD.main.Rda")
DF_MAIN_TMP <- ddply(main$DF_MAIN, .(end), summarise,  aas=sum(aas),sql_res_t_cs=sum(sql_res_t_cs))
DF_MAIN_TMP$db <- "CSPRD"

DF_MAIN2 <- rbind(DF_MAIN2,DF_MAIN_TMP)
rm(DF_MAIN_TMP)

ggplot(data=DF_MAIN2,aes(x=sql_res_t_cs))+
  geom_histogram(binwidth=0.05,stat = "bin")+
  facet_grid(db ~ .,scales="free_y")+
  scale_x_discrete("", breaks = levels(DF_MAIN2$sql_res_t_cs)