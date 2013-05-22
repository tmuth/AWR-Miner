load(file="M:/Dropbox/USPS/March-27th/main-PSASP2.Rda")
DF_AAS_OLD <- main$DF_AAS
rm(main)

load(file="M:/Dropbox/USPS/AWR-Miner/HP-DL980/PSASP0-main.Rda")
DF_AAS_NEW <- main$DF_AAS
rm(main)


avgSessTotalOld <- sum(DF_AAS_OLD$AVG_SESS)
avgSessTotalNew <- sum(DF_AAS_NEW$AVG_SESS)
DF_AAS_OLD$system <- "Mainframe"
DF_AAS_NEW$system <- "HP DL980"

DF_AAS_OLD$AVG_SESS_PCT <- round(DF_AAS_OLD$AVG_SESS/avgSessTotalOld,2)
DF_AAS_NEW$AVG_SESS_PCT <- round(DF_AAS_NEW$AVG_SESS/avgSessTotalNew,2)

summary(DF_AAS_OLD)
head(DF_AAS_OLD)



DF_AAS_COMBINED <- rbind(DF_AAS_OLD,DF_AAS_NEW)

DF_AAS_AGG <- DF_AAS_COMBINED[, list(AVG_SESS = sum(AVG_SESS)), by = list(WAIT_CLASS,system)]
print(DF_AAS_AGG)

DF_AAS_AGG$AVG_SESS_PCT <- round(DF_AAS_AGG$AVG_SESS/avgSessTotal,2)

DF_AAS_AGG[system=='Mainframe',AVG_SESS_PCT := round(AVG_SESS / sum(DF_AAS_AGG[system=='Mainframe']$AVG_SESS),2)]
DF_AAS_AGG[system=='HP DL980',AVG_SESS_PCT := round(AVG_SESS / sum(DF_AAS_AGG[system=='HP DL980']$AVG_SESS),2)]

DF_AAS_AGG <- DF_AAS_AGG[AVG_SESS_PCT > 0.00]

vals <- expand.grid(WAIT_CLASS = unique(DF_AAS_AGG$WAIT_CLASS),
                    system = unique(DF_AAS_AGG$system))
vals <- data.frame(vals)
DF_AAS_AGG1.2 <- data.frame(DF_AAS_AGG)
DF_AAS_AGG2 <- merge(DF_AAS_AGG1.2,vals,all.x = TRUE,by=c("system","WAIT_CLASS"),incomparables = NA)
DF_AAS_AGG2$systemAlpha <- 1
DF_AAS_AGG2 <- data.table(DF_AAS_AGG2)
DF_AAS_AGG2[system=='HP DL980',systemAlpha:=1.5]

print(DF_AAS_AGG2)

aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "System I/O" = "#0993de","User I/O" = "#054ae1")
gg_aas_colors <- scale_fill_manual("", values = aas_colors)

ggplot(data=DF_AAS_AGG2, aes(x=WAIT_CLASS, y=AVG_SESS_PCT,fill=WAIT_CLASS,group=system)) +
  geom_bar(stat="identity",aes(fill=WAIT_CLASS,alpha=system),position="dodge")+
  geom_text(aes(label=paste(round(AVG_SESS_PCT * 100, 2), "%", sep = "")),position = position_dodge(width=1),vjust=-1)+
  gg_aas_colors+
  scale_alpha_discrete(range = c('HP DL980'=0.95,'Mainframe'= 0.3))+
  labs(title="AAS % by Wait Class - Darker Colors = HP DL980")

ggsave(file="aas-by-percent.pdf",width=11, height=8)
dev.off()



#############################################################################################


