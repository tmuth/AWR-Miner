#head(main$DF_MAIN[,c("end","os_cpu","os_cpu_sd")])
library(ggplot2)
DF_MAIN_TMP <- data.frame(main$DF_MAIN)
DF_MAIN_TMP <- subset(DF_MAIN_TMP, inst==1)
DF_MAIN_TMP <- DF_MAIN_TMP[1:30,]
DF_MAIN_TMP <- DF_MAIN_TMP[,c("end","os_cpu","os_cpu_sd")]



DF_CPU <- structure(list(end = structure(c(1387315140, 1387316340, 1387317540, 
   1387318740, 1387319940, 1387321140, 1387322340, 1387323540, 1387324740, 
   1387325940, 1387327140, 1387328340, 1387329540, 1387330740, 1387331940, 
   1387333140, 1387334340, 1387335540, 1387336740, 1387337940, 1387339140, 
   1387340340, 1387341540, 1387342740, 1387343940, 1387345140, 1387346340, 
   1387347540, 1387348740, 1387349940), class = c("POSIXct", "POSIXt"
   ), tzone = "UTC"), os_cpu = c(14.8, 15.5, 17.4, 15.6, 14.9, 14.6, 
     15, 15.2, 14.6, 15.2, 15, 14.5, 14.8, 15, 14.6, 14.9, 14.9, 14.4, 
     14.8, 14.9, 14.5, 15, 14.6, 14.5, 15.3, 14.6, 14.6, 15.2, 14.5, 
     14.5), os_cpu_sd = c(1.3, 2.1, 3.2, 3.3, 0.9, 0.4, 1.4, 1.5, 
        0.4, 1.6, 1, 0.4, 1.4, 1.4, 0.4, 1.3, 0.9, 0.4, 1.4, 1.3, 0.4, 
        1.7, 0.4, 0.4, 1.7, 0.4, 0.4, 1.7, 0.5, 0.4)), .Names = c("end", 
            "os_cpu", "os_cpu_sd"), class = "data.frame", row.names = c(1L, 
                5L, 9L, 13L, 17L, 21L, 25L, 29L, 33L, 37L, 41L, 45L, 49L, 53L, 
                57L, 61L, 65L, 69L, 73L, 77L, 81L, 85L, 89L, 93L, 97L, 101L, 
                  105L, 109L, 113L, 117L))

ggplot(data=DF_CPU, aes(x=end, y=os_cpu)) +
  geom_line()+
  geom_errorbar(aes(ymin=os_cpu-os_cpu_sd,ymax=os_cpu+os_cpu_sd), alpha=0.2,color="red")

ggplot(data=DF_CPU, aes(x=end, y=os_cpu)) +
  geom_line()+
  geom_ribbon(aes(ymin=os_cpu-os_cpu_sd,ymax=os_cpu+os_cpu_sd), alpha=0.1,fill="red")+
  annotate("text", x = 4, y = 25, label = "Some text")


head(DF_CPU)