#ls(main)
#summary(main$DF_MAIN)

#DF_MAIN.exec.r.iops <- main$DF_MAIN
rdaFiles <- list.files(pattern="*-parsed.Rda")
DF_MAIN <- data.frame()
dbName <- NULL
for (file in rdaFiles) {
  print(file)
  
  load(file=file)
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <<- gsub(pattern = namePattern, replacement="\\1", file)
  
  DF_MAIN <<- main$DF_MAIN
  rm(main)
  break # only handling 1 file at a time right now
}


DF_MAIN.agg <- DF_MAIN  %>%
  group_by(snap) %>%
  summarise(Executions.per.Second=sum(exec_s),
            Average.Active.Sessions=sum(aas+aas_sd),
            Read.IOPs.Max=sum(read_iops_max),
            Write.IOPs.Max=sum(write_iops_max),
            Read.MB.per.Second.Max=sum(read_mb_s_max),
            Write.MB.per.Second.Max=sum(write_mb_s_max),
            CPU.Seconds.per.Second=sum(cpu_per_s+cpu_per_s_sd),
            Redo.MB.per.Second=sum(redo_mb_s)
            )%>%
  ungroup() 
rm(DF_MAIN)

save(main.save,file=paste0(dbName,"-DF_MAIN.agg.Rda"))

#%>%
#  select(exec_s,read_iops_max)


# ggplot(data=DF_MAIN.agg,aes(x=exec_s,y=read_iops_max))+
#   geom_point()+
#   geom_smooth(method=lm,fullrange=TRUE)+
#   xlim(0,(10*max(DF_MAIN.agg$exec_s)))


plot_scatter <- function(DF_IN,x_in,y_in,x_scale_in){
  xmax <- max(DF_IN[[x_in]])
  #foo <- substitute(lm(y_in ~ x_in , data=DF_IN))
  #mdl <- eval(foo)
  #theFormula <- "y_in ~ x_in"
  #mdl <- eval(lm(as.formula(theFormula) , data=DF_IN))
  
  theFormula <- as.formula(paste(y_in, x_in, sep="~"))
  fit <- do.call("lm", list(formula=theFormula, data=DF_IN))
  #x <- as.name(x_in)
  #newdata <- do.call("data.frame",list(x=50000)) # wrap the parameter
  
  newdata <-  eval(parse(text=paste0('data.frame(',x_in,' = 50000)')))
  predictedY <- round(predict(fit, newdata),1)
  theText <- paste0('data.frame(',x_in,' = 50000,',y_in,'=',predictedY,')')
  predicted_df <-  eval(parse(text=theText))
  
  #print(summary(predicted_df))
  
#   x = substitute(data.frame(x_in=50000))
#   newdata <- eval(x)
   #print(summary(newdata))
  
  #print(summary(fit))

  p1 <- ggplot(DF_IN,aes_string(x=x_in,y=y_in))+
    geom_point()+
    geom_smooth(method=lm,fullrange=TRUE)+
    xlim(0,(1*xmax))+
    labs(title="Current Data With Linear Regression Line")+
    #scale_x_continuous(breaks=pretty_breaks(n=10),minor_breaks=pretty_breaks(n=20))+
    scale_y_continuous(breaks=pretty_breaks(n=10),minor_breaks=pretty_breaks(n=20))
  
  theColor <- "#7a1010"
  p2 <- ggplot(DF_IN,aes_string(x=x_in,y=y_in))+
    geom_point()+
    geom_smooth(method=lm,fullrange=TRUE)+
    geom_point(data=predicted_df,aes(),color="#7a1010")+
    geom_text(data=predicted_df,aes_string(label=y_in),color=theColor,size=2.5, vjust=-0.4)+
    xlim(0,(x_scale_in*xmax))+
    labs(title=paste0("Linear Regression Line based on scaling x by a factor of ",x_scale_in,", confidence interval=0.95"))+
    #scale_x_continuous(breaks=pretty_breaks(n=10),minor_breaks=pretty_breaks(n=20))+
    scale_y_continuous(breaks=pretty_breaks(n=10),minor_breaks=pretty_breaks(n=20))
  
  print(p1)
  print(p2)
  
}

pdf("future-models.pdf", width = 11, height = 8.5, useDingbats=FALSE)
plot_scatter(DF_MAIN.agg,"Executions.per.Second","CPU.Seconds.per.Second",10)
plot_scatter(DF_MAIN.agg,"Executions.per.Second","Average.Active.Sessions",10)
plot_scatter(DF_MAIN.agg,"Executions.per.Second","Read.IOPs.Max",10)
plot_scatter(DF_MAIN.agg,"Executions.per.Second","Write.IOPs.Max",10)
plot_scatter(DF_MAIN.agg,"Executions.per.Second","Read.MB.per.Second.Max",10)
plot_scatter(DF_MAIN.agg,"Executions.per.Second","Write.MB.per.Second.Max",10)
plot_scatter(DF_MAIN.agg,"Executions.per.Second","Redo.MB.per.Second",10)

dev.off()

