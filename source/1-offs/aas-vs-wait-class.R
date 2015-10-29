DF_MAIN_INT <- data.frame()
DF_AAS_INT <- data.frame()

getAASdf<- function(file){
  stopifnot(length(file) == 1)
  namePattern <- "([a-zA-Z0-9_]+)-.*"
  dbName <- gsub(pattern = namePattern, replacement="\\1", file)
  #DF_AAS_TEMP <<- NULL
  load(file=file)
  #debugVars$main$DF_AAS$db <- dbName
  main.save$DF_MAIN$db <- dbName
  main.save$DF_AAS$db <- dbName
  
  DF_MAIN_INT <<- main.save$DF_MAIN
  DF_AAS_INT <<- main.save$DF_AAS
  
  
}

namePattern <- ".*-parsed.Rda"
fileName <- list.files(pattern=namePattern)

getAASdf(fileName)




DF_MAIN_INT.group <- DF_MAIN_INT %>%
  group_by(snap) %>%
  summarise(AAS=sum(aas),WAIT_RATIO=max(db_wait_ratio)) %>%
  select(AAS,WAIT_RATIO)

DF_MAIN_INT.2 <- select(DF_MAIN_INT,db_cpu_ratio,db_wait_ratio)

summary(DF_MAIN_INT.2)


#DF_AAS_INT <- main$DF_AAS

DF_AAS_INT.group <- DF_AAS_INT %>%
  group_by(SNAP_ID) %>%
  mutate(AAS=sum(AVG_SESS)) %>%
  ungroup() %>%
  select(WAIT_CLASS,AAS,AVG_SESS)

DF_AAS_INT.group.sum <- DF_AAS_INT.group %>%
  group_by(WAIT_CLASS) %>%
  summarise(AVG_SESS = max(AVG_SESS)) %>%
  filter(AVG_SESS >= (max(DF_AAS_INT.group$AAS) * .01) )

DF_AAS_INT.group2 <- semi_join(
  DF_AAS_INT.group,DF_AAS_INT.group.sum,
  by=c('WAIT_CLASS')
)
  
  

aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "System I/O" = "#0993de","User I/O" = "#054ae1")

gg_aas_colors <- scale_color_manual("", values = aas_colors)

pdf("aas-vs-wait-class.pdf", width = 11, height = 8.5, useDingbats=FALSE)

ggplot(data=DF_MAIN_INT.group,aes(y=WAIT_RATIO,x=AAS))+
  geom_point(aes(), alpha=0.2) +
  #y ~ x, y ~ poly(x, 2), y ~ log(x)
  stat_smooth(method='auto',formula='y ~ log(x)') 
  #stat_smooth(method="loess",size = 1,formula='y ~ log(x)') +
  #ylab('AAS per Wait Class') +
  #xlab('Total AAS')

ggplot(data=DF_AAS_INT.group2,aes(y=AVG_SESS,x=AAS,color=WAIT_CLASS))+
  geom_point(aes(), alpha=0.2) +
  #y ~ x, y ~ poly(x, 2), y ~ log(x)
  stat_smooth(method="loess",size = 1,formula='y ~ log(x)') +
  gg_aas_colors +
  facet_grid(. ~ WAIT_CLASS ) +
  ylab('AAS per Wait Class') +
  xlab('Total AAS')
dev.off()

# summary(DF_AAS_INT.group)
# unique(DF_AAS_INT.group$WAIT_CLASS)
# 
# DF_AAS_INT.group.Concurrency <- filter(DF_AAS_INT.group,WAIT_CLASS=='Concurrency')
# 
# 
# aovFit1 <- aov(Y ~ LVS)
# aovFit1 <- aov(DF_AAS_INT.group.Concurrency$AAS ~ DF_AAS_INT.group.Concurrency$AVG_SESS)
# aovFit2 <- aov(AAS ~ AVG_SESS,data=DF_AAS_INT.group.Concurrency)
# 
# ls(aovFit1)
# 
# head(aovFit1$fitted.values,n=40)
# 
# head(aovFit1$fitted.values,n=40)
# 
# 
# 
# 
# 
# 
# ggplot(data=DF_AAS_INT.group.Concurrency,aes(x=AVG_SESS,y=AAS,color=WAIT_CLASS))+
#   geom_line()
