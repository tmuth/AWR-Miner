rm(loggerVars)
loggerVars <- new.env()
loggerVars$DF <- data.frame()

appender.fn <- function(lineIn) { 
  lineVars <- str_extract_all(lineIn, "\\[.+?\\]")
  
  strFormat <- function(stringIn){
    stringInternal <- str_replace_all(stringIn,"\\[|\\]|\\n","")
    return(stringInternal)
  }
  
  lineVars <- lapply(lineVars,FUN=strFormat)
  
  oppCode <- as.character("")
  
  if(str_detect(lineVars[[1]][3]," - start$")){
    oppCode <- "start"
    lineVars[[1]][3] <- str_replace(lineVars[[1]][3]," - start$","")
  }
  
  if(str_detect(lineVars[[1]][3]," - end$")){
    oppCode <- "end"
    lineVars[[1]][3] <- str_replace(lineVars[[1]][3]," - end$","")
  }
  
  loggerVars$DF <<- rbind(loggerVars$DF,data.frame(db=main$current_db_name,level=lineVars[[1]][1],time=lineVars[[1]][2],message=lineVars[[1]][3],opp=oppCode))
  print(lineIn)
}

flog.appender(appender.fn)

layout <- layout.format('[~l] [~t] [~m]')
flog.layout(layout)

flog.threshold(TRACE)
flog.trace("hello")

head(loggerVars$DF,100)

loggerVars$DF[loggerVars$DF == ""] <- NA

subset(na.omit(loggerVars$DF),length(loggerVars$DF$opp)>2)

w <- reshape(subset(na.omit(loggerVars$DF),length(loggerVars$DF$opp)>2), 
             timevar = "opp",
             idvar = c("db", "level", "message"),
             direction = "wide")
head(w,100)

str(w)

x <- "[DEBUG] [2013-08-27 14:43:59] [apply_current_attributes - end\n]"

str_count(x, "\\[.+?\\]")
foo <- str_extract_all(x, "\\[.+?\\]")
print(foo)



str(foo)
foo[[1]][3]