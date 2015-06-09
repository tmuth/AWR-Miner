list.of.packages <- c("futile.logger","ggplot2", "plyr","dplyr","lazyeval","gridExtra","scales","reshape","xtable","ggthemes","stringr","data.table","lubridate","gplots")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  options(repos="http://cran.cnr.Berkeley.edu")
  install.packages(new.packages,dependencies = TRUE)  
}


lapply(list.of.packages, function(x) {
  library(x,character.only=TRUE,quietly = TRUE)
})



myTheme <- theme_stata(scheme = "s2color") +
  #myTheme <- theme_few() +
  #myTheme <- theme_bw() +
  
  theme(legend.position =    "bottom",
        #plot.margin =        unit(c(3, 3, 3, 3), "lines"),
        axis.title.y = element_text(vjust = .6),
        text =               element_text(family="sans",face = "plain",
                                          colour = "black", size = 8,
                                          hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
        axis.text.y =       element_text(angle = 0),
        panel.margin =       unit(0.25, "lines"),
        panel.grid.major = element_line(colour="#dedede", size = 0.2,linetype = "dotted"),
        panel.grid.minor = element_line(colour="#dedede", size = 0.1,linetype = "dotted"),
        axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6)
  )

theme_set(myTheme)



':=' = function(lhs, rhs) {
  frame = parent.frame()
  lhs = as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs = lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs = list(rhs)
  if (length(lhs) > length(rhs))
    rhs = c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}