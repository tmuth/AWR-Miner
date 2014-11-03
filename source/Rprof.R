# http://www.inside-r.org/r-doc/utils/summaryRprof


Rprof(tf <- "rprof.log", memory.profiling=TRUE)


Rprof(NULL)
summaryRprof(tf,memory="both")$by.total[1:30,]







Rprof(NULL)
Rprof(tf <- "rprof3.log")


Rprof(NULL)
summaryRprof(tf)$by.total[1:50,]







