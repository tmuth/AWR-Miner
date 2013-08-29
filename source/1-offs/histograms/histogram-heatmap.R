DF_IO_HIST <- subset(main$DF_IO_WAIT_HIST, EVENT_NAME=='cell list of blocks physical read')

df1 <- ddply(dfm, .(ID1), summarise, ID2 = ID2, pct = value / sum(value))
a = ddply(data, .(user_type), function(d) {
  data.frame(table(d$lag)/length(d$lag))
})

DF_TEST1 <- ddply(DF_IO_HIST,.(SNAP_ID,WAIT_CLASS),summarise, EVENT_NAME=EVENT_NAME,WAIT_COUNT=WAIT_COUNT,WAIT_PCT=WAIT_COUNT/sum(WAIT_COUNT))