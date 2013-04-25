generate_hours_bars <- function(DF_MAIN_INT){
  log_it.debug('generate_hours_bars - start')
  #DF_MAIN_INT <- subset(main$DF_MAIN, snap >= 48472 & snap <= 49900)
  #DF_MAIN_INT <- main$DF_MAIN
 #flog.trace("DF_MAIN_INT:",DF_MAIN_INT$end,capture=TRUE)
  
  #flog.trace("MAIN:",DF_MAIN_INT$end,capture=TRUE)
  #DF_NIGHT_HOURS_INT <- data.frame("end"= unique(as.POSIXlt(strptime(format(DF_MAIN_INT$end,"%y/%m/%d"),format="%y/%m/%d"))))
  DF_NIGHT_HOURS_INT <- data.frame("end"= unique(as.POSIXct(as.POSIXlt(strftime(DF_MAIN_INT$end,format="%Y/%m/%d")),tz="UTC")))
  #DF_NIGHT_HOURS_INT <- data.frame("end"= unique(strptime(strftime(DF_MAIN_INT$end,format="%y/%m/%d"),format="%y/%m/%d")))
  log_it.trace('yep')
 #flog.trace("hours:",DF_NIGHT_HOURS_INT,capture=TRUE)
  DF_NIGHT_HOURS_INT_A <- DF_NIGHT_HOURS_INT
  DF_NIGHT_HOURS_INT_A$work_start <- DF_NIGHT_HOURS_INT_A$end + 1
  DF_NIGHT_HOURS_INT_A$work_stop <- DF_NIGHT_HOURS_INT_A$work_start + ((3600*7)-1)
 #flog.trace("DF_NIGHT_HOURS_INT_A:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  #flog.trace("DF_NIGHT_HOURS_INT_A:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  
  DF_NIGHT_HOURS_INT_B <- DF_NIGHT_HOURS_INT
  DF_NIGHT_HOURS_INT_B$work_start <- DF_NIGHT_HOURS_INT_B$end + (3600*19)
  DF_NIGHT_HOURS_INT_B$work_stop <- DF_NIGHT_HOURS_INT_B$end + ((3600*24)-1)
  
  #flog.trace("DF_NIGHT_HOURS_INT_B:",DF_NIGHT_HOURS_INT_B,capture=TRUE)
  
  DF_NIGHT_HOURS_INT_A <- rbind(DF_NIGHT_HOURS_INT_A, DF_NIGHT_HOURS_INT_B)
  
  # find date ranges outside of the filtered min and max and REMOVE them
  idx_date_min_rm <- !with(DF_NIGHT_HOURS_INT_A, work_stop < min(main$DF_SNAP_ID_DATE$end))
 #flog.trace("main$DF_SNAP_ID_DATE$end:",main$DF_SNAP_ID_DATE$end,capture=TRUE)
 #flog.trace("idx_date_min_rm1:",idx_date_min_rm,capture=TRUE)
  DF_NIGHT_HOURS_INT_A_BACK <- DF_NIGHT_HOURS_INT_A
  DF_NIGHT_HOURS_INT_A<- DF_NIGHT_HOURS_INT_A[idx_date_min_rm,]
 #flog.trace("DF_NIGHT_HOURS_INT_A1:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  idx_date_max_rm <- !with(DF_NIGHT_HOURS_INT_A, work_start > max(main$DF_SNAP_ID_DATE$end))
 #flog.trace("idx_date_max_rm:",idx_date_max_rm,capture=TRUE)
  DF_NIGHT_HOURS_INT_A<- DF_NIGHT_HOURS_INT_A[idx_date_max_rm,]
 #flog.trace("DF_NIGHT_HOURS_INT_A:",DF_NIGHT_HOURS_INT_A,capture=TRUE)
  
  # find remaining date ranges outside of the filtered min and max and CHANGE them
  idx_date_min <- with(DF_NIGHT_HOURS_INT_A, work_start < min(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A$work_start[idx_date_min] <- min(main$DF_SNAP_ID_DATE$end)
  idx_date_max <- with(DF_NIGHT_HOURS_INT_A, work_stop > max(main$DF_SNAP_ID_DATE$end))
  DF_NIGHT_HOURS_INT_A$work_stop[idx_date_max] <- max(main$DF_SNAP_ID_DATE$end)
  
  DF_NIGHT_HOURS_INT_A$value <- 0
  
  hour_bars <- geom_rect(data=DF_NIGHT_HOURS_INT_A,aes(xmin=work_start,xmax=work_stop,ymin=-Inf,ymax=Inf),alpha=0.1,fill="#bdd1e6")
  log_it.debug('generate_hours_bars - end')
  return(hour_bars)
}