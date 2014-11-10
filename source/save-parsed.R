fullNamePattern <- "^awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-([0-9]+)-([0-9]+)(\\.|out|gz)+$"

save_parsed_data <- function(){

  main.save <- new.env()
  
  for (objName in ls(main)) {
    tmp <- get(objName,envir=main)
    if(inherits(tmp,what='data.frame')){
      if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
        print(objName)
        main.save[[objName]] <- tmp
      }
    }
    rm(tmp)
  }
  
  main.save$parseOverride <- parseOverride
  
  snap_id_min <- as.numeric(min(main.save$DF_MAIN$snap))
  snap_id_max <- as.numeric(max(main.save$DF_MAIN$snap))
  
  out_file_name <- paste0(main$current_db_name,"-",main$current_dbid,"-",snap_id_min,"-",snap_id_max)
  save(main.save,file=paste0(out_file_name,"-parsed.Rda"))
  #head(main.save$DF_AAS)
  #ls(main.save)
  
}

save_parsed_data()




main$current_db_name <- NULL
main$current_dbid <- NULL

str(main$current_db_name)
str(main$current_dbid)

get_db_name('awr-hist-test.out')




parsed_data_exists <- function(fileNameIn){

  if(!(str_detect(fileNameIn,fullNamePattern))){
    return(FALSE)
  }
  else{
    namePattern <- "awr-hist-([0-9]+)-([a-zA-Z0-9_]+)-([0-9]+)-([0-9]+).*"
    fileDBID <- gsub(pattern = namePattern, replacement="\\1", fileNameIn)
    fileDBName <- gsub(pattern = namePattern, replacement="\\2", fileNameIn)
    fileSnapMin <- as.numeric(gsub(pattern = namePattern, replacement="\\3", fileNameIn))+1
    fileSnapMax <- gsub(pattern = namePattern, replacement="\\4", fileNameIn)
    
#     print(fileDBID)
#     print(fileDBName)
#     print(fileSnapMin)
#     print(fileSnapMax)
    
    # DHLS-1376736500-100-412-parsed.Rda
    parsedFileName <- paste0(fileDBName,"-",fileDBID,"-",fileSnapMin,"-",fileSnapMax,"-parsed.Rda")
    #print(parsedFileName)
    if (file.exists(parsedFileName)){
      return(parsedFileName)
    }
    else{
      return(FALSE)
    }
  }
}





parsedFile <- parsed_data_exists('awr-hist-1376736500-DHLS-99-412.out')
parsedFile <- parsed_data_exists('awr-hist-1376736500-DHLS-99-411.out')
parsed_data_exists('awr-hist-137673650-DHLS-99-412.out')
parsed_data_exists('awr-hist-test.out')



t <- new.env()

load_parsed_data <- function(fileNameIn){
  load(fileNameIn)
  for (objName in ls(main.save)) {
    tmp <- get(objName,envir=main.save)
    if(inherits(tmp,what='data.frame')){
      if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
        print(objName)
        print(nrow(tmp))
        main[[objName]] <<- tmp
      }
    }
    rm(tmp)
  }
}

load_parsed_data(parsedFile)

ls(t)




filter_parsed_data <- function(snapIdMinIn,snapIdMaxIn){
  for (objName in ls(main)) {
    tmp <- get(objName,envir=main)
    if(inherits(tmp,what='data.frame')){
      #if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
      if((objName %in% c('DF_MAIN','DF_AAS'))){        
        print(objName)
        print(nrow(tmp))
        if(data_frame_col_not_null(tmp,'SNAP_ID')){
          print('SNAP_ID')
          tmp <- tmp[SNAP_ID >= snapIdMinIn & SNAP_ID <= snapIdMaxIn]
        } else if(data_frame_col_not_null(tmp,'snap')){
          print('snap')
          tmp <- tmp[snap >= snapIdMinIn & snap <= snapIdMaxIn]
        }
        
        print(nrow(tmp))
        
        main[[objName]] <<- tmp
      }
    }
    rm(tmp)
  }
}




filter_parsed_data(200,300)




filter_n_days_int <- function(DF_IN){
  #    filter_snap_min 
  flog.trace(str(attr$filter_snap_min),name="build_data_frames")
  flog.trace(str(attr$filter_snap_max),name="build_data_frames")
  if(inherits(DF_IN,what='data.table')){
    flog.trace('Its a data.table',name="build_data_frames")
    
    DF_IN_TMP <- DF_IN
    setkey(DF_IN_TMP,SNAP_ID)
    flog.trace(nrow(DF_IN_TMP),name="build_data_frames")
    
    DF_IN_TMP <- DF_IN_TMP[SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max]
    flog.trace(nrow(DF_IN_TMP),name="build_data_frames")
    return(DF_IN_TMP)
  }
  else{
    flog.trace('Its a data.frame',name="build_data_frames")
    return(subset(DF_IN, SNAP_ID >= attr$filter_snap_min & SNAP_ID <= attr$filter_snap_max))  
  }
  
}




date_to_snap_id <- function(dateIn,lowHigh = 'low'){
  dateInPOSIXct <- NULL
  
  # try YMD first
  dateInPOSIXct <- try(ymd(paste0(dateIn," UTC"),quiet=TRUE),silent=TRUE)
  
  if((is.na(dateInPOSIXct)) | (!(inherits(dateInPOSIXct,what='POSIXct')))){
    # if it wasn't YMD, try YMD_HMS 
    dateInPOSIXct <- try(ymd_hms(paste0(dateIn," UTC"),quiet=TRUE),silent=TRUE)
    print("ymd_hms")
  }
  
  if((is.na(dateInPOSIXct)) | (!(inherits(dateInPOSIXct,what='POSIXct')))){
    return(FALSE)
  }
  
  if(lowHigh == 'low'){
    dateInPOSIXct <- floor_date(dateInPOSIXct,"hour")
    snapReturn <- as.numeric(head(subset(main$DF_SNAP_ID_DATE,end >= dateInPOSIXct),n=1)$SNAP_ID)
    return(snapReturn)
  
  }
  else{
    dateInPOSIXct <- ceiling_date(dateInPOSIXct,"hour")
    snapReturn <- as.numeric(tail(subset(main$DF_SNAP_ID_DATE,end <= dateInPOSIXct),n=1)$SNAP_ID)
    return(snapReturn)
  }
}

date_to_snap_id('2014-10-13 23:45:00')
date_to_snap_id('2014-10-13')


date_to_snap_id('2014-10-13 23:45:00',lowHigh = 'high')

date_to_snap_id('2014-10-15 23:45:00','high')






for (objName in ls(main)) {
  tmp <- get(objName,envir=main)
  if(inherits(tmp,what='data.frame')){
    if(!(objName %in% c('overall_summary_df','capture_times','current_plot_attributes','plot_attributes'))){        
      print(objName)
      print(nrow(tmp))
      #main[[objName]] <- tmp
      #main[[objName]] <- data.frame()
      
    }
  }
  rm(tmp)
}



load(parsedFile)
