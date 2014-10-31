y <- 0

data_frame_col_not_null <- function(df_in, column_in, min_rows_in = 1){
  if((column_in %in% names(df_in))){
    # the relevant column is in the data frame
    if(nrow(df_in[!(is.na(df_in[[column_in]]))]) >= min_rows_in){
      # there are at least min_rows_in number of rows in this df that are not NA
      return(TRUE)
    }
    else{
      # the column exists, but there are not min_rows_in rows that are not NA (common case would be a column with all NAs)
      return(FALSE)
    }
  }
  else{
    # the relevant column is NOT in the data frame
    return(FALSE)
  }
  # fall through, defaults to false
  return(FALSE)
}


print(data_frame_col_not_null(main$DF_MAIN,'os_cpu'))
print(data_frame_col_not_null(main$DF_MAIN,'os_cpu1'))
print(data_frame_col_not_null(DF_MAIN2,'cpu2'))
print(data_frame_col_not_null(DF_MAIN2,'cpu2',150))

#nrow(subset(main$DF_MAIN,os_cpu >= 5))
#nrow(subset(main$DF_MAIN,os_cpu >= 5))


#x <- "cpu2"

#nrow(DF_MAIN2[!(is.na(DF_MAIN2$cpu2))])
#nrow(DF_MAIN2[!(is.na(DF_MAIN2[[x]]))])
#nrow(DF_MAIN2)
