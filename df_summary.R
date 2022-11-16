df_summary <- function(df){
  
  if(sum(stringr::str_detect(class(df),
                             paste(c("tbl_df","tbl","data.frame","tabyl","grouped_df","data.table"),
                                   collapse = "|"))) ==  0){
    stop("df must be a data.frame/tibble")
  }
  
  df_num <-
    df %>%
    dplyr::select_if(is.numeric)
  
  if(ncol(df_num) == 0){
    
    print("0 numeric variables")
    
  }else{
    
    df_num <-
      df_num %>%
      tidyr::gather(var,value) %>%
      dplyr::group_by(var) %>%
      dplyr::summarise(num_summary(value)) %>% 
      rename(Variavel = var) 
    

  }
  
  return(df_num)
  
}