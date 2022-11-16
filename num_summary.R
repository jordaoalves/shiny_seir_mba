num_summary <- function(x, minimal = FALSE){
  
  if(is.numeric(x) == FALSE){
    stop("x must be numeric")
  }
  
  if(minimal == FALSE){
    out <-
      dplyr::tibble(
        n = length(x),
        #equal_zero = sum(x == 0, na.rm = TRUE),
        min  = min(x, na.rm = TRUE),
        p25  = quantile(x,probs = .25, na.rm = TRUE),
        p50  = median(x, na.rm = TRUE),
        p75  = quantile(x,probs = .75, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        moda = ifelse(length(na.omit(x) ) < 3, NA_real_ ,num_mode(na.omit(x))),
        media = mean(x, na.rm = TRUE),
        cv = cv(x)
      )
  }else{
    out <-
      dplyr::tibble(
        n = length(x),
        min  = min(x, na.rm = TRUE),
        p25  = quantile(x,probs = .25, na.rm = TRUE),
        p50  = median(x, na.rm = TRUE),
        p75  = quantile(x,probs = .75, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        moda = ifelse(length(na.omit(x)) < 3, NA_real_ ,num_mode(na.omit(x))),
        media = mean(x, na.rm = TRUE),
        cv = cv(x)
      )
  }
  
  return(out)
  
}