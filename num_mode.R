num_mode <- function(x){
  
  if(!is.numeric(x)){
    stop("x must be numeric.")
  }
  
  d <- density(na.omit(x))
  
  return(d$x[which.max(d$y)])
  
}