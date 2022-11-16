num_format <- function(x, digits = 2,decimal_mark = ",",thousand_mark = "."){
  
  if(!is.numeric(x)){
    stop("x must be numeric.")
  }
  
  if(!is.numeric(digits)){
    stop("digits must be numeric.")
  }
  
  formatC(
    x,
    format="f",
    big.mark = thousand_mark,
    digits = digits,
    decimal.mark = decimal_mark
  )
}

num_format(iris$Sepal.Length)
iris %>%
  mutate(across(where(is.numeric), num_format))