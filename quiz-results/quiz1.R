
# Name: John Bonney
# Date: 7.2.18
# Quiz 1 - Workhorse Objects in R


temp_convert <- function(temperature, FtoC=TRUE, options) {
  
  if (FtoC) {
    f_temp <- temperature
    c_temp <- (f_temp - 32)/1.8
  }
  
  if (!FtoC) {
    c_temp <- temperature
    f_temp <- c_temp * 1.8 + 32
  }
  
  
  if (!option1 %in% options) {
    stop("error: options$option1 must exist")
  }
  dt <- data.table(Celsius = c_temp, Fahrenheit = f_temp)
  return(dt)
}

test_options <- list("option1" <- 5, "option2" <- 2)
temp_convert(c(32, 70), FtoC=TRUE, test_options)
temp_convert(c(100, 18), FtoC=FALSE, test_options)

# without option1 as an argument of options
bad_options <- list("option4" <- 3, "option5" <- 2)
temp_convert(c(32, 70), FtoC=TRUE, bad_options)
