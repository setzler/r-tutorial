
# Arjun Gopinath - Aug 1 2018
# R Tutorial Quiz 1

library(data.table)

# Defining the function to convert temperature between Celsius and Fahrenheit.
conv_temp <- function(temperature, FtoC = TRUE, options) {

  # Check if temperature is a numeric vector.
  if (!(is.numeric(temperature))) {
    stop("Temperature must be a numeric vector.")
  }
  
  # Check if FtoC is a boolean.
  if ((!(is.logical(FtoC))) || (length(FtoC) != 1)) {
    stop("FtoC must be a single Boolean variable.")
  }  
  
  # Check if options is a list.
  if (!(class(options) == "list")) {
    stop("Options must be a list.")
  }
  

# Different examples to prove that the code works.

conv_temp(c(25, 32, 100, 40 ), T, list("option1"))
conv_temp(c(0, -27, 200, 40 ), F, list("option1"))
conv_temp(temperature = c(25, 32, 100, 40 ), options = list("option2", "option1"))
conv_temp(c(23, 45), F, list("option2", "option3"))
conv_temp(c(23, 45), F, c("option1", "option2"))


