
# Arjun Gopinath - Aug 1 2018
# R Tutorial Quiz 1

library(data.table)
library(checkmate)

# Defining the function to convert temperature between Celsius and Fahrenheit.
conv_temp <- function(temperature, FtoC = TRUE, options) {
  
  # Checks to ensure that the inputs to the function are of the correct type.

  # Check if temperature is a numeric vector.
  
  assertNumeric(temperature)

  # Check if FtoC is a boolean.
 
  assertLogical(FtoC, len = 1)
  
  # Check if options is a list.
  
  assertList(options)
  
  # Perform temperature conversion operations.
  
  # Check if temperature is given in F. If true, convert to C
  if (FtoC == TRUE) {
    fahrenheit <- temperature
    celsius <- (temperature - 32) * 5 / 9
  }

  # If false, convert temperature to F
  else {
    celsius <- temperature
    fahrenheit <- (temperature * 9 / 5) + 32
  }

  # Check if options contains option1.
  if (!("option1" %in% options)) {
    stop("options$option1 must exist.")
  }

  # Tabulate results into a data table.
  results_table <- data.table(Celsius = celsius, Fahrenheit = fahrenheit)

  # Return as output.
  return(results_table)
}

# Different examples to prove that the code works.
conv_temp(c(25, 32, 100, 40), T, list("option1"))
conv_temp(c(0, -27, 200, 40), F, list("option1"))
conv_temp(temperature = c(25, 32, 100, 40), options = list("option2", "option1"))
conv_temp(c(23, 45), F, list("option2", "option3"))
conv_temp(c(23, 45), F, c("option1", "option2"))
conv_temp(c(23, 45), c(F, T), list("option1", "option2"))
conv_temp(c(23, 45), F, list("option1", "option2"))
conv_temp("temperature = 25", TRUE, options = list("option1"))
