
# Name: John Bonney
# Date: 7.2.18
# Quiz 1 - Workhorse Objects in R


# Create function to convert C to F and vice versa -------------

temp_convert <- function(temperature, FtoC = TRUE, options) {

  # check that all arguments are of the correct type
  if (!is.numeric(a)) {
    stop(sprintf("Input `a` must be numeric but is %s. \n", class(a)))
  }
  if (!is.logical(FtoC)) {
    stop(sprintf("Input `FtoC` must be logical but is %s. \n", class(FtoC)))
  }
  if (!is.list(options)) {
    stop(sprintf("Input `options` must be list but is %s. \n", class(options)))
  }

  if (FtoC) {
    f_temp <- temperature
    c_temp <- (f_temp - 32) / 1.8
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

# Test the function -------------------------------

test_options <- list("option1" <- 5, "option2" <- 2)
temp_convert(c(32, 70), FtoC = TRUE, test_options)
temp_convert(c(100, 18), FtoC = FALSE, test_options)

# without option1 as an argument of options
bad_options <- list("option4" <- 3, "option5" <- 2)
temp_convert(c(32, 70), FtoC = TRUE, bad_options)
