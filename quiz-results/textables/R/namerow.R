#' Function to label rows.
#'
#' @description This function creates LaTeX code to name rows.
#'
#' @param name The name of the rows (character).
#' @param num The number of &'s (numeric).
#'

namerow <- function(name, num) {
  if (!is.character(name)){
    stop(sprintf("Input `name` must be character but is %s. \n", class(name)))
  }
  if (!is.numeric(num)){
    stop(sprintf("Input `num` must be numeric but is %s. \n", class(num)))
  }
  paste(name, paste(paste(rep(" & ", num), collapse = ""), "\\\\"),
        collapse = "")
}
