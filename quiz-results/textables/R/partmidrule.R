#' Function to draw horizontal lines  in LaTeX.
#'
#' @description
#' This function draws horizontal lines across the columns specified.
#'
#' @param start The column where the line begins (numeric).
#' @param stop The column where the line ends (numeric).
#'

partmidrule <- function(start, stop) {
  if (!is.numeric(start)){
    stop(sprintf("Input `start` must be character but is %s.\n", class(start)))
  }
  if (!is.numeric(stop)){
    stop(sprintf("Input `stop` must be character but is %s.\n", class(stop)))
  }
  return(sprintf("\\cline{%s-%s}", start, stop))
}
