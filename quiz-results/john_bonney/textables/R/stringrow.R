#' Function to create rows of text in LaTeX table.
#'
#' @description This function creates rows of text (in
#'  characters) in a LaTeX table.
#'
#' @param title The title of the row (character).
#' @param array The array that contains the information
#'  that will be in the row.
#' @param spacer The LaTeX code for spacing between this
#'  row and the next row (character).

stringrow <- function(title, array, spacer = "\\\\"){
  if (!is.character(title)){
    stop(sprintf("Input `title` must be character but is %s.\n", class(title)))
  }
  if (!is.character(spacer)){
    stop(sprintf("Input `spacer` must be character but is %s.\n", class(spacer)))
  }
  y <- paste(paste0(" & ", array), collapse = "")
  return(paste0(title, y, spacer))
}

