#' Function to create title row in LaTeX document.
#'
#' @description
#' This function creates a title row in LaTeX.
#'
#' @param panel The name of the panel (character).
#' @param title The title of the row (character).
#' @param num (numeric).

titlerow <- function(panel, title, num) {
  if (!is.character(panel)){
    stop(sprintf("Input `panel` must be character but is %s. \n", class(panel)))
  }
  if (!is.character(title)){
    stop(sprintf("Input `title` must be character but is %s. \n", class(title)))
  }
  if (!is.numeric(num)){
    stop(sprintf("Input `array` must be numeric but is %s. \n", class(num)))
  }
  return(paste0("\\textbf{", panel, "}"," & ",
                sprintf("\\multicolumn{%d}{c}{\\textbf{", num), title, "}} \\\\"))
}

