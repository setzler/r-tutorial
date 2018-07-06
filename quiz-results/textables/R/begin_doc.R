#' Function to begin LaTeX document
#'
#' @description
#' This function begins a LaTeX document.
#'

beginDoc <- function() {
  return(c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{graphicx}",
    "\\usepackage[margin=1in]{geometry}",
    "\\begin{document}"
  ))
}
