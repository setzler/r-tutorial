#' Function to begin table within LaTeX document
#'
#' @description
#' This function begins a table within a LaTeX document.
#'
#' @param aligns This argument indicates the alignment to
#'  be used in each column and the vertical lines to insert.
#'  See https://en.wikibooks.org/wiki/LaTeX/Tables for more
#'  information (character).
#'

beginTable <- function(aligns) {
  if (!is.character(aligns)){
    stop(sprintf("Input `aligns` must be character but is %s.\n", class(aligns)))
  }
  return(paste0("\\begin{tabular}{", aligns, "}"))
}
