#' Function to do something in LaTeX document
#'
#' @description
#' This does something in a LaTeX document, but I
#'  am not exactly sure what, and I didn't want to
#'  intentionally write something untrue
#'
#' @param title The name of the row (character).
#' @param array The array that will be formatted in the row (numeric).
#' @param dec The number of decimal places (numeric).
#' @param stat (logical)
#' @param se (logical)
#' @param pvec (numeric)
#' @param spacer LaTeX code for space between rows (character).

numrow <- function(title, array, dec = 2, stat = F, se = F,
                   pvec = -1, spacer = "\\\\") {
  if (!is.character(title)){
    stop(sprintf("Input `title` must be character but is %s. \n", class(title)))
  }
  if (!is.numeric(array)){
    stop(sprintf("Input `array` must be numeric but is %s. \n", class(array)))
  }
  if (!is.numeric(dec)){
    stop(sprintf("Input `dec` must be numeric but is %s. \n", class(dec)))
  }
  if (!is.logical(stat)) {
    stop(sprintf("Input `stat` must be logical but is %s.\n", class(stat)))
  }
  if (!is.logical(se)) {
    stop(sprintf("Input `se` must be logical but is %s.\n", class(se)))
  }
  if (!is.numeric(pvec)) {
    stop(sprintf("Input `pvec` must be numeric but is %s.\n", class(pvec)))
  }
  if (!is.character(spacer)) {
    stop(sprintf("Input `spacer` must be character but is %s. \n", class(spacer)))
  }
  x <- formatNum(array, dec = dec, stat = stat, se = se, pvec = pvec)
  y <- paste(paste0(" & ", x), collapse = "")
  return(paste0(title, y, spacer))
}
