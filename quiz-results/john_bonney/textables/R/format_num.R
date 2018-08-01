#' Function to do format numbers within LaTeX document.
#'
#' @description
#' This function formats numbers in a table within a
#'  LaTeX document.
#'
#' @param x The number(s) to be formatted (numeric).
#' @param dec Number of decimal places (numeric).
#' @param big.mark Used as mark within large numbers; normally,
#'  a comma (as in 1,201,390) (character).
#' @param stat (logical)
#' @param se (logical)
#' @param pvec (numeric)

formatNum <- function(x, dec = 4, big.mark = ",", stat = F, se = F, pvec = -1) {
  if (!is.numeric(x)) {
    stop(sprintf("Input `x` must be numeric but is %s.\n", class(x)))
  }
  if (!is.numeric(dec)) {
    stop(sprintf("Input `dec` must be numeric but is %s.\n", class(dec)))
  }
  if (!is.character(big.mark)) {
    stop(sprintf("Input `big.mark` must be character but is %s. \n", class(big.mark)))
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

  x <- round(x, dec)
  y <- trimws(format(x, big.mark = ",", nsmall = dec, digits = dec,
                     scientific = F))
  if (sum(stat) != 0 & sum(stat) == 1 & length(stat) == 1) {
    y <- paste0(y, "\\%")
  }
  if (sum(se) != 0 & sum(se) == 1 & length(se) == 1) {
    y <- paste0("(", y, ")")
  }
  if (sum(stat) != 0 & sum(stat) >= 1 & length(stat) == length(x) & length(stat) > 1) {
    for (i in 1:length(x)) {
      if (stat[i] == 1) {
        y[i] <- paste0(y[i], "\\%")
      }
    }
  }
  if (sum(se) != 0 & sum(se) >= 1 & length(se) == length(x) & length(se) > 1) {
    for (i in 1:length(x)) {
      if (se[i] == 1) {
        y[i] <- paste0("(", y[i], ")")
      }
    }
  }
  if (length(pvec) == length(x) & max(pvec) >= 0 & min(pvec) <= 1) {
    for (i in 1:length(x)) {
      if (!is.na(pvec[i])) {
        if (pvec[i] <= .01) {
          y[i] <- paste0(y[i], "*")
        }
        if (pvec[i] <= .05) {
          y[i] <- paste0(y[i], "*")
        }
        if (pvec[i] <= .1) {
          y[i] <- paste0(y[i], "*")
        }
      }
    }
  }
  y[grep("NA", y)] <- rep("", 2)
  return(y)
}
