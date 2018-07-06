#' Example of what textables can be used to do
#'
#' @description
#' This file contains an example of what the functions contained
#'  in the textables package can be used to do.
#'

library(data.table)
library(stringr)

dd <- data.table(group = c("Full Sample", "Subsample"), coef = c(1.2, 3.42),
                 se = c(.6, .481), p = c(.051, .0), N = c(1234567, 891011))

colspec <- "lcc"
numcol <- nchar(colspec)

outtab <- c(
  beginTable(colspec),
  toprule(),
  midrule(),
  titlerow(" ", "Results at the Worker-level", numcol - 1),
  stringrow(" ", dd$group),
  partmidrule(2, numcol),
  numrow("Effect of X on Y", dd$coef, pvec = dd$p, dec = 3),
  numrow(" ", dd$se, dec = 3, se = T),
  midrule(),
  numrow("Sample Size (1,000)", dd$N / 1000, dec = 0),
  midrule(),
  bottomrule(),
  endTable()
)

## only do this if you want to be able to compile the table as a stand-alone document
outtab <- c(beginDoc(), outtab, endDoc())

## export the table as .tex
openfile <- file("inst/quiz4_example.tex")
writeLines(outtab, openfile)
close(openfile)
