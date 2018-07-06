

stringrow <- function(title, array, spacer = "\\\\"){

  y <- paste(paste0(" & ", array), collapse = "")
  return(paste0(title, y, spacer))

}

