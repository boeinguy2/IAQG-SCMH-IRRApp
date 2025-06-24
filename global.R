
#############################################################################################

### General Functions

decimalplaces <- function(x) {
  #browser()
  #x <- as.numeric(format(x, scientific = FALSE))
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    #nchar(sub("^-?\\d*\\.?","",format(y,scientific=F)))
    y <- (nchar(strsplit(sub('0+$', '', format(x,scientific=F)), ".", fixed = TRUE)[[1]][[2]]))
    #y <- (nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]]))
    #y <- as.numeric(format(y, scientific = FALSE))
    y
  } else {
    return(0)
  }
}
