count_negs <- function(s){
  x <- strsplit(s, " ")
  v <- length(grep(reg,unlist(x), ignore.case = TRUE, value=TRUE))
  if (v < 2){
    return(0)
  } else {
    return(1)
  }
}