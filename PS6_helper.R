find_three_highest <- function(entered_row){
  n <- order(entered_row, decreasing=T)[1:3]
  return(n+1)
}

find_three_lowest <- function(entered_row){
  n <- order(entered_row, decreasing=F)[1:3]
  return(n+1)
}