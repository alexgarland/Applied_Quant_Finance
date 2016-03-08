find_three_highest <- function(entered_row){
  n <- order(entered_row, decreasing=T)[1:3]
  return(n+1)
}

find_three_lowest <- function(entered_row){
  n <- order(entered_row, decreasing=F)[1:3]
  return(n+1)
}

return_acov <- function(entered_column){
  n <- length(entered_column)
  return(cov(entered_column[1:(n-1)], entered_column[2:n]))
}

collapse_year <- function(entered_column){
  n <- length(entered_column)
  twelve_returns <- rep(NA, n)
  for(i in 12:n){
    temp <- 1 + entered_column[(i-11):i]
    twelve_returns[i] <- prod(temp) - 1
  }
  return(twelve_returns)
}

collapse_212 <- function(entered_column){
  n <- length(entered_column)
  twelve_returns <- rep(NA, n)
  for(i in 12:n){
    temp <- 1 + entered_column[(i-11):(i-1)]
    twelve_returns[i] <- prod(temp) - 1
  }
  return(twelve_returns)
}