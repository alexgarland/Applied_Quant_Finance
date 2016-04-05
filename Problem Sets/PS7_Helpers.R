collapse_212 <- function(entered_column){
  n <- length(entered_column)
  twelve_returns <- rep(NA, n)
  for(i in 12:n){
    temp <- 1 + entered_column[(i-11):(i-1)]
    twelve_returns[i] <- prod(temp) - 1
  }
  return(twelve_returns)
}

collapse_1360 <- function(entered_column){
  n <- length(entered_column)
  sixty_returns <- rep(NA, n)
  for(i in 60:n){
    temp <- 1 + entered_column[(i-59):(i-12)]
    sixty_returns[i] <- prod(temp) - 1
  }
  return(sixty_returns)
}

index.top.N = function(xs, N){
  if(length(xs) > 0) {
    o = order(xs, na.last=FALSE)
    o.length = length(o)
    if (N > o.length) N = o.length
    o[((o.length-N+1):o.length)]
  }
  else {
    0
  }
}