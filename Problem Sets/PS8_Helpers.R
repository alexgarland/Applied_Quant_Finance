build_nav <- function(col){
  n <- length(col)
  nav <- 1
  time_nav <- c()
  for (i in 1:n){
    if (is.na(col[i])){
      nav <- nav * 1
      time_nav <- c(time_nav, nav)
    }
    else{
      nav <- nav * (1+col[i])
      time_nav <- c(time_nav, nav)
    }
  }
  return(time_nav)
}

find_mdd <- function(col_nav){
  MDD = 0
  peak_period <- 0
  period_dif <- 0
  peak = -99999
  n <- length(col_nav)
  for(i in 1:n){
    if(col_nav[i] > peak){
      peak = col_nav[i]
      peak_period <- i
    }
    drawdown <- (peak - col_nav[i]) / peak
    if(drawdown > MDD){
      MDD <- drawdown
      period_dif <- i - peak_period
    }
  }
  return(c(MDD, period_dif))
}