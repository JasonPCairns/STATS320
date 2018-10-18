set.seed(1234)

RookReturnTime <- function(){
  starting_state <- c(8,8) # top right corner
  curr <- starting_state  # current postion
  state_choices <- 1:8 
  row_column_choice <- 1:2
  time <- 0
  while (TRUE) {
    row_column <- sample(row_column_choice,1) # uniformly move horizontally/vertically
    curr[row_column] <- sample(state_choices,1) # uniformly move within row/column
    time <- time + 1
    if (identical(curr,starting_state)){ # test for return to start position
      return(time)
    }
  }
}

NRookReturnTimes <- function(N = 10000){
  times <- rep(NA,N)
  for (i in 1:N){
    times[i] <- RookReturnTime()
  }
  cat("Number of simulations: ",N, "\nE(T):   ", mean(times), 
      "\nVar(T): ", var(times), "\nsd(t):  ", sd(times))
}

NRookReturnTimes()