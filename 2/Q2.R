# This gives fixation time for A allele
FixationTime <- function (A = 50, N = 100){   
  time <- 0
  # Loop forms new generations until fixation
  while (A != 0 & A != N){
    time <- time + 1
    A <- rbinom(1,N,(A/N))
  }
  # Returns time to fixation
  return (time)
}

##################


# This gives fixation variance from large number of simulations
FixVar <- function(M=10000){
  times <- rep(NA,M)
  # Fills in the vector with each entry being a simulated fixation time
  for (i in 1:M){
    times[i] <- FixationTime()
  }
  # returns variance of the vector
  return (var(times))
}