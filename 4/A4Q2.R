set.seed(1)

# Question 2 b

ratpopulation = function (T=250, N0=10, lambda=0.2, theta=0.4, mu=0.25) {
  t = 0
  N = N0
  i = 1
  A.t = rexp(1,N*lambda+theta)             # first arrival time
  D.t = ifelse(N == 0, A.t,rexp(1,N*mu))   # first departure time
  while (t[i] < T) {
    if(N[i] == 0){                        # no rats = no departure
      t[i+1] = A.t
      N[i+1] = 1
      A.t <- t[i+1] + rexp(1,N[i+1]*lambda+theta)
      D.t <- t[i+1] + rexp(1,N[i+1]*mu)
    }
    else{
      t[i+1] = min(A.t,D.t)
      N[i+1] = N[i] + ifelse(A.t < D.t, 1, -1)
      if(A.t < D.t){                              # is arrival ?
        A.t = t[i+1] + rexp(1,N[i+1]*lambda+theta)   # next arrival time
      }
      else {                                      # is departure
        D.t = ifelse(N[i+1] == 0, t[i+1], t[i+1] + rexp(1,N[i+1]*mu))         # next departure time
        }
      }
  i = i + 1
  }
  cbind (t=t, N=N)
}

# Question 2 c

x1 = ratpopulation()
n1 = nrow (x1)
plot (c(x1[1,1], rep(x1[-1,1], each =2), x1[n1,1]), rep(x1[,2], each =2), col="blue",
      type ="l", xlab ="t (weeks)", ylab ="N(t)", xlim=c(0, 250), ylim=c(0, 50))

x2 = ratpopulation()
n2 = nrow (x2)
lines(c(x2[1,1], rep(x2[-1,1], each =2), x2[n2,1]), rep(x2[,2], each =2), col="red")

x3 = ratpopulation()
n3 = nrow (x3)
lines(c(x3[1,1], rep(x3[-1,1], each =2), x3[n3,1]), rep(x3[,2], each =2), col="green")

# Question 2 d

dist1yr <- function(N = 1000, T = 52){
  vr1yr <- rep(NA,N)
  for (i in 1:N){
    r1yr <- ratpopulation(T=T)
    len <- length(r1yr)
    vr1yr[i] <- r1yr[len]
  }
  return(vr1yr)
}

simdist <- dist1yr()

hist(simdist, xlim=c(0,100), breaks=200, prob=TRUE)
lines(density(simdist))

## Appears to follow Gamma (Erlang?) distribution

# Question 2 e

longavrats <- function(N=100, T=1000){
  avgs <- rep(NA, N)
  for (i in 1:N){
    x1 = ratpopulation(T=T)  
    k = x1[,1] >= .2 * T
    x2 = x1[k,] 
    avgs[i] <- mean(x2[,2])
  }
  return(mean(avgs))
}