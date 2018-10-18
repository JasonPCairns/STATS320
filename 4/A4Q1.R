set.seed(123)

# Question 1 vi
## code adapted from week 11 tutorial

me41 = function(T=20, lambda=2, k=4, mu=10) {
  t = 0
  N = 0
  i = 1
  A.t = rexp(1, lambda)
  D.t = Inf
  while(t[i] < T) {
    t[i+1] = min(A.t, D.t)
    N[i+1] = N[i] + ifelse(A.t < D.t, 1, -1)
    if(A.t < D.t) {
      A.t = A.t + rexp(1, lambda)
      if(N[i+1] == 1) D.t = t[i+1] + rgamma(1, k, mu)
    }
    else
      D.t = ifelse(N[i+1] == 0, Inf, t[i+1] + rgamma(1, k, mu))
    i = i + 1
  }
  cbind(t=t, N=N)
}
# Plot
x = me41(T=20)
n = nrow(x)
plot(c(x[1,1],rep(x[-1,1],each=2),x[n,1]), rep(x[,2], each=2),
     type="l", xlab="t", ylab="N(t)", col="blue")

# Question 1 vii

ssql <- function(T=10000){
  x2 = me41(T=T)  
  k = x2[,1] >= .2 * T 
  x3 = x2[k,]        
  d = diff(x3[,1])   
  N = x3[-nrow(x3),2] 
  xp <- rep(NA,max(N))
  for (i in 0:max(N)){
    xp[i] <- i*(sum(d[N == i]) / sum(d))
  }
  return(sum(xp))
}

ssql()
abs(l-(14/5))/max(l,(14/5))

# 3.6% difference!