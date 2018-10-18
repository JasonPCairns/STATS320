# 0 probability for anything other than the 3 options below
P <- matrix(rep(0,5^2),nrow = 5)

for (i in 0:4){
  for (j in 0:4){

    # to keep the same number of emails (nonzero)
    if (i == j & j>=1){
      P[i+1,j+1] <- (2/3)*dbinom(0,5,0.2)
    }
    
    # to increase to a higher number of emails
    if (j>i){
      P[i+1,j+1] <- (2/3)*dbinom((j-i),5,0.2)
    }
  }
}

# to go from a number of emails to no emails, or to maintain zero emails
for (i in 0:4){
  P[i+1,1] <- 1 - rowSums(P)[i+1]
}
P
