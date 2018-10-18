#adjacency matrix of Question 4 graph
mat <- matrix(c(0,0,1,1,0,0,1,0,0,
                1,0,1,0,0,0,0,0,0,
                1,1,0,0,0,0,0,0,0,
                0,0,0,0,0,0,1,0,0,
                0,0,0,1,0,1,0,0,0,
                0,0,0,0,0,0,1,0,0,
                0,0,1,0,0,0,0,1,1,
                0,0,0,0,0,0,1,0,0,
                0,0,0,0,0,0,1,0,0),nrow=9,byrow=TRUE)

GetM <- function (M){
  size.M <- nrow(M)
  distscores <- rep(NA,size.M)
  for (i in 1:size.M){
    distscores[i] <- 1/sum(M[i,])
  }
  for (i in 1:size.M){
    for (j in 1:size.M){
      if (M[i,j] == 1){
        M[i,j] <- distscores[i]
      }
    }
  }
  return (M)
}

GetS <- function(M){
  size.M <- nrow(M)
  alpha <- 0.85
  Z <- rep(1/size.M, size.M)
  v <- Z
  vP <- alpha*v%*%M + (1-alpha)*Z
  curr = c(rep(0,size.M))
  for (i in 1:size.M){
    while (vP[i] - curr[i] > 0.0000005){
      curr <- vP
      vP <- alpha*vP%*%M + (1-alpha)*Z
    }
  }
  return(vP)
}

GetS(GetM(mat))