
z <- matrix(rep(NA,10000),nrow=100)
for (x in 0:99){
  for (y in 0:99){
    if (x + y < 100){
      z[x,y] <- (x/100)^3*(y/100)^2
    }
  }
}
wireframe(z, row.values = ((0:99)/100), column.values = ((0:99)/100))
