montyhall <- function(n.games){
  change <- 0
  n.wins <- 0
  for (i in 1:n.games){
    car.behind.door <- sample(3,size=1)
    door.you.select.first <- sample(3,size=1)
    doors.available.to.host <- c(1,2,3)[-c(car.behind.door,door.you.select.first)]
    host.selects.door <- doors.available.to.host[1]
    if (change != 0){
      door.you.select.final <- c(1,2,3)[-c(host.selects.door,door.you.select.first)]
    } else {
      door.you.select.final <- door.you.select.first
    }
    if (door.you.select.final == car.behind.door){
      n.wins <- n.wins + 1
    }
  }
  return(n.wins/n.games)
}