require(data.table)
require(stringr)
require(dplyr)
require(secr)

parse_data <- function(){
  dt <- fread("input.txt", header = F)[, c("direction", "amount", "row") :=
                                         .(str_extract(V1, "[A-Z]"), as.numeric(str_extract(V1, "[0-9]+")), .I)]
  return(dt)
}

# part 1
substitute_F_with_NSEW <- function(dt){
  dt[,c("total_clockwise_angle", 
        "total_counterclockwise_angle") := 
       .(cumsum(ifelse(direction == "R", amount, 0)),
         cumsum(ifelse(direction == "L", amount, 0)))]
  dt[,total_angle := (total_clockwise_angle-total_counterclockwise_angle)%%360]
  
  direction_having_angle <- c("E", "S", "W", "N")
  names(direction_having_angle) = c("0", "90", "180", "270")
  
  dt[, direction := ifelse(direction=="F", 
                           direction_having_angle[[as.character(total_angle)]],
                           direction), by=row]
  return(dt)
}

get_cumulative_ship_distance <- function(dt){
  for (cardinal in c("E", "S", "W", "N")){
    dt[, (paste0("dx_",cardinal)) := cumsum((ifelse(direction == cardinal, amount, 0)))]
  }
  return(dt)
}

ans1 <- function(dt){
  dt <- parse_data() %>% 
    substitute_F_with_NSEW() %>% 
    get_cumulative_ship_distance()
  return(dt[.N, abs(dx_N-dx_S) + abs(dx_E-dx_W)])
}

# part 2
compute_waypoint_displacement <- function(dt, waypoint_dx0, waypoint_dy0){
  ds = c(waypoint_dx0, waypoint_dy0)
  for(i in 1:dt[,.N]){
    amount = dt[i, amount]
    if(dt[i,direction] == "N"){ds = c(ds[1], ds[2]+amount)}
    else if(dt[i,direction] == "S"){ds = c(ds[1], ds[2]-amount)}
    else if(dt[i,direction] == "E"){ds = c(ds[1]+amount, ds[2])}
    else if(dt[i,direction] == "W"){ds = c(ds[1]-amount, ds[2])}
    else if(dt[i,direction] == "R"){ds = rotate(t(c(ds[1], ds[2])), amount)}
    else if(dt[i,direction] == "L"){ds = rotate(t(c(ds[1], ds[2])), -amount)}
    dt[i, c("waypoint_dx", "waypoint_dy") := .(ds[1], ds[2])]
  }
  return(dt)
}

compute_ship_displacement <- function(dt){
  dt[, ship_dx := cumsum(ifelse(direction == "F", amount*waypoint_dx, 0))]
  dt[, ship_dy := cumsum(ifelse(direction == "F", amount*waypoint_dy, 0))]
  return(dt)
}

ans2 <- function(dt, dx0, dy0){
  dt <- dt %>% 
    compute_waypoint_displacement(dx0, dy0) %>% 
    compute_ship_displacement()
  return(dt[.N, abs(ship_dx) + abs(ship_dy)])
}

ans1(parse_data())
ans2(parse_data(), dx0 = 10, dy0 = 1)