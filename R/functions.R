#' @export
calculate_kda <- function(kills, deaths, assists){
  if(deaths == 0){
    res <- kills + assists
  } else {
    res <- round((kills + assists)/deaths, 1)
  }
  res
}

#' @export
calculate_kp <- function(kills, assists, teamkills){
  if(teamkills == 0){
    res <- 0
  } else {
    res <- round(100*(kills + assists)/teamkills, 1)
  }
  res
}

#' @export
calculate_ks <- function(kills, teamkills){
  if(teamkills == 0){
    res <- 0
  } else {
    res <- round(100*kills/teamkills, 1)
  }
  res
}

#' @export
calculate_ds <- function(deaths, teamdeaths){
  if(teamdeaths == 0){
    res <- 0
  } else {
    res <- round(100*deaths/teamdeaths, 1)
  }
  res
}