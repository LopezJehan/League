#' @export
match <- function(number, data){
  a <- 12*(number-1)+1
  b <- 12*number
  data[a:b,]
}

#' @export
buildTable <- function(number, data){
  matchNb <- match(number, data)
  
  teams <- matchNb[11:12,]
  title <- paste(teams$team[1], teams$result[1], "-", teams$result[2], teams$team[2])
  team1 <- matchNb %>% filter(team == teams$team[1])
  team2 <- matchNb %>% filter(team == teams$team[2]) 
  
  colonne1 <- c(team1$player)
  colonne2 <- c(team1$champion)
  colonne5 <- c(team2$champion)
  colonne6 <- c(team2$player)
  teamskda <- paste(teams$kills, teams$deaths, teams$assists, sep = " - ")
  colonne3 <- paste(team1$kills, team1$deaths, team1$assists, sep = " - ")
  colonne4 <- paste(team2$kills, team2$deaths, team2$assists, sep = " - ")
  
  tab <- data.frame(colonne1, colonne2, colonne3, colonne4, colonne5, colonne6)
  colnames(tab) <- c(rep(teams$team[1],2), teamskda[1], teamskda[2], rep(teams$team[2],2))
  return(list(tab[1:5,], title))
}
