library(League)

##### Original data #####
all_data <- as.data.frame(read.csv("data/2021_data.csv"))

##### Smaller datasets #####
data_LCK <- all_data %>% 
  filter(league == "LCK")  %>% 
  mutate(event = ifelse(playoffs == 1, paste0(split, " playoffs"), split))

data_LPL <- all_data %>% 
  filter(league == "LPL")  %>% 
  mutate(event = ifelse(playoffs == 1, paste0(split, " playoffs"), split))

data_LEC <- all_data %>% 
  filter(league == "LEC")  %>% 
  mutate(event = ifelse(playoffs == 1, paste0(split, " playoffs"), split))

# data_LFL <- all_data %>% filter(league == "LFL")

##### Stats #####

stats_LEC_players <- data_LEC %>% 
  group_by(player, team) %>% 
  summarise(
    kills = sum(kills),
    deaths = sum(deaths),
    assists = sum(assists)) %>% 
  mutate(kda = calculate_kda(kills, deaths, assists))

stats_LEC_teams <- stats_LEC_players[stats_LEC_players$player == "",]
stats_LEC_players <- stats_LEC_players[stats_LEC_players$player != "",]