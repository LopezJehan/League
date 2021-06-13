library(League)

##### Original data #####
# all_data <- as.data.frame(read.csv("inst/shiny/data/2021_data.csv"))
# save(all_data, file = "inst/shiny/data/all_data.RData")
load("data/all_data.RData")

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

data_LFL <- all_data %>% 
  filter(league == "LFL") %>% 
  mutate(event = ifelse(playoffs == 1, paste0(split, " playoffs"), split))
