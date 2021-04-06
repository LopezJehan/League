library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(fmsb)

##### Original data #####
all_data <- as.data.frame(read.csv("data/2021_data.csv"))

##### Functions #####
calculate_kda <- function(kills, deaths, assists){
  if(deaths == 0){
    res <- kills + assists
  } else {
    res <- round((kills + assists)/deaths, 1)
  }
  res
}

calculate_kp <- function(kills, assists, teamkills){
  if(teamkills == 0){
    res <- 0
  } else {
    res <- round(100*(kills + assists)/teamkills, 1)
  }
  res
}

calculate_ks <- function(kills, teamkills){
  if(teamkills == 0){
    res <- 0
  } else {
    res <- round(100*kills/teamkills, 1)
  }
  res
}

calculate_ds <- function(deaths, teamdeaths){
  if(teamdeaths == 0){
    res <- 0
  } else {
    res <- round(100*deaths/teamdeaths, 1)
  }
  res
}

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
