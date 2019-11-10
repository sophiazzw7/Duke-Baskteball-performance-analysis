library(tidyverse)
library(janitor)

games        <- readRDS("data//games.rds")
boxscores    <- readRDS("data//boxscores.rds")
player_names <- readRDS("data//player_names.rds")

players_data <- merge(games, boxscores, by = c("year", "date"))
players_data <- players_data %>% 
  mutate(month = date %>% strsplit(" ") %>% map(2) %>% unlist(),
         date = ifelse(month %in% c("Jan", "Feb", "Mar", "Apr"),
                       paste(date, year), paste(date, year - 1)) %>% 
           as.Date("%a, %b %d %Y"),
         year = paste0(year - 1, "-", year)
  ) %>%
  rename(season = year) %>% 
  select(-month)

players_data <- merge(players_data, player_names, 
                      by = c("player", "season")) %>% 
  rename(name = Name) %>% 
  select(-player) %>% 
  clean_names() %>% 
  gather(category, quantity, points, assists, blocks, steals, rebounds, fouls)


write_rds(players_data, path = "players_data.rds")