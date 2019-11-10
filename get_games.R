library(tidyverse)
library(rvest)

get_games <- function(year){
  games <- read_html(paste0("http://www.espn.com/mens-college-basketball/team/schedule/_/id/150/season/", 
                            year))
  scores <- games %>% 
    html_nodes(".Table2__td:nth-child(3)") %>% 
    html_text() %>% 
    .[which(. != "Result")]
  dates <- games %>% 
    html_nodes(".Table2__td:nth-child(1) span") %>% 
    html_text() %>% 
    .[which(. != "Date")] %>% 
    .[which(scores != "Postponed")]
  opponents <- games %>% 
    html_nodes(".tc+ span a") %>% 
    html_text() %>% 
    .[which(scores != "Postponed")]
  scores <- scores %>% 
    .[which(. != "Postponed")]
  
  df <- data_frame(
    year = rep(year, length(scores)),
    date = dates,
    opponent = opponents %>% str_trim(),
    outcome = scores %>% str_extract_all("[LW]") %>% unlist(),
    score = scores %>% str_extract_all("[0-9]+-[0-9]+")
  ) %>% 
    mutate(win = ifelse(outcome == "W", 1, 0),
           points_duke = ifelse(win, 
                           score %>% str_extract_all("^[0-9]+"),
                           score %>% str_extract_all("[0-9]+$")) %>% as.numeric(),
           points_other = ifelse(win, 
                                score %>% str_extract_all("[0-9]+$"),
                                score %>% str_extract_all("^[0-9]+")) %>% as.numeric()
    ) %>% 
    dplyr::select(year, date, opponent, win, points_duke, points_other)
  return(df)
}

years <- 2005:2018
game_history <- map_df(years, function(x){get_games(x)})

write_rds(game_history, path = "data//games.rds")
