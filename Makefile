STA523_final_project.html: STA523_final_project.Rmd players_data.rds
	Rscript -e "rmarkdown::render('STA523_final_project.Rmd')"
  
players_data.rds: parse_data.R data/games.rds data/boxscores.rds data/player_names.rds
	Rscript parse_data.R

data/games.rds: get_games.R
	Rscript get_games.R

data/boxscores.rds: get_boxscores.R
	Rscript get_boxscores.R
  
data/player_names.rds: get_player_names.R
	Rscript get_player_names.R


clean:
	rm -f STA523_final_project.html
	rm -rf data/