library(tidyverse)
library(rvest)
library(dplyr)
library(RCurl)

data_dir <- "data/"

get_player_names = function(season){
  base_url = "http://www.goduke.com/SportSelect.dbml?DB_OEM_ID=4200&SPID=1845&SPSID=22727&KEY=&Q_SEASON="
  url = paste0(base_url, as.character(season))
  page = read_html(url)
  number = page %>% html_nodes(".number") %>% html_text()
  index = (length(number)-1)/2
  leng = (length(number)+1)/2
  number = number[1:index]
  if (season >=2009){
    name = page %>% html_nodes(".showPopup") %>% html_text()}
  if (season <2009){
    name = page %>% html_nodes(".name") %>% html_text() %>% .[2:leng]}
  position = page %>% html_nodes(".position") %>% html_text() %>% .[1:index]
  height = page %>% html_nodes(".height") %>% html_text() %>% .[1:index]
  weight = page %>% html_nodes(".weight") %>% html_text() %>% .[2:leng]
  year = page %>% html_nodes(".year") %>% html_text() %>% .[1:index]
  home = page %>% html_nodes(".hometown") %>% html_text() %>% .[1:index]
  seas = paste0(as.character(season),"-",as.character(season+1))
  
  info = data.frame(
    Season = seas,
    Number = number,
    Name = name,
    Position = position,
    Height = height,
    Weight = weight,
    Year = year,
    Hometown = home
  )
  
  info = info %>% mutate(
    Number = Number %>% as.character(),
    Position = str_remove_all(Position, "Position:\\s|\\t|\\n"),
    Height = str_remove_all(Height, "\\n|\\t|\\d\\d\\d\\slbs"),
    Weight = str_remove_all(Weight, "\\t|\\n"),
    Year = str_remove_all(Year, "\\t|\\n|Year:\\s") %>%
      plyr::mapvalues(from=c("Fr.", "So.", "Jr.", "Sr.","RFr.","RSo.","RJr.","RSr."), 
                      to=c("Freshman", "Sophomore", "Junior", "Senior", 
                           "Redshirt Freshman","Redshirt Sophomore", "Redshirt Junior", "Redshirt Senior"),
                      warn_missing = FALSE),
    Hometown = str_remove_all(Hometown, "\\t|\\n|Hometown:\\s"))
  
  info$Name = str_remove_all(info$Name, "\\t|\\n")
  for (i in 1:nrow(info)){
    info$Name[i] = substr(info$Name[i],str_locate_all(info$Name, "\\s[A-Z]*[a-z]*[A-Z]*[a-z]*")[[i]][1,2]-
                            ((str_locate_all(info$Name, "\\s[A-Z]*[a-z]*[A-Z]*[a-z]*")[[i]][1,2] -
                                str_locate_all(info$Name, "\\s[A-Z]*[a-z]*[A-Z]*[a-z]*")[[i]][1,1])/2)+1,1000)}
  return (info)
}

seasons = c(1989:2018)

player_names = data.frame()
for (season in seasons){
  df = get_player_names(season)
  player_names = rbind(player_names, df)
}

for (i in 1:nrow(player_names)){
  start_index = str_locate(player_names$Hometown[i], ",\\s")[1,2]
  end_index = str_locate(player_names$Hometown[i], "\\(")[1,1]
  player_names$state[i] = substr(player_names$Hometown[i],start_index+1,end_index-2)
  if (player_names$state[i] %in% c("Ala.", "Ariz.","Ark.", "Calif.", "Colo.","Conn.","Del.","Fla.","Ga.","Ill.", "Ind.",
                                   "Kan.", "Ky.","La.","Md.","Mass.","Mich.","Minn.","Miss.", "Mo.","Nev.", "N.J.",
                                   "N.M.","N.Y.","N.C.", "Okla.", "Pa.","R.I.", "S.C.","Tenn.", "Vt.","Va.","Texas", "Ohio", "Utah", "D.C.",
                                   "Alaska", "Ore.","Ill.\\s")){
    player_names$state[i] = player_names$state[i] %>% 
      plyr::mapvalues(from=c("Ala.", "Ariz.","Ark.", "Calif.", "Colo.","Conn.","Del.","Fla.","Ga.","Ill.", "Ind.",
                             "Kan.", "Ky.","La.","Md.","Mass.","Mich.","Minn.","Miss.", "Mo.","Nev.", "N.J.",
                             "N.M.","N.Y.","N.C.", "Okla.", "Pa.","R.I.", "S.C.","Tenn.", "Vt.","Va.", "Texas", "Ohio", 
                             "Utah", "D.C.", "Alaska", "Ore.","Ill.\\s"), 
                      to=c(",_Alabama", ",_Arizona",",_Arkansas", ",_California", ",_Colorado", 
                           ",_Connecticut",",_Delaware", ",_Florida", ",_Georgia",",_Illinois", ",_Indiana",",_Kansas",
                           ",_Kentucky", ",_Louisiana", ",_Maryland",",_Massachusetts", ",_Michigan",",_Minnesota", 
                           ",_Mississippi",",_Missouri",",_Nevada",",_New_Jersey",",_New_Mexico",",_New_York",",_North_Carolina",
                           ",_Oklahoma", ",_Pennsylvania",",_Rhode Island", ",_South_Carolina", ",_Tennessee", ",_Vermont",
                           ",_Virginia", ",_Texas", ",_Ohio", ",_Utah", ",_D.C.", ",_Alaska", ",_Oregon",",_Illinois"),
                      warn_missing = FALSE)}
  else {player_names$state[i] = ""}
  player_names$city[i] = substr(player_names$Hometown[i],1,start_index-2)
}
#Manually changing this cuz idk what's wrong
player_names$state[236] = ",_Illinois"

for (i in 1:nrow(player_names)){
  wiki_url = "https://en.wikipedia.org/wiki/"
  full_wiki = paste0(wiki_url, as.character(player_names$city[i]), as.character(player_names$state[i]))
  if (RCurl::url.exists(full_wiki)){
    wiki_page = read_html(full_wiki)
    if (identical(wiki_page %>% html_nodes("#coordinates .latitude") %>% html_text(), character(0))==FALSE
        |identical(wiki_page %>% html_nodes("#coordinates .longitude") %>% html_text(), character(0))==FALSE){
      player_names$lati[i] = wiki_page %>% html_nodes("#coordinates .latitude") %>% html_text()
      player_names$long[i] = wiki_page %>% html_nodes("#coordinates .longitude") %>% html_text()}}
  else {player_names$lati[i] = "N/A"
  player_names$long[i] = "N/A"}
}
player_names$lati = gsub('°', ' ', player_names$lati)
player_names$lati = gsub('′', ' ', player_names$lati)
player_names$lati = gsub('″', ' ', player_names$lati)
player_names$lati = gsub(' N', '', player_names$lati)
player_names$lati = gsub(' S', '', player_names$lati)

player_names$long = gsub('°', ' ', player_names$long)
player_names$long = gsub('′', ' ', player_names$long)
player_names$long = gsub('″', ' ', player_names$long)
player_names$long = gsub(' W', ' ', player_names$long)
player_names$long = gsub(' E', '', player_names$long)

for (i in 1:nrow(player_names)){
  if (player_names$lati[i] != "N/A"|player_names$long[i] != "N/A"){
    player_names$lat[i] = measurements::conv_unit(player_names$lati[i], from = 'deg_min_sec', to = 'dec_deg')
    player_names$lon[i] = measurements::conv_unit(player_names$long[i], from = 'deg_min_sec', to = 'dec_deg')
  }
  else {player_names$lat[i] = "N/A"
  player_names$lon[i] = "N/A"}
}

player_names$lat = as.numeric(player_names$lat)
player_names$lon = as.numeric(player_names$lon)

write_rds(player_names, path = "data//player_names.rds")
