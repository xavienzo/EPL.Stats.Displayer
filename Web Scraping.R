library(rvest)

#Club link per season
URL <- vector()
for(year in 2011:2019){
  URL_club_season <- paste0("https://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1/plus/?saison_id=", year)
  WS <- read_html(URL_club_season)
  URL_names <- WS %>% html_nodes(".hide-for-pad .vereinprofil_tooltip") %>% html_attr("href") %>% as.character()
  URL <- c(URL, paste0("http://www.transfermarkt.com",URL_names))
}
URL <- as.data.frame(URL)

#Player stats per club per season
PlayerInfo <- data.frame()
for(i in 1:nrow(URL)){
  page <- URL[i,]
  scraped_page <- read_html(page)
  PlayerNames  <- scraped_page %>% html_nodes(".spielprofil_tooltip") %>% html_text() %>% as.character()
  Position <- scraped_page %>% html_nodes(".inline-table tr+ tr td") %>% html_text() %>% as.character()
  TransferValue <- scraped_page %>% html_nodes(".rechts.hauptlink") %>% html_text() %>% as.character()
  Club <- sapply(URL[i,], function(x) strsplit(x, '/')[[1]][4])
  Year <- sapply(URL[i,], function(x) strsplit(x, '/')[[1]][9])
  df <- data.frame(PlayerNames[seq(1,length(PlayerNames),2)], Position, TransferValue, Club, Year)
  PlayerInfo <- rbind(PlayerInfo, df)
}

#Club link per season on ESPN
URL <- vector()
for(year in 2011:2019){
  URL_club_season <- paste0("https://www.espn.com/soccer/standings/_/league/ENG.1/season/", year)
  WS <- read_html(URL_club_season)
  URLs <- WS %>% html_nodes(".hide-mobile .AnchorLink") %>% html_attr("href") %>% as.character()
  for(i in 1:length(URLs)){
    IDs <- strsplit(URLs[i], '/')[[1]][6]
    URL <- c(URL, paste0("https://www.espn.com/soccer/team/stats/_/id/",IDs,"/league/ENG.1/season/",year))
  }
}
URL <- as.data.frame(URL)

#Player goals per season per link on ESPN
PlayerGoals <- data.frame()
for(i in 1:nrow(URL)){
  page <- URL[i,]
  scraped_page <- read_html(page)
  Player  <- scraped_page %>% html_nodes(".InnerLayout__child--dividers:nth-child(1) .InnerLayout__child--dividers .AnchorLink") %>% html_text() %>% as.character()
  GamesPlayed <- scraped_page %>% html_nodes(".InnerLayout__child--dividers:nth-child(1) .InnerLayout__child--dividers .Table__TD:nth-child(3) .tar") %>% html_text() %>% as.character()
  Goals <- scraped_page %>% html_nodes(".InnerLayout__child--dividers:nth-child(1) .InnerLayout__child--dividers .tar+ .Table__TD .tar") %>% html_text() %>% as.character()
  #Goals <- Goal.Assist[1:(length(Goal.Assist)/2)]
  #Assists <- Goal.Assist[((length(Goal.Assist)/2)+1):length(Goal.Assist)]
  df <- data.frame(Player, GamesPlayed, Goals)
  PlayerGoals <- rbind(PlayerGoals, df)
}
PlayerGoals

#Player profile link
URL <- vector()
for(year in 2011:2019){
  URL_club_season <- paste0("https://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1/plus/?saison_id=", year)
  WS <- read_html(URL_club_season)
  URL_names <- WS %>% html_nodes(".hide-for-pad .vereinprofil_tooltip") %>% html_attr("href") %>% as.character()
  URL <- c(URL, paste0("http://www.transfermarkt.com",URL_names))
}
URL <- as.data.frame(URL)

ProfileLink <- data.frame()
for(i in 1:nrow(URL)){
  page <- URL[i,]
  scraped_page <- read_html(page)
  Player  <- scraped_page %>% html_nodes(".spielprofil_tooltip") %>% html_text() %>% as.character()
  Profile_path  <- scraped_page %>% html_nodes("#yw1 .spielprofil_tooltip") %>% html_attr("href") %>% as.character()
  Profile <- paste0("http://www.transfermarkt.com", Profile_path)
  #ImgLink <- vector()
  #for(k in Profile){
   # page <- read_html(k)
   # Image <- page %>% html_node(".dataBild img") %>% html_attr("src")
   # ImgLink <- c(ImgLink, Image)
  #}
  df <- data.frame(Player[seq(1,length(Player),2)], Profile[seq(1,length(Profile),2)])
  ProfileLink <- rbind(ProfileLink, df) %>% unique()
}
names(ProfileLink) <- c("Player", "ProfileLink")

#Club Logo link
URL <- vector()
for(year in 2011:2019){
  URL_club_season <- paste0("https://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1/plus/?saison_id=", year)
  WS <- read_html(URL_club_season)
  URL_names <- WS %>% html_nodes(".hide-for-pad .vereinprofil_tooltip") %>% html_attr("href") %>% as.character()
  URL <- c(URL, paste0("http://www.transfermarkt.com",URL_names))
}
URL <- as.data.frame(URL)

LogoLink <- data.frame()
for(i in 1:nrow(URL)){
  page <- read_html(URL[i,])
  club <- sapply(URL[i,], function(x) strsplit(x, '/')[[1]][4])
  logo <- page %>% html_node(".dataBild img") %>% html_attr("src")
  df <- data.frame(club, logo)
  LogoLink <- rbind(LogoLink, df)
  LogoLink <- LogoLink[!duplicated(LogoLink$club),]
}
rownames(LogoLink) <- 1:nrow(LogoLink)



