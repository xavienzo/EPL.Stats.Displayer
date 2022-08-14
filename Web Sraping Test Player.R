page <- "https://www.transfermarkt.co.uk/manchester-united/startseite/verein/985/saison_id/2011"

scraped_page <- read_html(page)

PlayerNames  <- scraped_page %>% html_nodes(".spielprofil_tooltip") %>% html_text() %>% as.character()
Position <- scraped_page %>% html_nodes(".inline-table tr+ tr td") %>% html_text() %>% as.character()
TransferValue <- scraped_page %>% html_nodes(".rechts.hauptlink") %>% html_text() %>% as.character()
Club <- sapply(page, function(x) strsplit(x, '/')[[1]][4])

df <- data.frame(PlayerNames[seq(1,length(PlayerNames),2)], Position, TransferValue, Club)



page <- "https://www.espn.com/soccer/team/stats/_/id/369/league/ENG.1/season/2016"

scraped_page <- read_html(page)

Player  <- scraped_page %>% html_nodes(".InnerLayout__child--dividers:nth-child(1) .InnerLayout__child--dividers .AnchorLink") %>% html_text() %>% as.character()
GamesPlayed <- scraped_page %>% html_nodes(".InnerLayout__child--dividers:nth-child(1) .InnerLayout__child--dividers .Table__TD:nth-child(3) .tar") %>% html_text() %>% as.character()
Goal.Assist <- scraped_page %>% html_nodes(".tar+ .Table__TD .tar") %>% html_text() %>% as.character()
Goals <- Goal.Assist[1:(length(Goal.Assist)/2)]
Assists <- Goal.Assist[((length(Goal.Assist)/2)+1):length(Goal.Assist)]
df <- data.frame(Player, GamesPlayed, Goals, Assists)


URL <- "https://www.premierleague.com/stats/top/players/goals?co=1&se=-1&co=1&cl=-1&iso=-1&po=-1?se=-1"
WS <- read_html(URL)
Goal.Assist <- scraped_page %>% html_nodes(".playerName strong") %>% html_text() %>% as.character()
Goal.Assist