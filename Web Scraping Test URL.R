library(rvest)
URL <- "https://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1/plus/?saison_id=2011"
WS <- read_html(URL)
URLs <- WS %>% html_nodes(".hide-for-pad .vereinprofil_tooltip") %>% html_attr("href") %>% as.character()
URLs <- paste0("http://www.transfermarkt.com",URLs)
URLs



URL <- "https://www.espn.com/soccer/standings/_/league/ENG.1/season/2011"
WS <- read_html(URL)
URLs <- WS %>% html_nodes(".hide-mobile .AnchorLink") %>% html_attr("href") %>% as.character()
IDs <- strsplit(URLs[1], '/')[[1]][6]
paste0("https://www.espn.com/soccer/team/stats/_/id/",IDs,"/league/ENG.1/season/2011")



UrlPage <- read_html("https://www.transfermarkt.co.uk/heurelho-gomes/profil/spieler/19059")
Imglink <- UrlPage %>% html_node(".dataBild img") %>% html_attr("src")
Imglink

