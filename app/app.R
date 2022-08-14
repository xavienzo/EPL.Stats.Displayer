#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load library
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggthemes)
library(rvest)
library(wesanderson)


#load datasets
PlayerGoals <- read.csv('./Data/PlayerGoals.csv')
PlayerInfo <- read.csv('./Data/PlayerInfo_clean.csv')
TeamInfo <- read.csv('./Data/TeamInfo.csv')
logo <- read.csv('./Data/Logo.csv')
ProfileLink <- read.csv('./Data/ProfileLink_Scraped.csv')

#TeamInfo cleaning
TeamInfo$home_lineup = sapply(TeamInfo$home_lineup, function(x) strsplit(as.character(x), ' - ')[[1]])
TeamInfo$away_lineup = sapply(TeamInfo$away_lineup, function(x) strsplit(as.character(x), ' - ')[[1]])
TeamInfo$home_yellow_pl = sapply(TeamInfo$home_yellow_pl, function(x) strsplit(as.character(x), ' - ')[[1]])
TeamInfo$away_yellow_pl = sapply(TeamInfo$away_yellow_pl, function(x) strsplit(as.character(x), ' - ')[[1]])

#Merging two player datasets to get full information
PlayerTable <- left_join(PlayerInfo,PlayerGoals,by=c("Player","Year")) %>% select(-c(X.x,X.y))
PlayerTable$Goals[is.na(PlayerTable$Goals)] <- as.integer(0)
PlayerTable$GamesPlayed[is.na(PlayerTable$GamesPlayed)] <- as.integer(0)

#getter functions
get_season <- function(first = 2011, n = 9){
  #' find all seasons in the form of startYear - endYear, e.g. 2011-2012
  #' @first the startYear of the first season
  #' @n number of seasons
  #' return a character vector with all seasons
  start <- first:(first + n - 1)
  end <- (first+1):(first + n)
  start_end <- replicate(n, NA)
  for(i in 1:n){
    start_end[i] <- paste0(as.character(start[i]), "/", as.character(end[i]))
  }
  return(start_end)
}

get_position <- function(){
  #' get all types of position
  #' return a vector of positions
  return(unique(PlayerInfo$Position))
}

get_team <- function(){
  #' get all team names in all seasons
  #' return a vector of team names
  return(sort(unique(append(TeamInfo$home_team, TeamInfo$away_team))))
}

get_away_team <- function(home){
  #' find the list of away teams that have played against the selected home team
  #' @home the home team in a game
  #' return the list of away teams
  away <- TeamInfo$away_team[TeamInfo$home_team == home]
  return(away)
}

get_player <- function(position="Goalkeeper"){
  #' get all player names in a certain position
  #' @position string, the position of players
  #' return a vector of player names in the given position

  #year <- str_sub(season, 1, 4)
  Player <- PlayerInfo$Player[PlayerInfo$Position == position]
  return(Player)
}

get_team_logo <- function(team){
  #' get the html link to the logo of the given team
  #' @team string, team name
  #' return string html link
  return(logo$logo[logo$club == tolower(team)])
}

get_player_img <- function(player){
  #' get the html link to the image of the given player through web scraping
  #' @player string, player name
  #' return string html link
  link <- ProfileLink$ProfileLink[ProfileLink$Player==player] #find the html profile link of the player
  page <- read_html(link)
  imglink <- page %>% html_node(".dataBild img") %>% html_attr("src") #find the image link attached to the profile page
  return(imglink)
}

get_tb1 <- function(team1, team2){
  #' cunstruct the table of previous game info and results
  #' @team1 home team name
  #' @team2 away team name
  #' return a cleaned table with previous game info and results extracted from TeamInfo dateset
  TeamInfo <- TeamInfo %>% filter(date != '')
  TeamInfo$dateObj = as.Date(TeamInfo$date, '%m/%d/%Y')
  goalResults <- TeamInfo %>% 
    filter(home_team %in% c(team1) & 
             away_team %in% c(team2)) %>%
    select(c('home_team', 'away_team','season', 'date', 'city', 'stadium', 'home_score',
             'away_score',  'result', 'dateObj', 'home_goals', 'away_goals')) %>%
    mutate(goal_diff = home_score - away_score)

  return(goalResults)
}

get_date <- function(tb){
  #'get all dates from the input table
  #'return a vector of date without NAs
  return(na.omit(tb$dateObj))
}

get_gmStats <- function(team1, team2, inputdate){
  #' cunstruct the table of the game statistics on a specific date between two teams
  #' @team1 home team name
  #' @team2 away team name
  #' @inputdate datetime object, the game date
  #' return a cleaned table with the game statistics on the date between home and away teams
  TeamInfo <- TeamInfo %>% filter(date != '')
  TeamInfo$dateObj = as.Date(TeamInfo$date, '%m/%d/%Y')
  gmStats <- TeamInfo %>% 
    filter(home_team %in% c(team1) & 
             away_team %in% c(team2) & 
             dateObj == inputdate) %>%
    select(c('home_shotsON','away_shotsON',
             'home_shots','away_shots','home_corners',
             'away_corners','home_offsides','away_offsides','home_yellows',
             'away_yellows','home_reds','away_reds','home_fouls','away_fouls')) %>%
    gather(key = 'name', value = 'stats', home_shotsON:away_fouls) %>% 
    mutate(team = substr(name, 1,4)) %>% 
    mutate(type = substr(name, 6, nchar(name)))
  gmStats$stats[gmStats$team == 'home'] <- 0-gmStats$stats[gmStats$team == 'home']
  return(gmStats)
}

get_avgGmStats <- function(team1, team2){
  #' cunstruct the table of the average past game statistics between two teams
  #' @team1 home team name
  #' @team2 away team name
  #' return a cleaned table with the average game statistics home and away teams
  TeamInfo <- TeamInfo %>% filter(date != '')
  avgStats <- TeamInfo %>% 
    filter(home_team %in% c(team1) & 
             away_team %in% c(team2)) %>% 
    select(c('home_shotsON','away_shotsON','home_shots','away_shots','home_corners',
             'away_corners','home_offsides','away_offsides','home_yellows',
             'away_yellows','home_reds','away_reds','home_fouls','away_fouls',
             'home_score', 'away_score')) %>% 
    summarize_all(mean) %>% 
    gather(key = 'name', value = 'stats', home_shotsON:away_score) %>%
    mutate(team = substr(name, 1,4)) %>% 
    mutate(type = substr(name, 6, nchar(name)))
  avgStats$stats[avgStats$team == 'home'] <- 0-avgStats$stats[avgStats$team == 'home']
  return(avgStats)
}

get_tab_player <- function(player){
  #' generate a general info table for the input player
  #' @player name of the player
  #' return a dataframe with player's market values, clubs, 
  #' number of game played and number of goals in each year
  PlayerTable <- PlayerTable %>%
    filter(Player == player) %>% 
    select(Year, MarketValue, Club, GamesPlayed, Goals)
  colnames(PlayerTable) <- c("Year", "Market Value \n (in million Euros)", "Club", "Games Played", "Goals")
  #colnames(PlayerTable) <- stringr::str_replace_all(colnames(PlayerTable), "\\n", "<br>")
  #kable(PlayerTable)
  return(PlayerTable)
}

get_stats_plot <- function(position){
  #' generate a table of average statistics for all players in a certain position
  #' @position position of players
  #' return a average statistics  table including market value, goals and 
  #' game played for players in the input position
  StatsPlot <- PlayerTable %>% 
    group_by(Player) %>% 
    summarize(AvgValue=mean(MarketValue, na.rm=TRUE), AvgGoals=mean(Goals/GamesPlayed), 
              AvgGamesPlayed=mean(GamesPlayed), Position=Position) %>% 
    unique()
  StatsPlot$AvgGoals[is.na(StatsPlot$AvgGoals)] <- 0 #set NAs to 0
  StatsPlot <- StatsPlot %>% na.omit() %>% filter(Position == position)
  return(StatsPlot)
}

get_player_comp <- function(player){
  #' get additional information of a certain player for comparison
  #' @player player name
  #' return a table with additional information of the input player
  playercomp <- PlayerTable %>% 
    filter(Player == player) %>% 
    summarize(ClubChange = length(unique(Club)),AvgGamesPlayed = mean(GamesPlayed),
              AvgGoals = mean(Goals/GamesPlayed, na.rm = T), AvgValue = mean(MarketValue, na.rm = T))
  
  return(playercomp)
}

sidebarPanel2 <- function (..., out = NULL, width = 4){
  #' creating a siderbar panel attaching a division / out space for additional content
  #' @out content of the division, default to NULL
  #' @width width of the division, default to 4
  #' return a sidebar panel with out space
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                navbarPage(
                  "Barclay’s Premier League",
                  tabPanel("Club Standings",
                           
                           #generating sidebar for the club standings tab
                           sidebarPanel(
                             #users are allowed for two inputs: home team and away team
                             selectInput(inputId = "team1Input", "Select home team:", choices = get_team()),
                             uiOutput("team2Output"),
                             uiOutput("compareButtonTeam")
                           ), # sidebarPanel
                           
                           #generating the main panel for club standings tab
                           mainPanel(
                             #1. evenly dividing in the top row with the team logos and text 'vs.'
                             fluidRow(
                               column(4, align="center",
                                   htmlOutput("team1LogoOutput")
                                   ),
                               column(4, h2('vs.'), align="center"
                               ),
                               column(4, align="center",
                                   htmlOutput("team2LogoOutput")
                               )
                             ),
                             #2. Overall past game stats table
                             div(
                               h3('Past Game Statistics'),
                               style = 'overflow-x: scroll', #table is long, scroll allowed
                               tableOutput('tb1') 
                               ),
                             #3. goal difference comparison interactive plot
                             div(
                               h3('Goal Difference Comparison'),
                               verbatimTextOutput("avgGD"), #calculate the average goal difference over the years
                               plotlyOutput("goalDiff", width = 'auto')
                             ),
                             br(),
                             #4. a tab box for game stats comparison plots
                             fluidRow(
                               tabBox(width = 24,
                                #4.1 game stats comparison plot on a user specified date
                                 tabPanel("Game Stats Comparison on Specific Date",
                                          fluidRow(
                                            column(6, align = "center", uiOutput("GameDate")), #user can choose from available dates
                                            column(6, align = "bottom", uiOutput("LookButton")) #then look
                                          ),
                                          
                                          div(
                                            plotlyOutput("dateStats", width = 'auto'),
                                            uiOutput('rmv') #removes plot once new teams are selected
                                          )
                                  ),
                                 #4.2 average game stats comparison plot over the years
                                 tabPanel("Average Game Stats Comparison",
                                          div(
                                            br(),
                                            plotlyOutput("avgGmStats", width = 'auto')
                                          )
                                  )
                               )
                             ),
                             #leave some space at the bottom of the page
                             br(),
                             br()
                             
                           ) # mainPanel closing
                  ), # Team tabPanel closing
                  
                  tabPanel("Player Stats", 
                           
                           #generating sidebar for the player stats tab
                           sidebarPanel2(
                             selectInput(inputId = "positionInput", "Choose a position:", choices = sort(get_position())),
                             uiOutput("playerOutput1"),
                             uiOutput("playerOutput2"),
                             uiOutput("compareButtonPlayer"),
                             br(),
                             em(p("* Player images are webscrapped on-click so it might take some time."), align="center"),
                             tags$style(type='text/css', '#PlayerGuide {white-space: pre-wrap;background-color: rgba(0,0,0,0);font-family:verdana}'),
                             out = textOutput("PlayerGuide") #attaching additional guiding content
                           ), # sidebarPanel end
                           
                           #generating main panel for the player stats tab
                           mainPanel(
                             fluidRow(
                               tabBox(
                                 width = 12,
                                 
                                 #1. first tab: player comparison
                                 tabPanel("Stats",
                                          fluidRow(
                                            br(),
                                            
                                            #1.1 first player info
                                            column(6, textOutput("playerText1"), align="center", #inserting header
                                                   br(),
                                                   div(htmlOutput("player1imgOutput")), #inserting image
                                                   br(),
                                                   HTML('<center><div id="TablePlayer1Output" class="shiny-html-output"></div></center>'), #inserting stats table
                                                   #attaching additional information for comparison analysis
                                                   verbatimTextOutput("PlayerCompOutput1"),
                                                   verbatimTextOutput("PlayerCompOutput3"),
                                                   verbatimTextOutput("PlayerCompOutput5"),
                                                   verbatimTextOutput("PlayerCompOutput7"),
                                                   tags$style(type='text/css', '#PlayerCompOutput1 {white-space: pre-wrap;background-color: rgba(0,0,0,0);font-family:verdana}'),
                                                   tags$style(type='text/css', '#PlayerCompOutput3 {white-space: pre-wrap;font-family:verdana}'),
                                                   tags$style(type='text/css', '#PlayerCompOutput5 {white-space: pre-wrap;background-color: rgba(0,0,0,0);font-family:verdana}'),
                                                   tags$style(type='text/css', '#PlayerCompOutput7 {white-space: pre-wrap;font-family:verdana}')
                                            ),
                                            
                                            #1.2 second player info
                                            column(6, textOutput("playerText2"), align="center", #inserting header
                                                   br(),
                                                   div(htmlOutput("player2imgOutput")), #inserting image
                                                   br(),
                                                   HTML('<center><div id="TablePlayer2Output" class="shiny-html-output"></div></center>'), #inserting stats table
                                                   #attaching additional information for comparison analysis
                                                   verbatimTextOutput("PlayerCompOutput2"),
                                                   verbatimTextOutput("PlayerCompOutput4"),
                                                   verbatimTextOutput("PlayerCompOutput6"),
                                                   verbatimTextOutput("PlayerCompOutput8"),
                                                   tags$style(type='text/css', '#PlayerCompOutput2 {white-space: pre-wrap;background-color: rgba(0,0,0,0);font-family:verdana}'),
                                                   tags$style(type='text/css', '#PlayerCompOutput4 {white-space: pre-wrap;font-family:verdana}'),
                                                   tags$style(type='text/css', '#PlayerCompOutput6 {white-space: pre-wrap;background-color: rgba(0,0,0,0);font-family:verdana}'),
                                                   tags$style(type='text/css', '#PlayerCompOutput8 {white-space: pre-wrap;font-family:verdana}')
                                            )
                                          )
                                 ),
                                 
                                 #2. Plot overview of all players' market value vs. their performance in the selected position
                                 tabPanel("Position Overview",
                                          br(),
                                          div(
                                            HTML(paste("*The two players selected are displayed in ", tags$span(style="color:red", "red"), sep = ""),"if their historical market values are in record.")
                                          ),
                                          br(),
                                          plotlyOutput("StatsPlotOutput", height = 750)
                                 )
                               )
                             )
                           )
                  ),
                  navbarMenu("User Guide",
                             tabPanel("How To Use",
                                      br(),
                                      em(h3("Welcome to EPL(English/Barclay's Premier League) Analysis App!")),
                                      br(),
                                      h4("In this App, you are able to compare statistics of all soccer clubs and players that have played in EPL from season 2011/2012 to season 2019/2020."),
                                      h3(code("Club Standings")),
                                      h5("1. Choose the team playing at home."),
                                      h5("2. Choose the team that played at away against the home team."),
                                      h5("3. The table will show detailed statistics of all games they played against each other as home vs. away during the 9 seasons."),
                                      h5("4. The plot below will show goal differences of each game with a note showing average goal differences between the two teams, and you can observe directly whether there was any home advantage or absolute dominance."),
                                      h5("5. Under the ",code("Game Stats Comparison on Specific Date"),"subtab, you can select a date in which they competed against each other and see a
                                         straightforward comparison of there game stats differences."),
                                      h5("6. Under the ",code("Average Game Stats Comparison"),"subtab, you can see averaged game stats differences between the two clubs as home vs. away."),
                                      h3(code("Player Stats")),
                                      h5("1. Choose the position."),
                                      h5("2. Select two players in this position that you want to compare."),
                                      h5("3. Under the ",code("Stats"),"subtab, the table will give you an overview of the selected players' career path within the timeframe of interest, 2011-2019. 
                                         You can see what clubs they have served in, how many goals they scored, how their market evaluation changed, and how many times they appeared in the team line-up." ),
                                      h5("4. The output on bottom will show you average stats of all seasons they have played in so that the comparisons will be more straightforward."),
                                      h5("5. Under the ",code("Position Overview"),"subtab, the interactive plot will give you an overview of all players in the position you selected. You can observe the selected players displayed in ",span(style="color:red", "red"),
                                         "if their historical market values are in record, in the meantime you can directly identify the outstanding players in this position with extremely high market values or high scores of goals per game. This plot will show the association of average market evaluation and average goals per game if the player
                                         plays in an offensive position, while it will show the association of average market evaluation and average appearances in line-up per season if the player is in a defensive position."),
                                      ),
                             tabPanel("Reference",
                                      br(),
                                      fluidRow(
                                        column(8,
                                               h4("Reference:"),
                                               p("Our data is mainly webscraped from: ", 
                                                 em(a("Transfermarkt.com", href="https://www.transfermarkt.us/")),", ",
                                                 em(a("ESPN Soccer", href="https://www.espn.com/soccer/")),"; ",
                                                 "and another portion of our data is from the ",
                                                 em(a("Github FootballData Repository", href="https://github.com/jokecamp/FootballData")),". ",
                                                 "All data usage complies with respective terms of use."
                                               )),
                                        column(4,
                                               h4("Contact Us:"),
                                               p("Yanyu Tao: ",
                                                 em(a("yanyu_tao@brown.edu", href="mailto:yanyu_tao@brown.edu"))),
                                               p("Yezhi Pan: ",
                                                 em(a("yezhi_pan@brown.edu", href="mailto:yezhi_pan@brown.edu"))),
                                               p("Priya Gajjar: ",
                                                 em(a("priya_gajjar@brown.edu", href="mailto:priya_gajjar@brown.edu"))),
                                        )
                                      )
                             )
                                      )
                )) # fluidPage


# Define server function  
server <- function(input, output, session) {

  #on-clicking the campare button in club standing tab, generate team1 logo
  displayTeam1Logo <- eventReactive(input$compareButtonTeam,{
    link = get_team_logo(input$team1Input)
    #html output
    output$team1LogoOutput <- renderText({c('<center>', '<img height="100" width="80" src="', link, '">', '</center>')})
  })
  
  #on-clicking the campare button in club standing tab, generate team2 logo
  displayTeam2Logo <- eventReactive(input$compareButtonTeam,{
    link = get_team_logo(input$team2Input)
    output$team2LogoOutput <- renderText({c('<center>', '<img height="100" width="80" src="', link, '">', '</center>')})
  })
  
  #on-clicking the campare button in club standing tab, generate table of previous game info and results
  displayTb1 <- eventReactive(input$compareButtonTeam, {
    goalResults = get_tb1(input$team1Input, input$team2Input) %>% select(-c('dateObj'))
    output$tb1 <- renderTable({
      goalResults
    })
  })
  
  #on-clicking the campare button in club standing tab, generate goal difference plot
  displayGoalDiff <- eventReactive(input$compareButtonTeam,{
    goalResults = get_tb1(input$team1Input, input$team2Input)
    output$goalDiff <- renderPlotly({
      #if the two teams have never gamed in all season
      if(nrow(goalResults) == 0){
        print(ggplotly(ggplot()))
      }else{
        print(
          #tranform ggplot to an interactive plot using plotly 
          fig <- ggplotly(
            ggplot(data = goalResults) +
              geom_line(aes(dateObj, goal_diff)) + 
              geom_point(aes(dateObj, goal_diff, color = result), size = 3) + 
              geom_hline(yintercept=0, linetype="dashed", color = "#2C528C", size=0.5) +
              ylim(c(0 - max(abs(goalResults$goal_diff)),0 + max(abs(goalResults$goal_diff)))) +
              scale_color_discrete(name = "Result") + 
              labs(y = "Goal Difference", x = "Game Date")
          ))
      }
      
    })
    
  })
  
  #on-clicking the campare button in club standing tab, calculate the average goal difference over all seasons
  displayavgGD <- eventReactive(input$compareButtonTeam, {
    goalResults = get_tb1(input$team1Input, input$team2Input)
    averageGoalDiff <- mean(goalResults$goal_diff)
    #text output
    output$avgGD <- renderText({
      paste('Average Goal Difference is ', as.character(averageGoalDiff))
    })
  })
  
  
  #on-clicking the campare button in club standing tab, generate a selection tab of previous game dates for users to look into
  displayGameDate <- eventReactive(input$compareButtonTeam, {
    #UI output
    output$GameDate <- renderUI({
      goalResults = get_tb1(input$team1Input, input$team2Input)
      selectInput(inputId = "GameDate", "Select the date to look into:", choices = get_date(goalResults))
    })
  })
  
  #on-selecting a specific date, generate a look button
  displayLookButton <- eventReactive(input$GameDate, {
    #UI output
    output$LookButton <- renderUI({
      HTML('<button id="LookButton" type="button" 
           class="btn btn-default action-button" 
           style="margin: 15px">Look!</button>')
    })
  })
  
  #on-clicking the look button, generate stats comparison on a specific date of game between the teams
  displayGameStats <- eventReactive(input$LookButton, {
    #get stats table
    statsTb <- get_gmStats(input$team1Input, input$team2Input, input$GameDate)
    #defining breaks and labels
    brks <- seq(-30, 30, 5)
    lbls = c(seq(30, 0, -5), seq(5, 30, 5))
    #plotly output
    output$dateStats <- renderPlotly({
      print(
        fig <- ggplotly(
          ggplot(statsTb, aes(x = type, y = stats, fill = team)) +   # Fill column
            geom_bar(stat = "identity", width = .6) +   # draw the bars
            scale_y_continuous(breaks = brks,   # Breaks
                               labels = lbls) + # Labels
            coord_flip() +  # Flip axes
            labs(title="Game Statistics Comparison on Specific Date") +
            theme_tufte() +
            theme(plot.title = element_text(hjust = .5), 
                  axis.ticks = element_blank()) +   # Center plot title
            scale_fill_brewer(palette = "Dark2", direction = -1) #let home team be green and away team be brick orange
          )
      )
    })
  })
  
  #on-clicking the campare button for reselection of teams, remove the previous generated game stats plot
  hidePlot <- eventReactive(input$compareButtonTeam, {
    output$dateStats <- renderPlotly({plotly_empty()})
  })
  
  #on-clicking the campare button in club standing tab, generate an average game stats comparison plots
  displayAvgGameStat <- eventReactive(input$compareButtonTeam, {
    #get average stats table
    avgStatsTb <- get_avgGmStats(input$team1Input, input$team2Input)
    #defining breaks and labels
    brks <- seq(-30, 30, 5)
    lbls = c(seq(30, 0, -5), seq(5, 30, 5))
    output$avgGmStats <- renderPlotly({
      print(
        fig <- ggplotly(
          ggplot(avgStatsTb, aes(x = type, y = stats, fill = team)) +   # Fill column
            geom_bar(stat = "identity", width = .6) +   # draw the bars
            scale_y_continuous(breaks = brks,   # Breaks
                               labels = lbls) + # Labels
            coord_flip() +  # Flip axes
            labs(title="Average Game Statistics Comparison") +
            theme_tufte() + 
            theme(plot.title = element_text(hjust = .5), 
                  axis.ticks = element_blank()) +   # Centre plot title
            scale_fill_brewer(palette = "Dark2", direction = -1)
        )
      )
    })
  })
  
  output$team2Output <- renderUI({
    selectInput(inputId = "team2Input", "Select away team:", choices = sort(get_away_team(input$team1Input)))
  })
  #========= end club standing tab =========
  
  
  #on-clicking the comparison button in player stats tab, generate player 1 image
  player1img <- eventReactive(input$compareButtonPlayer, {
    link <- get_player_img(input$player1Input)
    output$player1imgOutput <- renderText({
      c('<center>', '<img height=40% width=40% src="', link, '">', '</center>')
    })
  })
  #on-clicking the comparison button in player stats tab, generate player 2 image
  player2img <- eventReactive(input$compareButtonPlayer, {
    link <- get_player_img(input$player2Input)
    output$player2imgOutput <- renderText({
      c('<center>', '<img height=40% width=40% src="', link, '">', '</center>')
    })
  })
  #on-clicking the comparison button in player stats tab, generate player 1 header
  displayplayer1text <- eventReactive(input$compareButtonPlayer, {
    text1 <- input$player1Input
    output$playerText1 <- renderText({
      text1
    })
  })
  #on-clicking the comparison button in player stats tab, generate player 2 header
  displayplayer2text <- eventReactive(input$compareButtonPlayer, {
    text2 <- input$player2Input
    output$playerText2 <- renderText({
      text2
    })
  })
  
  #on-clicking the comparison button in player stats tab, generate player 1 stats table
  tab1 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_tab_player(input$player1Input)
    output$TablePlayer1Output <- renderTable({
      tab
    })
  })
  
  #on-clicking the comparison button in player stats tab, generate player 2 stats table
  tab2 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_tab_player(input$player2Input)
    output$TablePlayer2Output <- renderTable({
      tab
    })
  })
  
  #on-clicking the comparison button in player stats tab, generate plot overview of all players in the selected position
  displayStatsPlot <- eventReactive(input$compareButtonPlayer, {
    StatsPlot <- get_stats_plot(input$positionInput)
    n=dim(StatsPlot)
    StatsPlot$outstanding <- as.factor(ifelse(StatsPlot$Player == input$player1Input, 1, 0))
    StatsPlot$outstanding[StatsPlot$Player == input$player2Input] = 1
    Attack <- c("Attacking Midfield","Central Midfield","Centre-Forward","Right Winger","Second Striker","Left Winger","Left Midfield","Right Midfield")
    output$StatsPlotOutput <- renderPlotly({
      #if the position input is one of the attacking positions, scatter corresponding plot overview of players' market value vs. goals / game
      if(StatsPlot$Position[1] %in% Attack){
        print(fig <- ggplotly(ggplot(data = StatsPlot, aes(x = AvgValue, y = AvgGoals, label = Player)) +
                          geom_ribbon(stat = "smooth", method = "lm", alpha = .15)+
                          geom_text(size=3, aes(color=outstanding)) +           #highlight the two input players in the plot
                          geom_vline(xintercept = mean(StatsPlot$AvgValue), color = "black")+         #mean market value line indicator
                          annotate(geom = "text", x = mean(StatsPlot$AvgValue)+13, y = 0.9, 
                                   label = "← MEAN MARKET VALUES\nOF ALL PLAYERS IN THIS POSITION", size = 3.5, hjust = -0.1, color = "black")+
                          geom_hline(yintercept = mean(StatsPlot$AvgGoals), color = "black")+         #mean goals/ game line indicator
                          annotate(geom = "text", x = 60, y = mean(StatsPlot$AvgGoals)-0.05, 
                                   label = "↑ MEAN GOALS PER GAME \nOF ALL PLAYERS IN THIS POSITION", size = 3.5, hjust = -0.1, color = "black")+
                          theme_minimal()+
                          scale_color_manual(values = wes_palette("Royal1"))+
                          scale_x_continuous(breaks = seq(0,100,20),  
                                             labels = c("0M","20M", "40M", "60M", "80M", "100M")) +
                          theme(panel.grid.major.x = element_blank(), 
                                panel.grid.minor.x = element_blank(),
                                legend.position = "none")+
                          labs(x = "Average Market Values", y = "Average Goals Per Game")))
        
      #if the position input is not one of the attacking positions, scatter corresponding plot overview of players' market value vs. appearance / year
      }else{
        print(fig <- ggplotly(ggplot(data = StatsPlot, aes(x = AvgValue, y = AvgGamesPlayed, label = Player)) +
                          geom_ribbon(stat = "smooth", method = "lm", alpha = .15)+
                          geom_text(size=3, aes(color=outstanding)) +          #highlight the two input players in the plot
                          geom_vline(xintercept = mean(StatsPlot$AvgValue), color = "black")+         #mean market value line indicator
                          annotate(geom = "text", x = mean(StatsPlot$AvgValue)+13, y = 45, 
                                   label = "← MEAN MARKET VALUES\nOF ALL PLAYERS IN THIS POSITION", size = 3.5, hjust = -0.1, color = "black")+
                          geom_hline(yintercept = mean(StatsPlot$AvgGamesPlayed), color = "black")+         #mean appearance / year line indicator
                          annotate(geom = "text", x = 50, y = mean(StatsPlot$AvgGamesPlayed)-3, 
                                   label = "↑ MEAN APPEARANCES PER YEAR \nOF ALL PLAYERS IN THIS POSITION", size = 3.5, hjust = -0.1, color = "black")+
                          theme_minimal()+
                          scale_color_manual(values = wes_palette("Royal1"))+
                          scale_x_continuous(breaks = seq(0,100,20),  
                                             labels = c("0M","20M", "40M", "60M", "80M", "100M")) +
                          theme(panel.grid.major.x = element_blank(), 
                                panel.grid.minor.x = element_blank(),
                                legend.position = "none")+
                          labs(x = "Average Market Values", y = "Average Games Played Per Year")))
      }
    })
  })
  
  #on-clicking the comparison button in player stats tab, 
  #playerComp 1-8 generates additional stats of the two players for comparison analysis
  
  displayPlayerComp1 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player1Input)
    output$PlayerCompOutput1 <- renderText({
      paste("Played at", as.character(tab[1,1]),"clubs.")
    })
  })
  
  displayPlayerComp2 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player2Input)
    output$PlayerCompOutput2 <- renderText({
      paste("Played at", as.character(tab[1,1]),"clubs.")
    })
  })
  
  displayPlayerComp3 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player1Input)
    output$PlayerCompOutput3 <- renderText({
      paste("Played", format(round(tab[1,2], 2), nsmall = 2),"games per year on average.")
    })
  })
  
  displayPlayerComp4 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player2Input)
    output$PlayerCompOutput4 <- renderText({
      paste("Played", format(round(tab[1,2], 2), nsmall = 2),"games per year on average.")
    })
  })
  
  displayPlayerComp5 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player1Input)
    output$PlayerCompOutput5 <- renderText({
      paste("Scored", format(round(tab[1,3], 2), nsmall = 2),"goals per game on average.")
    })
  })
  
  displayPlayerComp6 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player2Input)
    output$PlayerCompOutput6 <- renderText({
      paste("Scored", format(round(tab[1,3], 2), nsmall = 2),"goals per game on average.")
    })
  })
  
  displayPlayerComp7 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player1Input)
    output$PlayerCompOutput7 <- renderText({
      paste("Average market Value is", format(round(tab[1,4]*1000000, 0), nsmall = 0, scientific = F),"Euros.")
    })
  })
  
  displayPlayerComp8 <- eventReactive(input$compareButtonPlayer, {
    tab <- get_player_comp(input$player2Input)
    output$PlayerCompOutput8 <- renderText({
      paste("Average market Value is", format(round(tab[1,4]*1000000, 0), nsmall = 0, scientific = F),"Euros.")
    })
  })
  #========= end player comparison tab =========
  
  output$PlayerGuide <- renderText({
    Attack <- c("Attacking Midfield","Central Midfield","Centre-Forward","Right Winger","Second Striker","Left Winger","Left Midfield","Right Midfield")
    if(input$positionInput %in% Attack){
      paste('You selected the position', input$positionInput,"! This is an offensive position so the position overview will display the association of average market evaluation and average goals per game. Try selecting defensive positions like Centre-Back and the overview will show differently :) ")
    }else{
      paste('You selected the position', input$positionInput,"! This is a defensive position and players in this position do not take duty in scoring goals, so the position overview will display the association of average market evaluation and average appearances in line-up per season over the set of seasons each player in this position has played in. Try selecting offensive positions like Centre-Forward and the overview will show differently :) ")
    }
  })
  
  #player1 input selection UI
  output$playerOutput1 <- renderUI({
    selectInput(inputId = "player1Input", "Select player 1:", choices = sort(get_player(input$positionInput)))
  })
  
  #player2 input selection UI
  output$playerOutput2 <- renderUI({
    selectInput(inputId = "player2Input", "Select player 2:", choices = sort(get_player(input$positionInput)))
  })
  
  #player compare button UI
  output$compareButtonPlayer <- renderUI({
    HTML('<center><button id="compareButtonPlayer" type="button" class="btn btn-default action-button">Compare</button></center>')
  })
  
  #team compare button UI
  output$compareButtonTeam <- renderUI({
    HTML('<center><button id="compareButtonTeam" type="button" class="btn btn-default action-button">Compare</button></center>')
  })
  
  #activating all event reactive variables
  observe(displayTeam1Logo())
  observe(displayTeam2Logo())
  observe(displayTb1())
  observe(displayGoalDiff())
  observe(displayavgGD())
  observe(displayGameDate())
  observe(displayLookButton())
  observe(displayGameStats())
  observe(hidePlot())
  observe(displayAvgGameStat())
  observe(player1img())
  observe(player2img())
  observe(tab1())
  observe(tab2())
  observe(displayplayer1text())
  observe(displayplayer2text())
  observe(displayStatsPlot())
  observe(displayPlayerComp1())
  observe(displayPlayerComp2())
  observe(displayPlayerComp3())
  observe(displayPlayerComp4())
  observe(displayPlayerComp5())
  observe(displayPlayerComp6())
  observe(displayPlayerComp7())
  observe(displayPlayerComp8())
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)