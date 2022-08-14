# Premier League Stats Analysis

  Premier League Stats Analyis a Shiny app that allows users to explore the Barclay's Premier League's historical statistics for the past nine seasons starting from the 2011/2012 season. Launching the application, users can compare team performance in past confrontation record and compare two players' career progress within the timeframe of interest, season 2011/2012 to 2019/2020, with an overview of average stats in a particular position. The link to the app is: https://xavienzo.shinyapps.io/EPL_Stats/

## Datasets
``` source
  Data/
  |-- TeamInfo.csv
  |-- PlayerGoals.csv
  |-- PlayerInfo_Scraped.csv
  |-- LogoLink_Scraped.csv
  |-- ProfileLink_Scraped.csv
  |-- PlayerInfo_clean.csv
  |-- Logo.csv
  ```

### Data Generating Process
Our dataset comes from [Github Football Data Repository](https://github.com/jokecamp/FootballData/tree/master/EPL%202011-2019) that contains massive soccer-related data. From this repository, we download the raw dataset `TeamInfo.csv` listed, in which there is data for each game between various teams over the past nine seasons. The cleaning process is done in our App. 

The second dataset `PlayerGoals.csv` listed is web-scraped from [ESPN soccer](https://www.espn.com/soccer/), which contains data on the number of goals made and games played in a year for each player in the league.. 

The rest of datasets `PlayerInfo_Scraped.csv`, containing player information from all seasons (2011-2019), `LogoLink_Scraped.csv`, containing links to team logos, and `ProfileLink_Scraped.csv`, containing links to profile pictures of each player, are web-scraped from Transfermarkt.com. All of these datasets are arranged during the scraping process accomodating with our usage. `PlayerInfo_clean.csv` and `logo.csv` are locally cleaned versions being used directly in our App.

### Data Origination and Legality
[Github Football Data Repository](https://github.com/jokecamp/FootballData) provides a collective public datasets with all sources specificed. 
The rest of our data is mainly webscraped from: Transfermarkt.com and [ESPN soccer](https://www.espn.com/soccer/). All data usage complies with respective terms of use.

### Privacy Concern
As all of our data are from legal online sources and all information regarding teams and players are public, we will not get into issues regarding privacy.

## Packages 
  Require the following libraries: `shiny`, `shinythemes`,and `shinydashboard` for generating interactive dashboards; `tidyverse` for cleaning data and manipulating datasets; `rvest` for webscraping; `plotly` and `ggthemes` for creating interactive visualizations; `wesanderson` for fancy palettes.

## In-App
1. In tab `Club Standings`, users can choose the team at home and then choose the away team that has contested against it, and the application will first display a table of detailed past game statistics consisting of the home team, away team, season, date, stadium, result, and the number of goals made by each team

2. Next The app will output a plot of the goal differences by date, with a sidenote of the average goal differences between the two teams as home vs. away. Users will be able to observe if there was any pattern of home advantage or absolute dominance.

3. In subtab `Game Stats Comparison on Specific Date`, users can select a date in which the two teams played against each other and see a visualized comparison of the game stats differences. 

4. In subtab `Average Game Stats Comparison`, users can see averaged game stats differences visualization between the two clubs contesting as home vs. away.

5. In tab `Player Stats`, users can select the position and then select two players in this position correspondingly to compare their career statistics.

6. In subtab `Stats`, the application will output an overview of selected players' career path within the past 9 seasons (if they ever played in any of the 9). The table will demonstrate the clubs they have served in, number of goals they scored, change of their market evaluation, and counts of their appearances in the team line-up in each season.

7. The application will also output text blocks that display the average stats of all seasons the two players have played in, so that the comparisons will be more straightforward.

8. In subtab `Position Overview`, the application will generate an interactive plot providing an overview of all players in the position selected. This plot will show the association of average market evaluation and average goals per game if the player plays in an offensive position, while it will show the association of average market evaluation and average appearances in line-up per season if the player is in a defensive position. Users can observe selected players' career performance as compared with all other players in this position (displayed in red if their historical market values are in record) with reference lines to compare with the average stats in the market. In the meantime you can directly identify the outstanding players in this position with extremely high market values or high scores of goals per game.

9. In dropdown tab `User Guide`, users can get in-app guidance on how to use the application under `How To Use`. Contact information and reference to the dataset are also provided in `Reference`.



## Functions
```r
get_season(first = 2011, n = 9)
```
Returns a character vector of seasons in the form of 2011-2012. 
```r
get_position()
```
Returns a character vector of player positions. This function is called in the selectInput for the Player Stats tab to retrieve all possible choices of position. 
```r
get_team()
```
Returns a character vector of teams. This function is called in the selectInput for the Club Standings tab to retrieve all possible choices of home team. 
```r
get_away_team(home)
```
Returns a character vector of teams that have ever played against the input home team. This function is called in the selectInput for the Club Standings tab to retrieve all possible choices of away team given the selected home team.
```r
get_player(position="Goalkeeper")
```
Returns a character vector of players given position. This function is called in the selectInput for the Player Comparison tab to retrieve all possible choices of player in the selected position. 
```r
get_team_logo(team)
```
Returns the link of logo of the user-selected team from the website www.transfermarkt.com. This function is called to retrieve logo html linkwhen displaying team logo. 
```r
get_player_img(player)
```
Web-scrapes the link of the user-selected player image from the website www.transfermarkt.com. This function is called to retrieve image html link when displaying player image. 
```r
get_tb1(team1, team2)
```
Returns a table with selected columns from the "TeamInfo" dataset based on the team1 and team2 arguments. This function is called in eventReactive when displaying the table of past game statistics and in displaying the plot of goal differences. 
```r
get_date(tb)
```
Returns vector of dates from tb argument. This function is called in selectInput so that the user can choose dates in which two teams played. The tb argument is the returned table when get_tb1(team1, team2) is called.
```r
get_gmStats(team1, team2, inputdate)
```
Returns table of game statistics from the user selected teams and game date. This function is called in eventReactive to create a plot of game statistics on a specific date.
```r
get_avgGmStats(team1, team2)
```
Returns table of average game statistics from the user selected teams over all seasons. This function is called to create a plot of average game statistics between the teams.
```r
get_tab_player(player)
```
Returns table of the games played and goals scored in a season for a player from the "PlayerGoals" dataset. 
```r
get_stats_plot(position)
```
Returns table of average statistics including market value, goals and game played for all players in the input position. This function is called in eventReactive for Player comparison tab to generate the scatter plot overview.
```r
get_player_comp(player)
```
Returns table of additional information of player including number of club change, average game played, average goals and average market value. This function is called to generate additional information of the input player for players comparison analysis.
```r
sidebarPanel2(..., out = NULL, width = 4)
```
Creates a sidebar panel attaching with an outer space for additional content. This function is called when developing sidebar panel UI for player comparison tab, and additional guiding content is added to the outer space.