library(rvest) #web scraping data
library(plyr) #package to scrape web data

#
#
# function to get seasonal data from pro football archive and clean it up
#
#


get_season <- function(year) {
  # prevents bad inputs
  if (!(year %in% 1970:2023)) {
    print("pick a year between 1970 and 2022")
    return()
  }
  
  url <- paste("https://www.profootballarchives.com/", year, ".html", sep = "") 
  url %>%
    read_html %>% # reads the html website
    html_table() %>% # gets the tables from the website
    .[[3]] -> df # stores table I need
  
  #cleaning data
  df <- as.data.frame(df) # convert to data frame
  names(df) <- c("Team", "W", "L", "T", "PCT", "PF", "PA", "Home W", "Home L",
                 "Home T", "Home PCT", "Away W", "Away L", "Away T", "Away PCT")
  df <- df[!is.na(as.numeric(df$W)),] #removing bad entries
  df[,2:15] <- sapply(df[,2:15], as.numeric) # making entries numeric
  df$Year = year # updates year column
  
  # Logic to only use the NFL teams by not including other teams on table
  if (year < 1976) {
    df <- df[1:26,] # 26 teams for years 1970 - 1975
  } else if (year < 1995) {
    df <- df[1:28,] # 28 teams years 1976 - 1998
  } else if (year < 1999) {
    df <- df[1:30,]# 30 teams years 1995 - 1998
  }else if (year < 2002) {
    df <- df[1:31,]# 31 teams years 1999 - 2001
  } else {
    df <- df[1:32,]# 32 teams 2002 -> now
  }
  
  return(df)
}





#
#
#   mining all the data
#
#


season <- get_season(2022)
season$Team <- gsub('[^[:alnum:] ]', '', season$Team)
wins <- season[,1:2]
wins # df of teams and the number of wins in the season

# dictionary of team names and how I  need to search them on pro football reference
# thank you to mjk2244 on github for this dictionary
team_hrefs = c(
  'Arizona Cardinals'= 'crd',
  'Baltimore Colts'= 'clt',
  'St Louis Cardinals'= 'crd',
  'Boston Patriots'= 'nwe',
  'Chicago Bears'= 'chi',
  'Green Bay Packers'= 'gnb',
  'New York Giants'= 'nyg',
  'Detroit Lions'= 'det',
  'Washington Commanders'= 'was',
  'Washington Football Team'= 'was',
  'Washington Redskins'= 'was',
  'Philadelphia Eagles'= 'phi',
  'Pittsburgh Steelers'= 'pit',
  'Los Angeles Chargers'= 'sdg',
  'San Francisco 49ers'= 'sfo',
  'Houston Oilers'= 'oti',
  'Cleveland Browns'= 'cle',
  'Indianapolis Colts'= 'clt',
  'Dallas Cowboys'= 'dal',
  'Kansas City Chiefs'= 'kan',
  'Los Angeles Rams'= 'ram',
  'Denver Broncos'= 'den',
  'New York Jets'= 'nyj',
  'New England Patriots'= 'nwe',
  'Las Vegas Raiders'= 'rai',
  'Tennessee Titans'= 'oti',
  'Tennessee Oilers'= 'oti',
  'Phoenix Cardinals'= 'crd',
  'Los Angeles Raiders'= 'rai',
  'Buffalo Bills'= 'buf',
  'Minnesota Vikings'= 'min',
  'Atlanta Falcons'= 'atl',
  'Miami Dolphins'= 'mia',
  'New Orleans Saints'= 'nor',
  'Cincinnati Bengals'= 'cin',
  'Seattle Seahawks'= 'sea',
  'Tampa Bay Buccaneers'= 'tam',
  'Carolina Panthers'= 'car',
  'Jacksonville Jaguars'= 'jax',
  'Baltimore Ravens'= 'rav',
  'Houston Texans'= 'htx',
  'Oakland Raiders'= 'rai',
  'San Diego Chargers'= 'sdg',
  'St Louis Rams'= 'ram',
  'Boston Patriots'= 'nwe'
)



#initializing data frames
wins <- data.frame(matrix(ncol = 3))
colnames(wins) <- c("Team", "Wins", "Year")
G1 <- data.frame(matrix(ncol = 27))
colnames(G1) = c("Week", "W.L", "Tm", "Opp","Pass.Cmp", "Pass.Att", "Pass.Yds", "Pass.TD", "Int",
                 "Sk", "Sk.Yds", "Pass.Y.A", "Pass.NY.A", "Cmp%", "Rush.Rate", "Rush.Att", "Rush.Yds",
                 "Rush.Y.A", "Rush.TD", "FGM", "FGA", "XPM", "XPA", "Pnt", "Pnt.Yds",
                 "Year", "Team")


for (i in 1970:2022) {
  print(i)
  season <- get_season(i)
  season <- season[,c(1:2,16)]
  season$Team <- gsub('[^[:alnum:] ]', '', season$Team) #gets rid of special chars
  
  #wins is total wins for each team for each season
  if (i == 1970) {
    wins <- season
  } else {
    wins <- rbind(wins, season)
  }
  
  
  for (team in season$Team) {
    print(team)
    url <- paste("https://www.pro-football-reference.com/teams/" , team_hrefs[team] , "/", i, "/gamelog/", sep = '')
    url %>%
      read_html %>%
      html_table() -> df # gets table from html
    teamOne <- as.data.frame(df[[1]]) # converts to data frame
    teamOne <- teamOne[c(1,5,9:31)] # gets rid of data I can't use in a linear model
    teamOne$year <- i #adds year so I can combine data frames later
    teamOne$team <- team # adds team name to data
    colnames(teamOne) = c("Week", "W.L", "Tm", "Opp","Pass.Cmp", "Pass.Att", "Pass.Yds", "Pass.TD", "Int",
                     "Sk", "Sk.Yds", "Pass.Y.A", "Pass.NY.A", "Cmp%", "Rush.Rate", "Rush.Att", "Rush.Yds",
                     "Rush.Y.A", "Rush.TD", "FGM", "FGA", "XPM", "XPA", "Pnt", "Pnt.Yds",
                     "Year", "Team")
    
    G1 <- rbind(G1, teamOne[-1,]) # excludes first row of blanks
    Sys.sleep(3) # more than 20 requests in a minute puts me in 'jail'
  }
}



#G1 comes out with the first row being NA values
games <- G1[-1,]
# week column is stored as chars, I need it to be numeric
games[,c(1,3:26)] <- sapply(games[,c(-2,-27)], as.numeric)

#breakpoint to download the file because I dont want to download it from the internet again
write.csv(games, file = 'NFL Data Unprocessed.csv', row.names = FALSE)
write.csv(wins, file = 'NFL wins.csv', row.names = FALSE)




#
#
# Getting subsets for first N weeks
# 
#


games <- read.csv("NFL Data Unprocessed.csv")
wins <- read.csv('NFL wins.csv')


n = 17
getNWeeks(n, wins, games)

# game subset function
# I want to show how it should get more accurate as more games are played
getNWeeks <- function(n, wins, games) {
  
  nWeeks <- as.data.frame(matrix(nrow = 0, ncol = 29))
  colnames(nWeeks) = c("Team", "Year", "Tm", "Opp", "Pass.Cmp", "Pass.Att",
                       "Pass.Yds", "Pass.TD", "Int", "Sk", "Sk.Yds", "Pass.Y.A",
                       "Pass.NY.A", "Cmp.", "Rush.Rate", "Rush.Att", "Rush.Yds",
                       "Rush.Y.A", "Rush.TD", "FGM", "FGA", "XPM", "XPA", "Pnt",
                       "Pnt.Yds", "L", "W", "T", "Wins")
  
  #other temp df with col names
  tempS <- nWeeks[0,]
  
  
  wlt <- as.data.frame(matrix(nrow = 1, ncol = 3))
  colnames(wlt) = c('L', 'T', 'W')
  
  #loop through wins data for each team, year
  # subset with year and team same as loop
  # get number of wins, loss, and ties
  # use row sum to make result of first n weeks per row
  # return df
  for (i in rownames(wins)) {
    #subset only the rows for the specific team, year
    tempS = games[intersect(which(games$Team == wins[i,1]), which(games$Year == wins[i,3])),]
    #subset only the first n games in a season
    tempS = tempS[1:n,]
    tempS <- tempS[!is.na(tempS[,1]),]
    
    #the number of games each season changes I don't want to only use data of a team
    # with less games than n
    if (nrow(tempS) >= n){
      # transform vector of W,L,T values to table of wins, losses, and ties
      wlt = rbind.fill(as.data.frame(t(as.matrix(table(tempS$W.L)))),wlt[-1,])
      wlt[is.na(wlt)] <- 0
      
      # getting the sum of all numeric rows
      rowToAdd <- colSums(tempS[3:25])
      # converting sums of averages back to averages
      rowToAdd[c(9:12,16)] = rowToAdd[c(9:12,16)] / n
      
      #add win loss tie
      rowToAdd = cbind(as.data.frame(t(as.matrix(rowToAdd))), wlt)
      #add name and year
      rowToAdd = cbind(rowToAdd, tempS[1, c(26,27)]) 
      
      #add row to dataframe
      nWeeks <- rbind(nWeeks, rowToAdd)
      
    }
  }
  return(merge(nWeeks, wins, by = c("Team", "Year")))
}





