# generating the current season data to make predictions

#ls for last season
ls <- get_season(2023)
ls$Team <- gsub('[^[:alnum:] ]', '', ls$Team)
ls <- ls[,c(1,2,16)]
colnames(ls)[2] = "Wins"
ls = ls[order(ls$Team),]

#ls$Wins is the vector I am trying to predict with all my models
ls$Wins


#getting sample data for the model
wins <- data.frame(matrix(ncol = 3))
colnames(wins) <- c("Team", "Wins", "Year")
lsSamp <- data.frame(matrix(ncol = 27))
colnames(lsSamp) = c("Week", "W.L", "Tm", "Opp","Pass.Cmp", "Pass.Att", "Pass.Yds", "Pass.TD", "Int",
                 "Sk", "Sk.Yds", "Pass.Y.A", "Pass.NY.A", "Cmp.", "Rush.Rate", "Rush.Att", "Rush.Yds",
                 "Rush.Y.A", "Rush.TD", "FGM", "FGA", "XPM", "XPA", "Pnt", "Pnt.Yds",
                 "Year", "Team")


for (team in ls$Team) {
  if(is.na(lsSamp[1,1])){ 
    lsSamp = lsSamp[-1,]
  }
  
  url <- paste("https://www.pro-football-reference.com/teams/" , team_hrefs[team] , "/2023/gamelog/", sep = '')
  url %>%
    read_html %>%
    html_table() -> df # gets table from html
  tempSeason <- as.data.frame(df[[1]]) # converts to data frame
  tempSeason <- tempSeason[c(1,5,9:31)] # gets rid of data I can't use in a linear model
  tempSeason$year <- 2023 #adds year
  tempSeason$team <- team # adds team name to data
  colnames(tempSeason) = colnames(lsSamp)
  
  lsSamp <- rbind(lsSamp, tempSeason[-1,])
  print(team)
  Sys.sleep(3) # more than 20 requests per minute puts me in jail 
}


# week column is stored as chars, I need it to be numeric
lsSamp[,c(1,3:26)] <- sapply(lsSamp[,c(-2,-27)], as.numeric)


 

########## Finally making the predictions!! #######
predictions <- matrix(nrow = 17, ncol = 32)
for (i in 1:17) {
  temp = getNWeeks(i,ls,lsSamp)
  temp = predict.lm(models[[i]],temp)
  predictions[i,] = temp
}







diff <- matrix(nrow = 17, ncol = 32)
difsq <- matrix(nrow = 17, ncol = 32)
floormse <- matrix(nrow = 17, ncol = 32)
roundmse <- matrix(nrow = 17, ncol = 32)




for (i in 1:17) {
  diff[i,] = predictions[i,] - ls$Wins
  difsq[i,] = (predictions[i,] - ls$Wins)^2
  floormse[i,] = (floor(predictions[i,]) - ls$Wins)^2
  roundmse[i,] = (round(predictions[i,]) - ls$Wins)^2
}


#need to convert to df to use ggplot
ggplot(floormse, aes) 

fmse = as.data.frame(floormse)
mse = apply(predictions,1, function(x) (x - ls$Wins)^2)

plot(x = 1:17, y = rowSums(sqerror), main = "sum squared error")


ggplot(floormse) +
  geom_bar(mapping = aes(x  ))





##accuracy every week

roundPre <- round(predictions)

#number of correct predictions
plot(apply(round(predictions), 1, function(x) length(which(x == ls$Wins))),
     main = "Number of correct predictions each week out of 32",
     ylab = "Number of correct predictions",
     xlab = "Number of Weeks in Data",
     pch = 16,
      )

# standard deviation of the predictions from the observed values
# after week six the predictions get better at a slower rate until week 13 when the variance gets crazy
rmseWeek <- apply(predictions, 1, function(x) sqrt(mean((x - ls$Wins)^2)))
plot(rmseWeek, main = "root mean square error", xlab = "total weeks used in model")
x = 1:17
y = (-2.5 * (x-1)) / 16 + 2.5
lines(x,y)


#which teams were easiest and hardest to graphcbind
teamPredict <- cbind(apply(rmse,2, function(x) sum(x) / 17), ls[,c(1,2)])
colnames(teamPredict)[1] = "teamMse"

ggplot(teamPredict, aes(x  = Wins, y = teamMse)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = toupper(team_hrefs[Team]), width = 0.055, alpha  = 0.7)) +
  ggplot2::labs(
    x = "total season wins",
    y = "team rsme",
    title = "2023 Total wins and rsme for each individual team"
  )

#I was able to get the most accurate predictions for the jags saints dolphins, and buccs
# least accurate team to predict was the chargers\


sqerror <- t(apply(predictions, 1, function(x) (x - ls$Wins)^2))

#rmse for each week and each team
weekrmse <- apply(sqerror, 1, function(x) sqrt(sum(x) / 32))
teamrmse <- apply(sqerror, 2, function(x) sqrt(sum(x) / 17))

# accuracy of predictions for each week, team
accW <- apply(floor(sqerror), 1, function(x) length(which(x == 0))) / 32
accT <- apply(floor(sqerror), 2, function(x) length(which(x == 0))) / 17

teamPredict <- cbind(teamrmse, accT, ls[,1:2])
weekPredict <- as.data.frame(cbind(weekrmse, accW, 1:17))
colnames(weekPredict)[3] = "week"

ggplot(teamPredict, aes(x = accT, y = teamrmse)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team_hrefs[Team]), width = 0.055, alpha = 0.7) +
  ggplot2::labs(x = "Percentage of Correct Predictions",
                y = "Team RMSE",
                title = "Accuracy and RMSE of Team Predictors")

ggplot(weekPredict, aes(x = accW, y = weekrmse)) +
  geom_text(aes(label = week)) +
  labs(x = "Percentage of correct Predictions",
       y = "Weekly RMSE",
       title = "Accuracy and RMSE of each model")


ggplot(weekPredict, aes(x = week, y = accW)) +
  geom_point() +
  labs(x = "WeeK",
       y = "% of games correctly predicted",
       title = "Accuracy Over Time")

ggplot(weekPredict, aes(x = week, y = rmse)) +
  geom_point() +
  labs(x = "RMSE",
       y = "% of games correctly predicted",
       title = "Accuracy Over Time")
