# reading initial data
games <- read.csv("NFL Data Unprocessed.csv")
wins <- read.csv('NFL wins.csv')

# getting all the files for each subset

one <- getNWeeks(1,wins,games)
two <- getNWeeks(2,wins,games)
three <- getNWeeks(3,wins,games)
four <- getNWeeks(4,wins,games)
five <- getNWeeks(5,wins,games)
six <- getNWeeks(6,wins,games)
seven <- getNWeeks(7,wins,games)
eight <- getNWeeks(8,wins,games)
nine <- getNWeeks(9,wins,games)
ten <- getNWeeks(10,wins,games)
eleven <- getNWeeks(11,wins,games)
twelve <- getNWeeks(12,wins,games)
thirteen <- getNWeeks(13,wins,games)
fourteen <- getNWeeks(14,wins,games)
fifteen <- getNWeeks(15,wins,games)
sixteen <- getNWeeks(16,wins,games)
seventeen <- getNWeeks(17,wins,games)




models <- list()
models[[1]] <- NFLRegression(one)
models[[2]] <- NFLRegression(two)
models[[3]] <- NFLRegression(three)
models[[4]] <- NFLRegression(four)
models[[5]] <- NFLRegression(five)
models[[6]] <- NFLRegression(six)
models[[7]] <- NFLRegression(seven)
models[[8]] <- NFLRegression(eight)
models[[9]] <- NFLRegression(nine)
models[[10]] <- NFLRegression(ten)
models[[11]] <- NFLRegression(eleven)
models[[12]] <- NFLRegression(twelve)
models[[13]] <- NFLRegression(thirteen)
models[[14]] <- NFLRegression(fourteen)
models[[15]] <- NFLRegression(fifteen)
models[[16]] <- NFLRegression(sixteen)
models[[17]] <- NFLRegression(seventeen)

models <- list()
for (i in 1:17) {
  models[[i]] <- NFLRegression(getNWeeks(i, wins, games))
}


ggplot(rsa, aes(x = X2, y = X1)) + geom_point() + labs(x = "Weeks played in each dataset", y = "Adjusted R^2", title = "Adj. R^2 Values")
