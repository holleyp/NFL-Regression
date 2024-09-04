#process 
#make model
#check residuals
#check multicollinearity
#statistically insignificant vars removal

#function to make the fist model with backwards step wise regression
makeNFLModel <- function(weeksdf){
  
  model <- lm(Wins ~ Tm + Opp + Pass.Cmp + Pass.Att + Pass.Yds + Pass.TD + Int +
              Sk + Sk.Yds + Pass.Y.A + Pass.NY.A + Cmp. + Rush.Rate + Rush.Att +
              Rush.Yds + Rush.Y.A + Rush.TD + FGM + FGA + XPM + XPA + Pnt +
              Pnt.Yds + L + W + T, data = weeksdf)
  #returns a backwards model
  return(step(model, direction = 'backward', trace = 0))
}

#check the residuals for normality
residCheck <- function(backModel, name){
  res <- resid(backModel)
  qqnorm(res)
  qqline(res)
  plot(density(res), main = paste(paste("Residual Density Curve for first", name), "weeks"))
}


#check Multicollinearity and remove vars
mlcCheck <- function(model, weeksdf) {
  #check variance inflation factor for multicollinearity 
  all_vifs <- car::vif(model)
  signif_all <- names(all_vifs)
  
  
  while(any(all_vifs > 4)){ 
    var_with_max_vif <- names(which(all_vifs == max(all_vifs))) # get var with max vif
    signif_all <- signif_all[!(signif_all) %in% var_with_max_vif] # remove
    myForm <- as.formula(paste("Wins ~ ", paste(signif_all, collapse="+"), sep = "")) # new formula
    selectedMod <- lm(myForm, data = weeksdf) #rebuild new style
    all_vifs <- car::vif(selectedMod)}
  
  return(selectedMod)
}

#last check for significance
sigCheck <- function(model, weeksdf) {
  all_vars <- names(model[[1]])[-1]
  summ <- summary(model)
  pvals <- summ[[4]][,4] # get p values
  not_sig <- character()
  not_sig <- names(which(pvals > 0.1))
  
  #if all vars are significant does not execute the while loop and return original model
  if(length(not_sig) == 0){
    print("break")
    return(model)
  }
  
  while(length(not_sig > 0)) {
    all_vars <- all_vars[!all_vars %in% not_sig[1]]
    myForm <- as.formula(paste("Wins ~ ", paste(all_vars, collapse=" + "), sep = ""))
    signif <- lm(myForm, data = weeksdf)
    
    summ <- summary(signif)
    pvals <- summ[[4]][,4]
    not_sig <- character()
    not_sig <- names(which(pvals > 0.1))
    not_sig <- not_sig[!not_sig %in% "(Intercept)"]
  }
  
  return(signif)
}

#function to do it all

NFLRegression <- function(weeksdf) {
  m1 <- makeNFLModel(weeksdf)
  residCheck(m1, deparse(substitute(weeksdf)))
  m2 <- mlcCheck(m1, weeksdf)
  m3 <- sigCheck(m2, weeksdf)
  return(m3)
}
