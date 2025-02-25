library(nflverse)
library(tidyverse)
library(faraway)
# Data Manipulation -------

dat <- read.csv('/Users/njgn92/Library/CloudStorage/OneDrive-UniversityofMissouri/2024Fall/STAT4520/Demos/nfl_team_stats_2002-2023.csv')
dat <- dat |> unite(uuid, season, week, date, away, home, remove = F)

home <- dat |> dplyr::select(uuid, season, week, date, home, ends_with("_home")) |> 
  rename_with(~str_remove(., '_home')) |> 
  mutate(location = 'home') |> 
  rename(team = home)
away <- dat |> dplyr::select(uuid, season, week, date, away, ends_with("_away")) |> 
  rename_with(~str_remove(., '_away')) |> 
  mutate(location = 'away') |> 
  rename(team = away)

cleanDat <- rbind(home, away)
cleanDat <- cleanDat |> arrange(date, uuid)

set.seed(1)
split <- sample(1:nrow(cleanDat), 0.9*nrow(cleanDat), replace = F)
split <- split[order(split)]

trainDat <- cleanDat[split,]
testDat <- cleanDat[-split,]

trainDat$season = as.factor(trainDat$season)
testDat$season = as.factor(testDat$season)


# Questions / things I want to learn about -------

# Does being at home affect the points you score?
# What are the significant variable based on AIC / hypotesting / 
#model based on what I think I know about football
# Are there different eras - is the year significantly related to points?

# Data exploration ------
cleanDat |> ggplot(aes(x = score, fill = location)) + 
  geom_histogram(alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("black", "blue"))

cleanDat |> ggplot(aes(x = score, fill = location)) + 
  geom_histogram(alpha=1, position = 'identity') +
  facet_wrap(~location) +
  scale_fill_manual(values=c("black", "blue"))
  

cleanDat |> ggplot(aes(x = score, fill = as.factor(season))) + 
  geom_histogram(alpha=1, position = 'identity') +
  facet_wrap(~as.factor(season))


# Poisson Model Nick Simple -------

nickMod <- glm(score ~ location + yards + pen_yards + interceptions + 
                fumbles, data = trainDat,
               family = poisson)
summary(nickMod)

tmp = predict(nickMod, newdata = testDat, type = 'response')
nickPred <- predprob(nickMod, newdata = testDat)

par(mfrow = c(3,3))
for(i in 1:9){
  barplot(nickPred[i,], main = paste(testDat$date[i], testDat$team[i])
          ,xlim = c(0,70), width = 1, space = 0)
  abline(v = testDat$score[i]+0.5, col = 'red')
}


# Goodness of fit test
#H0: proposed model fits well
#Ha: model ill fitting

# G-statistic method
pchisq(nickMod$deviance, df = nickMod$df.residual, lower.tail = F)
 #model seems poor

par(mfrow=c(1,1))
faraway::halfnorm(residuals(nickMod))

# Pearson's X^2 statistic
sum(residuals(nickMod, type="pearson")^2) / nickMod$df.residual
# much greater than 1 - so this proposed model may be incorrect


# AIC variable selection -------

modAIC <- glm(score ~ . -uuid -week -date -team -possession -yards , data = trainDat,
              family = poisson)

modAICfinal = step(modAIC)
summary(modAICfinal)

aicPred <- predprob(modAICfinal, newdata = testDat)

par(mfrow = c(3,3))
for(i in 1:9){
  barplot(aicPred[i,], main = paste(testDat$date[i], testDat$team[i])
          ,xlim = c(0,70), width = 1, space = 0)
  abline(v = testDat$score[i]+0.5, col = 'red')
}


pchisq(modAICfinal$deviance, df = modAICfinal$df.residual, lower.tail = F)
# still a poor fit


# Should we use quasi? ---------

nickModQuasi <- glm(score ~ location + yards + pen_yards + 
                      interceptions + fumbles, data = trainDat,
                    family = quasipoisson)
summary(nickModQuasi)

drop1(nickModQuasi, test = 'F')


# Hypotesting to get a better quasi Poisson? --------
modHypo <- glm(score ~ . -uuid -week -date -team -possession -yards, data = trainDat,
               family = quasipoisson)
drop1(modHypo, test = 'F')


modHypo <- glm(score ~ . -uuid -week -date -team -possession -yards -pass_comp, 
               data = trainDat,
               family = quasipoisson)
drop1(modHypo, test = 'F')

modHypo <- glm(score ~ . -uuid -week -date -team -possession -yards -pass_comp 
               -plays, data = trainDat,
               family = quasipoisson)
drop1(modHypo, test = 'F')

modHypo <- glm(score ~ . -uuid -week -date -team -possession -yards -pass_comp 
               -plays -sacks_yards, data = trainDat,
               family = quasipoisson)
drop1(modHypo, test = 'F')

modHypo <- glm(score ~ season + first_downs + third_down_comp + third_down_att 
               + fourth_down_comp+fourth_down_att+drives+pass_att+
                 pass_yards+sacks_num+rush_att+rush_yards+pen_num+
                 redzone_comp+redzone_att+fumbles+interceptions+
                 def_st_td + location, data = trainDat,
               family = quasipoisson)
drop1(modHypo, test = 'F')

sumary(modHypo)

# AIC quasipoisson ------

modAICquasi <- glm(score ~ season + first_downs + third_down_comp + 
                     third_down_att + fourth_down_comp + fourth_down_att + drives + 
                     pass_att + pass_yards + sacks_num + rush_att + rush_yards + 
                     pen_num + redzone_comp + redzone_att + fumbles + interceptions + 
                     def_st_td + location, family = poisson, data = trainDat)
step(modAICquasi)
summary(modAICquasi)


# Or Zero inflated  ------

library(pscl)
tmp = predprob(nickMod)
colSums(nickPred[,c(1,3,4,6,7,8,9,10,11,12)])
table(testDat$score)[1:10]

nickZero <- zeroinfl(score ~ location + yards + pen_yards + 
                       interceptions + fumbles, data = trainDat)
summary(nickZero)

# binomial part success = not scoring points

testDat$NickZeroPred <- predict(nickZero, newdata = testDat, type = 'response')
NickZeroPred <- predict(nickZero, newdata = testDat, type = 'prob')


par(mfrow = c(3,3))
for(i in 1:9){
  barplot(NickZeroPred[i,], main = paste(testDat$date[i], testDat$team[i])
          ,xlim = c(0,70), width = 1, space = 0)
  abline(v = testDat$score[i]+0.5, col = 'red')
}

#looking at just the zeros
predict(nickZero, newdata = testDat[4,], type = 'zero')
NickZeroPred[4,1]
step(nickZero)




# Or hurdle? -----

nickHurdle <- hurdle(score ~ location + yards + pen_yards + 
                       interceptions + fumbles, data = trainDat)

summary(nickHurdle)
# hurdle binomial part success = scoring points

nickHurdlePred = predict(nickHurdle, newdata = testDat, type = 'prob')

par(mfrow = c(3,3))
for(i in 1:9){
  barplot(nickHurdlePred[i,], main = paste(testDat$date[i], testDat$team[i])
          ,xlim = c(0,70), width = 1, space = 0)
  abline(v = testDat$score[i]+0.5, col = 'red')
}
