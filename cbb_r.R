library(readxl)
library(ggplot2)
library(ggiraph)
library(corrplot)
library(rpart)
library(FSelector)
library(tidyverse)
library(coda)
library(runjags)
library(psych)
library(plyr) #NEED TO DETATCH AT CERTAIN POINTS
library(dplyr)
library(patchwork)
library(writexl)
setwd("~/Desktop/cbb data")

##### Downloading the Data ##### 

cbb <- read_excel("cbb2007.xlsx")
# need to get rid of NAs
View(cbb)

cbb <- subset(cbb, is.na(cbb$ML) == FALSE)
view(cbb)

cbb %>%
  select(Year, ML)

cbb <- as.data.frame(cbb)
cbb$ML <- as.numeric(cbb$ML)
cbb <- subset(cbb, is.na(cbb$ML) == FALSE)

# Changing misentered value
cbb[21396,7] <- 50

View(cbb)


####### Binning with extreme ML values #########

cbb$ML_binned <- ifelse(cbb$ML < -1000 & cbb$ML > -8000, 
       cbb$ML_binned <- round_any(cbb$ML, 1000), 
       ifelse(cbb$ML > 1000 & cbb$ML < 8000,
              cbb$ML_binned <- round_any(cbb$ML, 1000),
              ifelse(cbb$ML < -8000 & cbb$ML > - 20000,
              cbb$ML_binned <- round_any(cbb$ML, 2000),
              ifelse(cbb$ML > 8000 & cbb$ML < 20000,
              cbb$ML_binned <- round_any(cbb$ML, 2000),
              ifelse(cbb$ML > 25000,
              cbb$ML_binned <- round_any(cbb$ML, 50000),
              ifelse(cbb$ML < -25000,
              cbb$ML_binned <- round_any(cbb$ML, 50000),
              cbb$ML_binned <- round_any(cbb$ML, 50)))))))
view(cbb)

for (i in 1:nrow(cbb)) {
  if(cbb[i,11] > 100000){
    cbb[i,11] <-  100000
  } else{
    if(cbb[i,11] < -100000){
      cbb[i,11] <-  -100000
    } else{
      cbb[i,11] <- cbb[i,11]
    }
  }
}

view(cbb)

prob_bin <- cbb %>%
  group_by(ML_binned)%>%
  tally
prob_bin$freq <- prob_bin$n / sum(prob_bin$n)
prob_bin$freq <- round(prob_bin$freq, digits = 6)
view(prob_bin)

prob_win <- cbb %>%
  group_by(ML_binned) %>%
  tally(Outcome)
view(prob_win)

prob_mat <- cbind(prob_bin, prob_win[,2])
colnames(prob_mat) <- c("ML_binned", "n_games", "freq_games", "n_wins")
prob_mat$win_prob <- prob_mat$n_wins / prob_mat$n_games
view(prob_mat)


##### Creating ML_spread Var for cbb Dataset #####

for (i in 1:(nrow(cbb)-1)) {
  if(cbb[i,3] == cbb[i+1,3]){
    cbb[i,12] = cbb[i,11] - cbb[i+1,11]
  } else{
    cbb[i,12] = cbb[i,11] - cbb[i-1,11]
  }
}

view(cbb)
colnames(cbb)[12] <- "ML_spread"

view(cbb)

cbb_ML_spread <- cbb[,12]
cbb_ML_spread <- as.data.frame(cbb_ML_spread)
colnames(cbb_ML_spread)[1] <- "ML_spread"
view(cbb_ML_spread)


##### Creating Win Probability Estimates Based on ML Spread #####

prob_bin_spread <- cbb %>%
  group_by(ML_spread) %>%
  tally
view(prob_bin_spread)
prob_bin_spread$freq <- prob_bin_spread$n / sum(prob_bin_spread$n)
prob_bin_spread$freq <- round(prob_bin_spread$freq, digits = 6)

prob_win_spread <- cbb %>%
  group_by(ML_spread)%>%
  tally(Outcome)
view(prob_win_spread)

prob_bin_spread <- cbind(prob_bin_spread, prob_win_spread[,2])
colnames(prob_bin_spread) <- c("ML_spread", "n_games", "freq_games", "n_wins")
view(prob_bin_spread)
prob_bin_spread$win_prob <- prob_bin_spread$n_wins / prob_bin_spread$n_games

prob_bin_spread %>%
  filter(ML_spread > 0) %>%
  ggplot(aes(x = log(ML_spread), y = win_prob))+
  geom_point()+
  geom_smooth(method = "loess")

View(prob_bin_spread)

# Creating dataset w/o outliers to create predictor

extrap <- prob_bin_spread %>%
  filter(ML_spread > 0) %>%
  filter(win_prob < 0.5)

# Creating Positive, Zero, and Negative DFs for Predictions

prob_bin_spread_pos <- prob_bin_spread %>%
  filter(ML_spread > 0)
prob_bin_spread_neg <- prob_bin_spread %>%
  filter(ML_spread < 0)
prob_bin_spread_0 <- prob_bin_spread %>%
  filter(ML_spread == 0)

#Using Loess model on Positive ML_spreads

M1_loess <- loess(win_prob ~ log(ML_spread), data = extrap)

prob_bin_spread_pos$win_prob_pred <- predict(M1_loess, prob_bin_spread_pos)
prob_bin_spread_pos$win_prob_pred <- round(prob_bin_spread_pos$win_prob_pred, digits = 6)
view(prob_bin_spread_pos)

for (i in 1:nrow(prob_bin_spread_pos)) {
  if(prob_bin_spread_pos[i,6] <= 0 ){
    prob_bin_spread_pos[i,6] <- 0.00001
  }else{
    prob_bin_spread_pos[i,6] <-  prob_bin_spread_pos[i,6]
  }
}
prob_bin_spread_pos[102:103,6] <- 0.00001
view(prob_bin_spread_pos)

write_xlsx(prob_bin_spread, "prob_bin_spread.xlsx")

Figure_1 <- prob_bin_spread %>%
  ggplot(aes(x = ML_spread, y = win_prob))+
  geom_point()+
  geom_line(aes(y = win_prob_pred, color = "Estimated Win Probability"))+
  labs(x = "Money Line Spread",
       y = "Win Probability",
       title = "Figure 1: Win Probability vs Estimated Win Probability
                  at Each Money Line Level")

# Binding Prediction Values

prob_bin_spread_neg <- prob_bin_spread_neg[
  with(prob_bin_spread_neg, order(ML_spread, decreasing = TRUE)),
]
view(prob_bin_spread_neg)

#prob_bin_spread_neg <- prob_bin_spread_neg[-38,]
view(prob_bin_spread_neg)

for (i in 1:nrow(prob_bin_spread_neg)) {
  prob_bin_spread_neg[i,6] <- 1 - prob_bin_spread_pos[i,6]
}
colnames(prob_bin_spread_neg)[6] <- "win_prob_pred"

prob_bin_spread_0[,6] <- 0.5
colnames(prob_bin_spread_0)[6] <- "win_prob_pred"
view(prob_bin_spread_0)

prob_bin_spread <- rbind(prob_bin_spread_pos, prob_bin_spread_0, prob_bin_spread_neg)
view(prob_bin_spread)


##### ML Percentage of Money Stuff #####

Money <- read_excel("ML_money_data.xlsx")
Money$ML <- as.numeric(Money$ML)
Money <- na.omit(Money)
view(Money)

# Creating ML_binned Var for Money Dataset

Money$ML_binned <- ifelse(Money$ML < -1000 & Money$ML > -8000, 
                        Money$ML_binned <- round_any(Money$ML, 1000), 
                        ifelse(Money$ML > 1000 & Money$ML < 8000,
                               Money$ML_binned <- round_any(Money$ML, 1000),
                               ifelse(Money$ML < -8000 & Money$ML > - 20000,
                                      Money$ML_binned <- round_any(Money$ML, 2000),
                                      ifelse(Money$ML > 8000 & Money$ML < 20000,
                                             Money$ML_binned <- round_any(Money$ML, 2000),
                                             ifelse(Money$ML > 25000,
                                                    Money$ML_binned <- round_any(Money$ML, 50000),
                                                    ifelse(Money$ML < -25000,
                                                           Money$ML_binned <- round_any(Money$ML, 50000),
                                                           Money$ML_binned <- round_any(Money$ML, 50)))))))

Money$ML_binned <- as.numeric(Money$ML_binned)
u_ml <- subset(Money, ML_binned >0)
f_ml <- subset(Money, ML_binned < 0)


# Creating ML_spread Var for Money Dataset

for (i in 1:7151) {
  if(Money[i,3] == Money[i+1,3]){
    Money[i,5] = Money[i,4] - Money[i+1,4]
  } else{
    Money[i,5] = Money[i,4] - Money[i-1,4]
  }
}
colnames(Money)[5] <- "ML_spread"
Money[7152,5] = 200
view(Money)

# Grouping ML_spread to find average ML_money

Money_group <- Money %>%
  group_by(ML_spread) %>%
    summarise(mean_money = mean(ML_money))
view(Money_group)
ML_group_positive <- subset(Money_group, ML_spread >= 0)


# Graphing Relationship between ML_spread and Mean Money 


write_xlsx(ML_group_positive, "ML_group_positive.xlsx")

Figure_2 <- ML_group_positive %>%
  ggplot(aes(x = sqrt(ML_spread), y = mean_money))+
  geom_point()+
  geom_smooth(aes(color = "Estimated Percentage 
of Money Bet 
on the Underdog"))+
  labs(x = "Square Root of the Money Line Spread",
       y = "Percentage of Money Bet on the Underdog",
       title = "Figure 2: Predicting Percentage of Bets for the Underdog vs Actual")

ML_m1 <- loess(mean_money ~ sqrt(ML_spread), data = ML_group_positive)
summary(ML_m1)


##### Creating the Money Predict Var for the cbb Dataset #####

cbb$ML_money_predict <- predict(ML_m1, cbb_ML_spread)
cbb$lag_id <- lag(cbb$Game_num)
view(cbb)

cbb$ML_money_predict <- replace_na(cbb$ML_money_predict, 0)

# Making the predictions on the Negative Values = 1 - positive values
for (i in 1:nrow(cbb)) {
  if(cbb[i,13] == 0){
    if(cbb[i,14] == cbb[i,3]){
      cbb[i,13] <- 1 - cbb[i-1,13]
    }else{
      cbb[i,13] <- 1 - cbb[i+1,13]
    }
  }else{
    cbb[i,13] <- cbb[i,13]
  }
}

# Changing all values of 1 into values approaching 1
for (i in 1:nrow(cbb)) {
  if(cbb[i,13] == 1) {
    cbb[i,13] <- 0.99999
  }else{
    if(cbb[i,13] == 0){
      cbb[i,13] <- 0.00001
    }else{
      cbb[i,13] <- cbb[i,13]
    }
  }
}
view(cbb)

# Making the win percentage for 0 ML Spread 50% (both teams have the same spread)
for (i in 1:nrow(cbb)) {
  if(cbb[i,12] == 0){
    cbb[i,13] <- 0.5
  } else{
    cbb[i,12] <- cbb[i,12]
  }
}
view(cbb)

# Creating Opp_ML_binned variable
cbb$opp_ML_binned <- cbb$ML_binned - cbb_ML_spread


# Adding the Win% Var to the cbb Dataset

prob_bin_spread %>%
  ggplot(aes(x = ML_spread, y = win_prob_pred))+
  geom_line(color = "blue")+
  geom_point(aes(y = win_prob, color = "Win Prob"))

prob_mat2 <- prob_mat[,c(1,5)]
view(prob_mat2)

prob_mat_spread <- prob_bin_spread[,c(1,6)]

cbb2 <- merge(cbb, prob_mat2, by = "ML_binned")
view(cbb2)
cbb3 <- merge(cbb, prob_mat_spread, by = "ML_spread")
colnames(cbb3)[15] <- "opp_ML_spread"
view(cbb3)

# Finding and removing Data point with no opposition team
cbb3 <- cbb3[
  with(cbb3, order(Game_num)),
]
view(cbb3)

view(cbb3 %>%
       filter(ML_spread == 0)%>%
       filter(Game_num != lag(Game_num) & Game_num != lead(Game_num))
)

for (i in 1:nrow(cbb3)) {
  cbb3[i,17] <- i
}
colnames(cbb3)[17] <- "id"

cbb3 <-  cbb3[-8888,]
cbb3 <- cbb3[,-17]


# Making win probabilities non-1
for (i in (1:(nrow(cbb2)-1))) {
  if(cbb2[i,16] == 1){
    cbb2[i,16] <- 0.99999
  }else{
    if(cbb2[i,16] == 0){
      cbb2[i,16] <- 0.00001
    }
  }
}
view(cbb2)



##### Creating a March Binary Variable #####

#cbb$March <- ifelse(substring(cbb$Date,1,1) == 3, 
                   # ifelse(cbb$Date < 1000,
 #                          cbb$March <- 1,
  #                         cbb$March <- 0),
   #                 cbb$March <- 0)

#cbb %>%
 # group_by(Year)%>%
  #summarise(games = sum(March))

#sum(cbb$March) / 13
#nrow(cbb) / 15

#cbb %>%
 # group_by(Year)%>%
  #summarise(games = sum(March))

#view(cbb)

############ Creating Risk Premium Calculations ##############

risk_prem <- vector(length = 65)

# GAME LEVEL cbb2

risk_prem_game <- vector(length = nrow(cbb2))
for (i in 1:nrow(cbb2)) {
  if(cbb2[i,1] > 0){
    if(cbb2[i,15] > 0){
    risk_prem_game[i] <- cbb2[i,13]*(1-cbb2[i,16]) + (1-cbb2[i,13])*cbb2[i,16] - 
      cbb2[i,13]*cbb2[i,16]*cbb2[i,1]/100 - (1-cbb2[i,13])*(1-cbb2[i,16])*cbb2[i,15]/100
  } else{
    risk_prem_game[i] <- cbb2[i,13]*(1-cbb2[i,16]) + (1-cbb2[i,13])*cbb2[i,16] - 
      cbb2[i,13]*cbb2[i,16]*(cbb2[i,1]/100) - (1-cbb2[i,13])*(1-cbb2[i,16])*(-100/cbb2[i,15])
  }
  } else{
    if(cbb2[i,15] > 0){
      risk_prem_game[i] <- cbb2[i,13]*(1-cbb2[i,16]) + (1-cbb2[i,13])*cbb2[i,16] - 
        cbb2[i,13]*cbb2[i,16]*(-100/cbb2[i,1]) - (1-cbb2[i,13])*(1-cbb2[i,16])*cbb2[i,15]/100
    } else{
      risk_prem_game[i] <- cbb2[i,13]*(1-cbb2[i,16]) + (1-cbb2[i,13])*cbb2[i,16] - 
        cbb2[i,13]*cbb2[i,16]*(-100/cbb2[i,1]) - (1-cbb2[i,13])*(1-cbb2[i,16])*(-100/cbb2[i,15])
    }
  }
}
view(risk_prem_game)

cbb2$risk_prem_game <- risk_prem_game
cbb2$risk_prem_game <- as.numeric(cbb2$risk_prem_game)
view(cbb2)
cbb2 %>% 
ggplot(aes(x = risk_prem_game)) +
  geom_histogram()

cbb2 %>%
  ggplot(aes(x = win_prob, y = risk_prem_game))+
  geom_point()

mean(cbb2$risk_prem_game)

##### Risk Premium for Money line Spread Win Predictions #####

cbb3 <- cbb3[
  with(cbb3, order(Year, Game_num)),
]

risk_prem_game_spread <- vector(length = nrow(cbb3))
for (i in 1:nrow(cbb3)) {
  if(cbb3[i,1] > 0){
    if(cbb3[i,15] > 0){
      risk_prem_game_spread[i] <- cbb3[i,13]*(1-cbb3[i,16]) + (1-cbb3[i,13])*cbb3[i,16] - 
        cbb3[i,13]*cbb3[i,16]*cbb3[i,12]/100 - (1-cbb3[i,13])*(1-cbb3[i,16])*cbb3[i,15]/100
    } else{
      risk_prem_game_spread[i] <- cbb3[i,13]*(1-cbb3[i,16]) + (1-cbb3[i,13])*cbb3[i,16] - 
        cbb3[i,13]*cbb3[i,16]*(cbb3[i,12]/100) - (1-cbb3[i,13])*(1-cbb3[i,16])*(-100/cbb3[i,15])
    }
  } else{
    if(cbb3[i,15] > 0){
      risk_prem_game_spread[i] <- cbb3[i,13]*(1-cbb3[i,16]) + (1-cbb3[i,13])*cbb3[i,16] - 
        cbb3[i,13]*cbb3[i,16]*(-100/cbb3[i,12]) - (1-cbb3[i,13])*(1-cbb3[i,16])*cbb3[i,15]/100
    } else{
      risk_prem_game_spread[i] <- cbb3[i,13]*(1-cbb3[i,16]) + (1-cbb3[i,13])*cbb3[i,16] - 
        cbb3[i,13]*cbb3[i,16]*(-100/cbb3[i,12]) - (1-cbb3[i,13])*(1-cbb3[i,16])*(-100/cbb3[i,15])
    }
  }
}
view(risk_prem_game_spread)

cbb3$risk_prem_game <- risk_prem_game_spread
cbb3$risk_prem_game <- as.numeric(cbb3$risk_prem_game)
view(cbb3)

cbb3 %>% 
  ggplot(aes(x = risk_prem_game)) +
  geom_histogram()

cbb3 %>%
  ggplot(aes(x = win_prob_pred, y = risk_prem_game))+
  geom_point()

mean(cbb3$risk_prem_game)


##### Hidden Risk Calculation, cbb2 #####
view(hr_mat)
df_losses1 <- vector(length = nrow(cbb2))
  for (i in 1:nrow(cbb2)) {
    if(cbb2[i,1] > 0 ){
      if(cbb2[i,15] > 0){
      df_losses1[i]  <- cbb2[i,16]*((cbb2[i,13]*cbb2[i,1]/100)^2) + (1-cbb2[i,16])*(((1-cbb2[i,13])*cbb2[i,15]/100)^2) -
          (cbb2[i,16]*((cbb2[i,13]*cbb2[i,1]/100)) + (1-cbb2[i,16])*(((1-cbb2[i,13])*cbb2[i,15]/100)))^2
      } else{
       df_losses1[i] <- cbb2[i,16]*((cbb2[i,13]*cbb2[i,1]/100)^2) + (1-cbb2[i,16])*(((1-cbb2[i,13])*(-100/cbb2[i,15]))^2) - 
          (cbb2[i,16]*((cbb2[i,13]*cbb2[i,1]/100)) + (1-cbb2[i,16])*(((1-cbb2[i,13])*(-100/cbb2[i,15]))))^2
      }
    } else{
    if(cbb[i,15] > 0){
      df_losses1[i] <- cbb2[i,16]*((cbb2[i,13]*(-100/cbb2[i,1]))^2) + (1-cbb2[i,16])*(((1-cbb2[i,13])*cbb2[i,15]/100)^2) - 
        (cbb2[i,16]*((cbb2[i,13]*(-100/cbb2[i,1]))) + (1-cbb2[i,16])*(((1-cbb2[i,13])*cbb2[i,15]/100)))^2
    } else{
      df_losses1[i] <- cbb2[i,16]*((cbb2[i,13]*(-100/cbb2[i,1]))^2) + (1-cbb2[i,16])*(((1-cbb2[i,13])*(-100/cbb2[i,15]))^2) - 
        (cbb2[i,16]*((cbb2[i,13]*(-100/cbb2[i,1]))) + (1-cbb2[i,16])*(((1-cbb2[i,13])*(-100/cbb2[i,15]))))^2
    }
  }
}
df_losses1 <- as.data.frame(df_losses1)
view(df_losses1)
ncol(df_losses1)
nrow(hr_mat)

hr_mat <- matrix(nrow = nrow(cbb2), ncol = 3)
for (i in 1:nrow(hr_mat)) {
  hr_mat[i,1] <- df_losses1[1,i]
}

view(hr_mat)

for (i in 1:nrow(cbb2)) {
      hr_mat[i,2]  <- (1-cbb2[i,16])*((cbb2[i,13])^2) + (cbb2[i,16])*((1 - cbb2[i,13])^2) -
        ((1-cbb2[i,16])*((cbb2[i,13])) + (cbb2[i,16])*((1 - cbb2[i,13])))^2
}

hr_mat[,3] <- hr_mat[,1]/hr_mat[,2]
colnames(hr_mat) <- c("losses", "gains", "hidden_risk")
hr_mat <- as.data.frame(hr_mat)
hr_mat[,3] <- replace_na(hr_mat[,3], 1)
hr_mat$hidden_risk[is.infinite(hr_mat$hidden_risk)] <- 1
view(hr_mat)

cbb2 <- cbind(cbb2, hr_mat)
cbb2$log_hidden_risk <- log(cbb2$hidden_risk)

cbb2 %>%
  ggplot(aes(x = risk_prem_game, y = log_hidden_risk))+
  geom_point()

cbb2 %>%
  filter(ML_spread <0) %>%
  ggplot(aes(x = risk_prem_game))+
  geom_histogram(binwidth = 0.01)



##### Hidden Risk For Moneyline Spread Win Predictions #####


df_losses1 <- vector(length = nrow(cbb3))
for (i in 1:nrow(cbb3)) {
  if(cbb3[i,12] > 0 ){
    if(cbb3[i,15] > 0){
      df_losses1[i]  <- cbb3[i,16]*((cbb3[i,13]*cbb3[i,12]/100)^2) + (1-cbb3[i,16])*(((1-cbb3[i,13])*cbb3[i,15]/100)^2) -
        (cbb3[i,16]*((cbb3[i,13]*cbb3[i,12]/100)) + (1-cbb3[i,16])*(((1-cbb3[i,13])*cbb3[i,15]/100)))^2
    } else{
      df_losses1[i] <- cbb3[i,16]*((cbb3[i,13]*cbb3[i,12]/100)^2) + (1-cbb3[i,16])*(((1-cbb3[i,13])*(-100/cbb3[i,15]))^2) - 
        (cbb3[i,16]*((cbb3[i,13]*cbb3[i,12]/100)) + (1-cbb3[i,16])*(((1-cbb3[i,13])*(-100/cbb3[i,15]))))^2
    }
  } else{
    if(cbb3[i,15] > 0){
      df_losses1[i]  <- cbb3[i,16]*((cbb3[i,13]*(-100/cbb3[i,12]))^2) + (1-cbb3[i,16])*(((1-cbb3[i,13])*cbb3[i,15]/100)^2) -
        (cbb3[i,16]*((cbb3[i,13]*(-100/cbb3[i,12]))) + (1-cbb3[i,16])*(((1-cbb3[i,13])*cbb3[i,15]/100)))^2
    } else{
      df_losses1[i] <- cbb3[i,16]*((cbb3[i,13]*(-100/cbb3[i,12]))^2) + (1-cbb3[i,16])*(((1-cbb3[i,13])*(-100/cbb3[i,15]))^2) - 
        (cbb3[i,16]*((cbb3[i,13]*(-100/cbb3[i,12]))) + (1-cbb3[i,16])*(((1-cbb3[i,13])*(-100/cbb3[i,15]))))^2
    }
  }
}

view(df_losses1)
df_losses1 <- as.data.frame(df_losses1)
view(df_losses1)
ncol(df_losses1)
nrow(hr_mat)

hr_mat_spread <- matrix(nrow = nrow(cbb3), ncol = 3)
for (i in 1:nrow(hr_mat_spread)) {
  hr_mat_spread[i,1] <- df_losses1[1,i]
}

view(hr_mat_spread)

for (i in 1:nrow(cbb3)) {
  hr_mat_spread[i,2]  <- (1-cbb3[i,16])*((cbb3[i,13])^2) + (cbb3[i,16])*((1 - cbb3[i,13])^2) -
    ((1-cbb3[i,16])*((cbb3[i,13])) + (cbb3[i,16])*((1 - cbb3[i,13])))^2
}

hr_mat_spread[,3] <- hr_mat_spread[,1]/hr_mat_spread[,2]
colnames(hr_mat_spread) <- c("losses", "gains", "hidden_risk")
hr_mat_spread <- as.data.frame(hr_mat_spread)
hr_mat_spread[,3] <- replace_na(hr_mat_spread[,3], 1)
hr_mat_spread$hidden_risk[is.infinite(hr_mat_spread$hidden_risk)] <- 1
view(hr_mat_spread)
view(hr_mat)


cbb3 <- cbind(cbb3, hr_mat_spread)
cbb3$log_hidden_risk <- log(cbb3$hidden_risk)
view(cbb3)

##### Data Visualization #####

hr_hist2 <- cbb2 %>%
  ggplot(aes(x = log_hidden_risk))+
  geom_histogram()

hr_hist3 <- cbb3 %>%
  ggplot(aes(x = log_hidden_risk))+
  geom_histogram()

hr_hist2 | hr_hist3

rp_hist2 <- cbb2 %>%
  ggplot(aes(x = risk_prem_game))+
  geom_histogram(binwidth = 0.1)

rp_hist3 <- cbb3 %>%
  ggplot(aes(x = risk_prem_game))+
  geom_histogram(binwidth = 0.1)

rp_hist2 | rp_hist3

cbb3 %>%
  ggplot(aes(x = risk_prem_game, y = log_hidden_risk))+
  geom_point()

view(cbb3)


view(prob_bin_spread_pos)
view(prob_mat)
view(cbb3)


write_xlsx(cbb3, "CBB3_data.xlsx")

##### Figures 1 and 2 #####

Figure_1 <- prob_bin_spread %>%
  ggplot(aes(x = ML_spread, y = win_prob))+
  geom_point()+
  geom_line(aes(y = win_prob_pred, color = "Estimated Win Probability"))+
  labs(x = "Money Line Spread",
       y = "Win Probability",
       title = "Figure 1: Win Probability vs Estimated Win Probability
                  at Each Money Line Level")

Figure_2 <- ML_group_positive %>%
  ggplot(aes(x = sqrt(ML_spread), y = mean_money))+
  geom_point()+
  geom_smooth(aes(color = "Estimated Percentage 
of Money Bet 
on the Underdog"))+
  labs(x = "Square Root of the Money Line Spread",
       y = "Percentage of Money Bet on the Underdog",
       title = "Figure 2: Predicting Percentage of Bets for the Underdog vs Actual")





