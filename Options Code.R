library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(mosaic)
library(readr)
library(psych)
library(scales)
library(patchwork)

setwd("~/Desktop/Stocks Data")

##### Stock Data Importing and Cleaning #####
combined <- read_csv("combined.csv")
combined <- combined[,1:5]
combined <- na.omit(combined)

# Fixing errors
x <- matrix(nrow = nrow(combined), ncol = 2)
x[,1] <- 1:nrow(combined)
combined <- cbind(combined, x[,1])
colnames(combined)[6] <- "id"

View(combined %>%
       filter(High < Open))

combined[243317,3] <- 71.330
combined[749172,3] <- 9.112
combined[749186,3] <- 9.112
combined[498534,3] <- 18.190
combined[749180,3] <- 9.112
combined[244318,3] <- 53.498
combined[244004,3] <- 53.498

# Creating High Change and Low Change DFs

high_change <- (combined[,3] - combined[,2]) / combined[,2]
high_change <- round(high_change, digits = 4)
high_change <- as.data.frame(high_change)
colnames(high_change) <- c("change")
View(high_change)
low_change <- (combined[,4] - combined[,2]) / combined[,2]
low_change <- round(low_change, digits = 4)
low_change <- as.data.frame(low_change)
colnames(low_change) <- c("change")
View(low_change)

##### Prob Def High Creation #####

prob_def_high <- high_change %>%
  group_by(round(change, digits = 3)) %>%
  tally
prob_def_high$prob <- prob_def_high$n / sum(prob_def_high$n)
prob_def_high$prob <- round(prob_def_high$prob, digits = 6)
prob_def_high <- as.data.frame(prob_def_high)
colnames(prob_def_high)[1] <- "change"

prob_def_high %>%
  filter(change > 0) %>%
  ggplot(aes(x = change, y = n))+
  geom_point()+
  labs(x = "% High Change",
       y = "Number of Instances",
       title = "Distribution of High Percent Change",
       caption = "Data: Boris Marjanovic, Kaggle"
  )


##### Prop Def Low Creation #####

prob_def_low <- low_change %>%
  group_by(round(change, digits = 3)) %>%
  tally
prob_def_low$prob <- prob_def_low$n / sum(prob_def_low$n)
prob_def_low$prob <- round(prob_def_low$prob, digits = 6)
prob_def_low <- as.data.frame(prob_def_low)
colnames(prob_def_low)[1] <- "change"

prob_def_low %>%
  ggplot(aes(x = change, y = n))+
  geom_point()+
  labs(x = "% Low Change",
       y = "Number of Instances",
       title = "Distribution of Low Percent Change",
       caption = "Data: Boris Marjanovic, Kaggle"
  )


##### Loading the Options Data and Variable Cleaning #####

options_new <- read_excel("~/Desktop/Stocks Data/options_data_wharton.xlsx")
spot_data <- read_excel("stock_bid_price.xlsx")
view(spot_data)


options_new <- merge(options_new, spot_data, by = c("price_date", "sym"))
options_new <- subset(options_new, vol > 0)
colnames(options_new)[13] <- "spot_price"
options_new$spot_price <- as.numeric(options_new$spot_price)
view(options_new)

call_new <- subset(options_new, c_p == "C")
call_new <- na.omit(call_new)
put_new <- subset(options_new, c_p == "P")
put_new <- na.omit(put_new)


call_new$necessary_returns <- ((call_new$strike_price+call_new$ask)-call_new$spot_price)/call_new$spot_price
put_new$necessary_returns <- ((put_new$strike_price-put_new$ask)-put_new$spot_price)/put_new$spot_price

# Getting rid of data that doesn't make sense
for (i in 1:nrow(call_new)) {
  if(call_new[i,14] <= 0){
    call_new[i,14] <- 0.00001
  }else{
    call_new[i,14] <- call_new[i,14]
  }
}
  
for (i in 1:nrow(put_new)) {
  if(put_new[i,14] >= 0){
    put_new[i,14] <- -.00001
  }else{
    put_new[i,14] <- put_new[i,14]
  }
}

options_new <- rbind(call_new,put_new)

view(options_new)
view(call_new)
view(put_new)

##### Call Options Hidden Risk Matrix #####

hidden_risk_mat <- matrix(ncol = 3, nrow = nrow(call_new))

# For Losses

df_losses_call1 <- matrix(ncol = nrow(call_new), nrow = nrow(prob_def_high))
for (i in 1:nrow(df_losses_call1)) {
  for (z in 1:ncol(df_losses_call1)) {
    if(prob_def_high[i,1] >= call_new[z,14]){
    df_losses_call1[i,z] <- (prob_def_high[i,3] * ((((1+prob_def_high[i,1])*call_new[z,13]) - (call_new[z,7]+call_new[z,9]))*call_new[z,10]*100)^2)
    }else{
      df_losses_call1[i,z] <- 0
   }
  }
}
view(df_losses_call1)

df_losses_call2 <- matrix(ncol = nrow(call_new), nrow = nrow(prob_def_high))
for (i in 1:nrow(df_losses_call2)) {
  for (z in 1:ncol(df_losses_call2)) {
    if(prob_def_high[i,1] >= call_new[z,14]){
      df_losses_call2[i,z] <- (prob_def_high[i,3] * ((((1+prob_def_high[i,1])*call_new[z,13]) - (call_new[z,7]+call_new[z,9]))*call_new[z,10]*100))
    }else{
      df_losses_call2[i,z] <- 0
    }
  }
}
view(df_losses_call2)

df_losses_call <- matrix(nrow = ncol(df_losses_call1), ncol = 3)
for (i in 1:nrow(call_new)) {
  df_losses_call[i,1] <- sum(df_losses_call1[,i])
}

for (i in 1:nrow(call_new)) {
  df_losses_call[i,2] <- (sum(df_losses_call2[,i]))^2
}

df_losses_call[,3] <- df_losses_call[,1] - df_losses_call[,2]

for (i in 1:nrow(df_losses_call)) {
  if(df_losses_call[i,3] <= 0){
    df_losses_call[i,3] <- 0.000001
  }else{
    df_losses_call[i,3] <- df_losses_call[i,3]
  }
}
view(df_losses_call)

# For Gains

df_gains_call1 <- matrix(ncol = nrow(call_new), nrow = nrow(prob_def_high))
for (i in 1:nrow(df_gains_call1)) {
  for (z in 1:ncol(df_gains_call1)) {
    if(prob_def_high[i,1] < call_new[z,14]){
      df_gains_call1[i,z] <- (prob_def_high[i,3] * (call_new[z,10] * call_new[z,9]*100)^2)
    }else{
      df_gains_call1[i,z] <- 0
    }
  }
}
view(df_gains_call1)

df_gains_call2 <- matrix(ncol = nrow(call_new), nrow = nrow(prob_def_high))
for (i in 1:nrow(df_gains_call2)) {
  for (z in 1:ncol(df_gains_call2)) {
    if(prob_def_high[i,1] < call_new[z,14]){
      df_gains_call2[i,z] <- (prob_def_high[i,3] * (call_new[z,10] * call_new[z,9]*100))
    }else{
      df_gains_call2[i,z] <- 0
    }
  }
}
view(df_gains_call2)

df_gains_call <- matrix(nrow = ncol(df_gains_call1), ncol = 3)
for (i in 1:nrow(call_new)) {
  df_gains_call[i,1] <- sum(df_gains_call1[,i])
}

for (i in 1:nrow(call_new)) {
  df_gains_call[i,2] <- (sum(df_gains_call2[,i]))^2
}

df_gains_call[,3] <- df_gains_call[,1] - df_gains_call[,2]

for (i in 1:nrow(df_gains_call)) {
  if(df_gains_call[i,3] <= 0){
    df_gains_call[i,3] <- 0.000001
  }else{
    df_gains_call[i,3] <- df_gains_call[i,3]
  }
}

view(df_gains_call)

# Creating Hidden Risk Matrix

hidden_risk_mat <- matrix(nrow = nrow(call_new), ncol = 3)

hidden_risk_mat[,1] <- df_losses_call[,3]
hidden_risk_mat[,2] <- df_gains_call[,3]
hidden_risk_mat[,3] <- hidden_risk_mat[,1]/hidden_risk_mat[,2]
colnames(hidden_risk_mat) <- c("Loss_Risk", "Gain_Risk", "Hidden_Risk")
hidden_risk_mat <- as.data.frame(hidden_risk_mat)
hidden_risk_mat[,4] <- log(hidden_risk_mat[,3])
colnames(hidden_risk_mat)[4] <- "Log_Hidden_Risk"
view(hidden_risk_mat)

call_new <- call_new[,-c(15:19)]
view(call_new)

call_new <- cbind(call_new, hidden_risk_mat)
view(call_new)


call_new %>%
  ggplot(aes(x = necessary_returns, y = Hidden_Risk))+
  geom_point(aes(color = spot_price))+
  labs(x = "Neccessary Returns",
       y = "Hidden Risk",
       title = "Hidden Risk of Call Options")+
  scale_y_continuous(labels = comma)

call_new %>%
  ggplot(aes(x = Log_Hidden_Risk))+
  geom_histogram()


##### Put Options Hidden Risk Matrix #####

hidden_risk_mat2 <- matrix(ncol = 3, nrow = nrow(put_new))

# For Losses

df_losses_put1 <- matrix(ncol = nrow(put_new), nrow = nrow(prob_def_low))
for (i in 1:nrow(df_losses_put1)) {
  for (z in 1:ncol(df_losses_put1)) {
    if(prob_def_low[i,1] <= put_new[z,14]){
      df_losses_put1[i,z] <- (prob_def_low[i,3] * (((put_new[z,7]-put_new[z,9]) - (1+prob_def_low[i,1])*put_new[z,13])*put_new[z,10]*100)^2)
    }else{
      df_losses_put1[i,z] <- 0
    }
  }
}
view(df_losses_put1)

df_losses_put2 <- matrix(ncol = nrow(put_new), nrow = nrow(prob_def_low))
for (i in 1:nrow(df_losses_put2)) {
  for (z in 1:ncol(df_losses_put2)) {
    if(prob_def_low[i,1] <= put_new[z,14]){
      df_losses_put2[i,z] <- (prob_def_low[i,3] * (((put_new[z,7]-put_new[z,9]) - (1+prob_def_low[i,1])*put_new[z,13])*put_new[z,10]*100))
    }else{
      df_losses_put2[i,z] <- 0
    }
  }
}
view(df_losses_put2)

df_losses_put <- matrix(nrow = ncol(df_losses_put1), ncol = 3)
for (i in 1:nrow(put_new)) {
  df_losses_put[i,1] <- sum(df_losses_put1[,i])
}

for (i in 1:nrow(put_new)) {
  df_losses_put[i,2] <- (sum(df_losses_put2[,i]))^2
}

df_losses_put[,3] <- df_losses_put[,1] - df_losses_put[,2]

for (i in 1:nrow(df_losses_put)) {
  if(df_losses_put[i,3] <= 0){
    df_losses_put[i,3] <- 0.000001
  }else{
    df_losses_put[i,3] <- df_losses_put[i,3]
  }
}
view(df_losses_put)


# For Gains 

df_gains_put1 <- matrix(ncol = nrow(put_new), nrow = nrow(prob_def_low))
for (i in 1:nrow(df_gains_put1)) {
  for (z in 1:ncol(df_gains_put1)) {
    if(prob_def_low[i,1] > put_new[z,14]){
      df_gains_put1[i,z] <- (prob_def_low[i,3] * (put_new[z,10] * put_new[z,9]*100)^2)
    }else{
      df_gains_put1[i,z] <- 0
    }
  }
}
view(df_gains_put1)

df_gains_put2 <- matrix(ncol = nrow(put_new), nrow = nrow(prob_def_low))
for (i in 1:nrow(df_gains_put2)) {
  for (z in 1:ncol(df_gains_put2)) {
    if(prob_def_low[i,1] > put_new[z,14]){
      df_gains_put2[i,z] <- (prob_def_low[i,3] * (put_new[z,10] * put_new[z,9]*100))
    }else{
      df_gains_put2[i,z] <- 0
    }
  }
}

df_gains_put <- matrix(nrow = ncol(df_gains_put1), ncol = 3)
for (i in 1:nrow(put_new)) {
  df_gains_put[i,1] <- sum(df_gains_put1[,i])
}

for (i in 1:nrow(put_new)) {
  df_gains_put[i,2] <- (sum(df_gains_put2[,i]))^2
}

df_gains_put[,3] <- df_gains_put[,1] - df_gains_put[,2]

for (i in 1:nrow(df_gains_put)) {
  if(df_gains_put[i,3] <= 0){
    df_gains_put[i,3] <- 0.000001
  }else{
    df_gains_put[i,3] <- df_gains_put[i,3]
  }
}
view(df_gains_put)


# Creating Hidden Risk Matrix

hidden_risk_mat2 <- matrix(nrow = nrow(put_new), ncol = 3)

hidden_risk_mat2[,1] <- df_losses_put[,3]
hidden_risk_mat2[,2] <- df_gains_put[,3]
hidden_risk_mat2[,3] <- hidden_risk_mat2[,1]/hidden_risk_mat2[,2]
colnames(hidden_risk_mat2) <- c("Loss_Risk", "Gain_Risk", "Hidden_Risk")
hidden_risk_mat2 <- as.data.frame(hidden_risk_mat2)
hidden_risk_mat2[,4] <- log(hidden_risk_mat2[,3])
colnames(hidden_risk_mat2)[4] <- "Log_Hidden_Risk"
view(hidden_risk_mat2)

put_new <- put_new[,-c(15:19)]

put_new <- cbind(put_new, hidden_risk_mat2)
view(put_new)


options_new <- rbind(call_new, put_new)
view(options_new)


##### Risk Premium Calc #####

# Call Options

RP_call <- matrix(nrow = nrow(prob_def_high), ncol = nrow(call_new))
for (i in 1:nrow(prob_def_high)) {
  for (z in 1:nrow(call_new)) {
    if(call_new[z,14] < prob_def_high[i,1]){
      RP_call[i,z] <- -prob_def_high[i,3]*((1+prob_def_high[i,1])*call_new[z,13]-(call_new[z,7]+call_new[z,9]))/call_new[z,9]
    }else{
      RP_call[i,z] <- prob_def_high[i,3]
    }
  }
}
view(RP_call)

RP_call_sum <- vector(length = ncol(RP_call))
for (i in 1:nrow(call_new)) {
  RP_call_sum[i] <- sum(RP_call[,i])
}
RP_call_sum <- round(RP_call_sum, digits = 4)
RP_call_sum <- as.data.frame(RP_call_sum)
RP_call_sum$RP_call_sum <- as.numeric(RP_call_sum$RP_call_sum)
colnames(RP_call_sum) <- "risk_prem"
view(RP_call_sum)

call_new <- cbind(call_new, RP_call_sum)
view(call_new)




# Put Options

RP_put <- matrix(nrow = nrow(prob_def_low), ncol = nrow(put_new))
for (i in 1:nrow(prob_def_low)) {
  for (z in 1:nrow(put_new)) {
    if(put_new[z,14] > prob_def_low[i,1]){
      RP_put[i,z] <- -prob_def_low[i,3] * ((put_new[z,7]-put_new[z,9]) - (1+prob_def_low[i,1])*put_new[z,13])
    }else{
      RP_put[i,z] <- prob_def_low[i,3]
    }
  }
}
view(RP_put)

RP_put_sum <- vector(length = ncol(RP_put))
for (i in 1:nrow(put_new)) {
  RP_put_sum[i] <- sum(RP_put[,i])
}
RP_put_sum <- round(RP_put_sum, digits = 4)
RP_put_sum <- as.data.frame(RP_put_sum)
RP_put_sum$RP_put_sum <- as.numeric(RP_put_sum$RP_put_sum)
colnames(RP_put_sum) <- "risk_prem"
view(RP_put_sum)

put_new <- cbind(put_new, RP_put_sum)
view(put_new)


options_new <- rbind(call_new, put_new)



##### Graphs and Vizuals #####

options_new %>%
  ggplot(aes(x = risk_prem, y = Log_Hidden_Risk))+
  geom_point()+
  labs( x = "Risk Premium",
        y = "Log Tail Risk Ratio",
        title = "Figure 4: Risk Premium vs Log Tail Risk Ratio",
        subtitle = "Options Sellers")


put_new %>%
  filter(risk_prem>0)%>%
  filter(sym == "AAPL")%>%
  filter(necessary_returns <= 0.05 & necessary_returns >= -0.05)%>%
  ggplot(aes(x = risk_prem, y = Log_Hidden_Risk))+
  geom_point(aes(color = necessary_returns))+
  labs(x = "Risk Premium",
       y = "Log Tail Risk Ratio",
       title = "Tail Risk Ratio of Apple Put Options Compared to 
                 Their Risk Premium")

call_new %>%
  filter(sym == "AAPL")%>%
  filter(risk_prem > 0)%>%
  filter(necessary_returns <= 0.05 & necessary_returns >= -0.05)%>%
  ggplot(aes(x = risk_prem, y = Log_Hidden_Risk))+
  geom_point(aes(color = necessary_returns))+
  labs(x = "Risk Premium",
       y = "Log Tail Risk Ratio",
       title = "Tail Risk Ratio of Apple Call Options Compared to 
                 Their Risk Premium")


View(options_new)
view(combined)


##### Loading the cbb data and cleaning #####

cbb3 <- read_excel("~/Desktop/Stocks Data/CBB3_data.xlsx")
ML_money_data <- read_excel("~/Desktop/Stocks Data/ML_money_data.xlsx")
ML_money_data <- na.omit(ML_money_data)

view(cbb3)

# Removing Teams with no opponent in the dataset (removed due to NAs)

cbb3 <-  cbb3[,-17]

for (i in 1:nrow(cbb3)) {
  cbb3[i,22] <- i
}
colnames(cbb3)[22] <- "id"

view(cbb3 %>%
       filter(Game_num != lag(Game_num) & 
                Game_num != lead(Game_num))
)

cbb3 <- cbb3[-c(16729, 64040, 70513, 71664),]

# Making CBB3 and options datasets compatable 
cbb3$id <- 0
options_new$id <- 1

cbb3_0 <- cbb3%>%
  filter(ML_spread == 0)%>%
  filter(Game_num == lag_id)

cbb3_pos <- cbb3 %>%
  filter(ML_spread > 0)

cbb3_test <- rbind(cbb3_pos, cbb3_0)

options_new_risk <- options_new[,c(19,17,20)]
cbb_risk <- cbb3_test[,c(17,20,22)]
colnames(cbb_risk) <- c("risk_prem","trr","id")
colnames(options_new_risk) <- c("risk_prem", "trr","id")
comp_risk <- rbind(options_new_risk, cbb_risk)
view(comp_risk)

cbb_log <- cbb3_test[,21]
view(cbb_log)
cbb_log$id <- 0
opt_log <- options_new$Log_Hidden_Risk
opt_log <- as.data.frame(opt_log)
colnames(opt_log) <- "log_hidden_risk"
view(opt_log)
opt_log$id <- 1
log_baby <- rbind(cbb_log, opt_log)

##### Running the t-test and White-Hausemann test #####

view(comp_risk)

median(options_new$risk_prem)

mean(options_new_risk$risk_prem)
comp_risk %>%
       filter(id == 1) %>%
       summarise(td = mean(trr))

rp_t <- t_test(~ risk_prem, groups = id, data = comp_risk)

lhr_t <- t_test(~ log_hidden_risk, groups = id, data = log_baby)

rp_w <- wilcox.test(options_new_risk$risk_prem, cbb_risk$risk_prem, conf.int = TRUE, 
            conf.level = 0.95)
lhr_w <- wilcox.test(log(options_new_risk$trr), log(cbb_risk$trr), 
            correct = TRUE, conf.int = TRUE, conf.level = 0.95)

##### Creating tables 4 and 5 #####

ttable <- tibble("Variable" = "Risk Premium",
              "t-value" = comma(rp_t$statistic, accuracy = 0.001),
      "Sportsbook Mean" = comma(rp_t$estimate[1], accuracy = 0.001),
      "Options Trading Mean" = comma(rp_t$estimate[2], accuracy = 0.001),
      "p-value" = rp_t$p.value,
      "Lower Bound Confidence Interval" = comma(rp_t$conf.int[1], accuracy = 0.001),
      "Upper Bound Confidence Interval" = comma(rp_t$conf.int[2], accuracy = 0.001),
      )

view(ttable)


ttable2 <- tibble("Variable" = "Log Tail Risk Ratio",
               "t-value" = comma(lhr_t$statistic, accuracy = 0.001),
             "Sportsbook Mean" = comma(lhr_t$estimate[1], accuracy = 0.001),
             "Options Trading Mean" = comma(lhr_t$estimate[2], accuracy = 0.001),
             "p-value" = lhr_t$p.value,
             "Lower Bound Confidence Interval" = comma(lhr_t$conf.int[1], accuracy = 0.001),
             "Upper Bound Confidence Interval" = comma(lhr_t$conf.int[2], accuracy = 0.001),
             )

table_4 <- rbind(ttable, ttable2) 

view(table_4)

wtable <- tibble("Variable" = "Risk Premium",
                 "W-value" = comma(rp_w$statistic, accuracy = 0.001),
                 "Sportsbook Median" = comma(median(cbb_risk$risk_prem), accuracy = 0.001),
                 "Options Trading Median" = comma(median(options_new_risk$risk_prem), accuracy = 0.001),
                 "p-value" = rp_w$p.value,
                 "Lower Bound Confidence Interval" = comma(rp_w$conf.int[1], accuracy = 0.001),
                 "Upper Bound Confidence Interval" = comma(rp_w$conf.int[2], accuracy = 0.001),
)

wtable2 <- tibble("Variable" = "Log Tail Risk Ratio",
                  "W-value" = comma(lhr_w$statistic, accuracy = 0.001),
                  "Sportsbook Median" = comma(median(cbb_log$log_hidden_risk), accuracy = 0.001),
                  "Options Trading Median" = comma(median(opt_log$log_hidden_risk), accuracy = 0.001),
                  "p-value" = lhr_w$p.value,
                  "Lower Bound Confidence Interval" = comma(lhr_w$conf.int[1], accuracy = 0.001),
                  "Upper Bound Confidence Interval" = comma(lhr_w$conf.int[2], accuracy = 0.001),
)
view(wtable2)

table_5 <- rbind(wtable, wtable2)

view(table_5)

##### Creating Tables 1, 2, and 3 #####

test_tab2 <- options_new %>%
  select(strike_price,
         spot_price,
         ask,
         necessary_returns) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(min = min(v), median = median(v),
            max = max(v), mean = mean(v), 
            sd = sd(v),
            n = n())

table_money <- ML_money_data %>%
  select(ML_money) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(min = min(v), median = median(v),
            max = max(v), mean = mean(v), 
            sd = sd(v),
            n = n())


test_tab1 <- cbb3 %>%
  select(ML_spread,
         ML) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(min = min(v), median = median(v),
            max = max(v), mean = mean(v), 
            sd = sd(v),
            n = n())

test_tab3 <- high_change %>%
  select(change) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(min = min(v), median = median(v),
            max = max(v), mean = mean(v), 
            sd = sd(v),
            n = n()) 

test_tab4 <- low_change %>%
  select(change) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(min = min(v), median = median(v),
            max = max(v), mean = mean(v), 
            sd = sd(v),
            n = n()) 

table_1 <- rbind(test_tab1, table_money, test_tab2, test_tab3, test_tab4)

view(table_1)
colnames(table_1) <- c("Variable Name", "Minimum", "Median",
                         "Maximum", "Mean", "Standard Deviation", "n")

table_1[1,1] <- "Money Line"
table_1[2,1] <- "Money Line Spread"
table_1[3,1] <- "Percentage of Money"
table_1[4,1] <- "Ask Price"
table_1[5,1] <- "Necessary Returns"
table_1[6,1] <- "Spot Price"
table_1[7,1] <- "Strike Price"
table_1[8,1] <- "High Change"
table_1[9,1] <- "Low Change"
view(table_1)


table_1$Minimum <- comma(table_1$Minimum, accuracy = 0.001)
table_1$Maximum <- comma(table_1$Maximum, accuracy = 0.001)
table_1$Median <- comma(table_1$Median, accuracy = 0.001)
table_1$Mean <- comma(table_1$Mean, accuracy = 0.001)
table_1$`Standard Deviation` <- comma(table_1$`Standard Deviation`, accuracy = 0.001)
table_1$n <- comma(table_1$n)



view(table_1)

x <- c("Sportsbook Reviews Online",
       "Sportsbook Reviews Online",
       "Sportsbettingdime.com",
       "Options Metrics",
       "Options Metrics",
       "Data Stream",
       "Options Metrics",
       "Boris Marjanovic, Kaggle",
       "Boris Marjanovic, Kaggle"
)

table_1 <- cbind(table_1, x)
colnames(table_1)[8] <- "Data Source"
view(table_1)

risk_tab_cbb <- cbb_risk %>%
  select(risk_prem,
         trr
  ) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(Minimum = min(v), Median = median(v),
            Maximum = max(v), Mean = mean(v), 
            "Standard Deviation" = sd(v),
            n = n())

view(risk_tab_cbb)

risk_tab_opt <- options_new %>%
  select(risk_prem,
         Hidden_Risk
  ) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(Minimum = min(v), Median = median(v),
            Maximum = max(v), Mean = mean(v), 
            "Standard Deviation" = sd(v),
            n = n())

view(risk_tab_opt)

table_2 <- rbind(risk_tab_cbb, risk_tab_opt[2,], risk_tab_opt[1,])
view(table_2)

colnames(table_2)[1] <- "Variable Name"

table_2[1,1] <- "Spors Betting Risk Premium"
table_2[2,1] <- "Spors Betting Tail Risk Ratio"
table_2[3,1] <- "Options Trading Risk Premium"
table_2[4,1] <- "Options Trading Tail Risk Ratio"


table_2$Minimum <- comma(table_2$Minimum, accuracy = 0.001)
table_2$Maximum <- comma(table_2$Maximum, accuracy = 0.001)
table_2$Mean <- comma(table_2$Mean, accuracy = 0.001)
table_2$Median <- comma(table_2$Median, accuracy = 0.001)
table_2$`Standard Deviation` <- comma(table_2$`Standard Deviation`, accuracy = 0.001)
table_2$n <- comma(table_2$n)
view(table_2)



log_table <- 
  options_new %>%
  select(Log_Hidden_Risk
  ) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(Minimum = min(v), Median = median(v),
            Maximum = max(v), Mean = mean(v), 
            "Standard Deviation" = sd(v),
            n = n())

view(log_table)



log_table2 <- 
  cbb_log %>%
  select(log_hidden_risk) %>%
  gather(k,v) %>% group_by(k) %>%
  summarise(Minimum = min(v), Median = median(v),
            Maximum = max(v), Mean = mean(v), 
            "Standard Deviation" = sd(v),
            n = n())

table_3 <- rbind(log_table2, log_table)
view(table_3)

for (i in 2:ncol(table_3)) {
  table_3[,i] <- round(table_3[,i], digits = 3)
}
view(table_3)

colnames(table_3)[1] <- "Variable Name"
table_3[1,1] <- "Sports Betting Log Hidden Risk"
table_3[2,1] <- "Options Trading Log Hidden Risk"

table_3$n <- comma(table_3$n)
view(table_3)


risk_tab2 <- rbind(risk_tab[c(1,3),], table_3)
view(risk_tab2)

##### Figures 3-8 #####

Figure_3 <- comp_risk %>%
  filter(id == 0) %>%
  ggplot(aes(x = risk_prem, y = log(trr)))+
  geom_point()+
  labs(x = "Risk Premium",
       y = "Log Tail Risk Ratio",
       title = "Figure 3: Risk Premium vs Log Tail Risk Ratio",
       subtitle = "Sportsbooks")

Figure_3

Figure_4 <- options_new %>%
  ggplot(aes(x = risk_prem, y = Log_Hidden_Risk))+
  geom_point()+
  labs(x = "Risk Premium",
       y = "Log Tail Risk Ratio",
       title = "Figure 4: Risk Premium vs Log Tail Risk Ratio",
       subtitle = "Options Sellers")

Figure_4

Figure_5 <- comp_risk %>%
  filter(id == 0) %>%
  ggplot(aes(x = risk_prem))+
  geom_histogram(binwidth = 0.05)+
  labs(x = "Risk Premium",
       title = "Figure 5: Sportsbooks' Risk Premium Histogram")


Figure_6 <- comp_risk %>%
  filter(id == 1) %>%
  ggplot(aes(x = risk_prem))+
  geom_histogram()+
  labs(x = "Risk Premium",
       title = "Figure 6: Options Sellers' Risk Premium Histogram")

Figure_7 <- comp_risk %>%
  filter(id == 0) %>%
  ggplot(aes(x = log(trr)))+
  geom_histogram()+
  labs(x = "Tail Risk Ratio",
       title = "Figure 7: Sportsbooks' Tail Risk Ratio Histogram")

Figure_8 <- comp_risk %>%
  filter(id == 1) %>%
  ggplot(aes(x = log(trr)))+
  geom_histogram()+
  labs(x = "Tail Risk Ratio",
       title = "Figure 8: Options Sellers' Tail Risk Ratio Histogram")

(Figure_5 | Figure_6) / (Figure_7 | Figure_8)


library(writexl)
write_xlsx(comp_risk, "comp_risk.xlsx")
write_xlsx(options_new, "options_new.xlsx")
write_xlsx(cbb3, "cbb3_final.xlsx")
write_xlsx(high_change, "high_change.xlsx")
write_xlsx(low_change, "low_change.xlsx")


