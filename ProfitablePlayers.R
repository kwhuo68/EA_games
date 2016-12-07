install.packages("dplyr")

library(dplyr)
library(car)
require("magrittr")

original_results = read.csv("focal_results.csv")
original_users = read.csv("focal_users.csv")
users = original_users
results = original_results

# Manipulation ###################################################################################
# change types and other stuff so that we can use regression

users$product_owned_count_at_start = as.numeric(as.character(users$product_owned_count_at_start))
# assume NULL (which got mapped to NA) means no products were owned
users$product_owned_count_at_start[is.na(users$product_owned_count_at_start)] = 0

users$got_bfbc2 = as.numeric(users$bfbc2 != "2000-01-01")
users$got_prem = as.numeric(users$bf3prem != "2000-01-01")
users$got_bf4 = as.numeric(users$bf4 != "2000-01-01")

# 2012-01-31 is the latest signup date
users$signup = as.numeric(difftime("2012-01-31", users$Signup_Date, units = "days"))
# lower user$signup = later signup in this case

# Models ########################################################################################

model_got_prem = lm(got_prem ~ Country_Code + signup + User_Age + Gender_Code + 
              Product_Owned_Cnt + got_bfbc2 + PS3_Platform_Activity_Flag + 
              XBOX360_Platform_Activity_Flag + PC_Platform_Activity_Flag +
              product_owned_count_at_start, data = users)
summary(model_got_prem)
# we see that the significant variables are signup (p-val = 6.8%, - sign), product_owned_cnt (+),
#      got_bfbc2 (+), XBOX360 (+), PC (+), and product_owned_at_start (-)

model_got_bf4 = lm(got_bf4 ~ Country_Code + signup + User_Age + Gender_Code + 
                      Product_Owned_Cnt + got_bfbc2 + PS3_Platform_Activity_Flag + 
                      XBOX360_Platform_Activity_Flag + PC_Platform_Activity_Flag +
                      product_owned_count_at_start, data = users)
summary(model_got_bf4)
# we see that the significant variables are user_age (+), product_owned_cnt (+),
#      got_bfbc2 (+), XBOX360 (+), and product_owned_at_start (-)

# Insights ######################################################################################

# Common indicators of profitable players across both models are the following:
#         higher product_owned_cnt and lower product_owned_at_start
#            --> so they bought products while playing
#         got_bfbc2 (which was said to be a sign of loyalty, so this is expected)
#         has played on XBOX360 (interesting...)

#################################################################################################
# New Push ######################################################################################

users = users %>% mutate(profit = got_bf4 + got_prem)
model_profit = lm(profit ~ Country_Code + signup + User_Age + Gender_Code + 
                     Product_Owned_Cnt + got_bfbc2 + PS3_Platform_Activity_Flag + 
                     XBOX360_Platform_Activity_Flag + PC_Platform_Activity_Flag +
                     product_owned_count_at_start, data = users)
summary(model_profit)
# we see that the significant variables are user_age (p-val = 9.6%, + sign), product_owned_cnt (+),
#      got_bfbc2 (+), XBOX360 (+), PC (+), and product_owned_at_start (-)

quantile(original_results$player_seconds, c(0.01, 0.02, 0.03))
# 9.94, 23.23, 39.20; lets filter out any data < 30 seconds
results = results %>% filter(player_seconds >= 30)
rounds = results %>% group_by(user_account_id) %>% count() 
quantile(rounds$n, c(0.01, 0.02, 0.03))
# 7, 14, 18; we will also filter out people who played fewer than 10 rounds

total_score = tapply(results$total_score, results$user_account_id, mean)
rank_diff = tapply(results$rank_at_end - results$rank_at_start, results$user_account_id, mean)
rank_diff[is.na(rank_diff)] = 0
combat_score = tapply(results$combat_score, results$user_account_id, mean)

killCnt = tapply(results$killCnt, results$user_account_id, sum)
deathCnt = tapply(results$deathCnt, results$user_account_id, sum)
kdr = killCnt / deathCnt
quantile(kdr, c(0.95, 0.96, 0.97)) # 95% is 14.5, 96% and above is infinity
summary(kdr)
head(sort(kdr[!is.infinite(kdr)], decreasing = TRUE))
# max removing infinity is 127, lets set inifnity to 130
kdr[is.infinite(kdr)] = 130

freq = results %>% select(user_account_id, round_start_date) %>%
  arrange(user_account_id, round_start_date)
user_freq = split(freq, freq$user_account_id)
round_diff = lapply(user_freq, function(x) {
  tmp = diff(x$round_start_date)
  tmp })
mean_freq = lapply(round_diff, function(x) mean(as.numeric(x)))

player_data = data.frame("user_account_id" = names(total_score),
                         "profit" = users$profit,
                         "rounds_played" = rounds$n,
                         "mean_total_score" = as.vector(total_score),
                         "mean_combat_score" = as.vector(combat_score),
                         "rank_diff" = as.vector(rank_diff),
                         "killCnt" = as.vector(killCnt),
                         "deathCnt" = as.vector(deathCnt),
                         "kdr" = as.vector(kdr),
                         "mean_round_interval" = (as.vector(unlist(mean_freq))*24))
player_data = player_data %>% filter(rounds_played >= 10)

rm(combat_score, deathCnt, freq, kdr, killCnt, mean_freq, rank_diff, round_diff,
   total_score, user_freq)

cor(player_data$mean_total_score, player_data$rounds_played) # 0.51
cor(player_data$mean_total_score, player_data$mean_combat_score) # 0.97
cor(player_data$mean_total_score, player_data$rank_diff) # -0.20
cor(player_data$mean_total_score, player_data$kdr) # -0.14
cor(player_data$mean_total_score, player_data$mean_round_interval) # -0.22

# we expect the following three simple regressions to have significant, + coefs
# we won't include all of them in the later multiple regression because it will 
# introduce multicollinearity (which we will formally check through VIFs)
# will will also remove deathCnt
summary(lm(profit ~ rounds_played, data = player_data))
summary(lm(profit ~ mean_total_score, data = player_data))
summary(lm(profit ~ mean_combat_score, data = player_data))
# all have significant, positive signs

model_all = lm(profit ~ rounds_played + mean_total_score + mean_combat_score
               + rank_diff + killCnt + deathCnt + kdr + mean_round_interval, 
               data = player_data)
summary(model_all)
vif(model_all) # clearly we need to remove some variables

model_adj = lm(profit ~ mean_total_score + rank_diff + killCnt
               + kdr + mean_round_interval, data = player_data)
summary(model_adj) # only mean_total_score is significant, lets check the other two
vif(model_adj) # all fine now

summary(lm(profit ~ mean_combat_score + rank_diff + killCnt + kdr
           + mean_round_interval, data = player_data))
# combat score is significant

summary(lm(profit ~ rounds_played + rank_diff + killCnt + kdr
           + mean_round_interval, data = player_data))
# rounds played isn't significant

# lets check all the simple regressions
summary(lm(profit ~ rank_diff, data = player_data))
summary(lm(profit ~ killCnt, data = player_data))
summary(lm(profit ~ deathCnt, data = player_data))
summary(lm(profit ~ kdr, data = player_data))
summary(lm(profit ~ mean_round_interval, data = player_data))
# only killCnt and deathCnt (both + sign)

# Insights ######################################################################################

# Profitable players are those that:
#       play many rounds (not necessarily in a short time frame)
#       get high total and combat scores
# All of these attributes are correlated with each other
# So overall, our indicators of profitability are:
#       high volume, good players
#       item purchases
#       XBOX or PC users
#       older players (maybe because they have money)
#       'loyal' players (that got bfbc2)

# Next step: try some other (more complex) models
#       logistical regression (multinom command http://www.ats.ucla.edu/stat/r/dae/mlogit.htm)
#       trees http://www.statmethods.net/advstats/cart.html
