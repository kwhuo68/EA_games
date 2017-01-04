#load data
focal.users = read.csv("~/#UPenn/#WUDAC Consulting/focal_users.csv")
focal.results = read.csv("~/#UPenn/#WUDAC Consulting/focal_results.csv")

#clean data 
focal.users$Country_Code <- NULL 
focal.users$User_Age <- NULL 
focal.users$Gender_Code <- NULL 
focal.users$Signup_Date[which(focal.users$Signup_Date == '2000-01-01')]= NA
focal.users$bf3[which(focal.users$bf3 == '2000-01-01')]= NA
focal.users$bf3prem[which(focal.users$bf3prem == "2000-01-01")]= NA
focal.users$bf4[which(focal.users$bf4 == "2000-01-01")]= NA
focal.users$bfbc2[which(focal.users$bfbc2 == "2000-01-01")]= NA
#between focal users vs focal results - focal results only has 611 unique ids, focal users has 1309 ids 
user.id <- unique(focal.users$User_Account_ID)
user.id.results <- unique(focal.results$user_account_id)




#find intensive players - product count - range 1-63 
#convert factors to numerics 
focal.users$User_Account_ID <- as.numeric(as.character(focal.users$User_Account_ID))
focal.users$Product_Owned_Cnt <- as.numeric(as.character(focal.users$Product_Owned_Cnt))
focal.users$product_owned_count_at_start <- as.numeric(as.character(focal.users$product_owned_count_at_start))
#find products bought over time (since first GAME played of bf3)
focal.users$proddiff <- with(focal.users, Product_Owned_Cnt - product_owned_count_at_start)
#find difference in dates (assuming first GAME played is @bf3)
#change factors to dates 
focal.results$round_start_date <- as.Date(focal.results$round_start_date, format = "%Y-%m-%d")
#find minimum date for each user 
bf3.start <- setNames(aggregate.data.frame(focal.results$round_start_date, list(focal.results$user_account_id), function(x) min(as.character(x)) ), c("user_account_id", "bf3_start"))
#calculate days played - insert list of end date (march 4, 2014)
bf3.start$end_date <- rep("2014-03-04", length(bf3.start$bf3_start))
#calculate date diff 
bf3.start$days_start <- as.Date(as.character(bf3.start$end_date)) - as.Date(as.character(bf3.start$bf3_start))
#calculate avg rate each user purchased products - combine the columns 
bf3.products <- merge(bf3.start, focal.users, by.x = "user_account_id", by.y = "User_Account_ID", all.x = TRUE)
#calculate prod / yr
bf3.products$prod_yr <- (bf3.products$proddiff / as.numeric(bf3.products$days_start)) * 365

#look at the distribution of the data - round data to make graphing easier 
bf3.products$prod_round <- round(bf3.products$prod_yr, digits = 0)
hist(bf3.products$prod_round, breaks = 20, main = "Distribution of Product Rates", xlab = "Product Rates")

#split into two groups that can be compared - instead of basing on cohorts, want casual v intense 
#611 entries, so median is at 306
order.bf3.products<- bf3.products[order(bf3.products$prod_round),]
#median falls into the 3-range 
order.bf3.products[301:311,]
#we'll split the data into [0,3] and [3,20] - matches distribution 




#find premium users 
#compare prem v non-prem user 
focal.users$prem <- ifelse(is.na(focal.users$bf3prem), 0, 1)
#split into groups - those with premium, those without 



#find gameplay frequency & compare with 'intensive' & 'premium' 

#find the days played/days when started to play 
#first calculate total games played by userID 
games.played <- as.data.frame(table(focal.results$user_account_id))
#connect length of play with days played 
games.days <- merge(games.played, bf3.start, by.x = "Var1", by.y = "user_account_id", all.x = TRUE)
#find freq of games/year
games.days$days_start <- as.numeric(games.days$days_start)
games.days$games_freq <- games.days$Freq * 365 / games.days$days_start

#following Pareto principle? 
#check if top 20% of intensive players account for 80% of the gameplay frequency
#out of the 611 intensive players, top 20% = top 122 players 
pareto <- order.bf3.products[order(order.bf3.products$prod_yr,decreasing=TRUE)[1:122],] 
#subset # of rounds that have the same user ids 
pareto.rounds <- merge(focal.results, pareto, by.x = "user_account_id", by.y = "user_account_id", all.y = TRUE)
#this becomes 1800 out of 7895, which is = approx. 22.8% 
#this does not follow the Pareto principle 

#compare distribution of freq with 'intensive' 
#combine data with product data 
games.intensive <- merge(games.days, order.bf3.products, by.x = "Var1", by.y = "user_account_id", all.x = TRUE)
#make a scatterplot with prods vs game freq 
plot(games.intensive$prod_yr, games.intensive$games_freq, main="Intensity v Frequency", col = ifelse(games.intensive$prod_yr < 3.000000000001,'red','blue'), xlab="Products/Year ", ylab="Games/Year")
#not a strong relationship between games/year and products bought/year 
#check correlation
cor(games.intensive$prod_yr, y=games.intensive$games_freq, use = "complete.obs")
#cor = .08823461 = not a very strong correlation / relationship between the two 
#check boxplots
games.intensive$intensive <- ifelse(games.intensive$prod_yr > 3, 1, 0)
boxplot(games.intensive$games_freq~ games.intensive$intensive, data = games_nint, main="Distribution of Games/Year by Products", xlab="Product", ylab="Games/Year")
#doesn't seem to be super different, can check statistical significance 
games_nint <- subset(games.intensive, games.intensive$prod_yr < 3.000000001)
games_int <- subset(games.intensive, games.intensive$prod_yr > 3)
t.test(games_nint$games_freq, games_int$games_freq)
#Welch Two Sample t-test
#data:  games_nint$games_freq and games_int$games_freq
#t = -1.0905, df = 441.76, p-value = 0.2761
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.9762331  0.5657782
#sample estimates:
 # mean of x mean of y 
#5.801720  6.506948 
#fail to reject null hypothesis, there is not a statistically significant difference between 'intensive' players v freq of gameplay

#compare distribution of freq with 'premium' 
#combine data with premium data 
games.prem <- merge(games.days, focal.users, by.x = "Var1", by.y = "User_Account_ID", all.x = TRUE)
#split two boxplots depending on prem v non-prem 
boxplot(games.prem$games_freq ~ games.prem$prem, data = games.prem, main="Distribution of Games/Year by Premium", xlab="Premium", ylab="Games/Year")
#premium users do play more games (on average)!! larger skew, larger median. 
#there is a relationship between buying premium and playing more games 
#is the difference statistically significant? 
games_nprem <- subset(games.prem, games.prem$prem == 0)
games_prem <- subset(games.prem, games.prem$prem == 1)
#two-sample diff in means t test 
t.test(games_nprem$games_freq, games_prem$games_freq)
#data:  games_nprem$games_freq and games_prem$games_freq
#t = -8.7259, df = 442.8, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # -6.028561 -3.812147
#sample estimates:
#  mean of x mean of y 
#3.781417  8.701771 
#p-value is smaller than alpha (.05), so we reject the null hypothesis that the means between prem v non-prem are the same; so our data is statistically significant

#can be used to influence advertising strategy - don't necessarily push for other products, can encourage more premium purchases more 

#find engagement/round 
#subset roundtime data 
round_time <- as.data.frame(cbind(focal.results$user_account_id, focal.results$player_seconds, focal.results$round_seconds))
#find % of active play each round 
round_time$play_time <- with(round_time, V2 * 100 /V3)
#calculate the aggregate active play for each user
play.mean <- aggregate(play_time ~ V1, round_time, mean)
play.median <- aggregate(play_time ~ V1, round_time, median)
play.stat <- merge(play.mean, play.median, by.x = "V1", by.y = "V1", all.x = TRUE)
play.intensive <- merge(order.bf3.products, play.stat, by.x = "user_account_id", by.y = "V1", all.y = TRUE)

#intensity v mean active play 
plot(play.intensive$prod_yr, play.intensive$play_time.x, main="Intensity v Mean Active Play", col = ifelse(games.intensive$prod_yr < 3.000000000001,'red','blue'), xlab="Products/Year ", ylab="% Active Play")
#intensity v median active play 
plot(play.intensive$prod_yr, play.intensive$play_time.y, main="Intensity v Median Active Play", col = ifelse(games.intensive$prod_yr < 3.000000000001,'red','blue'), xlab="Products/Year ", ylab="% Active Play")
#not a strong relationship between % active play and products/year 
cor(play.intensive$prod_yr, y=play.intensive$play_time.x, use = "complete.obs")
#cor = -0.01237227 = slightly negative relationship 
cor(play.intensive$prod_yr, y=play.intensive$play_time.y, use = "complete.obs")
#cor = 0.002219681 = slightly positive relationship 
#check boxplots - mean
play.intensive$intensive <- ifelse(play.intensive$prod_yr > 3, 1, 0)
boxplot(play.intensive$play_time.x~ play.intensive$intensive, data = games_nint, main="Distribution of Mean Active Play by Products", xlab="Product", ylab="% Active Play")
#check boxplots - median 
play.intensive$intensive <- ifelse(play.intensive$prod_yr > 3, 1, 0)
boxplot(play.intensive$play_time.y ~ play.intensive$intensive, data = games_nint, main="Distribution of Median Active Play by Products", xlab="Product", ylab="% Active Play")
#boxplots do not seem to be very different, can check statistical significance 
#mean 
play_nint <- subset(play.intensive, play.intensive$prod_yr < 3.000000001)
play_int <- subset(play.intensive, play.intensive$prod_yr > 3)
t.test(play_nint$play_time.x, play_int$play_time.x)
#Welch Two Sample t-test
#data:  play_nint$play_time.x and play_int$play_time.x
#t = 0.060502, df = 414.2, p-value = 0.9518
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.783780  2.960585
#sample estimates:
#  mean of x mean of y 
#61.43685  61.34844 
#p-value is greater than alpha (.05), so we fail to reject the null hypothesis and there is not a significant difference 
#median 
play_nint <- subset(play.intensive, play.intensive$prod_yr < 3.000000001)
play_int <- subset(play.intensive, play.intensive$prod_yr > 3)
t.test(play_nint$play_time.y, play_int$play_time.y)
#Welch Two Sample t-test
#data:  play_nint$play_time.y and play_int$play_time.y
#t = 0.1707, df = 407.22, p-value = 0.8645
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -3.205042  3.814589
#sample estimates:
#  mean of x mean of y 
#66.18686  65.88209 
#p-value is greater than alpha (.05), so we fail to reject the null hypothesis and there is not a significant difference 

#premium v active play 
play.prem <- merge(play.stat, focal.users, by.x = "V1", by.y = "User_Account_ID", all.x = TRUE)
#split two boxplots depending on prem v non-prem 
#mean
boxplot(play.prem$play_time.x ~ play.prem$prem, data = play.prem, main="Distribution of Active Play by Premium", xlab="Premium", ylab="% Active Play")
#premium users do tend to be more active players (on average), and the data is not as skewed to lower activity as non-premium players
#median
boxplot(play.prem$play_time.y ~ play.prem$prem, data = play.prem, main="Distribution of Active Play by Premium", xlab="Premium", ylab="% Active Play")
#premium users do tend to be more active players (on average), and the data is not as skewed to lower activity as non-premium players
#difference is more distinct than when looking at means 
#is the difference statistically significant? 
play_nprem <- subset(play.prem, play.prem$prem == 0)
play_prem <- subset(play.prem, play.prem$prem == 1)
#two-sample diff in means t test 
#mean
t.test(play_nprem$play_time.x, play_prem$play_time.x)
#Welch Two Sample t-test
#data:  play_nprem$play_time.x and play_prem$play_time.x
#t = -1.4602, df = 605.76, p-value = 0.1447
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -4.3259870  0.6363156
#sample estimates:
#  mean of x mean of y 
#60.38660  62.23144 
#p-value is greater than alpha (.05), so we fail to reject the null hypothesis and the difference is not statistically significant
#median
t.test(play_nprem$play_time.y, play_prem$play_time.y)
#Welch Two Sample t-test
#data:  play_nprem$play_time.y and play_prem$play_time.y
#t = -2.6384, df = 603.86, p-value = 0.008544
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -7.204386 -1.055873
#sample estimates:
#  mean of x mean of y 
#63.80993  67.94006 
#p-value is less than alpha (.05), therefore the difference is statistically significant between premium and non-premium users. 

#conclusion: the marketing strategy can also continue to push premium towards users who have been more active in games. 
