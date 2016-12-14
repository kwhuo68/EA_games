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
as.numeric(games.days$days_start)
games.days$games_freq <- games.days$Freq * 365 / games.days$days_start

#compare distribution of freq with 'intensive' 
#combine data with product data 
games.intensive <- merge(games.days, order.bf3.products, by.x = "Var1", by.y = "user_account_id", all.x = TRUE)
#make a scatterplot with prods vs game freq 
plot(order.bf3.products$prod_yr, games.days$games_freq, main="Intensity v Frequency", col = ifelse(order.bf3.products$prod_yr < 3.000000000001,'red','blue'), xlab="Products/Year ", ylab="Games/Year")
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
boxplot(games.prem$games_freq ~ games.prem$prem, data = games.prem, main="Distribution of Games/Year by Premium", 
        +         xlab="Premium", ylab="Games/Year")
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
