
focal_results <- read.csv("focal_results.csv", stringsAsFactors=FALSE)
focal_users <- read.csv("focal_users.csv", stringsAsFactors=FALSE)
results <- focal_results
users <- focal_users

#fixing types
users$Signup_Date <- as.Date(users$Signup_Date)
users$bf3 <- as.Date(users$bf3)
users$bf3prem <- as.Date(users$bf3prem)
users$bf4 <- as.Date(users$bf4)
users$bfbc2 <- as.Date(users$bfbc2)
product_owned_count_at_start <- as.numeric(users$product_owned_count_at_start)
results$round_start_date <- as.Date(results$round_start_date)

#Replace all incidences of 2000-01-01 with null
nulldate <- as.Date("2000-01-01")


  ##just assume ankit's work is true (found bf2 and number of products bought were strong indicators, so skip re-proving that relationship?


#compare (product_owned_cnt - product_owned_count_at_start)
franchise_loyal$product_owned_count_at_start <- as.numeric(franchise_loyal$product_owned_count_at_start)
franchise_new$product_owned_count_at_start <- as.numeric(franchise_new$product_owned_count_at_start)

franchise_loyal$other_purchases <- franchise_loyal$Product_Owned_Cnt - franchise_loyal$product_owned_count_at_start
franchise_new$other_purchases <- franchise_new$Product_Owned_Cnt - franchise_new$product_owned_count_at_start

#compare incidence of valid dates vs. null in bf3prem and bf4.
loyal_bought <- sum(franchise_loyal$bf3prem != nulldate | franchise_loyal$bf4 != nulldate)
new_bought <- sum(franchise_new$bf3prem != nulldate | franchise_new$bf4 != nulldate)
loyal_bought_percent <- loyal_bought/nrow(franchise_loyal)
new_bought_percent <- new_bought/nrow(franchise_new)


summary(franchise_loyal$other_purchases)
summary(franchise_new$other_purchases)

#compare (bf3prem - bf3). Possibly also compare bf4?
franchise_loyal$upgrade_time <- ifelse(franchise_loyal$bf3prem != nulldate, franchise_loyal$bf3prem - franchise_loyal$bf3, NA)
franchise_new$upgrade_time <- ifelse(franchise_new$bf3prem != nulldate, franchise_new$bf3prem - franchise_new$bf3, NA)

  #incidence of valid dates in bf3prem and bf4 vs. product_owned_count_at_start
users$upgrade_time <- ifelse(users$bf3prem != nulldate, users$bf3prem - users$bf3, NA)

  # (bf3prem - bf3) vs product_owned_count_at_start

plot(users$product_owned_count_at_start[users$bf3prem != nulldate], users$upgrade_time[users$bf3prem != nulldate],
xlab = "Products owned when buying Battlefield 3",
ylab = "Length of time to upgrade to BF 3 Premium",
)

#split into those who bought bf3prem/bf4, and those who didn’t. Between these subsets, compare gameplay factors pre-purchase: 
franchise_loyal_kept <- subset(franchise_loyal, bf3prem != nulldate|bf4 != nulldate)
franchise_loyal_lost <- subset(franchise_loyal, bf3prem == nulldate & bf4 == nulldate)

#Inner join the subset of franchise_loyal to the focal_results on user_account_id
colnames(results)[2] <- "User_Account_ID"
franchise_loyal_matches = merge(franchise_loyal, results, by = "User_Account_ID")

#split up by kept and lost
franchise_loyal_matches_kept = merge(franchise_loyal_kept, results, by = "User_Account_ID")
franchise_loyal_matches_lost = merge(franchise_loyal_lost, results, by = "User_Account_ID")


#number of matches played
freq = franchise_loyal_matches %>% select(User_Account_ID, round_start_date) %>% arrange(User_Account_ID, round_start_date)
user_freq = split(freq, freq$User_Account_ID)

kept_user = franchise_loyal_matches_kept %>% select(User_Account_ID, round_start_date) %>% arrange(User_Account_ID, round_start_date)
kept_user_freq = split(kept_user, kept_user$User_Account_ID)

lost_user = franchise_loyal_matches_lost %>% select(User_Account_ID, round_start_date) %>% arrange(User_Account_ID, round_start_date)
lost_user_freq = split(lost_user, lost_user$User_Account_ID)



#calculate match frequency from ankit's code
#apply similar freq analysis used by melinda

franchise_loyal_matches_kept = merge(franchise_loyal_kept, results, by = "User_Account_ID")
franchise_loyal_matches_lost = merge(franchise_loyal_lost, results, by = "User_Account_ID")

freq = franchise_loyal_matches %>% select(User_Account_ID, round_start_date) %>% arrange(User_Account_ID, round_start_date)
user_freq = split(freq, freq$User_Account_ID)
#round_diff = lapply(user_freq, function(x) {tmp = diff(x$round_start_date) tmp })
mean_freq = lapply(round_diff, function(x) mean(as.numeric(x)))

kept_user = franchise_loyal_matches_kept %>% select(User_Account_ID, round_start_date) %>% arrange(User_Account_ID, round_start_date)
kept_user_freq = split(kept_user, kept_user$User_Account_ID)

lost_user = franchise_loyal_matches_lost %>% select(User_Account_ID, round_start_date) %>% arrange(User_Account_ID, round_start_date)
lost_user_freq = split(lost_user, lost_user$User_Account_ID)

#Try to use this instead of calculating means separately:
#   subset(mean_freq, mean_freq$User_Account_ID %in% franchise_loyal_kept$User_Account_ID )
#   t.test (kept_user_freq ~ lost_user_freq)

#t-test total score overall
raw_score_kept = franchise_loyal_matches_kept %>% select(User_Account_ID, total_score) %>% arrange(User_Account_ID, total_score)

#t-test total score pre-purchase
raw_score_kept = subset(franchise_loyal_matches_kept, round_start_date < bf3prem & round_start_date < bf4, select = c(User_Account_ID, total_score)) %>% select(User_Account_ID, total_score) %>% arrange(User_Account_ID, total_score)
user_score_kept = split(raw_score_kept, raw_score_kept$User_Account_ID)
# list object cannot be coerced to type double:
#mean_score_kept = lapply(user_score_kept, function(x) mean(as.numeric(unlist(x))))

raw_score_lost = franchise_loyal_matches_lost %>% select(User_Account_ID, total_score) %>% arrange(User_Account_ID, total_score)
user_score_lost = split(score, score$User_Account_ID)
#mean_score_lost = lapply(user_score, function(x) mean(as.numeric(x)))

t.test (mean_score_kept ~ mean_score_lost)

#t-test kill / death ratio overall
raw_kd_kept = franchise_loyal_matches_kept %>% select(User_Account_ID, killCnt, deathCnt) %>% arrange(User_Account_ID)

#kill/death stop before premium date. problem is that some people buy the premium version right away before playing any. valuable to keep both overall and pre-purchase t test?
raw_kd_kept = subset(franchise_loyal_matches_kept, round_start_date < bf3prem & round_start_date < bf4, select = c(User_Account_ID, killCnt, deathCnt)) %>% arrange(User_Account_ID)
user_kd_kept = split(raw_kd_kept, raw_kd_kept$User_Account_ID)
#doesn't work for rounds with no deaths 
#   ratio_kd_kept = lapply(user_kd_kept, function(x) x$killCnt/x$deathCnt)
#other possible proxies: kills - deaths, kills + deaths
ratio_kd_kept = lapply(user_kd_kept, function(x) (x$killCnt+1)/(x$deathCnt+1))
mean_kd_kept = lapply(ratio_kd_kept, function(x) mean(as.numeric(x)))
#try piping?

raw_kd_lost = franchise_loyal_matches_lost %>% select(User_Account_ID, killCnt, deathCnt) %>% arrange(User_Account_ID)
user_kd_lost = split(raw_kd_lost, raw_kd_lost$User_Account_ID)
ratio_kd_lost = lapply(user_kd_lost, function(x) (x$killCnt+1)/(x$deathCnt+1)
mean_kd_lost = lapply(ratio_kd_lost, function(x) mean(as.numeric(x)))

#t.test (as.vector(mean_kd_kept) ~ as.vector(mean_kd_lost))

#win ratio - convert winningTeam to numeric and check the means first. possibly combine with kdr
raw_win_kept = subset(franchise_loyal_matches_kept, roundComplete == 1 & round_start_date < bf3prem & round_start_date < bf4, select = c(User_Account_ID, winningTeam) %>% arrange(User_Account_ID, winningTeam)
user_win_kept = split(raw_win_kept, raw_kd_kept$User_Account_ID)
#ratio_win_kept = lapply(user_kd_kept, function(x) x$killCnt/x$deathCnt)
#mean_win_kept = lapply(ratio_kd_kept, function(x) mean(as.numeric(x)))

raw_win_lost = subset(franchise_loyal_matches_lost, roundComplete == 1, select = c(User_Account_ID, winningTeam)) %>% arrange(User_Account_ID)
user_win_lost = split(raw_win_lost, raw_win_lost$User_Account_ID)
#ratio_win_lost = lapply(user_win_lost, function(x) x$killCnt/x$deathCnt)
#mean_win_lost = lapply(ratio_win_lost, function(x) mean(as.numeric(x)))

#roundComplete jk probably pointless the means are about the same
raw_complete_kept = subset(franchise_loyal_matches_kept, round_start_date < bf3prem & round_start_date < bf4, select = c(User_Account_ID, roundComplete)) %>% arrange(User_Account_ID)
user_complete_kept = split(raw_complete_kept, raw_complete_kept$User_Account_ID)
# same type problem
#   mean_complete_kept = lapply(user_complete_kept, function(x) mean(as.numeric(x)))
#try piping?

raw_complete_lost = franchise_loyal_matches_lost %>% select(User_Account_ID, roundComplete) %>% arrange(User_Account_ID)
user_complete_lost = split(raw_complete_lost, raw_complete_lost$User_Account_ID)
#mean_complete_lost = lapply(ratio_kd_lost, function(x) mean(as.numeric(x)))

#diversity of roles of each player
raw_roles_kept = subset(franchise_loyal_matches_kept, round_start_date < bf3prem & round_start_date < bf4, select = c(User_Account_ID, assaultFlg, reconFlg, engineerFlg, support Flg) %>% arrange(User_Account_ID)
user_roles_kept = split(raw_roles_kept, raw_roles_kept$User_Account_ID)
#sum_roles_kept = lapply(user_roles_kept, function(x) as.data.frame(table(x$assaultFlg, x$reconFlg, x$engineerFlg, x$support Flg)))
#var_roles_kept = lapply(sum_roles_kept, function (x) var(as.numeric(x)) )

raw_roles_lost = 
