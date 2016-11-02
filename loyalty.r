
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
results$round_start_date <- as.Date(results$round_start_date)

#Replace all incidences of 2000-01-01 with null - this gives error, (list) object cannot be coerced to type 'double'
nulldate <- as.Date("2000-01-01")
#users[users == nulldate] <- NA
#results[results == nulldate] <- NA

#See whether purchasing Battlefield 2 has any correlation with purchasing Battlefeld 3 premium or Battlefield 4
franchise_loyal <- subset(users, bfbc2 != nulldate)
franchise_new <- subset(users, bfbc2 = nulldate)

#compare incidence of valid dates vs. null in bf3prem and bf4.
loyal_bought <- sum(franchise_loyal$bf3prem != nulldate | franchise_loyal$bf4 != nulldate)
new_bought <- sum(franchise_new$bf3prem != nulldate | franchise_new$bf4 != nulldate)
loyal_bought_percent <- loyal_bought/nrow(franchise_loyal)
new_bought_percent <- new_bought/nrow(franchise_new)

  #visualization or some kind of statistical significance test?

#compare (product_owned_cnt - product_owned_count_at_start)
franchise_loyal$product_owned_count_at_start <- as.numeric(franchise_loyal$product_owned_count_at_start)
franchise_new$product_owned_count_at_start <- as.numeric(franchise_new$product_owned_count_at_start)

franchise_loyal$other_purchases <- franchise_loyal$Product_Owned_Cnt - franchise_loyal$product_owned_count_at_start
franchise_new$other_purchases <- franchise_new$Product_Owned_Cnt - franchise_new$product_owned_count_at_start
summary(franchise_loyal$other_purchases)
summary(franchise_new$other_purchases)

#compare (bf3prem - bf3). Possibly also compare bf4?
franchise_loyal$upgrade_time <- ifelse(franchise_loyal$bf3prem != nulldate, franchise_loyal$bf3prem - franchise_loyal$bf3, NA)
franchise_new$upgrade_time <- ifelse(franchise_new$bf3prem != nulldate, franchise_new$bf3prem - franchise_new$bf3, NA)