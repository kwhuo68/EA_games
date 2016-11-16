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
games<-as.data.frame(table(focal.results$user_account_id))



















------------------------------------------------------------------------------------------------------
Old SQL code

#See distributions of gameplay freq ###########################################################################
#premium v non prem users 

alter table focal_users 
add column bf3_played int;

update focal_users
set bf3_played = datediff(end_date, bf3);

select * from focal_users;

alter table focal_results 
add column days_played int;

#can't index on column of medium text or long text 
alter table focal_results 
change column user_account_id user_account_id varchar(12)
alter table focal_results 
add index user_account_id (user_account_id);
alter table focal_users 
change column user_account_id user_account_id varchar(12)
alter table focal_users 
add index user_account_id (user_account_id);

select user_account_id, count(*) 
from focal_results 
group by user_account_id;

alter table focal_users
add column results_count int; 

update focal_users u
set results_count = (select count(*) 
from focal_results r
where r.user_account_id = u.user_account_id
group by u.user_account_id)
where user_account_id 

select * from focal_users order by results_count desc;
select * from focal_users where results_count is null; 


select user_account_id, year(round_start_date), 
month(round_start_date), week(round_start_date), dayname(round_start_date), count(*)
from focal_results 
group by user_account_id, week(round_start_date), dayname(round_start_date)
order by;
######################## FIX LATER VVVVVVVVVVVVVVVVVV
create table focal_results_per_day (
select user_account_id, week(round_start_date), dayname(round_start_date), count(*)
from focal_results 
group by user_account_id, week(round_start_date), dayname(round_start_date)
order by );

select * from focal_results_per_day; 


#testing days_played numbers
select user_account_id, count(user_account_id) from focal_results 
group by user_account_id;
#trying to make a new column ----- HELP!!!!!!
insert into focal_results (days_played) 
(select count(user_account_id) from focal_results
group by user_account_id);
#this is fine 
alter table focal_results 
add column bf3_played int;
#joined
UPDATE focal_results r
  INNER JOIN focal_users u ON u.user_account_id = r.user_account_id
SET r.bf3_played = u.bf3_played;

select * from focal_results;
#This doesn't run, also bc days_played is not set
update focal_results 
set total_freq = (days_played * 100 )/bf3_played;
#see if people with premium play more overall than people without (freq = buying products?) 
select r.total freq, u.premium from focal_results r 
inner join focal_users u on u.user_account_id = r.user_account_id
group by r.user_account_id

#look into more specifc freq 

#finding avg days between game rounds?? 
select user_account_id, round_start_date from focal_results 
group by user_account_id;

select r.user_account_id, 
sum(if(round_start_date =
group by r.user_account_id; 

