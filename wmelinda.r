#load data
focal.users = read.csv("~/#UPenn/#WUDAC Consulting/focal_users.csv")
focal.results = read.csv("~/#UPenn/#WUDAC Consulting/focal_results.csv")

#clean data 
focal.users$Country_Code <- NULL 
focal.users$User_Age <- NULL 
focal.users$Gender_Code <- NULL 
focal.users$Signup_Date[which(focal.users$Signup_Date == '2001-01-01')]= NA
focal.users$bf3[which(focal.users$bf3 == '2001-01-01')]= NA
focal.users$bf3prem[which(focal.users$bf3prem == "2001-01-01")]= NA
focal.users$bf4[which(focal.users$bf4 == "2001-01-01")]= NA
focal.users$bfbc2[which(focal.users$bfbc2 == "2001-01-01")]= NA


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

#

#make a histogram of the distribution 
hist(focal.users$proddiff, 
     main= "Histogram of Users Buying History",
     xlab = "Products bought since Signup"),






















------------------------------------------------------------------------------------------------------
I ended up using SQL ! (FYI) 

#create database
create database wudac;
use wudac;

#create table
create table focal_users (
wcai_cohort varchar(10), user_account_id varchar(100), country_code varchar(5), signup_date datetime, user_age int(100), gender_code varchar(10), 
product_owned_cnt int(100), bf3 datetime, bf3_platform varchar(10), bf3prem datetime, bf4 datetime, bfbc2 datetime, ps3_platform_activity_flag varchar(5), 
xbox360_platform_activity_flag varchar(5), pc_platform_activity_flag varchar(5), product_owned_count_at_start int(100));
select * from focal_users;

#load data into table 
load data local infile 
'/Users/wmelinda/Documents/#UPenn/#WUDAC Consulting/focal_users.csv'
into table focal_users
fields terminated by ',' 
optionally enclosed by '''' 
lines terminated by '\n'
ignore 1 lines
(@wcai_cohort, @user_account_id, @country_code, @signup_date, @user_age, @gender_code, @product_owned_cnt, @bf3, @bf3_platform, @bf3prem, @bf4, @bfbc2, 
@ps3_platform_activity_flag, @xbox360_platform_activity_flag, @pc_platform_activity_flag, @product_owned_count_at_start)
set 
wcai_cohort = nullif(@wcai_cohort, ''), user_account_id = nullif(@user_account_id, ''), country_code = nullif(@country_code, ''), 
signup_date = str_to_date(nullif(@signup_date, '2000-01-01'),'%Y-%m-%d'), user_age = nullif(@user_age, ''), 
gender_code = nullif(@gender_code, ''), product_owned_cnt = nullif(@product_owned_cnt, ''), 
bf3 = str_to_date(nullif(@bf3, '2000-01-01'),'%Y-%m-%d'), bf3_platform = nullif(@bf3_platform, ''), 
bf3prem = str_to_date(nullif(@bf3prem, '2000-01-01'),'%Y-%m-%d'), bf4 = str_to_date(nullif(@bf4, '2000-01-01'),'%Y-%m-%d'), 
bfbc2 = str_to_date(nullif(@bfbc2, '2000-01-01'),'%Y-%m-%d'), 
ps3_platform_activity_flag = nullif(@ps3_platform_activity_flag, ''), 
xbox360_platform_activity_flag = nullif(@xbox360_platform_activity_flag, ''), 
pc_platform_activity_flag = nullif(@pc_platform_activity_flag, ''), 
product_owned_count_at_start = nullif(@product_owned_count_at_start, '');
select * from focal_users;
truncate focal_users;
#remove bad data 
#self-entered data - age, gender, country = not reliable 
#date of 2000-01-01 is never good 

#create table
create table focal_results (
wcai_cohort varchar(10), user_account_id varchar(10000000), perona_id int, 
round_id varchar(100000000), player_id int, round_start_date datetime, 
player_seconds decimal(10,4), rank_at_start int, rank_at_end int, total_score int, 
combat_score int, killcnt int, deathcnt int, assultflg int, reconflg int, 
engineerflg int, supportflg int, armoredlandflg int, unarmoredlandflg int, 
jetflg int, helicopterflg int, boatflg int, servername varchar(10000000), 
gamemode varchar(1000000000), round_seconds decimal(10,4), roundcomplete int, 
maxsimultaneousplayersallowed int, maxsimultaneousplayers int, uniqueplayers int, 
initialplayers int, nth_day_of_play int, team int, teamcount int, winningteam int, 
attackerteam int, deathsperteama int, deathsperteamb int, 
objectiveswonperteama int, objectiveswonperteamb int, levelname varchar(1000000));
select * from focal_results;
#load data into table 
load data local infile 
'/Users/wmelinda/Documents/#UPenn/#WUDAC Consulting/focal_results.csv'
into table focal_results
fields terminated by ',' 
optionally enclosed by '''' 
lines terminated by '\n'
ignore 1 lines; 

select * from focal_results;

#find 'intensive' players #####################################################################################
#based on product count - range 1-63 
alter table focal_users
add column end_date datetime not null 
default '2014-03-04';
select * from focal_users;

select user_account_id, (product_owned_cnt - product_owned_count_at_start ) * 100/ datediff(end_date, bf3)
from focal_users
group by user_account_id;

#Compare prem v non-prem user ################################################################################
select user_account_id,  
sum(if(bf3prem is NULL, 1, 0))'NA',
sum(if(bf3prem is not NULL, 1, 0)) 'Premium'
from focal_users;

alter table focal_users 
add column premium int; 

update focal_users 
set premium = if(bf3prem is not null, 1, 0); 

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

