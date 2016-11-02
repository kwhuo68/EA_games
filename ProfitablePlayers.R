results = read.csv("focal_results.csv")
original_users = read.csv("focal_users.csv")
users = original_users

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