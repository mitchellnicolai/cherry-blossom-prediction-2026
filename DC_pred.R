DC = read.csv("/Users/mitchellnicolai/Downloads/washingtondc.csv")
daily_temp = read.csv("/Users/mitchellnicolai/Downloads/dc_daily_temp.csv")

str(DC)
str(daily_temp)

daily_temp$DATE = as.Date(daily_temp$DATE)
daily_temp$Year = as.numeric(format(daily_temp$DATE, "%Y"))
range(daily_temp$Year)
#Returns 1980 2026

DC_sub = subset(DC, year >= min(daily_temp$Year))
range(DC_sub$year)
nrow(DC_sub)
#Returns 1980 2025

#Build predictor variables 
daily_temp$TMEAN = (daily_temp$TMAX + daily_temp$TMIN) / 2
daily_temp$Month = as.numeric(format(daily_temp$DATE, "%m"))

#Restrict to Jan-Mar
temp_jm = subset(daily_temp, Month %in% c(1, 2, 3))
table(temp_jm$Month)

#Growing degree days (41º F)
temp_jm$GDD_daily = pmax(0, temp_jm$TMEAN - 41)

#Aggregate by year
gdd_year = aggregate(GDD_daily ~ Year, data = temp_jm, sum)
colnames(gdd_year)[2] = "GDD"

#Mean Jan-Mar Temp
jm_mean = aggregate(TMEAN ~ Year, data = temp_jm, mean)
colnames(jm_mean)[2] = "JanMarTemp"

#Merge with Bloom Data 
cherryblossom = merge(DC_sub[, c("year", "bloom_doy")],
                      gdd_year,
                      by.x = "year",
                      by.y = "Year")
cherryblossom = merge(cherryblossom, jm_mean,
                      by.x = "year",
                      by.y = "Year")
colnames(cherryblossom) = c("year", "Bloom_DOY", "GDD", "JanMarTemp")
str(cherryblossom)
summary(cherryblossom)

#Exploratory 
plot(cherryblossom$GDD,
     cherryblossom$Bloom_DOY)
abline(lm(Bloom_DOY ~ GDD,
          data = cherryblossom),
       col = "red")
#Reveals a clear negative slope

plot(cherryblossom$JanMarTemp,
     cherryblossom$Bloom_DOY)
abline(lm(Bloom_DOY ~ JanMarTemp,
          data = cherryblossom), 
       col = "red")
#Reveals a clear negative slope

cor(cherryblossom[, c("Bloom_DOY","GDD","JanMarTemp")])

#Bloom_DOY vs. GDD -0.834 Is a strong negative correlation. Higher accumelated heat leads 
#to an earlier bloom (lower DOY). GDD alone explains substantial portion of variation.

#Bloom_DOY vs. JanMarTemp -0.77 Is a strong negative relationship. Warmer early season temp 
#means earlier bloom. Weaker than GDD. 

#GDD vs. JanMarTemp 0.905 extreamly high positive correlation. These predictors are nearly
#collinear. By including both in the LM we raise the risk of introducing multicollineatity 
#and wil inflate standered errors. 

#First model 
model1 = lm(Bloom_DOY ~ GDD, data = cherryblossom)
summary(model1)

#MAE
pred1 = predict(model1)
mae1 = mean(abs(cherryblossom$Bloom_DOY - pred1))
mae1
#3.075732 = moderate, refinement needed

#Adding Year
model2 = lm(Bloom_DOY ~ GDD + year, data = cherryblossom)
summary(model2)

#MAE 3.078151 = worse than when year was not included
pred2 = predict(model2)
mae2 = mean(abs(cherryblossom$Bloom_DOY - pred2))
mae2
#3.078151

#Restricting GDD to Jan and Feb only
temp_jan_feb = subset(daily_temp, Month %in% c(1, 2))
temp_jan_feb$GDD_daily = pmax(0, temp_jan_feb$TMEAN - 41)

#Aggregate Jan-Feb GDD by year
gdd_jf_year = aggregate(GDD_daily ~ Year, data = temp_jan_feb, sum)
colnames(gdd_jf_year)[2] = "GDD_Jan_Feb"

#Merge back in with Cherry blossom 
cherryblossom = merge(cherryblossom, gdd_jf_year, by.x = "year",
                      by.y = "Year")

#Jan-Feb GDD
model3 = lm(Bloom_DOY ~ GDD_Jan_Feb, data = cherryblossom)
summary(model3)

pred3 = predict(model3)
mae3 = mean(abs(cherryblossom$Bloom_DOY - pred3))
mae3
#4.631949
#Much worse, March temp. must be important in prediction

#NEXT STEPS
#Break apart the data to have GDD into three separate monthly variables 
#January GDD, February GDD, and March GDD and see if this improves

daily_temp$GDD_daily = pmax(0, daily_temp$TMEAN - 41)

#Reshape to have each month having its own column
gdd_monthly = aggregate(GDD_daily ~ Year + Month, data = daily_temp, sum)
gdd_monthly_sub = subset(gdd_monthly, Month %in% c(1, 2, 3))
gdd_wide = reshape(gdd_monthly_sub, timevar = "Month", 
                   idvar = "Year", direction = "wide")

#Rename columns and merge back with bloom data
colnames(gdd_wide) = c("Year", "GDD_Jan", "GDD_Feb", "GDD_Mar")

cherryblossom_V2 = merge(DC_sub[, c("year", "bloom_doy")], 
                         gdd_wide, by.x = "year", 
                         by.y = "Year")

#MLR with separated GDDs
model5 = lm(bloom_doy ~ GDD_Jan + GDD_Feb + GDD_Mar, data = cherryblossom_V2)
summary(model5)

#MAE
pred5 = predict(model5)
mae5 = mean(abs(cherryblossom_V2$bloom_doy - pred5))
mae5
#2.899676
#MAE is no less than 3 s were within the three day prediction

#Now I will explore using LASSO and test different base temperatures
library(glmnet)

daily_temp$GDD_35 = pmax(0, daily_temp$TMEAN - 35)
daily_temp$Freeze = ifelse(daily_temp$TMIN < 32, 1, 0)

#Function to aggregate all these features 
features_monthly = aggregate(
  cbind(GDD_35, Freeze) ~ Year + Month,
  data = daily_temp, FUN = sum)

#Subset for Jan, Feb, and Mar
features_sub = subset(features_monthly, Month %in% c(1, 2, 3))
gdd_wide = reshape(features_sub[, c("Year", "Month", "GDD_35")], 
                   timevar = "Month", idvar = "Year", direction = "wide")
colnames(gdd_wide) = c("Year", "GDD_Jan", "GDD_Feb", "GDD_Mar")

freeze_wide = reshape(features_sub[, c("Year", "Month", "Freeze")], 
                      timevar = "Month", idvar = "Year", direction = "wide")
colnames(freeze_wide) = c("Year", "Freeze_Jan", "Freeze_Feb", "Freeze_Mar")

#Merge with main dataset
cherryblossom_lasso = merge(DC_sub[, c("year", "bloom_doy")], gdd_wide, 
                                       by.x = "year",
                                       by.y = "Year")
cherryblossom_lasso = merge(cherryblossom_lasso, freeze_wide,
                            by.x = "year",
                            by.y = "Year")
cherryblossom_lasso = na.omit(cherryblossom_lasso)

#Prep data for LASSO
x = as.matrix(cherryblossom_lasso[, c("GDD_Jan", "GDD_Feb", "GDD_Mar", 
                                      "Freeze_Jan", "Freeze_Feb", "Freeze_Mar")])
y = cherryblossom_lasso$bloom_doy

#Train LASSO
set.seed(490)
lasso_cv = cv.glmnet(x, y, alpha = 1, type.measure = "mae")

best_lambda = lasso_cv$lambda.min
print(coef(lasso_cv, s = best_lambda))

#MAE
lasso_pred = predict(lasso_cv, s = best_lambda, newx = x)
lasso_mae = mean(abs(y - lasso_pred))
lasso_mae
#2.785853

#Final LASSO Model

library(glmnet)
DC = read.csv("/Users/mitchellnicolai/Downloads/washingtondc.csv")
daily_temp = read.csv("/Users/mitchellnicolai/Downloads/dc_daily_temp.csv")

#Format dates 
daily_temp$DATE = as.Date(daily_temp$DATE)
daily_temp$Year = as.numeric(format(daily_temp$DATE, "%Y"))
daily_temp$Month = as.numeric(format(daily_temp$DATE, "%m"))
daily_temp$DOY = as.numeric(format(daily_temp$DATE, "%j")) # Day of Year

# Calculate TMEAN, Base 35 GDD, and Freeze Days
daily_temp$TMEAN = (daily_temp$TMAX + daily_temp$TMIN) / 2
daily_temp$GDD_35 = pmax(0, daily_temp$TMEAN - 35)
daily_temp$Freeze = ifelse(daily_temp$TMIN < 32, 1, 0)

#PREP HISTORICAL DATA FOR TRAINING
hist_temp = subset(daily_temp, Year >= min(DC$year) & Year < 2026)
hist_features = aggregate(cbind(GDD_35, Freeze) ~ Year + Month, data = hist_temp, sum)
hist_sub = subset(hist_features, Month %in% c(1, 2, 3))

gdd_wide = reshape(hist_sub[, c("Year", "Month", "GDD_35")], timevar="Month", idvar="Year", direction="wide")
freeze_wide = reshape(hist_sub[, c("Year", "Month", "Freeze")], timevar="Month", idvar="Year", direction="wide")
colnames(gdd_wide) = c("Year", "GDD_Jan", "GDD_Feb", "GDD_Mar")
colnames(freeze_wide) = c("Year", "Freeze_Jan", "Freeze_Feb", "Freeze_Mar")

train_data = merge(DC[, c("year", "bloom_doy")], gdd_wide, by.x="year", by.y="Year")
train_data = merge(train_data, freeze_wide, by.x="year", by.y="Year")
train_data = na.omit(train_data)

#TRAIN THE FINAL LASSO MODEL
x_train = as.matrix(train_data[, c("GDD_Jan", "GDD_Feb", "GDD_Mar", "Freeze_Jan", "Freeze_Feb", "Freeze_Mar")])
y_train = train_data$bloom_doy

set.seed(490)
lasso_final = cv.glmnet(x_train, y_train, alpha = 1)
best_lambda = lasso_final$lambda.min
lasso_train_pred = predict(lasso_final, s = best_lambda, newx =x_train)
lasso_mae = mean(abs(y_train - lasso_train_pred))
lasso_mae
# 2.785853

#SIMULATE THE REST OF 2026
actual_2026 = subset(daily_temp, Year == 2026)
# Create a "10-Year Normal" dataset (2015-2024) to fill in the future
recent_past = subset(daily_temp, Year >= 2015 & Year <= 2024)
normals = aggregate(cbind(TMAX, TMIN, TMEAN, GDD_35, Freeze) ~ DOY + Month, data = recent_past, mean)

# Find out what Days of the Year (DOY) are missing in 2026
max_doy_2026 = max(actual_2026$DOY)
missing_doys = (max_doy_2026 + 1):90  # 90 is end of March

# Grab the normal/average values for those missing days
simulated_future = subset(normals, DOY %in% missing_doys)
simulated_future$Year = 2026

# Combine the actual 2026 data with our simulated late Feb/Mar data
combined_2026 = rbind(
  actual_2026[, c("Year", "Month", "DOY", "GDD_35", "Freeze")],
  simulated_future[, c("Year", "Month", "DOY", "GDD_35", "Freeze")]
)


#PREPARE 2026 PREDICTION FEATURES
features_2026 = aggregate(cbind(GDD_35, Freeze) ~ Year + Month, data = combined_2026, sum)
pred_features = c(
  GDD_Jan = features_2026$GDD_35[features_2026$Month == 1],
  GDD_Feb = features_2026$GDD_35[features_2026$Month == 2],
  GDD_Mar = features_2026$GDD_35[features_2026$Month == 3],
  Freeze_Jan = features_2026$Freeze[features_2026$Month == 1],
  Freeze_Feb = features_2026$Freeze[features_2026$Month == 2],
  Freeze_Mar = features_2026$Freeze[features_2026$Month == 3]
)

# Format into matrix for glmnet
x_2026 = matrix(pred_features, nrow = 1)
colnames(x_2026) = c("GDD_Jan", "GDD_Feb", "GDD_Mar", "Freeze_Jan", "Freeze_Feb", "Freeze_Mar")

bloom_prediction_2026 = predict(lasso_final, s = best_lambda, newx = x_2026)
print(paste("Predicted 2026 Bloom DOY:", round(bloom_prediction_2026[1,1], 1)))

as.Date(round(bloom_prediction_2026[1,1]), origin = "2025-12-31")
