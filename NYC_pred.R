#NYC
library(glmnet)
weather_data = read.csv("/Users/R.studio/GMU-CherryBlossomCompetition/4237167.csv")
nyc_bloom = read.csv("/Users/R.studio/GMU-CherryBlossomCompetition/nyc.csv")

#prep weather
weather_data$DATE = as.Date(weather_data$DATE)
weather_data$Year = as.numeric(format(weather_data$DATE, "%Y"))
weather_data$Month = as.numeric(format(weather_data$DATE, "%m"))
weather_data$DOY = as.numeric(format(weather_data$DATE, "%j"))
weather_data$TMEAN = (weather_data$TMAX + weather_data$TMIN) / 2

#Filter NYC
nyc_weather = subset(weather_data, STATION == "USW00014732" & !is.na(TMEAN))

#FIND THE BEST BASE TEMPERATURE + TRAIN MODEL
best_mae = 100
best_base = 35
best_model = NULL

for (base in 35:45) {
  # We use a temporary variable so we don't overwrite the main dataframe in the loop
  nyc_weather$GDD_temp = pmax(0, nyc_weather$TMEAN - base)
  nyc_weather$Freeze_temp = ifelse(nyc_weather$TMIN < 32, 1, 0)
  
  # Historical features (up to 2025)
  hist_temp = subset(nyc_weather, Year >= min(nyc_bloom$year) & Year < 2026)
  hist_features = aggregate(cbind(GDD_temp, Freeze_temp) ~ Year + Month, data = hist_temp, sum)
  hist_sub = subset(hist_features, Month %in% c(1, 2, 3))
  
  if(nrow(hist_sub) < 3) next # Skip if not enough months/years
  
  gdd_wide = reshape(hist_sub[, c("Year", "Month", "GDD_temp")], timevar="Month", idvar="Year", direction="wide")
  freeze_wide = reshape(hist_sub[, c("Year", "Month", "Freeze_temp")], timevar="Month", idvar="Year", direction="wide")
  
  train_data = merge(nyc_bloom[, c("year", "bloom_doy")], gdd_wide, by.x="year", by.y="Year")
  train_data = merge(train_data, freeze_wide, by.x="year", by.y="Year")
  train_data = na.omit(train_data)
  
  # MODELING LOGIC
  if(nrow(train_data) >= 2) { # Changed from > 2 to >= 2 to try and catch the NYC data
    X_train = as.matrix(train_data[, c(3:ncol(train_data))])
    Y_train = train_data$bloom_doy
    
    set.seed(42)
    
    # If the dataset is tiny (< 10 rows), use fixed penalty LASSO
    if (nrow(train_data) < 10) {
      lasso_fixed = glmnet(X_train, Y_train, alpha = 1, lambda = 0.5)
      preds = predict(lasso_fixed, s = 0.5, newx = X_train)
      mae = mean(abs(Y_train - preds))
      
      if (mae < best_mae) {
        best_mae = mae
        best_base = base
        # Create a helper field so the final prediction line works
        lasso_fixed$lambda_to_use = 0.5
        best_model = lasso_fixed
      }
    } else {
      # For larger datasets (DC/Kyoto), use Cross-Validation
      lasso_cv = cv.glmnet(X_train, Y_train, alpha = 1)
      preds = predict(lasso_cv, s = lasso_cv$lambda.min, newx = X_train)
      mae = mean(abs(Y_train - preds))
      
      if (mae < best_mae) {
        best_mae = mae
        best_base = base
        # Create a helper field so the final prediction line works
        lasso_cv$lambda_to_use = lasso_cv$lambda.min
        best_model = lasso_cv
      }
    }
  }
}

# If we found a model, use it. If not, use the historical average.
if (is.null(best_model)) {
  final_doy = round(mean(nyc_bloom$bloom_doy))
  cat("Note: Dataset too small for LASSO. Using historical average.\n")
  cat("Optimal Base Temp: N/A\n")
  cat("Historical MAE: N/A\n")
  cat("--> PREDICTED 2026 DOY:", final_doy, "\n")
} else {
  # Run the simulation for 2026 using the best base temp found
  nyc_weather$GDD = pmax(0, nyc_weather$TMEAN - best_base)
  nyc_weather$Freeze = ifelse(nyc_weather$TMIN < 32, 1, 0)
  
  actual_2026 = subset(nyc_weather, Year == 2026)
  recent_past = subset(nyc_weather, Year >= 2015 & Year <= 2024)
  normals = aggregate(cbind(GDD, Freeze) ~ DOY + Month, data = recent_past, mean)
  
  max_doy_2026 = max(actual_2026$DOY, na.rm=TRUE)
  missing_doys = (max_doy_2026 + 1):90
  simulated_future = subset(normals, DOY %in% missing_doys)
  simulated_future$Year = 2026
  
  combined_2026 = rbind(
    actual_2026[, c("Year", "Month", "DOY", "GDD", "Freeze")],
    simulated_future[, c("Year", "Month", "DOY", "GDD", "Freeze")]
  )
  
  features_2026 = aggregate(cbind(GDD, Freeze) ~ Year + Month, data = combined_2026, sum)
  pred_features = c(
    features_2026$GDD[features_2026$Month == 1],
    features_2026$GDD[features_2026$Month == 2],
    features_2026$GDD[features_2026$Month == 3],
    features_2026$Freeze[features_2026$Month == 1],
    features_2026$Freeze[features_2026$Month == 2],
    features_2026$Freeze[features_2026$Month == 3]
  )
  
  pred_features[is.na(pred_features)] = 0
  X_2026 = matrix(pred_features, nrow = 1)
  
  # Make the prediction using our helper field
  pred_2026 = predict(best_model, s = best_model$lambda_to_use, newx = X_2026)
  
  cat("Optimal Base Temp:", best_base, "F\n")
  cat("Historical MAE:", round(best_mae, 2), "days\n")
  cat("--> PREDICTED 2026 DOY:", round(pred_2026[1,1]), "\n")
}
as.Date(round(pred_2026[1,1]), origin = "2025-12-31")