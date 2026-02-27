load("/Users/R.studio/bloom_history.RData")
load("/Users/R.studio/climate_data.RData")

library(dplyr)
library(tidyr)
library(lubridate)
library(glmnet)

#Distance function
calculate_distance = function(lat1, lon1, lat2, lon2) {
  rad = pi / 180
  dlon = (lon2 - lon1) * rad
  dlat = (lat2 - lat1) * rad
  a = sin(dlat/2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon/2)^2
  c = 2 * asin(min(1, sqrt(a)))
  return(6371 * c) 
}

#Station coordinates
station_map = data.frame(
  STATION = c("USW00013743", "CA001108395", "JA000047759", "SZ000001940", "USW00014732"), 
  # DC, Vancouver, Kyoto, Liestal, NYC (LaGuardia)
  station_lat = c(38.852, 49.193, 35.011, 47.483, 40.776),
  station_long = c(-77.037, -123.184, 135.733, 7.733, -73.874)
)

climate_clean = climate_data %>%
  dplyr::mutate(
    Year = lubridate::year(DATE),
    Month = lubridate::month(DATE),
    DOY = lubridate::yday(DATE),
    
    TAVG_calc = ifelse(!is.na(TAVG), TAVG, (TMAX + TMIN) / 2),
    TMEAN_F = (TAVG_calc / 10) * (9/5) + 32,
    TMIN_F = (TMIN / 10) * (9/5) + 32,
    
    GDD_35 = pmax(0, TMEAN_F - 35),
    GDD_40 = pmax(0, TMEAN_F - 40),
    Chill_Days = ifelse(!is.na(TMEAN_F) & TMEAN_F >= 32 & TMEAN_F <= 45, 1, 0),
    Freeze = ifelse(!is.na(TMIN_F) & TMIN_F < 32, 1, 0),
    Precip = tidyr::replace_na(PRCP, 0)
  ) %>%
  dplyr::left_join(station_map, by = "STATION")

#Aggregate monthly features
monthly_features = climate_clean %>%
  dplyr::filter(Month %in% c(1, 2, 3)) %>%
  dplyr::group_by(location, Year, Month, station_lat, station_long) %>%
  dplyr::summarize(
    GDD_35 = sum(GDD_35, na.rm = TRUE),
    GDD_40 = sum(GDD_40, na.rm = TRUE),
    Chill_Days = sum(Chill_Days, na.rm = TRUE),
    Freeze = sum(Freeze, na.rm = TRUE),
    Precip = sum(Precip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = Month,
    values_from = c(GDD_35, GDD_40, Chill_Days, Freeze, Precip),
    names_glue = "{.value}_{Month}"
  ) 

monthly_features[is.na(monthly_features)] = 0
# -----------------------------------------------------------------------------


#Merge data + train the model
global_train_data = bloom_history %>%
  dplyr::inner_join(monthly_features, by = c("location" = "location", "year" = "Year")) %>%
  dplyr::mutate(
    station_lat = ifelse(is.na(station_lat), lat, station_lat),
    station_long = ifelse(is.na(station_long), long, station_long),
    Distance_km = calculate_distance(lat, long, station_lat, station_long),
    alt = tidyr::replace_na(alt, 0)
  ) %>%
  tidyr::drop_na(GDD_35_1, Freeze_1, lat, Distance_km, bloom_doy)

x_global = global_train_data %>%
  dplyr::select(
    GDD_35_1, GDD_35_2, GDD_35_3, 
    GDD_40_1, GDD_40_2, GDD_40_3,
    Chill_Days_1, Chill_Days_2, Chill_Days_3, 
    Freeze_1, Freeze_2, Freeze_3,
    Precip_1, Precip_2, Precip_3,
    lat, long, alt, Distance_km
  ) %>%
  as.matrix()

y_global = global_train_data$bloom_doy

set.seed(490)
lasso_global = cv.glmnet(x_global, y_global, alpha = 1, standardize = TRUE)
best_lambda_global = lasso_global$lambda.min

lasso_pred_global = predict(lasso_global, s = best_lambda_global, newx = x_global)

# Calculate MAE and MSE
global_mae = mean(abs(y_global - lasso_pred_global))
global_mse = mean((y_global - lasso_pred_global)^2)

# Print the metrics so you can see them in your console
cat("Model Performance ---\n")
cat("Training MAE:", round(global_mae, 2), "days\n")
cat("Training MSE:", round(global_mse, 2))
# -----------------------------------------------------------------------------


#2026 Data
actual_2026 = subset(climate_clean, Year == 2026)

recent_past = subset(climate_clean, Year >= 2015 & Year <= 2025)
normals = aggregate(
  cbind(GDD_35, GDD_40, Chill_Days, Freeze, Precip) ~ DOY + Month + location + station_lat + station_long + STATION, 
  data = recent_past, 
  FUN = mean,
  na.rm = TRUE
)

target_grid = normals %>%
  dplyr::select(DOY, Month, location, station_lat, station_long, STATION) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Year = 2026)

combined_2026 = target_grid %>%
  dplyr::left_join(
    actual_2026 %>% dplyr::select(Year, DOY, location, GDD_35, GDD_40, Chill_Days, Freeze, Precip), 
    by = c("Year", "DOY", "location")
  ) %>%
  dplyr::left_join(
    normals %>% dplyr::select(DOY, location, GDD_35, GDD_40, Chill_Days, Freeze, Precip),
    by = c("DOY", "location"), 
    suffix = c("_actual", "_normal")
  ) %>%
  dplyr::mutate(
    GDD_35 = dplyr::coalesce(GDD_35_actual, GDD_35_normal, 0),
    GDD_40 = dplyr::coalesce(GDD_40_actual, GDD_40_normal, 0),
    Chill_Days = dplyr::coalesce(Chill_Days_actual, Chill_Days_normal, 0),
    Freeze = dplyr::coalesce(Freeze_actual, Freeze_normal, 0),
    Precip = dplyr::coalesce(Precip_actual, Precip_normal, 0)
  )

features_2026 = combined_2026 %>%
  dplyr::filter(Month %in% c(1, 2, 3)) %>%
  dplyr::group_by(location, Year, Month, station_lat, station_long) %>%
  dplyr::summarize(
    GDD_35 = sum(GDD_35, na.rm = TRUE),
    GDD_40 = sum(GDD_40, na.rm = TRUE),
    Chill_Days = sum(Chill_Days, na.rm = TRUE),
    Freeze = sum(Freeze, na.rm = TRUE),
    Precip = sum(Precip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = Month,
    values_from = c(GDD_35, GDD_40, Chill_Days, Freeze, Precip),
    names_glue = "{.value}_{Month}"
  )

features_2026[is.na(features_2026)] = 0
# -----------------------------------------------------------------------------

#Merge with tree coordinate + predict 
tree_coords = bloom_history %>% 
  dplyr::select(location, lat, long, alt) %>% 
  dplyr::distinct()

final_2026_data = features_2026 %>%
  dplyr::inner_join(tree_coords, by = "location") %>%
  dplyr::mutate(
    Distance_km = calculate_distance(lat, long, station_lat, station_long),
    alt = tidyr::replace_na(alt, 0)
  )

x_2026 = as.matrix(final_2026_data[, c(
  "GDD_35_1", "GDD_35_2", "GDD_35_3", 
  "GDD_40_1", "GDD_40_2", "GDD_40_3",
  "Chill_Days_1", "Chill_Days_2", "Chill_Days_3", 
  "Freeze_1", "Freeze_2", "Freeze_3",
  "Precip_1", "Precip_2", "Precip_3",
  "lat", "long", "alt", "Distance_km"
)])

preds_2026 = predict(lasso_global, s = best_lambda_global, newx = x_2026)

results_2026 = data.frame(
  Location = final_2026_data$location,
  Predicted_DOY = round(as.numeric(preds_2026))
) %>%
  dplyr::mutate(
    Predicted_Date = as.Date(Predicted_DOY, origin = as.Date("2025-12-31")),
    Lower_Bound = format(as.Date(Predicted_DOY - round(global_mae * 1.28), origin = as.Date("2025-12-31")), "%b %d"),
    Upper_Bound = format(as.Date(Predicted_DOY + round(global_mae * 1.28), origin = as.Date("2025-12-31")), "%b %d, %Y")
  )

cat("2026 Final 5 City Predictions (80% Confidence Interval) \n")
print(results_2026[, c("Location", "Predicted_DOY", "Predicted_Date", "Lower_Bound", "Upper_Bound")])
# -----------------------------------------------------------------------------


#New 2026 data via ACCUWEATHER
#Load data
forecast_data = read.csv("/Users/R.studio/Forcast.csv")

#Format dates + convert to Fahrenheit
forecast_clean = forecast_data %>%
  dplyr::mutate(
    DATE = as.Date(date, format="%m/%d/%y"),
    Year = lubridate::year(DATE),
    Month = lubridate::month(DATE),
    DOY = lubridate::yday(DATE),
    
    #Celsius to Fahrenheit
    TMAX_F = (tmax * 9/5) + 32,
    TMIN_F = (tmin * 9/5) + 32,
    TMEAN_F = (TMAX_F + TMIN_F) / 2,
    
    # Replicate your global model features
    GDD_35 = pmax(0, TMEAN_F - 35),
    GDD_40 = pmax(0, TMEAN_F - 40),
    Chill_Days = ifelse(!is.na(TMEAN_F) & TMEAN_F >= 32 & TMEAN_F <= 45, 1, 0),
    Freeze = ifelse(!is.na(TMIN_F) & TMIN_F < 32, 1, 0),
    Precip = 0 # Forecast doesn't include precipitation
  )

#Standardize location names
forecast_clean = forecast_clean %>%
  dplyr::mutate(
    location = dplyr::case_when(
      location == "newyork" ~ "newyorkcity",   
      location == "washington" ~ "washingtondc",
      TRUE ~ location
    )
  )

max_actual_dates = actual_2026 %>%
  dplyr::group_by(location) %>%
  dplyr::summarize(last_actual_date = max(DATE, na.rm = TRUE), .groups = "drop")

forecast_filtered = forecast_clean %>%
  dplyr::left_join(max_actual_dates, by = "location") %>%
  # FIXED: If a city (Vancouver) has no actual 2026 data yet, start from Dec 31, 2025
  dplyr::mutate(last_actual_date = dplyr::coalesce(last_actual_date, as.Date("2025-12-31"))) %>%
  dplyr::filter(DATE > last_actual_date & Month %in% c(1, 2, 3))


#actuals + forecast 
cols_to_keep = c("location", "Year", "Month", "GDD_35", "GDD_40", "Chill_Days", "Freeze", "Precip")

combined_2026_accu = dplyr::bind_rows(
  actual_2026 %>% dplyr::filter(Month %in% c(1, 2, 3)) %>% dplyr::select(dplyr::all_of(cols_to_keep)),
  forecast_filtered %>% dplyr::select(dplyr::all_of(cols_to_keep))
)

#Aggregate features by month
features_2026_accu = combined_2026_accu %>%
  dplyr::group_by(location, Year, Month) %>%
  dplyr::summarize(
    GDD_35 = sum(GDD_35, na.rm = TRUE),
    GDD_40 = sum(GDD_40, na.rm = TRUE),
    Chill_Days = sum(Chill_Days, na.rm = TRUE),
    Freeze = sum(Freeze, na.rm = TRUE),
    Precip = sum(Precip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = Month,
    values_from = c(GDD_35, GDD_40, Chill_Days, Freeze, Precip),
    names_glue = "{.value}_{Month}"
  )

features_2026_accu[is.na(features_2026_accu)] = 0

#Merge with station data and tree coordinates
location_lookup = climate_clean %>% 
  dplyr::select(location, station_lat, station_long) %>% 
  dplyr::distinct()

final_accu_data = features_2026_accu %>%
  dplyr::inner_join(location_lookup, by = "location") %>%
  dplyr::inner_join(tree_coords, by = "location") %>%
  dplyr::mutate(
    Distance_km = calculate_distance(lat, long, station_lat, station_long),
    alt = tidyr::replace_na(alt, 0)
  )

# Ensure columns are exactly matched for the matrix
expected_cols = c(
  "GDD_35_1", "GDD_35_2", "GDD_35_3", 
  "GDD_40_1", "GDD_40_2", "GDD_40_3",
  "Chill_Days_1", "Chill_Days_2", "Chill_Days_3", 
  "Freeze_1", "Freeze_2", "Freeze_3",
  "Precip_1", "Precip_2", "Precip_3",
  "lat", "long", "alt", "Distance_km"
)

# Catch missing months 
for (col in expected_cols) {
  if (!col %in% names(final_accu_data)) final_accu_data[[col]] = 0
}

x_2026_accu = as.matrix(final_accu_data[, expected_cols])

#Final prediction
preds_2026_accu = predict(lasso_global, s = best_lambda_global, newx = x_2026_accu)

results_2026_accu = data.frame(
  Location = final_accu_data$location,
  Predicted_DOY = round(as.numeric(preds_2026_accu))
) %>%
  dplyr::mutate(
    Predicted_Date = as.Date(Predicted_DOY, origin = as.Date("2025-12-31")),
    Lower_Bound = format(as.Date(Predicted_DOY - round(global_mae * 1.28), origin = as.Date("2025-12-31")), "%b %d"),
    Upper_Bound = format(as.Date(Predicted_DOY + round(global_mae * 1.28), origin = as.Date("2025-12-31")), "%b %d, %Y")
  )

cat("Final 2026 Prediction")
print(results_2026_accu[, c("Location", "Predicted_DOY", "Predicted_Date", "Lower_Bound", "Upper_Bound")])
