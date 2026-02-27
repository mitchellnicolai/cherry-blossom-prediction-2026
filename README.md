# 2026 Peak Bloom Prediction
This repository contains the data and R code used for the **George Mason University Cherry Blossom Prediction Competition**.

## The Model
I built a **Global LASSO Regression model** that optimizes for the biological Base Temperature of different cherry species across all five competition sites simultaneously, utilizing geographic distance calculations to correct for station-to-tree climatic drift.

## Features Used:
- **GDD (Growing Degree Days):** Calculated using multi-threshold base temperatures (35°F and 40°F) to capture varying biological heat requirements.
- **Chill & Freezing Days:** Count of days below 32°F and between 32°F-45°F in Jan-March to account for winter vernalization and chilling requirements.
- **AccuWeather 90-Day Forecast:** Replaced historical 10-year climate normals with real-time projected meteorological data (converted to Fahrenheit) to accurately bridge the "March Gap" and predict 2026 spring anomalies.

## Final 2026 Predictions
| Location | DOY | Predicted Date | 80% Confidence Interval |
| :--- | :---: | :--- | :--- |
| **Washington D.C.** | 92 | April 2 | Mar 29 - Apr 6 |
| **Kyoto** | 92 | April 2 | Mar 29 - Apr 6 |
| **Liestal** | 95 | April 5 | Apr 1 - Apr 9 |
| **Vancouver** | 88 | March 29 | Mar 25 - Apr 2 |
| **New York City** | 103 | April 13 | Apr 9 - Apr 17 |
