# cherry-blossom-prediction-2026
This repository contains the data and R code used for the George Mason University Cherry Blossom Prediction Competition.

# The Model
I used a LASSO Regression model that optimizes for the biological Base Temperature of different cherry species. 

# Features Used:
- GDD (Growing Degree Days): Calculated using city-specific base temperatures (35°F to 45°F).
- Freezing Days: Count of days below 32°F in Jan-March to account for chilling requirements.
- Climate Normals: A 10-year rolling average to simulate 2026 weather from late February through April.

# Final 2026 Predictions
| Location | DOY | Date |
| :--- | :---: | :--- |
| Washington D.C. | 88 | March 29 |
| Kyoto | 88 | March 29 |
| New York City | 91 | April 1 |
| Liestal | 95 | April 5 |
| Vancouver | 100 | April 10 |
