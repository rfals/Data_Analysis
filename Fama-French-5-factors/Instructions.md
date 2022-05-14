# Fama French 5-factor auto-regressor

This R file helps you to easily find Fama-French 5 factor alpha and beta coefficients as well as all relevant T-statistics and p-values.
The model works very well for large data sets as it computes the regressions automatically and stores the relevant variables allowing for easy further analysis

# Limitations

Right now the model only accepts clean data with loaded stock returns and factors gathered from French's official website.
However I will update the model with automatic data gathering options with yfinance API

# Data input

the model accepts data in the following format:

 -The first column should consist of date
 -The next 5 columns should consist of the 5 factors
 -Column 6-n should consist of stock return data
 
