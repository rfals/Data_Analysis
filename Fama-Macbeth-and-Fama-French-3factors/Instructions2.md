# Fama French 3-factor and Fama-Macbeth auto-regressor

This R file helps you to easily find Fama-French 5 factor alpha and beta coefficients as well as all relevant T-statistics and p-values.
The model works very well for large data sets as it computes the regressions automatically and stores the relevant variables allowing for easy further analysis

# Limitations

Right now the model only accepts clean data with loaded stock returns and factors gathered from French's official website.
However I will update the model with automatic data gathering options with yfinance API

# Data input

the model accepts data in the following format:

 -The first column should consist of date
 -The next 3 columns should consist of the 3 factors
 -Column 4-n should consist of stock return data
 
