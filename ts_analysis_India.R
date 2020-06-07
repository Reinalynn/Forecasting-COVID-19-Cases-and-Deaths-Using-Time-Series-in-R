# Repeat for India
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesIndia <- tsIndia[, "Cases"]
casesIndia_train <- casesIndia %>% window(end = c(2020, 120))
casesIndia_test <- casesIndia %>% window(end = c(2020, 130))
diff_casesIndia <- diff(casesIndia)
deathsIndia <- tsIndia[, "Deaths"]
deathsIndia_train <- deathsIndia %>% window(end = c(2020, 120))
deathsIndia_test <- deathsIndia %>% window(end = c(2020, 130))
diff_deathsIndia <- diff(deathsIndia)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesIndia <- auto.arima(casesIndia, stepwise = FALSE, approximation = FALSE)
fit_casesIndia # ARIMA(0, 1, 2), AICc - 1470.79
autoplot(fit_casesIndia)
sarima.for(casesIndia_train, n.ahead = 20, 0, 1, 2)
lines(casesIndia_test)
checkresiduals(fit_casesIndia) # passes the Ljung Box test
fit_deathsIndia <- auto.arima(deathsIndia, stepwise = FALSE, approximation = FALSE)
fit_deathsIndia # ARIMA(4, 1, 0), AICc - 848.95
autoplot(fit_deathsIndia)
sarima.for(deathsIndia_train, n.ahead = 20, 4, 1, 0)
lines(deathsIndia_test)
checkresiduals(fit_deathsIndia) # passes

# Use best models to forecast further ahead
fc_10_India <- sarima.for(casesIndia, n.ahead = 10, 0, 1, 2)
fc_10_India$pred
fcd_10_India <- sarima.for(deathsIndia, n.ahead = 10, 4, 1, 0) 
fcd_10_India$pred

# both models are ignoring the recent spike but trending up
