# Repeat for NE
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesNE <- tsNE[, "Cases"]
casesNE_train <- casesNE %>% window(end = c(2020, 120))
casesNE_test <- casesNE %>% window(end = c(2020, 130))
diff_casesNE <- diff(casesNE)
deathsNE <- tsNE[, "Deaths"]
deathsNE_train <- deathsNE %>% window(end = c(2020, 120))
deathsNE_test <- deathsNE %>% window(end = c(2020, 130))
diff_deathsNE <- diff(deathsNE)

# BEST MODELS - use auto.arima models for simplicity and Consistency
fit_casesNE <- auto.arima(casesNE, stepwise = FALSE, approximation = FALSE)
fit_casesNE # ARIMA(5, 1, 0), AICc - 1183.13
autoplot(fit_casesNE)
sarima.for(casesNE_train, n.ahead = 20, 5, 1, 0)
lines(casesNE_test)
checkresiduals(fit_casesNE)
fit_casesNE2 <- arima(casesNE, order = c(6, 1, 1))
checkresiduals(fit_casesNE2) # p-value - 0.005 (closest I could get to 0.05) - not a good model
fit_deathsNE <- auto.arima(deathsNE, stepwise = FALSE, approximation = FALSE)
fit_deathsNE # ARIMA(2, 1, 3), AICc - 441.16
autoplot(fit_deathsNE)
sarima.for(deathsNE_train, n.ahead = 20, 2, 1, 3)
lines(deathsNE_test)
checkresiduals(fit_deathsNE)
fit_deathsNE2 <- arima(deathsNE, order = c(2, 2, 4))
checkresiduals(fit_deathsNE2) # passes the Ljung Box test

# Use best models to forecast further ahead
fc_10_NE <- sarima.for(casesNE, n.ahead = 10, 6, 1, 1)
fc_10_NE$pred
fcd_10_NE <- sarima.for(deathsNE, n.ahead = 10, 2, 2, 4) 
fcd_10_NE$pred

# cases model looks okay but deaths model is skewed because negative deaths were reported by NE