# Repeat for PA
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesPA <- tsPA[, "Cases"]
casesPA_train <- casesPA %>% window(end = c(2020, 120))
casesPA_test <- casesPA %>% window(end = c(2020, 130))
diff_casesPA <- diff(casesPA)
deathsPA <- tsPA[, "Deaths"]
deathsPA_train <- deathsPA %>% window(end = c(2020, 120))
deathsPA_test <- deathsPA %>% window(end = c(2020, 130))
diff_deathsPA <- diff(deathsPA)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesPA <- auto.arima(casesPA, stepwise = FALSE, approximation = FALSE)
fit_casesPA # ARIMA(0, 1, 5), AICc - 1478.93
autoplot(fit_casesPA)
sarima.for(casesPA_train, n.ahead = 20, 0, 1, 5)
lines(casesPA_test)
checkresiduals(fit_casesPA) # p-value too low
fit_casesPA2 <- arima(casesPA, order = c(1, 1, 6))
checkresiduals(fit_casesPA2) # passes the Ljung Box test
fit_deathsPA <- auto.arima(deathsPA, stepwise = FALSE, approximation = FALSE)
fit_deathsPA # ARIMA(5, 1, 0), AICc - 1117.7
autoplot(fit_deathsPA)
sarima.for(deathsPA_train, n.ahead = 20, 5, 1, 0)
lines(deathsPA_test)
checkresiduals(fit_deathsPA) # passes

# Use best models to forecast further ahead
fc_10_PA <- sarima.for(casesPA, n.ahead = 10, 1, 1, 6)
fc_10_PA$pred
fcd_10_PA <- sarima.for(deathsPA, n.ahead = 10, 5, 1, 0) 
fcd_10_PA$pred

# both model match prior trends although deaths is more active