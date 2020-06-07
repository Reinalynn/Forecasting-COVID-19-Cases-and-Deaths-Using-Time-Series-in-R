# Repeat for CO
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesCO <- tsCO[, "Cases"]
casesCO_train <- casesCO %>% window(end = c(2020, 120))
casesCO_test <- casesCO %>% window(end = c(2020, 130))
diff_casesCO <- diff(casesCO)
deathsCO <- tsCO[, "Deaths"]
deathsCO_train <- deathsCO %>% window(end = c(2020, 120))
deathsCO_test <- deathsCO %>% window(end = c(2020, 130))
diff_deathsCO <- diff(deathsCO)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesCO <- auto.arima(casesCO, stepwise = FALSE, approximation = FALSE)
fit_casesCO # ARIMA(4, 1, 1), AICc - 1374.34
autoplot(fit_casesCO)
sarima.for(casesCO_train, n.ahead = 20, 4, 1, 1)
lines(casesCO_test)
checkresiduals(fit_casesCO) # passes the Ljung Box test
fit_deathsCO <- auto.arima(deathsCO, stepwise = FALSE, approximation = FALSE)
fit_deathsCO # ARIMA(2, 1, 2), AICc - 871.11
autoplot(fit_deathsCO)
sarima.for(deathsCO_train, n.ahead = 20, 2, 1, 2)
lines(deathsCO_test)
checkresiduals(fit_deathsCO)
fit_deathsCO2 <- arima(deathsCO, order = c(10, 1, 2))
checkresiduals(fit_deathsCO2) # passes the Ljung Box test

# Use best models to forecast further ahead
fc_10_CO <- sarima.for(casesCO, n.ahead = 10, 4, 1, 1)
fc_10_CO$pred
fcd_10_CO <- sarima.for(deathsCO, n.ahead = 10, 10, 1, 2) 
fcd_10_CO$pred

# cases are trending up but deaths appear to be steady