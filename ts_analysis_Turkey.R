# Repeat for Turkey
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesTurkey <- tsTurkey[, "Cases"]
casesTurkey_train <- casesTurkey %>% window(end = c(2020, 120))
casesTurkey_test <- casesTurkey %>% window(end = c(2020, 130))
diff_casesTurkey <- diff(casesTurkey)
deathsTurkey <- tsTurkey[, "Deaths"]
deathsTurkey_train <- deathsTurkey %>% window(end = c(2020, 120))
deathsTurkey_test <- deathsTurkey %>% window(end = c(2020, 130))
diff_deathsTurkey <- diff(deathsTurkey)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesTurkey <- auto.arima(casesTurkey, stepwise = FALSE, approximation = FALSE)
fit_casesTurkey # ARIMA(1, 1, 4), AICc - 1518.22
autoplot(fit_casesTurkey)
sarima.for(casesTurkey_train, n.ahead = 20, 1, 1, 4)
lines(casesTurkey_test)
checkresiduals(fit_casesTurkey) # passes Ljung Box test
fit_deathsTurkey <- auto.arima(deathsTurkey, stepwise = FALSE, approximation = FALSE)
fit_deathsTurkey # ARIMA(1, 1, 1), AICc - 648.99
autoplot(fit_deathsTurkey)
sarima.for(deathsTurkey_train, n.ahead = 20, 1, 1, 3)
lines(deathsTurkey_test)
checkresiduals(fit_deathsTurkey) # passes

# Use best models to forecast further ahead
fc_10_Turkey <- sarima.for(casesTurkey, n.ahead = 10, 1, 1, 4)
fc_10_Turkey$pred
fcd_10_Turkey <- sarima.for(deathsTurkey, n.ahead = 10, 1, 1, 3) 
fcd_10_Turkey$pred

# models are forecasting downward trend