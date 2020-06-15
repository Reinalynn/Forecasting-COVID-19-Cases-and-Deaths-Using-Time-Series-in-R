# Repeat for TX
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesTX <- tsTX[, "Cases"]
casesTX_train <- casesTX %>% window(end = c(2020, 120))
casesTX_test <- casesTX %>% window(end = c(2020, 130))
diff_casesTX <- diff(casesTX)
deathsTX <- tsTX[, "Deaths"]
deathsTX_train <- deathsTX %>% window(end = c(2020, 120))
deathsTX_test <- deathsTX %>% window(end = c(2020, 130))
diff_deathsTX <- diff(deathsTX)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesTX <- auto.arima(casesTX, stepwise = FALSE, approximation = FALSE)
fit_casesTX # ARIMA(5, 1, 0), AICc - 1358.83
autoplot(fit_casesTX)
sarima.for(casesTX_train, n.ahead = 20, 5, 1, 0)
lines(casesTX_test)
checkresiduals(fit_casesTX) # too low
fit_casesTX2 <- arima(casesTX, order = c(7, 1, 0))
checkresiduals(fit_casesTX2) # passes the Ljung Box test
fit_deathsTX <- auto.arima(deathsTX, stepwise = FALSE, approximation = FALSE)
fit_deathsTX # ARIMA(5, 1, 0), AICc - 689.06
autoplot(fit_deathsTX)
sarima.for(deathsTX_train, n.ahead = 20, 5, 1, 0)
lines(deathsTX_test)
checkresiduals(fit_deathsTX) # passes

# Use best models to forecast further ahead
fc_10_TX <- sarima.for(casesTX, n.ahead = 10, 7, 1, 0)
fc_10_TX$pred
fcd_10_TX <- sarima.for(deathsTX, n.ahead = 10, 5, 1, 0) 
fcd_10_TX$pred

# both models appear very strong
