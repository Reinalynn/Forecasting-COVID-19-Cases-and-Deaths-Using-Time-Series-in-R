# Repeat for UK
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesUK <- tsUK[, "Cases"]
casesUK_train <- casesUK %>% window(end = c(2020, 120))
casesUK_test <- casesUK %>% window(end = c(2020, 130))
diff_casesUK <- diff(casesUK)
deathsUK <- tsUK[, "Deaths"]
deathsUK_train <- deathsUK %>% window(end = c(2020, 120))
deathsUK_test <- deathsUK %>% window(end = c(2020, 130))
diff_deathsUK <- diff(deathsUK)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesUK <- auto.arima(casesUK, stepwise = FALSE, approximation = FALSE)
fit_casesUK # ARIMA(3, 1, 2), AICc - 1700.4
autoplot(fit_casesUK)
sarima.for(casesUK_train, n.ahead = 20, 3, 1, 2)
lines(casesUK_test)
checkresiduals(fit_casesUK) # passes the LJung Box test
fit_deathsUK <- auto.arima(deathsUK, stepwise = FALSE, approximation = FALSE)
fit_deathsUK # ARIMA(1, 1, 3), AICc - 1350.23
autoplot(fit_deathsUK)
sarima.for(deathsUK_train, n.ahead = 20, 1, 1, 3)
lines(deathsUK_test)
checkresiduals(fit_deathsUK)
fit_deathsUK2 <- arima(deathsUK, order = c(6, 2, 9))
checkresiduals(fit_deathsUK2) # p-value = 0.019, closest I could get to 0.05

# Use best models to forecast further ahead
fc_10_UK <- sarima.for(casesUK, n.ahead = 10, 3, 1, 2)
fc_10_UK$pred
fcd_10_UK <- sarima.for(deathsUK, n.ahead = 10, 6, 2, 9) 
fcd_10_UK$pred

# models appear to capture trend but not variation