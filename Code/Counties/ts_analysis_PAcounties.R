# Repeat for PA Counties
# Bucks
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsBucksPA)
Bucks_cases <- tsBucksPA[, "Cases"]
Bucks_cases_train <- Bucks_cases %>% window(end = c(2020, 120))
Bucks_cases_test <- Bucks_cases %>% window(end = c(2020, 130))
Bucks_deaths <- tsBucksPA[, "Deaths"]
Bucks_deaths_train <- Bucks_deaths %>% window(end = c(2020, 120))
Bucks_deaths_test <- Bucks_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Bucks_cases <- auto.arima(Bucks_cases, stepwise = FALSE, approximation = FALSE)
fit_Bucks_cases # ARIMA(2, 1, 2), AICc - 1021.76
autoplot(fit_Bucks_cases)
sarima.for(Bucks_cases_train, n.ahead = 20, 2, 1, 2)
lines(Bucks_cases_test)
checkresiduals(fit_Bucks_cases) # passes Ljung Box test
fit_Bucks_deaths <- auto.arima(Bucks_deaths, stepwise = FALSE, approximation = FALSE)
fit_Bucks_deaths # ARIMA(4, 1, 1), AICc - 635.77
autoplot(fit_Bucks_deaths)
sarima.for(Bucks_deaths_train, n.ahead = 20, 4, 1, 1)
lines(Bucks_deaths_test)
checkresiduals(fit_Bucks_deaths) # does not pass
fit_Bucks_deaths2 <- arima(Bucks_deaths, order = c(1, 2, 9))
checkresiduals(fit_Bucks_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Bucks <- sarima.for(Bucks_cases, n.ahead = 10, 2, 1, 2)
fc_10_Bucks$pred
fcd_10_Bucks <- sarima.for(Bucks_deaths, n.ahead = 10, 1, 2, 9) 
fcd_10_Bucks$pred

# Montgomery
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsMontgomeryPA)
Montgomery_cases <- tsMontgomeryPA[, "Cases"]
Montgomery_cases_train <- Montgomery_cases %>% window(end = c(2020, 120))
Montgomery_cases_test <- Montgomery_cases %>% window(end = c(2020, 130))
Montgomery_deaths <- tsMontgomeryPA[, "Deaths"]
Montgomery_deaths_train <- Montgomery_deaths %>% window(end = c(2020, 120))
Montgomery_deaths_test <- Montgomery_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Montgomery_cases <- auto.arima(Montgomery_cases, stepwise = FALSE, approximation = FALSE)
fit_Montgomery_cases # ARIMA(1, 1, 0), AICc - 987.53
autoplot(fit_Montgomery_cases)
sarima.for(Montgomery_cases_train, n.ahead = 20, 1, 1, 0)
lines(Montgomery_cases_test)
checkresiduals(fit_Montgomery_cases) # passes
fit_Montgomery_deaths <- auto.arima(Montgomery_deaths, stepwise = FALSE, approximation = FALSE)
fit_Montgomery_deaths # ARIMA(5, 1, 0), AICc - 800.29
autoplot(fit_Montgomery_deaths)
sarima.for(Montgomery_deaths_train, n.ahead = 20, 5, 1, 0)
lines(Montgomery_deaths_test)
checkresiduals(fit_Montgomery_deaths) # passes
# Use best models to forecast further ahead
fc_10_Montgomery <- sarima.for(Montgomery_cases, n.ahead = 10, 1, 1, 0)
fc_10_Montgomery$pred
fcd_10_Montgomery <- sarima.for(Montgomery_deaths, n.ahead = 10, 5, 1, 0) 
fcd_10_Montgomery$pred

# Philadelphia
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsPhiladelphiaPA)
Philadelphia_cases <- tsPhiladelphiaPA[, "Cases"]
Philadelphia_cases_train <- Philadelphia_cases %>% window(end = c(2020, 120))
Philadelphia_cases_test <- Philadelphia_cases %>% window(end = c(2020, 130))
Philadelphia_deaths <- tsPhiladelphiaPA[, "Deaths"]
Philadelphia_deaths_train <- Philadelphia_deaths %>% window(end = c(2020, 120))
Philadelphia_deaths_test <- Philadelphia_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Philadelphia_cases <- auto.arima(Philadelphia_cases, stepwise = FALSE, approximation = FALSE)
fit_Philadelphia_cases # ARIMA(0, 1, 3), AICc - 1377.94
autoplot(fit_Philadelphia_cases)
sarima.for(Philadelphia_cases_train, n.ahead = 20, 0, 1, 3)
lines(Philadelphia_cases_test)
checkresiduals(fit_Philadelphia_cases) # passes
fit_Philadelphia_deaths <- auto.arima(Philadelphia_deaths, stepwise = FALSE, approximation = FALSE)
fit_Philadelphia_deaths # ARIMA(0, 1, 2), AICc - 849.76
autoplot(fit_Philadelphia_deaths)
sarima.for(Philadelphia_deaths_train, n.ahead = 20, 0, 1, 2)
lines(Philadelphia_deaths_test)
checkresiduals(fit_Philadelphia_deaths) # passes
# Use best models to forecast further ahead
fc_10_Philadelphia <- sarima.for(Philadelphia_cases, n.ahead = 10, 0, 1, 3)
fc_10_Philadelphia$pred
fcd_10_Philadelphia <- sarima.for(Philadelphia_deaths, n.ahead = 10, 0, 1, 2) 
fcd_10_Philadelphia$pred
