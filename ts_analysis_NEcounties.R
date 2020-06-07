# Repeat for NE Counties
# Dakota
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsDakotaNE)
Dakota_cases <- tsDakotaNE[, "Cases"]
Dakota_cases_train <- Dakota_cases %>% window(end = c(2020, 120))
Dakota_cases_test <- Dakota_cases %>% window(end = c(2020, 130))
Dakota_deaths <- tsDakotaNE[, "Deaths"]
Dakota_deaths_train <- Dakota_deaths %>% window(end = c(2020, 120))
Dakota_deaths_test <- Dakota_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Dakota_cases <- auto.arima(Dakota_cases, stepwise = FALSE, approximation = FALSE)
fit_Dakota_cases # ARIMA(0, 1, 5), AICc - 1102.55
autoplot(fit_Dakota_cases)
sarima.for(Dakota_cases_train, n.ahead = 20, 0, 1, 5)
lines(Dakota_cases_test)
checkresiduals(fit_Dakota_cases) # passes Ljung Box test
fit_Dakota_deaths <- auto.arima(Dakota_deaths, stepwise = FALSE, approximation = FALSE)
fit_Dakota_deaths # ARIMA(3, 1, 2), AICc - 45.75
autoplot(fit_Dakota_deaths)
sarima.for(Dakota_deaths_train, n.ahead = 20, 3, 1, 2)
lines(Dakota_deaths_test)
checkresiduals(fit_Dakota_deaths) # passes
# Use best models to forecast further ahead
fc_10_Dakota <- sarima.for(Dakota_cases, n.ahead = 10, 0, 1, 5)
fc_10_Dakota$pred
fcd_10_Dakota <- sarima.for(Dakota_deaths, n.ahead = 10, 3, 1, 2) 
fcd_10_Dakota$pred

# Douglas
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsDouglasNE)
Douglas_cases <- tsDouglasNE[, "Cases"]
Douglas_cases_train <- Douglas_cases %>% window(end = c(2020, 120))
Douglas_cases_test <- Douglas_cases %>% window(end = c(2020, 130))
Douglas_deaths <- tsDouglasNE[, "Deaths"]
Douglas_deaths_train <- Douglas_deaths %>% window(end = c(2020, 120))
Douglas_deaths_test <- Douglas_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Douglas_cases <- auto.arima(Douglas_cases, stepwise = FALSE, approximation = FALSE)
fit_Douglas_cases # ARIMA(1, 2, 4), AICc - 832.97
autoplot(fit_Douglas_cases)
sarima.for(Douglas_cases_train, n.ahead = 20, 1, 2, 4)
lines(Douglas_cases_test)
checkresiduals(fit_Douglas_cases) # passes
fit_Douglas_deaths <- auto.arima(Douglas_deaths, stepwise = FALSE, approximation = FALSE)
fit_Douglas_deaths # ARIMA(0, 1, 1), AICc - 187.38
autoplot(fit_Douglas_deaths)
sarima.for(Douglas_deaths_train, n.ahead = 20, 0, 1, 1)
lines(Douglas_deaths_test)
checkresiduals(fit_Douglas_deaths) # does not pass
fit_Douglas_deaths2 <- arima(Douglas_deaths, order = c(10, 2, 10))
checkresiduals(fit_Douglas_deaths2) # p-value = 0.4827, closest I could get to 0.05
# Use best models to forecast further ahead
fc_10_Douglas <- sarima.for(Douglas_cases, n.ahead = 10, 1, 2, 5)
fc_10_Douglas$pred
fcd_10_Douglas <- sarima.for(Douglas_deaths, n.ahead = 10, 10, 2, 10) 
fcd_10_Douglas$pred

# Hall
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsHallNE)
Hall_cases <- tsHallNE[, "Cases"]
Hall_cases_train <- Hall_cases %>% window(end = c(2020, 120))
Hall_cases_test <- Hall_cases %>% window(end = c(2020, 130))
Hall_deaths <- tsHallNE[, "Deaths"]
Hall_deaths_train <- Hall_deaths %>% window(end = c(2020, 120))
Hall_deaths_test <- Hall_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Hall_cases <- auto.arima(Hall_cases, stepwise = FALSE, approximation = FALSE)
fit_Hall_cases # ARIMA(5, 1, 0), AICc - 933.76
autoplot(fit_Hall_cases)
sarima.for(Hall_cases_train, n.ahead = 20, 5, 1, 0)
lines(Hall_cases_test)
checkresiduals(fit_Hall_cases) # passes
fit_Hall_deaths <- auto.arima(Hall_deaths, stepwise = FALSE, approximation = FALSE)
fit_Hall_deaths # ARIMA(0, 1, 1), AICc - 427.91
autoplot(fit_Hall_deaths)
sarima.for(Hall_deaths_train, n.ahead = 20, 0, 1, 1)
lines(Hall_deaths_test)
checkresiduals(fit_Hall_deaths) # passes
# Use best models to forecast further ahead
fc_10_Hall <- sarima.for(Hall_cases, n.ahead = 10, 5, 1, 0)
fc_10_Hall$pred
fcd_10_Hall <- sarima.for(Hall_deaths, n.ahead = 10, 0, 1, 1) 
fcd_10_Hall$pred

# Scotts Bluff
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsScottsNE)
Scotts_cases <- tsScottsNE[, "Cases"]
Scotts_cases_train <- Scotts_cases %>% window(end = c(2020, 120))
Scotts_cases_test <- Scotts_cases %>% window(end = c(2020, 130))
Scotts_deaths <- tsScottsNE[, "Deaths"]
Scotts_deaths_train <- Scotts_deaths %>% window(end = c(2020, 120))
Scotts_deaths_test <- Scotts_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Scotts_cases <- auto.arima(Scotts_cases, stepwise = FALSE, approximation = FALSE)
fit_Scotts_cases # ARIMA(1, 1, 2), AICc - 268.7
autoplot(fit_Scotts_cases)
sarima.for(Scotts_cases_train, n.ahead = 20, 1, 1, 2)
lines(Scotts_cases_test)
checkresiduals(fit_Scotts_cases) # passes
fit_Scotts_deaths <- auto.arima(Scotts_deaths, stepwise = FALSE, approximation = FALSE)
fit_Scotts_deaths # ARIMA(0, 0, 0), AICc - Inf (no deaths recorded)
autoplot(fit_Scotts_deaths)
sarima.for(Scotts_deaths_train, n.ahead = 20, 0, 0, 0)
lines(Scotts_deaths_test)
checkresiduals(fit_Scotts_deaths) # passes
# Use best models to forecast further ahead
fc_10_Scotts <- sarima.for(Scotts_cases, n.ahead = 10, 1, 1, 2)
fc_10_Scotts$pred
