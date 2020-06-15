# Repeat for TX Counties
# Dallas
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsDallasTX)
Dallas_cases <- tsDallasTX[, "Cases"]
Dallas_cases_train <- Dallas_cases %>% window(end = c(2020, 120))
Dallas_cases_test <- Dallas_cases %>% window(end = c(2020, 130))
Dallas_deaths <- tsDallasTX[, "Deaths"]
Dallas_deaths_train <- Dallas_deaths %>% window(end = c(2020, 120))
Dallas_deaths_test <- Dallas_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Dallas_cases <- auto.arima(Dallas_cases, stepwise = FALSE, approximation = FALSE)
fit_Dallas_cases # ARIMA(2, 1, 0), AICc - 1073.6
autoplot(fit_Dallas_cases)
sarima.for(Dallas_cases_train, n.ahead = 20, 2, 1, 0)
lines(Dallas_cases_test)
checkresiduals(fit_Dallas_cases) # passes Ljung Box test
fit_Dallas_deaths <- auto.arima(Dallas_deaths, stepwise = FALSE, approximation = FALSE)
fit_Dallas_deaths # ARIMA(2, 1, 3), AICc - 453
autoplot(fit_Dallas_deaths)
sarima.for(Dallas_deaths_train, n.ahead = 20, 2, 1, 3)
lines(Dallas_deaths_test)
checkresiduals(fit_Dallas_deaths) # passes
# Use best models to forecast further ahead
fc_10_Dallas <- sarima.for(Dallas_cases, n.ahead = 10, 2, 1, 0)
fc_10_Dallas$pred
fcd_10_Dallas <- sarima.for(Dallas_deaths, n.ahead = 10, 2, 1, 3)
fcd_10_Dallas$pred

# Ellis
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsEllisTX)
Ellis_cases <- tsEllisTX[, "Cases"]
Ellis_cases_train <- Ellis_cases %>% window(end = c(2020, 120))
Ellis_cases_test <- Ellis_cases %>% window(end = c(2020, 130))
Ellis_deaths <- tsEllisTX[, "Deaths"]
Ellis_deaths_train <- Ellis_deaths %>% window(end = c(2020, 120))
Ellis_deaths_test <- Ellis_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Ellis_cases <- auto.arima(Ellis_cases, stepwise = FALSE, approximation = FALSE)
fit_Ellis_cases # ARIMA(0, 1, 5), AICc - 509.19
autoplot(fit_Ellis_cases)
sarima.for(Ellis_cases_train, n.ahead = 20, 0, 1, 5)
lines(Ellis_cases_test)
checkresiduals(fit_Ellis_cases) # passes
fit_Ellis_deaths <- auto.arima(Ellis_deaths, stepwise = FALSE, approximation = FALSE)
fit_Ellis_deaths # ARIMA(1, 1, 4), AICc - 94.5
autoplot(fit_Ellis_deaths)
sarima.for(Ellis_deaths_train, n.ahead = 20, 1, 1, 4)
lines(Ellis_deaths_test)
checkresiduals(fit_Ellis_deaths) # passes
# Use best models to forecast further ahead
fc_10_Ellis <- sarima.for(Ellis_cases, n.ahead = 10, 0, 1, 5)
fc_10_Ellis$pred
fcd_10_Ellis <- sarima.for(Ellis_deaths, n.ahead = 10, 1, 1, 4) 
fcd_10_Ellis$pred

# Harris
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsHarrisTX)
Harris_cases <- tsHarrisTX[, "Cases"]
Harris_cases_train <- Harris_cases %>% window(end = c(2020, 120))
Harris_cases_test <- Harris_cases %>% window(end = c(2020, 130))
Harris_deaths <- tsHarrisTX[, "Deaths"]
Harris_deaths_train <- Harris_deaths %>% window(end = c(2020, 120))
Harris_deaths_test <- Harris_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Harris_cases <- auto.arima(Harris_cases, stepwise = FALSE, approximation = FALSE)
fit_Harris_cases # ARIMA(2, 1, 0), AICc - 1233.25
autoplot(fit_Harris_cases)
sarima.for(Harris_cases_train, n.ahead = 20, 2, 1, 0)
lines(Harris_cases_test)
checkresiduals(fit_Harris_cases) # does not pass
fit_Harris_cases2 <- arima(Harris_cases, order = c(1, 1, 5))
checkresiduals(fit_Harris_cases2) # pass
fit_Harris_deaths <- auto.arima(Harris_deaths, stepwise = FALSE, approximation = FALSE)
fit_Harris_deaths # ARIMA(1, 1, 1), AICc - 444.78
autoplot(fit_Harris_deaths)
sarima.for(Harris_deaths_train, n.ahead = 20, 1, 1, 1)
lines(Harris_deaths_test)
checkresiduals(fit_Harris_deaths) # passes
# Use best models to forecast further ahead
fc_10_Harris <- sarima.for(Harris_cases, n.ahead = 10, 1, 1, 5)
fc_10_Harris$pred
fcd_10_Harris <- sarima.for(Harris_deaths, n.ahead = 10, 1, 1, 1)
fcd_10_Harris$pred

# Tarrant
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsTarrantTX)
Tarrant_cases <- tsTarrantTX[, "Cases"]
Tarrant_cases_train <- Tarrant_cases %>% window(end = c(2020, 120))
Tarrant_cases_test <- Tarrant_cases %>% window(end = c(2020, 130))
Tarrant_deaths <- tsTarrantTX[, "Deaths"]
Tarrant_deaths_train <- Tarrant_deaths %>% window(end = c(2020, 120))
Tarrant_deaths_test <- Tarrant_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Tarrant_cases <- auto.arima(Tarrant_cases, stepwise = FALSE, approximation = FALSE)
fit_Tarrant_cases # ARIMA(3, 1, 2), AICc - 1076.92
autoplot(fit_Tarrant_cases)
sarima.for(Tarrant_cases_train, n.ahead = 20, 3, 1, 2)
lines(Tarrant_cases_test)
checkresiduals(fit_Tarrant_cases) # does not pass Ljung Box test
fit_Tarrant_cases2 <- arima(Tarrant_cases, order = c(3, 1, 8))
checkresiduals(fit_Tarrant_cases2) # passes
fit_Tarrant_deaths <- auto.arima(Tarrant_deaths, stepwise = FALSE, approximation = FALSE)
fit_Tarrant_deaths # ARIMA(2, 1, 3), AICc - 375.33
autoplot(fit_Tarrant_deaths)
sarima.for(Tarrant_deaths_train, n.ahead = 20, 2, 1, 3)
lines(Tarrant_deaths_test)
checkresiduals(fit_Tarrant_deaths) # does not pass
fit_Tarrant_deaths2 <- arima(Tarrant_deaths, order = c(2, 2, 6))
checkresiduals(fit_Tarrant_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Tarrant <- sarima.for(Tarrant_cases, n.ahead = 10, 3, 1, 8)
fc_10_Tarrant$pred
fcd_10_Tarrant <- sarima.for(Tarrant_deaths, n.ahead = 10, 2, 2, 6) 
fcd_10_Tarrant$pred
