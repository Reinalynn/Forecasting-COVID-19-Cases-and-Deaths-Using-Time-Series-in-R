# Repeat for MI Counties
# Monroe
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsMonroeMI)
Monroe_cases <- tsMonroeMI[, "Cases"]
Monroe_cases_train <- Monroe_cases %>% window(end = c(2020, 120))
Monroe_cases_test <- Monroe_cases %>% window(end = c(2020, 130))
Monroe_deaths <- tsMonroeMI[, "Deaths"]
Monroe_deaths_train <- Monroe_deaths %>% window(end = c(2020, 120))
Monroe_deaths_test <- Monroe_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Monroe_cases <- auto.arima(Monroe_cases, stepwise = FALSE, approximation = FALSE)
fit_Monroe_cases # ARIMA(3, 1, 2), AICc - 575.13
autoplot(fit_Monroe_cases)
sarima.for(Monroe_cases_train, n.ahead = 20, 3, 1, 2)
lines(Monroe_cases_test)
checkresiduals(fit_Monroe_cases) # passes Ljung Box test
fit_Monroe_deaths <- auto.arima(Monroe_deaths, stepwise = FALSE, approximation = FALSE)
fit_Monroe_deaths # ARIMA(2, 1, 1), AICc - 105.55
autoplot(fit_Monroe_deaths)
sarima.for(Monroe_deaths_train, n.ahead = 20, 2, 1, 1)
lines(Monroe_deaths_test)
checkresiduals(fit_Monroe_deaths) # passes
# Use best models to forecast further ahead
fc_10_Monroe <- sarima.for(Monroe_cases, n.ahead = 10, 3, 1, 2)
fc_10_Monroe$pred
fcd_10_Monroe <- sarima.for(Monroe_deaths, n.ahead = 10, 2, 1, 1) 
fcd_10_Monroe$pred

# Macomb
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsMacombMI)
Macomb_cases <- tsMacombMI[, "Cases"]
Macomb_cases_train <- Macomb_cases %>% window(end = c(2020, 120))
Macomb_cases_test <- Macomb_cases %>% window(end = c(2020, 130))
Macomb_deaths <- tsMacombMI[, "Deaths"]
Macomb_deaths_train <- Macomb_deaths %>% window(end = c(2020, 120))
Macomb_deaths_test <- Macomb_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Macomb_cases <- auto.arima(Macomb_cases, stepwise = FALSE, approximation = FALSE)
fit_Macomb_cases # ARIMA(2, 1, 3), AICc - 1058.58
autoplot(fit_Macomb_cases)
sarima.for(Macomb_cases_train, n.ahead = 20, 2, 1, 3)
lines(Macomb_cases_test)
checkresiduals(fit_Macomb_cases) # does not pass
fit_Macomb_cases2 <- arima(Macomb_cases, order = c(5, 1, 9))
checkresiduals(fit_Macomb_cases2) # passes
fit_Macomb_deaths <- auto.arima(Macomb_deaths, stepwise = FALSE, approximation = FALSE)
fit_Macomb_deaths # ARIMA(5, 1, 0), AICc - 713.8
autoplot(fit_Macomb_deaths)
sarima.for(Macomb_deaths_train, n.ahead = 20, 5, 1, 0)
lines(Macomb_deaths_test)
checkresiduals(fit_Macomb_deaths) # does not pass
fit_Macomb_deaths2 <- arima(Macomb_deaths, order = c(5, 1, 4))
checkresiduals(fit_Macomb_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Macomb <- sarima.for(Macomb_cases, n.ahead = 10, 5, 1, 9)
fc_10_Macomb$pred
fcd_10_Macomb <- sarima.for(Macomb_deaths, n.ahead = 10, 5, 1, 4) 
fcd_10_Macomb$pred

# Oakland
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsOaklandMI)
Oakland_cases <- tsOaklandMI[, "Cases"]
Oakland_cases_train <- Oakland_cases %>% window(end = c(2020, 120))
Oakland_cases_test <- Oakland_cases %>% window(end = c(2020, 130))
Oakland_deaths <- tsOaklandMI[, "Deaths"]
Oakland_deaths_train <- Oakland_deaths %>% window(end = c(2020, 120))
Oakland_deaths_test <- Oakland_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Oakland_cases <- auto.arima(Oakland_cases, stepwise = FALSE, approximation = FALSE)
fit_Oakland_cases # ARIMA(0, 1, 5), AICc - 1086.09
autoplot(fit_Oakland_cases)
sarima.for(Oakland_cases_train, n.ahead = 20, 0, 1, 5)
lines(Oakland_cases_test)
checkresiduals(fit_Oakland_cases) # does not pass
fit_Oakland_cases2 <- arima(Oakland_cases, order = c(6, 1, 8))
checkresiduals(fit_Oakland_cases2) # passes
fit_Oakland_deaths <- auto.arima(Oakland_deaths, stepwise = FALSE, approximation = FALSE)
fit_Oakland_deaths # ARIMA(3, 1, 2), AICc - 704.36
autoplot(fit_Oakland_deaths)
sarima.for(Oakland_deaths_train, n.ahead = 20, 3, 1, 2)
lines(Oakland_deaths_test)
checkresiduals(fit_Oakland_deaths) # does not pass
fit_Oakland_deaths2 <- arima(Oakland_deaths, order = c(4, 2, 3))
checkresiduals(fit_Oakland_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Oakland <- sarima.for(Oakland_cases, n.ahead = 10, 6, 1, 8)
fc_10_Oakland$pred
fcd_10_Oakland <- sarima.for(Oakland_deaths, n.ahead = 10, 4, 2, 3) 
fcd_10_Oakland$pred

# Wayne
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsWayneMI)
Wayne_cases <- tsWayneMI[, "Cases"]
Wayne_cases_train <- Wayne_cases %>% window(end = c(2020, 120))
Wayne_cases_test <- Wayne_cases %>% window(end = c(2020, 130))
Wayne_deaths <- tsWayneMI[, "Deaths"]
Wayne_deaths_train <- Wayne_deaths %>% window(end = c(2020, 120))
Wayne_deaths_test <- Wayne_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Wayne_cases <- auto.arima(Wayne_cases, stepwise = FALSE, approximation = FALSE)
fit_Wayne_cases # ARIMA(1, 1, 2), AICc - 1344.93
autoplot(fit_Wayne_cases)
sarima.for(Wayne_cases_train, n.ahead = 20, 1, 1, 2)
lines(Wayne_cases_test)
checkresiduals(fit_Wayne_cases) # passes
fit_Wayne_deaths <- auto.arima(Wayne_deaths, stepwise = FALSE, approximation = FALSE)
fit_Wayne_deaths # ARIMA(0, 1, 5), AICc - 971.62
autoplot(fit_Wayne_deaths)
sarima.for(Wayne_deaths_train, n.ahead = 20, 0, 1, 5)
lines(Wayne_deaths_test)
checkresiduals(fit_Wayne_deaths) # passes
# Use best models to forecast further ahead
fc_10_Wayne <- sarima.for(Wayne_cases, n.ahead = 10, 1, 1, 2)
fc_10_Wayne$pred
fcd_10_Wayne <- sarima.for(Wayne_deaths, n.ahead = 10, 0, 1, 5) 
fcd_10_Wayne$pred
