# Repeat for US
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesUS <- tsUS[, "Cases"]
casesUS_train <- casesUS %>% window(end = c(2020, 120))
casesUS_test <- casesUS %>% window(end = c(2020, 130))
diff_casesUS <- diff(casesUS)
deathsUS <- tsUS[, "Deaths"]
deathsUS_train <- deathsUS %>% window(end = c(2020, 120))
deathsUS_test <- deathsUS %>% window(end = c(2020, 130))
diff_deathsUS <- diff(deathsUS)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesUS <- auto.arima(casesUS, stepwise = FALSE, approximation = FALSE, trace = TRUE)
fit_casesUS # ARIMA(3, 1, 0), AICc - 1941.55
autoplot(fit_casesUS)
accuracy(fit_casesUS)
sarima.for(casesUS_train, n.ahead = 20, 3, 1, 0)
lines(casesUS_test)
checkresiduals(fit_casesUS) # p-value too low
fit_casesUS2 <- arima(casesUS, order = c(6, 1, 2))
checkresiduals(fit_casesUS2) # passes Ljung-Box test
fit_casesUS2
accuracy(fit_casesUS2)
fit_deathsUS <- auto.arima(deathsUS, stepwise = FALSE, approximation = FALSE)
fit_deathsUS # ARIMA(4, 1, 0), AICc - 1155.41
autoplot(fit_deathsUS)
accuracy(fit_deathsUS)
sarima.for(deathsUS_train, n.ahead = 20, 4, 1, 0)
lines(deathsUS_test)
checkresiduals(fit_deathsUS) # passes

# Use best models to forecast further ahead
fc_10_US <- sarima.for(casesUS, n.ahead = 10, 6, 1, 2)
fc_10_US$pred
fcd_10_US <- sarima.for(deathsUS, n.ahead = 10, 4, 1, 0) 
fcd_10_US$pred

# models show cases declining while deaths are steady