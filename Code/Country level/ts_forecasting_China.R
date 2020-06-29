# Repeat for China
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesChina <- tsChina[, "Cases"]
casesChina_train <- casesChina %>% window(end = c(2020, 120))
casesChina_test <- casesChina %>% window(end = c(2020, 130))
diff_casesChina <- diff(casesChina)
deathsChina <- tsChina[, "Deaths"]
deathsChina_train <- deathsChina %>% window(end = c(2020, 120))
deathsChina_test <- deathsChina %>% window(end = c(2020, 130))
diff_deathsChina <- diff(deathsChina)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesChina <- auto.arima(casesChina, stepwise = FALSE, approximation = FALSE)
fit_casesChina # ARIMA(0, 1, 1), AICc - 1874.66
autoplot(fit_casesChina)
sarima.for(casesChina_train, n.ahead = 20, 0, 1, 1)
lines(casesChina_test)
checkresiduals(fit_casesChina) # passes Ljung Box test
fit_deathsChina <- auto.arima(deathsChina, stepwise = FALSE, approximation = FALSE)
fit_deathsChina # ARIMA(0, 0, 0) WHITE NOISE,AICc - 1361.29

# Both models are flat (0) to match actuals
