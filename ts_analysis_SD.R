# Repeat for SD
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesSD <- tsSD[, "Cases"]
casesSD_train <- casesSD %>% window(end = c(2020, 120))
casesSD_test <- casesSD %>% window(end = c(2020, 130))
diff_casesSD <- diff(casesSD)
deathsSD <- tsSD[, "Deaths"]
deathsSD_train <- deathsSD %>% window(end = c(2020, 120))
deathsSD_test <- deathsSD %>% window(end = c(2020, 130))
diff_deathsSD <- diff(deathsSD)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesSD <- auto.arima(casesSD, stepwise = FALSE, approximation = FALSE)
fit_casesSD # ARIMA(0, 1, 3), AICc - 933.34
autoplot(fit_casesSD)
sarima.for(casesSD_train, n.ahead = 20, 0, 1, 3)
lines(casesSD_test)
checkresiduals(fit_casesSD) # passes Ljung-Box test
fit_deathsSD <- auto.arima(deathsSD, stepwise = FALSE, approximation = FALSE)
fit_deathsSD # ARIMA(4, 1, 0), AICc - 217.37
autoplot(fit_deathsSD)
sarima.for(deathsSD_train, n.ahead = 20, 4, 1, 0)
lines(deathsSD_test)
checkresiduals(fit_deathsSD) # passes Ljung-Box test

# Use best models to forecast further ahead
fc_10_SD <- sarima.for(casesSD, n.ahead = 10, 0, 1, 3)
fc_10_SD$pred
fcd_10_SD <- sarima.for(deathsSD, n.ahead = 10, 4, 1, 0) 
fcd_10_SD$pred

# both model match prior trends although deaths is more active