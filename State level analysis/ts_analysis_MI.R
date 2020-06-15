# Repeat for MI
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesMI <- tsMI[, "Cases"]
casesMI_train <- casesMI %>% window(end = c(2020, 120))
casesMI_test <- casesMI %>% window(end = c(2020, 130))
diff_casesMI <- diff(casesMI)
deathsMI <- tsMI[, "Deaths"]
deathsMI_train <- deathsMI %>% window(end = c(2020, 120))
deathsMI_test <- deathsMI %>% window(end = c(2020, 130))
diff_deathsMI <- diff(deathsMI)

# BEST MODELS - use auto.arima models for simplicity and Consistency
fit_casesMI <- auto.arima(casesMI, stepwise = FALSE, approximation = FALSE)
fit_casesMI # ARIMA(3, 1, 0), AICc - 1446.67
autoplot(fit_casesMI)
sarima.for(casesMI_train, n.ahead = 20, 3, 1, 0)
lines(casesMI_test)
checkresiduals(fit_casesMI) # passes the Ljung Box test
fit_deathsMI <- auto.arima(deathsMI, stepwise = FALSE, approximation = FALSE)
fit_deathsMI # ARIMA(3, 1, 2), AICc - 1030.15
autoplot(fit_deathsMI)
sarima.for(deathsMI_train, n.ahead = 20, 3, 1, 2)
lines(deathsMI_test)
checkresiduals(fit_deathsMI) # passes

# Use best models to forecast further ahead
fc_10_MI <- sarima.for(casesMI, n.ahead = 10, 3, 1, 0)
fc_10_MI$pred
fcd_10_MI <- sarima.for(deathsMI, n.ahead = 10, 3, 1, 2) 
fcd_10_MI$pred

# both models show a slight upward trend when the graphs appear to be trending down