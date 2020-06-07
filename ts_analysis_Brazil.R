# Repeat for Brazil
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesBrazil <- tsBrazil[, "Cases"]
casesBrazil_train <- casesBrazil %>% window(end = c(2020, 120))
casesBrazil_test <- casesBrazil %>% window(end = c(2020, 130))
diff_casesBrazil <- diff(casesBrazil)
deathsBrazil <- tsBrazil[, "Deaths"]
deathsBrazil_train <- deathsBrazil %>% window(end = c(2020, 120))
deathsBrazil_test <- deathsBrazil %>% window(end = c(2020, 130))
diff_deathsBrazil <- diff(deathsBrazil)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesBrazil <- auto.arima(casesBrazil, stepwise = FALSE, approximation = FALSE)
fit_casesBrazil # ARIMA(3, 1, 2), AICc - 1680.4
autoplot(fit_casesBrazil)
sarima.for(casesBrazil_train, n.ahead = 20, 3, 1, 2)
lines(casesBrazil_test)
checkresiduals(fit_casesBrazil)
fit_casesBrazil2 <- arima(casesBrazil, order = c(4, 2, 4))
checkresiduals(fit_casesBrazil2) # passes the Ljung Box test
fit_deathsBrazil <- auto.arima(deathsBrazil, stepwise = FALSE, approximation = FALSE)
fit_deathsBrazil # ARIMA(4, 1, 0), AICc - 1155.41
autoplot(fit_deathsBrazil)
sarima.for(deathsBrazil_train, n.ahead = 20, 4, 1, 0)
lines(deathsBrazil_test)
checkresiduals(fit_deathsBrazil) # passes

# Use best models to forecast further ahead
fc_10_Brazil <- sarima.for(casesBrazil, n.ahead = 10, 4, 2, 4)
fc_10_Brazil$pred
fcd_10_Brazil <- sarima.for(deathsBrazil, n.ahead = 10, 4, 1, 0) 
fcd_10_Brazil$pred

# models are following upward trend but more conservatively than recent spike