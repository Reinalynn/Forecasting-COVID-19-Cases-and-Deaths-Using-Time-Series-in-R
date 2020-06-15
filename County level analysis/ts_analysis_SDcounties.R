# Repeat for SD Counties
# Brown
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsBrownSD)
Brown_cases <- tsBrownSD[, "Cases"]
Brown_cases_train <- Brown_cases %>% window(end = c(2020, 120))
Brown_cases_test <- Brown_cases %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Brown_cases <- auto.arima(Brown_cases, stepwise = FALSE, approximation = FALSE)
fit_Brown_cases # ARIMA(5, 1, 0), AICc - 427.12
autoplot(fit_Brown_cases)
sarima.for(Brown_cases_train, n.ahead = 20, 5, 1, 0)
lines(Brown_cases_test)
checkresiduals(fit_Brown_cases) # passes Ljung Box test
# Use best models to forecast further ahead
fc_10_Brown <- sarima.for(Brown_cases, n.ahead = 10, 2, 1, 2)
fc_10_Brown$pred

# Minnehaha
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsMinnehahaSD)
Minnehaha_cases <- tsMinnehahaSD[, "Cases"]
Minnehaha_cases_train <- Minnehaha_cases %>% window(end = c(2020, 120))
Minnehaha_cases_test <- Minnehaha_cases %>% window(end = c(2020, 130))
Minnehaha_deaths <- tsMinnehahaSD[, "Deaths"]
Minnehaha_deaths_train <- Minnehaha_deaths %>% window(end = c(2020, 120))
Minnehaha_deaths_test <- Minnehaha_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Minnehaha_cases <- auto.arima(Minnehaha_cases, stepwise = FALSE, approximation = FALSE)
fit_Minnehaha_cases # ARIMA(3, 1, 0), AICc - 921.22
autoplot(fit_Minnehaha_cases)
sarima.for(Minnehaha_cases_train, n.ahead = 20, 3, 1, 0)
lines(Minnehaha_cases_test)
checkresiduals(fit_Minnehaha_cases) # does not pass
fit_Minnehaha_cases2 <- arima(Minnehaha_cases, order = c(6, 1, 2))
checkresiduals(fit_Minnehaha_cases2) # passes
fit_Minnehaha_deaths <- auto.arima(Minnehaha_deaths, stepwise = FALSE, approximation = FALSE)
fit_Minnehaha_deaths # ARIMA(1, 1, 4), AICc - 300.79
autoplot(fit_Minnehaha_deaths)
sarima.for(Minnehaha_deaths_train, n.ahead = 20, 1, 1, 4)
lines(Minnehaha_deaths_test)
checkresiduals(fit_Minnehaha_deaths) # passes
# Use best models to forecast further ahead
fc_10_Minnehaha <- sarima.for(Minnehaha_cases, n.ahead = 10, 6, 1, 2)
fc_10_Minnehaha$pred
fcd_10_Minnehaha <- sarima.for(Minnehaha_deaths, n.ahead = 10, 1, 1, 4) 
fcd_10_Minnehaha$pred
