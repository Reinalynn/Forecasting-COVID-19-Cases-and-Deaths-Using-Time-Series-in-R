# Repeat for CO Counties
# Adams
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsAdamsCO)
Adams_cases <- tsAdamsCO[, "Cases"]
Adams_cases_train <- Adams_cases %>% window(end = c(2020, 120))
Adams_cases_test <- Adams_cases %>% window(end = c(2020, 130))
Adams_deaths <- tsAdamsCO[, "Deaths"]
Adams_deaths_train <- Adams_deaths %>% window(end = c(2020, 120))
Adams_deaths_test <- Adams_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Adams_cases <- auto.arima(Adams_cases, stepwise = FALSE, approximation = FALSE)
fit_Adams_cases # ARIMA(4, 1, 1), AICc - 936.18
autoplot(fit_Adams_cases)
sarima.for(Adams_cases_train, n.ahead = 20, 4, 1, 1)
lines(Adams_cases_test)
checkresiduals(fit_Adams_cases) # passes Ljung Box test
fit_Adams_deaths <- auto.arima(Adams_deaths, stepwise = FALSE, approximation = FALSE)
fit_Adams_deaths # ARIMA(2, 1, 3), AICc - 414.61
autoplot(fit_Adams_deaths)
sarima.for(Adams_deaths_train, n.ahead = 20, 2, 1, 3)
lines(Adams_deaths_test)
checkresiduals(fit_Adams_deaths) # passes
# Use best models to forecast further ahead
fc_10_Adams <- sarima.for(Adams_cases, n.ahead = 10, 4, 1, 1)
fc_10_Adams$pred
fcd_10_Adams <- sarima.for(Adams_deaths, n.ahead = 10, 2, 1, 3) 
fcd_10_Adams$pred

# Arapahoe
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsArapahoeCO)
Arapahoe_cases <- tsArapahoeCO[, "Cases"]
Arapahoe_cases_train <- Arapahoe_cases %>% window(end = c(2020, 120))
Arapahoe_cases_test <- Arapahoe_cases %>% window(end = c(2020, 130))
Arapahoe_deaths <- tsArapahoeCO[, "Deaths"]
Arapahoe_deaths_train <- Arapahoe_deaths %>% window(end = c(2020, 120))
Arapahoe_deaths_test <- Arapahoe_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Arapahoe_cases <- auto.arima(Arapahoe_cases, stepwise = FALSE, approximation = FALSE)
fit_Arapahoe_cases # ARIMA(5, 1, 0), AICc - 991.94
autoplot(fit_Arapahoe_cases)
sarima.for(Arapahoe_cases_train, n.ahead = 20, 5, 1, 0)
lines(Arapahoe_cases_test)
checkresiduals(fit_Arapahoe_cases) # passes
fit_Arapahoe_deaths <- auto.arima(Arapahoe_deaths, stepwise = FALSE, approximation = FALSE)
fit_Arapahoe_deaths # ARIMA(0, 1, 1), AICc - 554.39
autoplot(fit_Arapahoe_deaths)
sarima.for(Arapahoe_deaths_train, n.ahead = 20, 0, 1, 1)
lines(Arapahoe_deaths_test)
checkresiduals(fit_Arapahoe_deaths) # passes
# Use best models to forecast further ahead
fc_10_Arapahoe <- sarima.for(Arapahoe_cases, n.ahead = 10, 5, 1, 0)
fc_10_Arapahoe$pred
fcd_10_Arapahoe <- sarima.for(Arapahoe_deaths, n.ahead = 10, 0, 1, 1) 
fcd_10_Arapahoe$pred

# Boulder
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsBoulderCO)
Boulder_cases <- tsBoulderCO[, "Cases"]
Boulder_cases_train <- Boulder_cases %>% window(end = c(2020, 120))
Boulder_cases_test <- Boulder_cases %>% window(end = c(2020, 130))
Boulder_deaths <- tsBoulderCO[, "Deaths"]
Boulder_deaths_train <- Boulder_deaths %>% window(end = c(2020, 120))
Boulder_deaths_test <- Boulder_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Boulder_cases <- auto.arima(Boulder_cases, stepwise = FALSE, approximation = FALSE)
fit_Boulder_cases # ARIMA(0, 1, 5), AICc - 733.9
autoplot(fit_Boulder_cases)
sarima.for(Boulder_cases_train, n.ahead = 20, 0, 1, 5)
lines(Boulder_cases_test)
checkresiduals(fit_Boulder_cases) # passes
fit_Boulder_deaths <- auto.arima(Boulder_deaths, stepwise = FALSE, approximation = FALSE)
fit_Boulder_deaths # ARIMA(0, 1, 1), AICc - 299.91
autoplot(fit_Boulder_deaths)
sarima.for(Boulder_deaths_train, n.ahead = 20, 0, 1, 1)
lines(Boulder_deaths_test)
checkresiduals(fit_Boulder_deaths) # passes
# Use best models to forecast further ahead
fc_10_Boulder <- sarima.for(Boulder_cases, n.ahead = 10, 0, 1, 5)
fc_10_Boulder$pred
fcd_10_Boulder <- sarima.for(Boulder_deaths, n.ahead = 10, 0, 1, 1) 
fcd_10_Boulder$pred

# Denver
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsDenverCO)
Denver_cases <- tsDenverCO[, "Cases"]
Denver_cases_train <- Denver_cases %>% window(end = c(2020, 120))
Denver_cases_test <- Denver_cases %>% window(end = c(2020, 130))
Denver_deaths <- tsDenverCO[, "Deaths"]
Denver_deaths_train <- Denver_deaths %>% window(end = c(2020, 120))
Denver_deaths_test <- Denver_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Denver_cases <- auto.arima(Denver_cases, stepwise = FALSE, approximation = FALSE)
fit_Denver_cases # ARIMA(4, 1, 1), AICc - 1036.68
autoplot(fit_Denver_cases)
sarima.for(Denver_cases_train, n.ahead = 20, 4, 1, 1)
lines(Denver_cases_test)
checkresiduals(fit_Denver_cases) # passes
fit_Denver_deaths <- auto.arima(Denver_deaths, stepwise = FALSE, approximation = FALSE)
fit_Denver_deaths # ARIMA(1, 1, 3), AICc - 601.56
autoplot(fit_Denver_deaths)
sarima.for(Denver_deaths_train, n.ahead = 20, 1, 1, 3)
lines(Denver_deaths_test)
checkresiduals(fit_Denver_deaths) # does not pass
fit_Denver_deaths2 <- arima(Denver_deaths, order = c(10, 1, 3))
checkresiduals(fit_Denver_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Denver <- sarima.for(Denver_cases, n.ahead = 10, 4, 1, 1)
fc_10_Denver$pred
fcd_10_Denver <- sarima.for(Denver_deaths, n.ahead = 10, 10, 1, 3) 
fcd_10_Denver$pred

# Douglas
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsDouglasCO)
Douglas_cases <- tsDouglasCO[, "Cases"]
Douglas_cases_train <- Douglas_cases %>% window(end = c(2020, 120))
Douglas_cases_test <- Douglas_cases %>% window(end = c(2020, 130))
Douglas_deaths <- tsDouglasCO[, "Deaths"]
Douglas_deaths_train <- Douglas_deaths %>% window(end = c(2020, 120))
Douglas_deaths_test <- Douglas_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Douglas_cases <- auto.arima(Douglas_cases, stepwise = FALSE, approximation = FALSE)
fit_Douglas_cases # ARIMA(4, 1, 1), AICc - 689.07
autoplot(fit_Douglas_cases)
sarima.for(Douglas_cases_train, n.ahead = 20, 4, 1, 1)
lines(Douglas_cases_test)
checkresiduals(fit_Douglas_cases) # does not pass
fit_Douglas_cases2 <- arima(Douglas_cases, order = c(4, 1, 6))
checkresiduals(fit_Douglas_cases2) # passes
fit_Douglas_deaths <- auto.arima(Douglas_deaths, stepwise = FALSE, approximation = FALSE)
fit_Douglas_deaths # ARIMA(2, 1, 3), AICc - 201.78
autoplot(fit_Douglas_deaths)
sarima.for(Douglas_deaths_train, n.ahead = 20, 2, 1, 3)
lines(Douglas_deaths_test)
checkresiduals(fit_Douglas_deaths) # does not pass
fit_Douglas_deaths2 <- arima(Douglas_deaths, order = c(8, 1, 4))
checkresiduals(fit_Douglas_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Douglas <- sarima.for(Douglas_cases, n.ahead = 10, 4, 1, 6)
fc_10_Douglas$pred
fcd_10_Douglas <- sarima.for(Douglas_deaths, n.ahead = 10, 8, 1, 4) 
fcd_10_Douglas$pred

# El Paso
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsElPasoCO)
ElPaso_cases <- tsElPasoCO[, "Cases"]
ElPaso_cases_train <- ElPaso_cases %>% window(end = c(2020, 120))
ElPaso_cases_test <- ElPaso_cases %>% window(end = c(2020, 130))
ElPaso_deaths <- tsElPasoCO[, "Deaths"]
ElPaso_deaths_train <- ElPaso_deaths %>% window(end = c(2020, 120))
ElPaso_deaths_test <- ElPaso_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_ElPaso_cases <- auto.arima(ElPaso_cases, stepwise = FALSE, approximation = FALSE)
fit_ElPaso_cases # ARIMA(0, 1, 5), AICc - 848.24
autoplot(fit_ElPaso_cases)
sarima.for(ElPaso_cases_train, n.ahead = 20, 0, 1, 5)
lines(ElPaso_cases_test)
checkresiduals(fit_ElPaso_cases) # does not pass
fit_ElPaso_cases2 <- arima(ElPaso_cases, order = c(6, 1, 10))
checkresiduals(fit_ElPaso_cases2) # passes
fit_ElPaso_deaths <- auto.arima(ElPaso_deaths, stepwise = FALSE, approximation = FALSE)
fit_ElPaso_deaths # ARIMA(0, 1, 1), AICc - 457.28
autoplot(fit_ElPaso_deaths)
sarima.for(ElPaso_deaths_train, n.ahead = 20, 0, 1, 1)
lines(ElPaso_deaths_test)
checkresiduals(fit_ElPaso_deaths) # passes
# Use best models to forecast further ahead
fc_10_ElPaso <- sarima.for(ElPaso_cases, n.ahead = 10, 6, 1, 10)
fc_10_ElPaso$pred
fcd_10_ElPaso <- sarima.for(ElPaso_deaths, n.ahead = 10, 0, 1, 1) 
fcd_10_ElPaso$pred

# Jefferson
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsJeffersonCO)
Jefferson_cases <- tsJeffersonCO[, "Cases"]
Jefferson_cases_train <- Jefferson_cases %>% window(end = c(2020, 120))
Jefferson_cases_test <- Jefferson_cases %>% window(end = c(2020, 130))
Jefferson_deaths <- tsJeffersonCO[, "Deaths"]
Jefferson_deaths_train <- Jefferson_deaths %>% window(end = c(2020, 120))
Jefferson_deaths_test <- Jefferson_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Jefferson_cases <- auto.arima(Jefferson_cases, stepwise = FALSE, approximation = FALSE)
fit_Jefferson_cases # ARIMA(2, 1, 3), AICc - 916.82
autoplot(fit_Jefferson_cases)
sarima.for(Jefferson_cases_train, n.ahead = 20, 2, 1, 3)
lines(Jefferson_cases_test)
checkresiduals(fit_Jefferson_cases) # passes
fit_Jefferson_deaths <- auto.arima(Jefferson_deaths, stepwise = FALSE, approximation = FALSE)
fit_Jefferson_deaths # ARIMA(0, 1, 1), AICc - 402.45
autoplot(fit_Jefferson_deaths)
sarima.for(Jefferson_deaths_train, n.ahead = 20, 0, 1, 1)
lines(Jefferson_deaths_test)
checkresiduals(fit_Jefferson_deaths) # passes
# Use best models to forecast further ahead
fc_10_Jefferson <- sarima.for(Jefferson_cases, n.ahead = 10, 2, 1, 3)
fc_10_Jefferson$pred
fcd_10_Jefferson <- sarima.for(Jefferson_deaths, n.ahead = 10, 0, 1, 1) 
fcd_10_Jefferson$pred

# Weld
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsWeldCO)
Weld_cases <- tsWeldCO[, "Cases"]
Weld_cases_train <- Weld_cases %>% window(end = c(2020, 120))
Weld_cases_test <- Weld_cases %>% window(end = c(2020, 130))
Weld_deaths <- tsWeldCO[, "Deaths"]
Weld_deaths_train <- Weld_deaths %>% window(end = c(2020, 120))
Weld_deaths_test <- Weld_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Weld_cases <- auto.arima(Weld_cases, stepwise = FALSE, approximation = FALSE)
fit_Weld_cases # ARIMA(2, 1, 3), AICc - 934.35
autoplot(fit_Weld_cases)
sarima.for(Weld_cases_train, n.ahead = 20, 2, 1, 3)
lines(Weld_cases_test)
checkresiduals(fit_Weld_cases) # does not pass
fit_Weld_cases2 <- arima(Weld_cases, order = c(2, 1, 5))
checkresiduals(fit_Weld_cases2) # passes
fit_Weld_deaths <- auto.arima(Weld_deaths, stepwise = FALSE, approximation = FALSE)
fit_Weld_deaths # ARIMA(4, 1, 1), AICc - 447.12
autoplot(fit_Weld_deaths)
sarima.for(Weld_deaths_train, n.ahead = 20, 4, 1, 1)
lines(Weld_deaths_test)
checkresiduals(fit_Weld_deaths) # passes
# Use best models to forecast further ahead
fc_10_Weld <- sarima.for(Weld_cases, n.ahead = 10, 2, 1, 5)
fc_10_Weld$pred
fcd_10_Weld <- sarima.for(Weld_deaths, n.ahead = 10, 4, 1, 1) 
fcd_10_Weld$pred
