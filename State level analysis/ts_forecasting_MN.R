# Repeat for MN
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
casesMN <- tsMN[, "Cases"]
casesMN_train <- casesMN %>% window(end = c(2020, 120))
casesMN_test <- casesMN %>% window(end = c(2020, 130))
diff_casesMN <- diff(casesMN)
deathsMN <- tsMN[, "Deaths"]
deathsMN_train <- deathsMN %>% window(end = c(2020, 120))
deathsMN_test <- deathsMN %>% window(end = c(2020, 130))
diff_deathsMN <- diff(deathsMN)

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesMN <- auto.arima(casesMN, stepwise = FALSE, approximation = FALSE)
fit_casesMN # ARIMA(4, 2, 1), AICc - 1105.85
autoplot(fit_casesMN)
sarima.for(casesMN_train, n.ahead = 20, 4, 2, 1)
lines(casesMN_test)
checkresiduals(fit_casesMN) # p-value too low
fit_casesMN2 <- arima(casesMN, order = c(6, 2, 1))
checkresiduals(fit_casesMN2) # passes Ljung-Box test
coeftest(fit_casesMN2)
fit_deathsMN <- auto.arima(deathsMN, stepwise = FALSE, approximation = FALSE)
fit_deathsMN # ARIMA(1, 1, 2), AICc - 577.18
autoplot(fit_deathsMN)
sarima.for(deathsMN_train, n.ahead = 20, 1, 1, 2)
lines(deathsMN_test)
checkresiduals(fit_deathsMN)
fit_deathsMN2 <- arima(deathsMN, order = c(4, 1, 2))
checkresiduals(fit_deathsMN2) # passes Ljung-Box test
coeftest(fit_deathsMN2)

# Use best models to forecast further ahead
fc_10_MN <- sarima.for(casesMN, n.ahead = 30, 6, 2, 1)
fc_10_MN$pred
fcd_10_MN <- sarima.for(deathsMN, n.ahead = 30, 4, 1, 2) 
fcd_10_MN$pred

# both models are trending up but deaths are climbing steadily while cases show variation

# repeat process for multivariate time series analysis
# https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/multivariate-ts-analysis.html
# http://past.rinfinance.com/agenda/2013/talk/RueyTsay.pdf
# https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783552078/1/ch01lvl1sec08/multivariate-time-series-analysis
apply(tsMN, 2, adf.test)
tsMN
tsMN1 <- tsMN[1:120, ]
tsMN2 <- tsMN[121:130, ]
tsMN1_diff <- diffM(tsMN1)
apply(tsMN1_diff, 2, adf.test) # now stationary (p value < 0.05)
plot.ts(tsMN1_diff)
class(tsMN1_diff)
autoplot(ts(tsMN1_diff, start = c(2020, 23), frequency = 365)) +
  ggtitle("Time Series Plot of the stationary tsMN")
# lag order identification
VARselect(tsMN_diff, type = "none", lag.max = 10)
var.MN1 <- VAR(tsMN1_diff, lag.max = 10, ic = "AIC")
summary(var.MN1)
plot(var.MN1)
coef(var.MN1)
residuals(var.MN1)
fitted(var.MN1)
Phi(var.MN1)
serial.test(var.MN1)
causality(var.MN1, cause = c("Cases"))
causality(var.MN1, cause = c("Deaths"))
var.pred <- predict(var.MN1, n.ahead = 10)
plot(var.pred)
var.irf <- irf(var.MN1)
plot(var.irf)
par(mar = c(2.5, 2.5, 2.5, 2.5))

pred_casesMN <- var.pred$fcst[1]
pred_casesMN
x <- pred_casesMN$Cases[, 1]
pred_deathsMN <- var.pred$fcst[2]
pred_deathsMN
y <- pred_deathsMN$Deaths[, 1]
# inverting the difference (add last value from time series to x and y)
tsMN1
x <- cumsum(x) + 463 # last value for cases
y <- cumsum(y) + 18 # last value for deaths
par(mfrow = c(2, 1))
plot.ts(x)
plot.ts(y)
par(mfrow = c(1, 1))
z <- cbind(x, y)
z
colnames(z) <- c("Cases", "Deaths")
z <- ts(z, start = c(2020, 121), frequency = 365)
autoplot(z)
autoplot(z, color = "black") +
  autolayer(tsMN)
tail(tsMN)
# cointegrated VAR and VECM (Vector Error Correction Models)
cregr <- lm(x ~ y)
r = cregr$residuals
adf.test(r) # if r constitutes a stationary series, 2 series are cointegrated
checkresiduals(cregr) # higher p-value indicates stationary
po.coint <- po.test(z, demean = TRUE, lshort = TRUE)
po.coint # p-value higher than 0.05 means that we can't reject null hypothesis, so two series are not cointegrated
zJoTest <- ca.jo(z, type = c("trace"), ecdet = c("none"), K = 2)
zJoTest

# MARSS package
# https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf
# Multivariate Auto-Regressive (sames as dynamic linear models (DLMs) and vector autoregressives (VAR) state-space models)
# https://nwfsc-timeseries.github.io/atsa-labs/sec-msscov-model-diagnostics.html
dat <- data.frame(Yr = floor(time(tsMN) + .Machine$double.eps), Day = cycle(tsMN), tsMN)
dat
dat <- t(dat)
dat
covariates = t(tsMN[, c("Cases", "Deaths")])
# Observation-error only model
Q <- U <- x0 <- "zero"
B <- Z <- "identity"
d <- covariates
A <- "zero"
D <- "unconstrained"
y <- dat
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, D = D, d = d, x0 = x0)
marssfit <- MARSS(y, model = model.list)
# Process-error only model
R <- A <- U <- "zero"
Q <- "equalvarcov"
C <- "unconstrained"
model.list2 <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, C = C, c = covariates)
marssfit2 <- MARSS(y, model = model.list2)
# switch to better autoregressive model (mean reverting model)
model.list2$B <- "diagonal and unequal"
marssfit3 <- MARSS(y, model = model.list2) # higher log-likelihood means better model
# both process- and observation-error models
D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "diagonal and unequal"
Q <- "equalvarcov"
C <- "unconstrained"
c <- covariates
x0 <- "unequal"
tinitx <- 1
model.list4 <- list(B = B, U = U, Q = Q, Z = Z, A = A, D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
marssfit4 <- MARSS(y, model = model.list4) # log likelihood down
# both process and obs error but covariates only affect the observation process
C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "diagonal and unequal"
Q <- "equalvarcov"
D <- "unconstrained"
d <- covariates
x0 <- "unequal"
tinitx <- 1
model.list5 <- list(B = B, U = U, Q = Q, Z = Z, A = A, D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
marssfit5 <- MARSS(y, model = model.list5) # better model than 4 but worse than 3

