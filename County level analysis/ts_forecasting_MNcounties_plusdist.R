# https://stats.stackexchange.com/questions/99907/how-to-test-the-influence-of-external-factors-on-time-series
# USE SELECT COUNTY DISTANCE DATA FILE
# ADD POPULATION, TOTAL CASES, TOTAL DEATHS
# RUN ML ALGORITHM TO PREDICT (LINEAR REG, DECISION TREE, RANDOM FOREST)

getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

library(DAAG)
library(funModeling)
library(tidyverse)

train <- read.csv("train2.csv", header = TRUE, stringsAsFactors = FALSE)

train_countries <- train %>% 
  filter(Country_Region == c("US", "India", "Brazil", "China", "United Kingdom", "Turkey")) %>%
  filter(Province_State == (""))
train_states <- train %>% filter(Country_Region == "US") %>% 
  filter(Province_State == 
           c("Colorado", "Michigan", "Minnesota", "Pennsylvania", "South Dakota", "Texas")) %>%
  filter(County != "")

train_states %>% filter(Province_State == "South Dakota") %>% filter(County == "Shannon")

summary(train_states)
str(train_states)
write.csv(train_states, "train_states.csv", row.names = FALSE)

# forgot NE! 
Ne <- train %>% filter(Country_Region == "US") %>%
  filter(Province_State == "Nebraska") %>%
  filter(County != "")
write.csv(Ne, "NE counties.csv", row.names = FALSE)

# load in county level data
# (single value for pop, dist from highest pop county, dist category, total cases, total deaths)
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data/County Level')
dist <- read.csv("Select_county_data_distance.csv", header = TRUE, stringsAsFactors = FALSE)
head(dist)
cases_dist <- dist[, c(4, 7, 9, 10, 11)]
cases_dist
str(cases_dist)
summary(cases_dist)
cases_dist$meat_plant <- as.integer(cases_dist$meat_plant)
str(cases_dist)
plot_num(cases_dist)
cor(cases_dist[, 2:5])
# predict cases using linear regression
# https://www.statmethods.net/stats/regression.html
# http://r-statistics.co/Model-Selection-in-R.html
linearMod <- lm(cases ~ dist_from_maxpop + meat_plant + population, data = cases_dist)
summary(linearMod) # adj R-squared is quite low but all variables are significant
selectedMod <- step(linearMod)
summary(selectedMod) # all predictors are statistically significant, so original model is best
all_vifs <- car::vif(selectedMod) # variance inflation factor
all_vifs # very close to 1, meaning that the predictor is not related to other variables (not multicollinear)
coefficients(linearMod)
anova(linearMod)
vcov(linearMod)
par(mfrow = c(2, 2))
plot(linearMod)

# since all 3 variables appear to be significant (dist_from_maxpop, meat_plant, population),
# add those variables to ts data to attempt forecasting with xreg

# do the above for MN data to start
MNdata <- train %>% filter(Country_Region == "US") %>%
  filter(Province_State == "Minnesota") %>%
  filter(County != "")
head(MNdata)
write.csv(MNdata, "MN_data.csv", row.names = FALSE)
MNdata_full <- read.csv("MN_data_full.csv", header = TRUE, stringsAsFactors = FALSE)
head(MNdata_full)
MNdata_full$meat_plant <- as.factor(MNdata_full$meat_plant)
MNdata_full$meat_plant <- as.integer(MNdata_full$meat_plant)
head(MNdata_full)
# run auto arima with xreg on MN full data, start with Stearns
MNdata_Stearns <- MNdata_full %>% filter(County == "Stearns")
head(MNdata_Stearns)
# convert dataset to time series and plot
tsMNdata_Hen <- ts(MNdata_Hennepin[, c(4, 6, 7, 8, 9)], start = c(2020, 23), frequency = 365)
str(tsMNdata_Hen)
autoplot(tsMNdata_Hen) # issues with NA's introduced by coercion? Ask Kellen for suggestions...
 