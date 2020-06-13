# https://www.otexts.org/fpp2
# DataCamp course on ARIMA with R
# counties = 87 in MN, 11 in CO, 4 in PA, 2 in MI, 2 in SD, 3 in NE, 4 in TX

getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

# load time series data
library(forecast)
library(fpp2)
library(tidyverse)
library(readxl) 
train <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
# break out cases and deaths
train_cases <- train %>% filter(Target == "ConfirmedCases")
train_cases$Cases <- train_cases$TargetValue
train_deaths <- train %>% filter(Target == "Fatalities")
train_deaths$Deaths <- train_deaths$TargetValue
# merge back to one dataset
train2 <- merge(train_cases[, c(2, 3, 4, 5, 7, 10)], train_deaths[, c(2, 3, 4, 5, 7, 10)])
# convert date column to date format (for EDA)
train2$Date <- as.Date(train2$Date)
# save train2
write.csv(train2, "train2.csv", row.names = FALSE)

# filter to US totals only (remove state)
train_US <- train2 %>% filter(Country_Region == "US") %>% filter(Province_State == "")
head(train_US)
tail(train_US)
# convert dataset to time series and plot
tsUS <- ts(train_US[, 6:7], start = c(2020, 23), frequency = 365)
str(tsUS)
autoplot(tsUS)
# repeat for China
train_China <- train2 %>% filter(Country_Region == "China") %>% filter(Province_State == "")
tail(train_China)
tsChina <- ts(train_China[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsChina)
# repeat for Turkey
train_Turkey <- train2 %>% filter(Country_Region == "Turkey")
head(train_Turkey)
str(train_Turkey)
tsTurkey <- ts(train_Turkey[, 6:7], start = c(2020, 23), frequency = 365)
str(tsTurkey)
autoplot(tsTurkey)
# repeat for United Kingdom
train_UK <- train2 %>% filter(Country_Region == "United Kingdom") %>% filter(Province_State == "")
tail(train_UK)
tsUK <- ts(train_UK[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsUK)
# repeat for India
train_India <- train2 %>% filter(Country_Region == "India") %>% filter(Province_State == "")
tail(train_India)
tsIndia <- ts(train_India[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsIndia)
# repeat for Brazil
train_Brazil <- train2 %>% filter(Country_Region == "Brazil") %>% filter(Province_State == "")
tail(train_Brazil)
tsBrazil <- ts(train_Brazil[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsBrazil)
