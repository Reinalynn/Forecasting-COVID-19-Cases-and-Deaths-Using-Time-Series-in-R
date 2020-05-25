
getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

# load time series data
library(forecast)
library(fpp2)
library(tidyverse)
library(readxl) 

train <- read.csv("train2.csv", header = TRUE, stringsAsFactors = FALSE)
head(train)
# filter to US states only (remove county)
train_states <- train2 %>% filter(Country_Region == "US") %>% filter(County == "")
head(train_states)
tail(train_states)
# filter to MN
train_MN <- train_states %>% filter(Province_State == "Minnesota")
head(train_MN)
tail(train_MN)
# convert dataset to time series and plot
tsMN <- ts(train_MN[, 6:7], start = c(2020, 23), frequency = 365)
str(tsMN)
autoplot(tsMN)
# repeat for CO
train_CO <- train_states %>% filter(Province_State == "Colorado") %>% filter(County == "")
tail(train_CO)
tsCO <- ts(train_CO[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsCO)
# repeat for MI
train_MI <- train_states %>% filter(Province_State == "Michigan") %>% filter(County == "")
tail(train_MI)
tsMI <- ts(train_MI[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsMI)
# repeat for NE
train_NE <- train_states %>% filter(Province_State == "Nebraska") %>% filter(County == "")
tail(train_NE)
tsNE <- ts(train_NE[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsNE)
# repeat for PA
train_PA <- train_states %>% filter(Province_State == "Pennsylvania") %>% filter(County == "")
tail(train_PA)
tsPA <- ts(train_PA[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsPA)
# repeat for TX
train_TX <- train_states %>% filter(Province_State == "Texas") %>% filter(County == "")
tail(train_TX)
tsTX <- ts(train_TX[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsTX)
