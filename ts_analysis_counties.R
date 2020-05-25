# https://www.otexts.org/fpp2
# DataCamp course on ARIMA with R
# counties = 18 in MN, 7 in CO, 4 in PA, 2 in MI, 2 in SD, 3 in NE, 4 in TX

getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

# load time series data
library(forecast)
library(fpp2)
library(tidyverse)
library(readxl) 

train <- read.csv("train2.csv", header = TRUE, stringsAsFactors = FALSE)
head(train)
# filter to MN counties
# Chippewa, Cottonwood, Dakota, Goodhue, Hennepin, Kandiyohi, Lac qui Parle, Mower, Murray, 
# Nobles, Otter Tail, Renville, Redwood, Rice, Rock, Stearns, Todd, Wantonwan
MN_counties <- train2 %>% filter(Province_State == "Minnesota") %>% filter(County == "Chippewa"
head(MN_counties)
tail(MN_counties)
