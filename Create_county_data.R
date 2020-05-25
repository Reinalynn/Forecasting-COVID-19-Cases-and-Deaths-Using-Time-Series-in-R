# Update County_dist_maxpop file to include state, max_pop_county, and max_pop_countyname
# code used to create "County_data_distance.csv" file necessary for EDA

library(tidyverse)
getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

data <- read.csv("County_dist_maxpop.csv", header = TRUE)
class(data)
data2 <- read.csv("uscities.csv", header = TRUE)
class(data2)

arrange(data2, -population)
results <- for (i in unique(data2$state_id)){
  data2 %>% filter(state_id == i) %>%
    filter(population == max(population)) %>%
    return(state_id, county_fips, county_name)
}

# results
data3 <- read.csv("sf12010countydistancemiles.csv", header = TRUE)
tail(data3)
data3$concatenate <- paste0(data3$county1, data3$county2)
tail(data3)

data4 <- read.csv("County_dist_urban.csv", header = TRUE)
head(data4)
data4 <- data4[, 1:6]
head(data4)
data4$concatenate <- data4$concatenate_BD
head(data4)
data4 <- data4[, -6]
head(data4)
dim(data4)
data_full <- merge(data3[, c("concatenate", "mi_to_county")], data4, by.y, all.y = all)
tail(data_full)
summary(data_full)
str(data_full)
data_full$state <- as.character(data_full$state)
data_full <- data_full %>% arrange(state)
head(data_full)
final_data <- data_full[, c(3, 4, 5, 6, 7, 2)]
head(final_data)
is.na(final_data)
write.csv(final_data, "County_data_distance.csv", row.names = FALSE)

# combine county data with train2 dataset
