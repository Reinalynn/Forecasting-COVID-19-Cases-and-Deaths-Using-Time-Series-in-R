getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

library(dplyr)
library(funModeling)
library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tmaptools)
library(urbnmapr)

# load file containing county distances
dist1 <- read.csv("County Level/County_data_distance.csv", header = TRUE)
head(dist1, 10)
# perform basic EDA
summary(dist1)
str(dist1)
# convert distance category to factor
dist1$dist_cat <- as.factor(as.character(dist1$dist_cat))
# create new fips column for later merge with geo data
dist1$county_fips <- dist1$county
# add total cases/deaths by county and map
total_cases <- read.csv("County Level/total_cases.csv", header = TRUE)
str(total_cases)
data <- left_join(dist1, total_cases, by = "county_fips")
data <- data[, c(2, 3, 4, 5, 6, 7, 8, 9, 12)]
head(data)
total_deaths <- read.csv("County Level/total_deaths.csv", header = TRUE)
str(total_deaths)
data <- left_join(data, total_deaths, by = "county_fips")
data <- data[, 3:12]
data <- data[, c("county_fips", "countyname", "state", "max_pop_county", "max_pop_countyname", 
                 "mi_to_county", "dist_cat", "meat_plant", "total_cases", "total_deaths")]
head(data)
# EDA on data
summary(data)
freq(data)
plot_num(data)
hist(data$mi_to_county)
plot(data$dist_cat) # categories  = 1 (0mi), 2 (< 25), 3 (< 75), 4 (< 150), 5 (< 300), 6 (300 +)
plot(data$total_cases)
plot(data$total_deaths)
ggplot(data, aes(x = total_cases, y = total_deaths, color = meat_plant)) +
  geom_point(aes(size = dist_cat))
#load file with selected counties and repeat process
dist2 <- read.csv("County Level/Select_county_data_distance.csv", header = TRUE)
dist2$dist_cat <- as.factor(as.character(dist2$dist_cat))
dist2$county_fips <- dist2$county
data2 <- left_join(dist2, total_cases, by = "county_fips")
data2 <- left_join(data2, total_deaths, by = "county_fips")
data2 <- data2[, c(4, 5, 6, 7, 8, 9, 12, 13, 14, 15)]
data2 <- data2[, c("county_fips", "countyname", "state", "max_pop_county", "max_pop_countyname", 
                 "mi_to_county", "dist_cat", "meat_plant", "total_cases", "total_deaths")]
# EDA on data2
summary(data2)
freq(data2)
plot_num(data2)
hist(data2$mi_to_county)
plot(data2$dist_cat) # categories  = 1 (0mi), 2 (< 25), 3 (< 75), 4 (< 150), 5 (< 300), 6 (300 +)
plot(data2$total_cases)
plot(data2$total_deaths)
ggplot(data2, aes(x = total_cases, y = total_deaths, color = meat_plant)) +
  geom_point(aes(size = dist_cat))
# map using urbnmapr
# https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
head(states)
counties <- get_urbn_map("counties", sf = TRUE)
counties$county_fips <- as.integer(counties$county_fips)
head(counties)
str(counties)
spatial_data <- merge(counties, data, by = "county_fips")
head(spatial_data)
spatial_data2 <- merge(counties, dist2, by = "county_fips")
# https://mgimond.github.io/ES218/Week12a.html
# https://mgimond.github.io/Spatial/mapping-data-in-r.html
ggplot(spatial_data) + geom_sf(aes(fill = mi_to_county))
ggplot(spatial_data) + geom_sf(aes(fill = dist_cat))
ggplot(spatial_data) + geom_sf(aes(fill = total_cases))
ggplot(spatial_data) + geom_sf(aes(fill = total_deaths))
spatial_data_MN <- spatial_data %>% filter(state_abbv == "MN")
ggplot(spatial_data_MN) + geom_sf(aes(fill = mi_to_county))
ggplot(spatial_data_MN) + geom_sf(aes(fill = dist_cat))
ggplot(spatial_data_MN) + geom_sf(aes(fill = total_cases))
ggplot(spatial_data_MN) + geom_sf(aes(fill = total_deaths))

# use tmap package
# http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/
# https://cran.r-project.org/web/packages/tmap/tmap.pdf
cuts <- c(0, 25, 75, 150, 300, 1400)
palette_explorer()
# Distance maps of US counties
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "mi_to_county", breaks = cuts, palette = "-BuPu") +
  tm_layout(title = "Distance to State Population Center",
            title.size = 1.1,
            title.position = c("center", "top"))
plants <- spatial_data %>% filter(meat_plant == "Y") # add counties w/meat plants
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "mi_to_county", breaks = cuts, palette = "-BuPu") +
  tm_shape(plants, projections = 2163) +
  tm_polygons(col = "meat_plant", palette = "Oranges") +
  tm_layout(title = "Dist to State Pop Center, Counties with Meat Plants",
            title.size = 1.1,
            title.position = c("center", "top"))
# COVID maps of US counties
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "total_cases", style = "quantile", palette = "OrRd") +
  tm_layout(title = "Total COVID cases by County",
            title.size = 1.1,
            title.position = c("center", "top"))
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "total_deaths", style = "log10_pretty", palette = "OrRd") +
  tm_layout(title = "Total COVID deaths by County",
            title.size = 1.1,
            title.position = c("center", "top"))
# Distance maps of MN counties
MNmap <- tm_shape(spatial_data_MN, projection = 2163) + 
  tm_polygons(col = "mi_to_county", style = "quantile", palette = "-YlGnBu") + 
  tm_legend(position = c("right", "center")) +
  tm_layout(title = "Minnesota Counties,\nDist to Twin Cities",
            title.size = 1.1,
            title.position = c("center", "top"))
MNmap
MNmap + tm_shape(plants, projection = 2163) +
  tm_polygons(col = "meat_plant", palette = "Oranges")
# COVID maps of MN counties
MNmapCOVID <- tm_shape(spatial_data_MN, projection = 2163) + 
  tm_polygons(c("total_cases", "total_deaths"), style = c("quantile", "pretty"), 
              palette = list("YlGnBu", "YlOrRd")) + 
  tm_legend(position = c("right", "center")) +
  tm_layout(title = " MN Counties, COVID Data",
            title.size = 1.1,
            title.position = c("center", "top"))
MNmapCOVID