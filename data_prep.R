# Update County_dist_maxpop file to include state, max_pop_county, and max_pop_countyname

library(tidyverse)
getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

# data <- read.csv("County_dist_maxpop.csv", header = TRUE)
# class(data)
# data2 <- read.csv("uscities.csv", header = TRUE)
# class(data2)
# 
# arrange(data2, -population)
# results <- for (i in unique(data2$state_id)){
#   data2 %>% filter(state_id == i) %>%
#     filter(population == max(population)) %>%
#     return(state_id, county_fips, county_name)
# }
# 
# results
# 
# data3 <- read.csv("sf12010countydistancemiles.csv", header = TRUE)
# tail(data3)
# data3$concatenate <- paste0(data3$county1, data3$county2)
# tail(data3)
# 
# data4 <- read.csv("County_dist_urban.csv", header = TRUE)
# head(data4)
# data4 <- data4[, 1:6]
# head(data4)
# data4$concatenate <- data4$concatenate_BD
# head(data4)
# data4 <- data4[, -6]
# head(data4)
# dim(data4)
# data_full <- merge(data3[, c("concatenate", "mi_to_county")], data4, by.y, all.y = all)
# tail(data_full)
# summary(data_full)
# str(data_full)
# data_full$state <- as.character(data_full$state)
# data_full <- data_full %>% arrange(state)
# head(data_full)
# final_data <- data_full[, c(3, 4, 5, 6, 7, 2)]
# head(final_data)
# is.na(final_data)
# write.csv(final_data, "County_data_distance.csv", row.names = FALSE)

# map FIPS codes using maps package
# https://www.rdocumentation.org/packages/maps/versions/3.3.0/topics/county.fips
# https://stackoverflow.com/questions/33129917/shading-counties-using-fips-code-in-r-map
library(maps)
data("county.fips")
str(county.fips)
df_pop_county <- data.frame(region = county.fips$fips)
df_pop_county$value <- county.fips$fips
y <- df_pop_county$value
df_pop_county$color <- gray(y / max(y))
counties <- county.fips %>%
  left_join(df_pop_county, by = c("fips" = "region"))
map("county", fill = TRUE, col = counties$color)
# map("county", fill = TRUE, col= distance_data$mi_to_county)

distance_data <- read.csv("County_data_distance.csv", header = TRUE)
head(distance_data, 10)
summary(distance_data)
str(distance_data)
distance_data$dist_cat <- as.factor(as.character(distance_data$dist_cat))
head(distance_data)
str(distance_data)
distance_data$county_fips <- distance_data$county

# distance_data$mi_to_county <- as.factor(as.character(distance_data$mi_to_county))
map("county", fill = TRUE, col = distance_data$dist_cat)
map("county", fill = TRUE, col = distance_data$mi_to_county)

# map using urbnmapr
# https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
library(urbnmapr)
ggplot() +
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
head(states)

counties <- get_urbn_map("counties", sf = TRUE)
counties$county_fips <- as.integer(counties$county_fips)
head(counties)
str(counties)
spatial_data <- merge(counties, distance_data, by = "county_fips")
head(spatial_data)

# https://mgimond.github.io/ES218/Week12a.html
# https://mgimond.github.io/Spatial/mapping-data-in-r.html
library(sf)
ggplot(spatial_data) + geom_sf(aes(fill = mi_to_county))
ggplot(spatial_data) + geom_sf(aes(fill = dist_cat))
spatial_data_MN <- spatial_data %>% filter(state_abbv == "MN")
spatial_data_MN %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = mi_to_county),
          color = "#ffffff", size = 25) +
  coord_sf(datum = NA) +
  scale_fill_gradient() +
  labs(fill = "Miles to State's Population Center")
library(tmap)
tm_shape(spatial_data_MN) + 
  tm_polygons(col = "mi_to_county", palette = "YlGnBu") + 
  tm_legend(position = c("right", "center"))
spatial_data_MN
