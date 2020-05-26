# http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/
# work through reference above to load in uninsured data from 2012 and 2016 census data
# census_api_key("f2b7871922a305e526f71c250dc5ca8bc3752b19", install = TRUE)
# readRenviron("~/.Renviron")
# Load health data
health12 <- get_acs("county", table = "B27001", year = 2012,
                    output = "tidy", state = NULL, geometry = FALSE) %>%
  rename("2012" = estimate) %>%
  select(-NAME, -moe)
health18 <- get_acs("county", table = "B27001", year = 2018,
                    output = "tidy", state = NULL, geometry = TRUE, shift_geo = TRUE) %>%
  rename("2018" = estimate) %>%
  select(-moe)
# County data
data("county_laea", package = "tidycensus")
# State data
data("state_laea", package = "tidycensus")
# Join data and drop geometry to leave a table
dat <- left_join(health18, health12, by = c("GEOID", "variable"))
st_geometry(dat) <- NULL
str(dat)
# assign health insurance categories (18-34 insured, 18-34 not insured)
dat <- mutate(dat, cat = case_when(variable %in% paste0("B27001_0", 
                                                        c("09", "12", "37", "40")) ~ "pop1834", 
                                   variable %in% paste0("B27001_0", 
                                                        c("11", "14", "39", "42")) ~ "pop1834ni")) %>% 
  filter(!is.na(cat))
# summarize the data by county-year-category
dat <- tidyr::gather(dat, year, estimate, c("2012", "2018"))
dat <- group_by(dat, GEOID, NAME, year, cat) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup() %>%
  tidyr::spread(cat, estimate)
head(dat)
# calculate the final estimates
dat <- mutate(dat, est = (pop1834ni/pop1834) * 100) %>%
  select(-c(pop1834, pop1834ni)) %>%
  tidyr::spread(year, est) 
dat <- mutate(dat, diff = dat$`2018` - dat$`2012`)
head(dat)
# visualizations - distributions by year
datlong <- select(dat, -diff) %>%
  tidyr::gather(year, estimate, c(`2012`, `2018`)) %>%
  group_by(year) %>%
  mutate(med = round(median(estimate, na.rm = TRUE), 1))
ggplot(datlong, aes(estimate)) +
  geom_histogram(fill = "firebrick2",
                 color = "white", bins = 60) +
  xlab("Uninsured adults ages 18-34 by county (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~year, ncol = 1) +
  geom_vline(aes(xintercept = med,
                 group = year), lty = "dashed") +
  geom_text(aes(label = paste("Median = ", med), x = med, y = 55))
# counties with greatest change (+/-) in % uninsured
d10 <- top_n(dat, 10, diff) %>%
  mutate(type = "Insured population decreased",
         difftemp = diff)
i10 <- top_n(dat, -10, diff) %>%
  mutate(type = "Insured population increased",
         difftemp = abs(diff))
id10 <- bind_rows(list(i10, d10)) %>%
  arrange(desc(difftemp))
ggplot(id10) +
  geom_col(aes(x = forcats::fct_reorder(NAME, difftemp),
               y = difftemp, fill = type)) +
  coord_flip() +
  scale_fill_manual(values = c("firebrick2", "cyan4")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Counties with the greatest change (+/-) in
          insured population, ages 18-34, 2012-2018") +
  ylab("Difference in % insured (2018 - 2012)") +
  xlab("")
# create geographic file for use with tmap
tail(health18)
shp <- health18 %>%
  filter(variable == "B27001_001") %>%
  select(GEOID, NAME) %>%
  left_join(dat, by = c("GEOID", "NAME")) %>%
  arrange(GEOID) %>%
  rename(uninsured_2012 = "2012",
         uninsured_2018 = "2018",
         uninsured_diff = diff)
head(shp)
shp <- filter(shp, GEOID != "02016") # remove Aleutians West for display purposes
# mapping
tm_shape(shp) +
  tm_polygons()
tm_shape(shp) + 
  tm_polygons("uninsured_2012")
tm_shape(shp) + 
  tm_bubbles("uninsured_2012")
tm_shape(shp) +
  tm_polygons("uninsured_2018")
var <- "uninsured_2018"
tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              style = "quantile",
              palette = "BuPu") +
  tm_legend(legend.position = c("left", "bottom")) # apply quantile style
cuts <- c(0, 10, 20, 30, 40, 100) # define your own color breaks
tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "BuPu",
              border.col = "white",
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))
tm_shape(shp, projection = 2163) + 
  tm_polygons(var,
              breaks = cuts,
              palette = "seq",
              border.col = "white",
              border.alpha = 0.5) + 
  tm_legend(legend.position = c("left", "bottom")) # change palette to "seq"
tm_shape(shp, projection = 2163) + 
  tm_polygons(var,
              breaks = cuts,
              palette = "-BuPu",
              border.col = "white",
              border.alpha = 0.5) + 
  tm_legend(legend.position = c("left", "bottom")) # reverse the color scheme
mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717") # assign custom colors
tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = mycols, 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))
mymap <- tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "Uninsured (%)") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Uninsured adults ages 18-34 by county, 2018",
            title.size = 1.1,
            title.position = c("center", "top")) # add titles
mymap
mymap + tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08)) # increase map margins
mymap + 
  tm_scale_bar() + 
  tm_compass() # add scalebar and north arrow (default)
# tm_shape(shp, projection = 2163, unit = "mi") # customize shp
mymap + tm_scale_bar(color.dark = "gray60",
                     position = c("right", "bottom")) + 
  tm_compass(type = "4star", size = 2.5, text.size = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "top"))
# add multiple maps to a page using tmap_arrange()
