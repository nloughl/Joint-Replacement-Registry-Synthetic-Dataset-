# Load libraries
library(tidyverse)
library(tidyr)
library(sf)
library(ggthemes)
library(leaflet) 
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterpie)

# Read in datasets 
dataset1 <- read.csv("dataset1.csv")
dataset2 <- read.csv("dataset2.csv")

# Join datasets 
merged <- dataset2 %>%
  rename(De.identified.Joint.ID = De.identified.primary.Joint.ID) %>%
  right_join(dataset1, by = "De.identified.Joint.ID")

# Trends in brands used by region 
canada_sf <- ne_states(country = "Canada", returnclass = "sf") # Load Canadian provinces (admin-1 level regions)
unique(canada_sf$name_en)

canada_centroids <- st_centroid(canada_sf)
canada_centroids <- cbind(canada_sf["name_en"], st_coordinates(canada_centroids))

province_lookup <- tibble::tibble(
  abbr = c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"),
  name_en = c("Alberta", "British Columbia", "Manitoba", "New Brunswick",
              "Newfoundland and Labrador", "Nova Scotia", "Northwest Territories", "Nunavut",
              "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon")
)

brand_region <- merged %>%
  count(Province.Territory, Manufacturer) %>%
  group_by(Province.Territory) %>%
  mutate(pct = n / sum(n)) %>%
  pivot_wider(names_from = Manufacturer, values_from = pct, values_fill = 0) 

# Add full names to provinces
brand_region <- brand_region %>%
  left_join(province_lookup, by = c("Province.Territory" = "abbr"))

# Summarize to one row per province, sum proportions
brand_wide <- brand_region %>%
  group_by(name_en) %>%
  summarise(across(c(`DePuy`, `Smith & Nephew`, `Stryker`, `Zimmer`), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup()

pie_data <- left_join(canada_centroids, brand_region, by = "name_en")

manufacturer_cols <- c("DePuy", "Smith & Nephew", "Stryker", "Zimmer")

pie_df <- pie_data %>%
  st_drop_geometry() %>%
  select(X, Y, all_of(manufacturer_cols)) %>%
  mutate(across(all_of(manufacturer_cols), ~ as.numeric(.))) %>%
  replace_na(as.list(setNames(rep(0, length(manufacturer_cols)), manufacturer_cols)))

ggplot() +
  geom_sf(data = canada_sf, fill = "gray95", color = "white") +
  geom_scatterpie(
    aes(x = X, y = Y),
    data = pie_df,
    cols = manufacturer_cols,
    pie_scale = 1
  ) +
  coord_sf() +
  labs(title = "Manufacturer Usage by Province") +
  theme_minimal()


# Brands/models by sex 
merged %>%
  count(Patient.Sex, Manufacturer) %>%
  group_by(Patient.Sex) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = Manufacturer, y = pct, fill = Patient.Sex)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = "Brand Share by Sex", y = "Percentage", x = "Manufacturer")

# TKA Counts over time (by sex)
merged %>%
  filter(Primary.procedure.type == "Total Knee Arthroplasty") %>%
  count(Surgery.Year, Patient.Sex) %>%
  ggplot(aes(x = Surgery.Year, y = n, color = Patient.Sex)) +
  geom_line() +
  labs(title = "Annual TKA Volume by Sex", y = "Count of Procedures", x = "Year") +
  scale_x_continuous(breaks = unique(merged$Surgery.Year)) +
  theme_minimal()

# Cemented vs uncemented implants over time (by sex)
## Using Cement Flag
merged %>%
  count(Surgery.Year, Patient.Sex, Cement.Flag..CJRR.) %>%
  ggplot(aes(x = Surgery.Year, y = n, color = Cement.Flag..CJRR.)) +
  geom_line() +
  facet_wrap(~ Patient.Sex) +
  labs(title = "Cemented vs Uncemented Implants, by Sex", 
       x = "Year", y = "Count") +
  theme_minimal()

### Chi sq test 
chisq_test <- merged %>%
  filter(Cement.Flag..CJRR. %in% c("Yes", "No")) %>%
  with(table(Patient.Sex, Cement.Flag..CJRR.)) %>%
  chisq.test()
chisq_test

## Using Fixation method 
merged %>%
  count(Surgery.Year, Patient.Sex, Fixation) %>%
  ggplot(aes(x = Surgery.Year, y = n, color = Fixation)) +
  geom_line() +
  facet_wrap(~ Patient.Sex) +
  labs(title = "Cemented vs Uncemented Implants, by Sex", 
       x = "Year", y = "Count") +
  theme_minimal()

### Chi sq test 
chisq_test <- merged %>%
  filter(Fixation %in% c("Cemented", "Cementless")) %>%
  with(table(Patient.Sex, Fixation)) %>%
  chisq.test()
chisq_test


# Teaching vs non-teaching hospitals 
teach_reg <- merged %>%
  filter(Primary.procedure.type == "Total Knee Arthroplasty") %>%
  count(Province.Territory, Teaching.Hospital.Flag)

prov_loc <- canada_sf %>% left_join(teach_reg, by = c("postal" = "Province.Territory"))

# Convert MULTIPOLYGON to POINT (centroids)
prov_points <- prov_loc %>%
  st_centroid() %>%
  cbind(st_coordinates(.))  # adds X, Y columns for leaflet

# Use leaflet with X/Y as lng/lat
leaflet(prov_points) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X,
    lat = ~Y,
    radius = ~sqrt(n) * 2,
    color = ~ifelse(Teaching.Hospital.Flag == "Yes", "blue", "red"),
    stroke = FALSE,
    fillOpacity = 0.6,
    label = ~paste(postal, Teaching.Hospital.Flag, n)
  ) %>%
  addLegend(colors = c("blue", "red"), labels = c("Teaching", "Non-teaching"))


# Age at time of surgury over time (and by sex)
merged %>%
  group_by(Surgery.Year, Patient.Sex) %>%
  summarise(age_mean = mean(Patient.Age, na.rm = TRUE),
            age_sd = sd(Patient.Age, na.rm = TRUE),
            N = n(), .groups = "drop") %>%
  ggplot(aes(x = Surgery.Year, y = age_mean, color = Patient.Sex)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = age_mean - age_sd, ymax = age_mean + age_sd, fill = Patient.Sex), alpha = 0.2) +
  labs(
    title = "Mean Age at TKA, by Sex and Year",
    y = "Mean Age (years)", x = "Year"
  ) +
  theme_minimal()

## Regression test 
lm_fit <- lm(Patient.Age ~ Surgery.Year * Patient.Sex, data = merged)
summary(lm_fit)
