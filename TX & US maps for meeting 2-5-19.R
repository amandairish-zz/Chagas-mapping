library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(rmapshaper)
options(tigris_use_cache = TRUE)

# Census API key (if not already entered - Sys.getenv("CENSUS_API_KEY") or see email 
# for key if needed)


# US MAP

# Get ACS data for % Latin American foreign-born by county for 2012-2016
data.16 <- get_acs(geography = "county",
                 variables = "B05006_138",   # Total Latin American foreign-born population
                 summary_var = "B01003_001", # Total population
                 year = 2016,
                 output = "tidy",
                 state = NULL,
                 geometry = TRUE,
                 shift_geo = TRUE) %>% # shifts AK & HI
  rename(`la_2016` = estimate, `total_2016` = summary_est) %>%
  select(-moe, -summary_moe) %>%
  mutate(pct_2016 = 100 * (la_2016 / total_2016))

data.16
summary(data.16$pct_2016)


# Map ACS data for % Latin American foreign-born by county

var <- "pct_2016"

cuts <- c(0, 0.2, 0.5, 1, 5, 15, 25, 35)

us.map.county.16 <- tm_shape(data.16, projection = 2163) +
  tm_polygons(var,
              #style = "pretty",
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "% of total population") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Latin American foreign-born population by county, 2012-2016",
            title.size = 1.5,
            title.position = c("center", "top"),
            inner.margins = c(0.10, 0.10, 0.10, 0.10))


# Add state outlines

# Add state FIPS codes to dataset
data.16 <- mutate(data.16, STFIPS = stringr::str_sub(GEOID, 1, 2)) 

# Aggregate the county layer by state
states <- data.16 %>%
  aggregate_map(by = "STFIPS")

# Add the new state layer
us.map.county.16 + tm_shape(states) +
  tm_borders(col = "grey")



# TEXAS MAPS

# COUNTY

# Get ACS data for % Latin American foreign-born by county for 2012-2016
tx.county.16 <- get_acs(geography = "county",
                   variables = "B05006_138",   # Total Latin American foreign-born population
                   summary_var = "B01003_001", # Total population
                   year = 2016,
                   output = "tidy",
                   state = "TX",
                   geometry = TRUE) %>%
  rename(`la_2016` = estimate, `total_2016` = summary_est) %>%
  select(-moe, -summary_moe) %>%
  mutate(pct_2016 = 100 * (la_2016 / total_2016))

tx.county.16
summary(tx.county.16$pct_2016)


# Map ACS data for % Latin American foreign-born by county
#cuts <- c(0, 0.2, 0.5, 1, 5, 15, 25, 35)

tx.county.16.map <- tm_shape(tx.county.16, projection = 2163) +
  tm_polygons(var,
              style = "quantile",
              #breaks = cuts,
              palette = "YlGnBu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "% of total population") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Latin American foreign-born population by county, 2012-2016",
            title.size = 1.5,
            title.position = c("center", "top"),
            inner.margins = c(0.10, 0.10, 0.10, 0.10))
tx.county.16.map


# TRACT

# Get ACS data for % Latin American foreign-born by census tract for 2012-2016
tx.tract.16 <- get_acs(geography = "tract",
                 variables = "B05006_138",   # Total Latin American foreign-born population
                 summary_var = "B01003_001", # Total population
                 year = 2016,
                 output = "tidy",
                 state = "TX",
                 geometry = TRUE) %>%
  rename(`la_2016` = estimate, `total_2016` = summary_est) %>%
  select(-moe, -summary_moe) %>%
  mutate(pct_2016 = 100 * (la_2016 / total_2016))

tx.tract.16
summary(tx.tract.16$pct_2016)


# Map ACS data for % Latin American foreign-born by county
#cuts <- c(0, 0.2, 0.5, 1, 5, 15, 25, 35)

tx.tract.16.map <- tm_shape(tx.tract.16, projection = 2163) +
  tm_polygons(var,
              style = "quantile",
              #breaks = cuts,
              palette = "RdPu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "% of total population") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Latin American foreign-born population by census tract, 2012-2016",
            title.size = 1.5,
            title.position = c("center", "top"),
            inner.margins = c(0.10, 0.10, 0.10, 0.10))
tx.tract.16.map


# PUMA

# For PUMA level, need to pull geographic data separately in tigirs package
# and join
tx.puma.16 <- get_acs(geography = "public use microdata area", 
                   variables = "B05006_138",   # Total Latin American foreign-born population
                   summary_var = "B01003_001", # Total population
                   year = 2016,
                   output = "tidy",
                   state = "TX")  %>%
  rename(`la_2016` = estimate, `total_2016` = summary_est) %>%
  select(-moe, -summary_moe) %>%
  mutate(pct_2016 = 100 * (la_2016 / total_2016))
tx.puma.16

# Grab the PUMA spatial data (tigris)
tx.puma.spatial <- pumas(state = "Texas") 
tx.puma.spatial.sf <- st_as_sf(tx.puma.spatial) %>%
  rename(GEOID = GEOID10) %>%
  select(-ends_with("10"))
tx.puma.spatial.sf

# Merge the spatial & summary data
tx.puma.merge <- geo_join(tx.puma.spatial.sf, tx.puma.16, by = "GEOID")
head(tx.puma.merge)


# Map ACS data for % Latin American foreign-born by PUMA
#cuts <- c(0, 0.2, 0.5, 1, 5, 15, 25, 35)

tx.puma.16.map <- tm_shape(tx.puma.merge, projection = 2163) +
  tm_polygons(var,
              style = "quantile",
              #breaks = cuts,
              palette = "GnBu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "% of total population") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Latin American foreign-born population by PUMA, 2012-2016",
            title.size = 1.5,
            title.position = c("center", "top"),
            inner.margins = c(0.10, 0.10, 0.10, 0.10))
tx.puma.16.map


