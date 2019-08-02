# First crack at ACS census data

library(pacman)
library(tidycensus)
library(ggplot2) 
library(tidyverse)
library(viridis)
library(sf)
library(tigris)
library(ggthemes)
library(stringr)
library(dplyr)


# Census API key
acs_key <- Sys.getenv("CENSUS_API_KEY")

#Enter the variables and geographies below
census_title <- c("Median Household Income by County:\n
                  Coefficient of Variation")
census_var <- c("B19013_001E")
census_geog <- c("county")
census_state <- c("or")

acs_data <- get_acs(geography = census_geog, variables =
                      census_var, state = census_state, output = "wide")

# Make more readable column names
acs_data <- acs_data %>%
  rename(MHI_est = B19013_001E ,
         MHI_moe = B19013_001M)

# Calculate the SE, CV for future reference
acs_data <- acs_data %>%
  mutate(se = MHI_moe/1.645,
         cv = (se/MHI_est)*100)

# Plot Percentages with Derived MOE
acs_plot <- acs_data %>%
  ggplot(aes(x = MHI_est,
             y = reorder(NAME, MHI_est))) +
  geom_point(color = "black", size = 2) +
  geom_errorbarh(aes(xmin = MHI_est - MHI_moe,
                     xmax = MHI_est + MHI_moe )) +
  labs(title = paste(census_title),
       subtitle =
         paste0("Oregon 2011-2015 American Community Survey"),
       x = "Median Household Income") +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
plot(acs_plot)


# Mapping

options(tigris_class = "sf", tigris_use_cache = TRUE)
acs_key <- Sys.getenv("CENSUS_API_KEY")
mhi_tables <- c("B19013_001")

# Download tracts and county, get the tracts for PDX
# Metro counties and counties for the state
mhi_tract <- get_acs(geography = "tract",
                     variables = mhi_tables,
                     state = "OR",
                     geometry = TRUE)

p1 <- ggplot() +
  geom_sf(data = metro_tract, aes(fill = estimate)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar,
                     name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for PDX Metro\n at the census tract level",
       subtitle = "An R 'sf' Example") + theme_minimal()

  
