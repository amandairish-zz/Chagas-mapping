# Census tract -> zip code estimates of Latin-American born women in TX
# ACS data - 2013-2017 5-year estimate
# Amanda Irish
# Last updated 5/22/19

library(sf)
library(tidyverse)
library(tidycensus)
library(acs)
library(stringr) # to pad fips codes
library(tigris)
library(magrittr)
library(viridis)
library(rgdal)
library(USAboundaries) # for easier access to zip code tab areas
library(raster)
options(tigris_use_cache = TRUE)


# Load data at census tract level

# Already installed census API key; Sys.getenv("CENSUS_API_KEY") or see email 
# for key if needed

v17 <- load_variables(2017, "acs5", cache = TRUE)
View(v17)

fb.vars <- c(Total.la = "B05006_124", 
             Female.la = "B05008_045")

texas.fb.tract <- get_acs(geography = "tract", 
                          variables = fb.vars, 
                          state = "TX",
                          output = "tidy",
                          summary_var = "B01003_001", # Total population
                          geometry = TRUE) %>%
  dplyr::select(-moe, -summary_moe) %>%
  mutate(pct = 100 * (estimate / summary_est))
texas.fb.tract


# Can possibly do with the zip to tract files from census rather than shapefile?
# ZCTA-level data
# Grab the ZCTA spatial data (tigris)
tx.zcta.spatial <- zctas(state = "Texas") # Note: this is actually entire US
# and not TX, b/c state option only avail. for 2000 & 2010. See if this works
# with just zctas() to simplify code & make clearer

tx.zcta.spatial.sf <- st_as_sf(tx.zcta.spatial) %>%
  rename(ZCTA5 = ZCTA5CE10) %>%
  select(-ends_with("10")) # just need ZCTA # for merge
tx.zcta.spatial.sf$ZCTA5 <- as.numeric(tx.zcta.spatial.sf$ZCTA5)
tx.zcta.spatial.sf

# Pair tract data with ZCTA data
# Load zip-to-tract relationship file
zip.to.tract <- read_csv("/Users/amandairish/Desktop/Chagas project/Shapefiles/zcta_tract_rel_10.csv") %>%
  filter(STATE==48) %>% # limit to TX
  select(ZCTA5, COUNTY, TRACT, GEOID)

zip.to.tract$GEOID <- as.character(zip.to.tract$GEOID)
zip.to.tract$ZCTA5 <- as.numeric(zip.to.tract$ZCTA5)
head(zip.to.tract)

# Merge Latin American FB data with relationship file
# Create dataset with just LA FB data for tract level
tract.la <- texas.fb.tract %>%
  filter(variable == "Total.la")
head(tract.la)

# Merge tract data with relationship file data, and get rid of unneeded columns
# to prepare for geo_join spatial merge with ZCTA tigris file
ztt.la <- left_join(tract.la, zip.to.tract) %>%
  select(NAME, variable, estimate, summary_est, pct, ZCTA5) %>%
  rename(tract_name = NAME)
ztt.la$ZCTA5 <- as.numeric(ztt.la$ZCTA5)
head(ztt.la)

# Extract just data from ztt.la so can join with ZCTA geometry
st_geometry(ztt.la) <- NULL
class(ztt.la)

# Geo_join doesn't work - have to do left join, which works since everything's
# tidyverse compatible. Left join allows just the TX ZCTAs to be selected
ztt.la.spatial <- left_join(ztt.la, tx.zcta.spatial.sf, by = "ZCTA5")
head(ztt.la.spatial)
summary(ztt.la.spatial$ZCTA5)

# Convert to sf object for mapping
ztt.la.spatial.sf <- st_as_sf(ztt.la.spatial)

# Map % Latin American at ZCTA level
pal5 <- colorNumeric(
  palette = "YlGnBu",
  domain = ztt.la.spatial.sf$pct
)

map7 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = ztt.la.spatial.sf,
              fillColor = ~pal5(pct),
              fillOpacity = 0.7,
              color = "#b2aeae",
              weight = 1,
              smoothFactor = 0,
              popup = ztt.la.spatial.sf$ZCTA5) %>%
  addLegend(position = "bottomright",
            pal = pal5,
            values = ztt.la.spatial.sf$pct,
            labFormat = labelFormat(suffix = "%"),
            title = "Percent Latin American foreign-born by ZCTA")

saveWidget(map7, '/Users/amandairish/Desktop/Chagas project/TXzctala.html', selfcontained = TRUE)


# Load shapefiles
zcta.shape <- readOGR("/Users/amandairish/Desktop/Chagas project/Shapefiles/cb_2016_us_zcta510_500k")



# Sample/overlay zip code to census tract