library(rgdal)
library(sf)
library(USAboundaries) # for easier access to zip code tab areas
library(tidyverse)
library(raster)

# Load shapefiles
zcta.shape <- readOGR("/Users/amandairish/Desktop/Chagas project/Shapefiles/cb_2016_us_zcta510_500k")
puma.shape <- readOGR("/Users/amandairish/Desktop/Chagas project/Shapefiles/cb_2016_48_puma10_500k")

# Load relationship files
zip.to.tract <- read_csv("/Users/amandairish/Desktop/Chagas project/Shapefiles/zcta_tract_rel_10.csv")
tract.to.puma <- read_csv("/Users/amandairish/Desktop/Chagas project/Shapefiles/2010_Census_Tract_to_2010_PUMA.csv")

# Limit relationship files to texas
tx.ztt <- filter(zip.to.tract, STATE==48)
tx.ttp <- filter(tract.to.puma, STATEFP==48)

# Create vectors of PUMA and ZCTAs to cross-check later
tx.zcta.list <- tx.ztt$ZCTA5
tx.puma.list <- tx.ttp$PUMA5CE

# Rename columns in tx.ttp to be consistent with those in tx.ztt
tx.ttp <- tx.ttp %>%
  rename(
    STATE = STATEFP, 
    COUNTY = COUNTYFP, 
    TRACT = TRACTCE,
    PUMA = PUMA5CE
  )

# Merge the tx datasets & select columns to keep
tx.ztt$STATE <- as.character(tx.ztt$STATE)
tx.ztp <- left_join(tx.ttp, tx.ztt) %>%
  dplyr::select(STATE, COUNTY, TRACT, PUMA, ZCTA5, GEOID, POPPT, ZPOP) %>%
  rename(ZCTA = ZCTA5)

# Rename columns in shapefile datasets to be consistent with tx.ztp &
# remove GEOID columns since they are not universal (e.g. the GEOID for a PUMA 
# is not the same as the GEOID for a tract or a zip code)
puma.shape@data <- puma.shape@data %>%
  rename(
    STATE = STATEFP10,
    PUMA = PUMACE10
  ) %>%
  dplyr::select(
    -GEOID10,
    -AFFGEOID10
  )

zcta.shape@data <- zcta.shape@data %>%
  rename(ZCTA = ZCTA5CE10) %>%
  dplyr::select(
    -GEOID10,
    -AFFGEOID10
  )

# Merge tx.ztp data into puma and zip shapefiles
#puma.shape@data <- left_join(tx.ztp, puma.shape@data)
#zcta.shape@data <- left_join(tx.ztp, zcta.shape@data)

# Subset ZCTA to only include texas
tx.zcta <- subset(zcta.shape, zcta.shape@data$ZCTA %in% tx.zcta.list)

library(leaflet)

tx.shape.map <- leaflet() %>% 
  addProviderTiles(providers$Esri) %>% 
  addPolygons(data = tx.zcta,
              weight = 2,
              color = "red",
              fillOpacity=0) %>%
  addPolygons(data = puma.shape,
              weight = 2,
              color = "blue",
              fillOpacity = 0) %>%
  addLegend(position = "bottomright",
            colors = c("red", "blue"),  
            labels = c("Zip code tabulation area", "Public use microdata area"),
            title = "TX ZCTAs and PUMAs")

library(htmlwidgets)
saveWidget(tx.shape.map, 'TXshapemap.html', selfcontained = TRUE)

# need to check and make sure that the merges above (commented out) do not affect the 
# number of rows in puma/zcta SPDFs
