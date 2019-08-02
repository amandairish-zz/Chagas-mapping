# Using tidycensus to access and map Chagas disease-relevant variables in TX
# Amanda Irish
# Last updated 1/14/19

library(sf)
library(tidyverse)
library(tidycensus)
library(acs)
library(stringr) # to pad fips codes
library(tigris)
library(magrittr)
library(viridis)
options(tigris_use_cache = TRUE)

# Already installed census API key; Sys.getenv("CENSUS_API_KEY") or see email 
# for key if needed

v16 <- load_variables(2016, "acs5", cache = TRUE)
View(v16)

fb.vars <- c(Total.fb = "B05006_001", 
             Total.la = "B05006_124", 
             Total.ca = "B05006_138", 
             Mexico = "B05006_139", 
             Belize = "B05006_140",
             Costa.Rica = "B05006_141",
             El.Salvador = "B05006_142",
             Guatemala = "B05006_143",
             Honduras = "B05006_144",
             Nicaragua = "B05006_145",
             Panama = "B05006_146",
             Other.ca = "B05006_147",
             Total.sa = "B05006_148", 
             Argentina = "B05006_149",
             Bolivia = "B05006_150",
             Brazil = "B05006_151",
             Chile = "B05006_152",
             Colombia = "B05006_153",
             Ecuador = "B05006_154",
             Guyana = "B05006_155",
             Peru = "B05006_156",
             Uruguay = "B05006_157",
             Venezuela = "B05006_158",
             Other.sa = "B05006_159",
             Male.la = "B05008_011",
             Male.ca = "B05008_019",
             Male.Mexico = "B05008_020",
             Male.other.ca = "B05008_024",
             Male.sa = "B05008_028",
             Female.la = "B05008_045",
             Female.ca = "B05008_053",
             Female.Mexico = "B05008_054",
             Female.other.ca = "B05008_058",
             Female.sa = "B05008_062")

tx.fb.county <- get_acs(geography = "county", 
                        variables = fb.vars, 
                        state = "TX",
                        output = "tidy",
                        summary_var = "B01003_001", # Total population
                        geometry = TRUE) %>% 
  select(-moe, -summary_moe) %>%
  mutate(pct = 100 * (estimate / summary_est))
tx.fb.county

texas.fb.tract <- get_acs(geography = "tract", 
                          variables = fb.vars, 
                          state = "TX",
                          output = "tidy",
                          summary_var = "B01003_001", # Total population
                          geometry = TRUE) %>%
  select(-moe, -summary_moe) %>%
  mutate(pct = 100 * (estimate / summary_est))
texas.fb.tract

# For PUMA level, need to pull geographic data separately in tigirs package
# and join
tx.puma <- get_acs(geography = "public use microdata area", 
                           variables = fb.vars, 
                           state = "TX",
                           output = "tidy",
                           summary_var = "B01003_001")  %>% # Total population
  select(-moe, -summary_moe) %>%
  mutate(pct = 100 * (estimate / summary_est))
tx.puma

# Grab the PUMA spatial data (tigris)
tx.puma.spatial <- pumas(state = "Texas") 
tx.puma.spatial.sf <- st_as_sf(tx.puma.spatial) %>%
  rename(GEOID = GEOID10) %>%
  select(-ends_with("10"))
tx.puma.spatial.sf

# Merge the PUMA tabular & spatial data - NOTE: geo_join only works to join one-
# to-one, so can't do a one-many join (e.g. can't match one GEOID from spatial
# data frame to the many of the same GEOIDs (e.g. for the same PUMA) that are there
# because of the multiple variables. Basically, geo_join just picks the first
# instance of the GEOID in the data frame and removes the rest. So, if using
# geo_join need to select variable from df first
puma.la.df <- tx.puma %>%
  filter(variable == "Total.la")

tx.puma.merge.la <- geo_join(tx.puma.spatial.sf, puma.la.df, by = "GEOID")
head(tx.puma.merge.la)


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

# can't pull data at this level
#texas.fb.zcta <- get_acs(geography = "zcta", 
#                         variables = fb.vars, 
#                         state = "TX",
#                         output = "wide",
#                         summary_var = "B01003_001", # Total population
#                         geometry = TRUE) 


# Maps

map1 <- tx.fb.county %>%
  filter(variable=="Total.la") %>%
  ggplot(aes(fill = pct, color = pct)) + 
  geom_sf() + 
  coord_sf(crs = 26915) + 
  scale_fill_viridis(option = "viridis") + 
  scale_color_viridis(option = "viridis") +
  ggtitle("Percent Latin American foreign-born by county", 
          subtitle = "2012-2016 ACS data")
map1

map2 <- tx.fb.county %>%
  filter(variable=="Mexico") %>%
  ggplot(aes(fill = pct, color = pct)) + 
  geom_sf() + 
  coord_sf(crs = 26915) + 
  scale_fill_viridis(option = "viridis") + 
  scale_color_viridis(option = "viridis") +
  ggtitle("Percent Mexican foreign-born by county", 
          subtitle = "2012-2016 ACS data")
map2

# Note should use total female as denominator here instead of total pop
map6 <- tx.fb.county %>%
  filter(variable=="Female.la") %>%
  ggplot(aes(fill = pct, color = pct)) + 
  geom_sf() + 
  coord_sf(crs = 26915) + 
  scale_fill_viridis(option = "viridis") + 
  scale_color_viridis(option = "viridis") +
  ggtitle("Percent female Latin American foreign-born by county", 
          subtitle = "2012-2016 ACS data")
map6

# Need to plot tract-level w/ leaflet so can zoom
#map3 <- texas.fb.tract %>%
#  filter(variable=="Total.la") %>%
#  ggplot(aes(fill = pct, color = pct)) + 
#  geom_sf() + 
#  coord_sf(crs = 26915) + 
#  scale_fill_viridis(option = "viridis") + 
#  scale_color_viridis(option = "viridis") +
#  ggtitle("Percent Latin American foreign-born by census tract", 
#          subtitle = "2012-2016 ACS data")


library(leaflet)

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = tract.la$pct
)

map3 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = tract.la,
              fillColor = ~pal(pct),
              color = "#b2aeae",
              fillOpacity = 0.7,
              weight = 1,
              smoothFactor = 0,
              popup = tract.la$NAME) %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = tract.la$pct,
            labFormat = labelFormat(suffix = "%"),
            title = "Percent Latin American foreign-born by census tract")

library(htmlwidgets)
saveWidget(map3, '/Users/amandairish/Desktop/Chagas project/TXtractla.html', selfcontained = TRUE)

# Need to plot with leaflet as noted above
# First manipulate data as needed

tract.mex <- texas.fb.tract %>%
  filter(variable == "Mexico")
head(tract.mex)

# want to also find the percent LA but not Mexican
# tried manipulating data in tidyverse but just pulling a wide dataset is probably easier
tract.wide <- get_acs(geography = "tract", 
                          variables = fb.vars, 
                          state = "TX",
                          output = "wide",
                          summary_var = "B01003_001", # Total population
                          geometry = TRUE) %>%
  select(-ends_with("M"), -summary_moe) # get rid of margin of error columns for now
head(tract.wide)

# Create df with total # and percentage (out of total population) of LA not-Mexican born
tract.not.mex <- tract.wide %>%
  mutate(not.mex = Total.laE - MexicoE,
         pct.not.mex = 100*(not.mex / summary_est))
head(tract.not.mex)

# Palette for not Mexican map  
pal2 <- colorNumeric(
  palette = "YlGnBu",
  domain = tract.not.mex$pct.not.mex
)

# Palette for Mexican map
pal3 <- colorNumeric(
  palette = "YlGnBu",
  domain = tract.mex$pct
)

# Map % not Mexican
map4 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = tract.not.mex,
              fillColor = ~pal2(pct.not.mex),
              fillOpacity = 0.7,
              color = "#b2aeae",
              weight = 1,
              smoothFactor = 0,
              popup = tract.not.mex$NAME) %>%
  addLegend(position = "bottomright",
            pal = pal2,
            values = tract.not.mex$pct.not.mex,
            labFormat = labelFormat(suffix = "%"),
            title = "Percent Latin American foreign-born, excluding Mexican-born, by census tract")

saveWidget(map4, '/Users/amandairish/Desktop/Chagas project/TXtractnotmex.html', selfcontained = TRUE)


# Map % Mexican
map5 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = tract.mex,
              fillColor = ~pal3(pct),
              fillOpacity = 0.7,
              color = "#b2aeae",
              weight = 1,
              smoothFactor = 0,
              popup = tract.mex$NAME) %>%
  addLegend(position = "bottomright",
            pal = pal3,
            values = tract.mex$pct,
            labFormat = labelFormat(suffix = "%"),
            title = "Percent Mexican foreign-born by census tract")

saveWidget(map5, '/Users/amandairish/Desktop/Chagas project/TXtractmex.html', selfcontained = TRUE)


# Map % Latin American at PUMA level
pal4 <- colorNumeric(
  palette = "YlGnBu",
  domain = tx.puma.merge.la$pct
)

map6 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = tx.puma.merge.la,
              fillColor = ~pal4(pct),
              fillOpacity = 0.7,
              color = "#b2aeae",
              weight = 1,
              smoothFactor = 0,
              popup = tx.puma.merge.la$NAME) %>%
  addLegend(position = "bottomright",
            pal = pal4,
            values = tx.puma.merge.la$pct,
            labFormat = labelFormat(suffix = "%"),
            title = "Percent Latin American foreign-born by PUMA")

saveWidget(map6, '/Users/amandairish/Desktop/Chagas project/TXpumala.html', selfcontained = TRUE)


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




ggsave("/Users/amandairish/Desktop/Chagas project/ACS data/txmapla.jpg", map1)
ggsave("/Users/amandairish/Desktop/Chagas project/ACS data/txmapmex.jpg", map2)
ggsave("/Users/amandairish/Desktop/Chagas project/ACS data/txmaplafem.jpg", map6)





tm_shape(texas.fb.puma, projection = 2163) +
  tm_polygons(var,
              #style = "quantile",
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "% of total population") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Latin American foreign-born population by county, 2015",
            title.size = 1.5,
            title.position = c("center", "top"),
            inner.margins = c(0.10, 0.10, 0.10, 0.10))
