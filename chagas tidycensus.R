# Using tidycensus to access and map Chagas disease-relevant variables
# Amanda Irish
# Last updated 11/26/18

library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)

# Already installed census API key; Sys.getenv("CENSUS_API_KEY") or see email 
# for key if needed

v15 <- load_variables(2016, "acs5", cache = TRUE)
View(v15)

v11 <- load_variables(2010, "acs5", cache = TRUE)
View(v11)

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
             Other.sa = "B05006_159")

texas.fb.tract <- get_acs(geography = "tract", 
                     variables = fb.vars, 
                     state = "TX",
                     output = "wide",
                     summary_var = "B01003_001", # Total population
                     geometry = TRUE) 

texas.fb.tract

texas.fb.county <- get_acs(geography = "county", 
                          variables = fb.vars, 
                          state = "TX",
                          output = "wide",
                          summary_var = "B01003_001", # Total population
                          geometry = TRUE) 

texas.fb.county

ca.fb.county.long <- get_acs(geography = "county", 
                        variables = fb.vars, 
                        state = "CA",
                        summary_var = "B01003_001", # Total population
                        geometry = TRUE) %>%
  mutate(pct = 100 * (estimate / summary_est))
ca.fb.county.long

ca.fb.county.wide <- get_acs(geography = "county", 
                             variables = fb.vars, 
                             state = "CA",
                             summary_var = "B01003_001", # Total population
                             output = "wide",
                             geometry = TRUE)
ca.fb.county.wide


tx.fb.county.long <- get_acs(geography = "county", 
                             variables = fb.vars, 
                             state = "TX",
                             summary_var = "B01003_001", # Total population
                             geometry = TRUE) %>%
  mutate(pct = 100 * (estimate / summary_est))
tx.fb.county.long


us.long <- get_acs(geography = "us", 
                  variables = fb.vars, 
                  summary_var = "B01003_001" # Total population
                  ) %>%
  mutate(pct = 100 * (estimate / summary_est))

ca <- get_acs(geography = "state",
              variables = fb.vars,
              state = "CA")
ca


# Clean & organize tract data
texas.fb.tract <- texas.fb.tract %>%
  mutate(CountyFIPS = str_sub(GEOID, 1, 5)) # add CountyFIPS ID variable


library(viridis)

texas.fb.tract %>%
  ggplot(aes(fill = Total.laE, color = Total.laE)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

texas.fb.county %>%
  ggplot(aes(fill = MexicoE, color = MexicoE)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")


map1 <- ca.fb.county.long %>%
  filter(variable=="Total.la") %>%
  ggplot(aes(fill = pct, color = pct)) + 
  geom_sf() + 
  coord_sf(crs = 26915) + 
  scale_fill_viridis(option = "viridis") + 
  scale_color_viridis(option = "viridis")

map2 <- tx.fb.county.long %>%
  filter(variable=="Total.la") %>%
  ggplot(aes(fill = pct, color = pct)) + 
  geom_sf() + 
  coord_sf(crs = 26915) + 
  scale_fill_viridis(option = "viridis") + 
  scale_color_viridis(option = "viridis")

ggsave("/Users/amandairish/Desktop/Chagas project/ACS data/camap1.jpg", map1)
ggsave("/Users/amandairish/Desktop/Chagas project/ACS data/txmap1.jpg", map2)




# Number of Latin American FB residents in CA
ca.fb.county.wide %>%
  mutate(NAME = gsub(" County, California", "", NAME)) %>%
  ggplot(aes(x = Total.laE, y = reorder(NAME, Total.laE))) +
  geom_errorbarh(aes(xmin = Total.laE - Total.laM, xmax = Total.laE + Total.laM)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Number of Latin American foreign-born residents in California",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

# Number of Mexican FB resients in CA
ca.fb.county.wide %>%
  mutate(NAME = gsub(" County, California", "", NAME)) %>%
  slice(-19) %>% #-LA b/c such huge numbers
  ggplot(aes(x = MexicoE, y = reorder(NAME, MexicoE))) +
  geom_errorbarh(aes(xmin = MexicoE - MexicoM, xmax = MexicoE + MexicoM)) +
  geom_point(color = "red", size = 0.5) +
  scale_x_continuous(breaks = c(0, 100000, 200000, 300000, 400000),
                     labels = c("0", "100,000", "200,000", "300,000", "400,000")) +
  labs(title = "Number of Mexican foreign-born residents in California",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")
