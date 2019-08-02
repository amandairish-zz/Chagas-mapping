library(tidycensus)
library(tidyverse)
library(tigris)
library(viridis)
library(sf)
library(tmap)
library(tmaptools)
library(rmapshaper)
options(tigris_use_cache = TRUE)

us <- unique(fips_codes$state)[1:51]

totalpop_sf <- reduce(
  map(us, function(x) {
    get_acs(geography = "county", 
            variables = "B05006_138", # Total Latin American pop 
            summary_var = "B01003_001", # Total population
            geometry = TRUE,
            state = x)
  }), 
  rbind
)

str(totalpop_sf)

totalpop_sf <- totalpop_sf %>%
  mutate(pct = 100 * (estimate / summary_est))

totalpop_sf

#http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/

data2 <- get_acs(geography = "county",
                 variables = "B05006_138",
                 summary_var = "B01003_001",
                 year = 2015,
                 output = "tidy",
                 state = NULL,
                 geometry = TRUE,
                 shift_geo = TRUE) %>% # shifts AK & HI
  rename(`la_2015` = estimate, `total_2015` = summary_est) %>%
  select(-moe, -summary_moe) %>%
  mutate(pct_2015 = 100 * (la_2015 / total_2015))


data3 <- get_acs(geography = "county",
                 variables = "B05006_138",
                 summary_var = "B01003_001",
                 year = 2010,
                 output = "tidy",
                 state = NULL,
                 geometry = TRUE,
                 shift_geo = TRUE) %>% # shifts AK & HI
  rename(`la_2010` = estimate, `total_2010` = summary_est) %>%
  select(-moe, -summary_moe) %>%
  mutate(pct_2010 = 100 * (la_2010 / total_2010))

dat.temp.15 <- data2 %>%
  as_data_frame(data2) %>%
  select(-geometry)

dat.temp.10 <- data3 %>%
  as_data_frame(data3) %>%
  select(-geometry)

dat <- left_join(dat.temp.15, dat.temp.10, by = c("GEOID", "NAME", "variable"))

dat2 <- dat %>%
  select(GEOID, NAME, pct_2015, pct_2010) %>%
  mutate(diff = pct_2015 - pct_2010) %>%
  rename(`2015` = pct_2015, `2010` = pct_2010)

dat.long <- select(dat2, -diff) %>%
  tidyr::gather(year, estimate, c(`2015`, `2010`)) %>%
  group_by(year) %>%
  mutate(med = round(median(estimate, na.rm = TRUE), 1))


ggplot(dat.long, aes(estimate)) +
  geom_histogram(fill = "firebrick2", 
                 color = "white", bins = 60) +
  xlab("Latin American foreign-born residents by county (% of total population)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~year, ncol = 1) +
  geom_vline(aes(xintercept = med, group = year), lty = "dashed") +
  geom_text(aes(label = paste("Median = ", med), x = med, y = 55)) +
  scale_x_log10()


# Counties with biggest changes in LA FB pop
d10 <- top_n(dat2, 10, diff) %>%
  mutate(type = "Latin American foreign-born population decreased",
         difftemp = diff)

i10 <- top_n(dat2, -10, diff) %>%
  mutate(type = "Latin American foreign-born population increased",
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
          Latin American foreign-born population, 2010-2015") +
  ylab("Difference in % Latin American foreign-born (2015 - 2010)") +
  xlab("")

# Mapping
# data2 is our original geographic object for 2015 and dat2 is the tabular data
shp <- data2 %>%
  select(GEOID, NAME, geometry) %>%
  left_join(dat2, by = c("GEOID", "NAME")) %>%
  arrange(GEOID) %>%
  rename(pct_2010 = `2010`,
         pct_2015 = `2015`)


# Remove the Aleutians West from shp for display purposes.
# NOTE: this isn't necessary since I'm using the shift_geo
# argument in the get_acs function. However if you're not
# using shift_geo or joining to a different spatial layer
# for the full US you may want to consider removing this 
# record for display purposes.
shp <- filter(shp, GEOID != "02016")


tm_shape(shp) +
  tm_bubbles("pct_2015")

var <- "pct_2015"
var2 <- "pct_2010"

cuts <- c(0, 0.2, 0.5, 1, 5, 10, 20, 30, 40)

tm_shape(shp, projection = 2163) +
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

tm_shape(shp, projection = 2163) +
  tm_polygons(var2,
              #style = "quantile",
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "% of total population") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Latin American foreign-born population by county, 2010",
            title.size = 1.5,
            title.position = c("center", "top"),
            inner.margins = c(0.10, 0.10, 0.10, 0.10))




##########
# Need to change name on legend and specify scale!
us.map.1 <- totalpop_sf %>%
  filter(!grepl('Alaska|Hawaii', NAME))  %>% # only want continental us
  ggplot(aes(fill = pct, color = pct)) + 
  geom_sf() + 
  coord_sf(crs = 26915) + 
  scale_fill_viridis(option = "viridis") + 
  scale_color_viridis(option = "viridis") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Percent Latin American foreign-born population by county in the 
       continental US") + 
  theme_minimal()

#us.map.1

ggsave("/Users/amandairish/Desktop/Chagas project/ACS data/usmap1.jpg", us.map.1)
