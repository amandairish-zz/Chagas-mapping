# Chagas project
# IPUMS microdata
#
# Amanda Irish
# Last updated 2/2/19

library(ipumsr)
library(tidyverse)
library(htmltools)
library(shiny)
library(DT)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

setwd("/Users/amandairish/Desktop/Chagas project/ACS data")

# Load the microdata

ddi <- read_ipums_ddi("usa_00007.xml")
data <- read_ipums_micro(ddi)

ipums_view(data)

ipums_val_labels(data$COUNTYFIPS)
ipums_val_labels(data$PUMA)
ipums_val_labels(data$SEX) # 1 = male, 2 = female
ipums_val_labels(data$AGE)
ipums_val_labels(data$BPL)
ipums_val_labels(data$BPLD)

# Convert the labels to factors, drop labels from age & convert to numeric
data <- data %>%
  mutate(BPL_factor = as_factor(lbl_clean(BPL)),
         BPLD_factor = as_factor(lbl_clean(BPLD)),
         SEX_factor = as_factor(lbl_clean(SEX))) %>%
  mutate_at(vars(AGE), zap_labels) %>% # labels not needed for this dataset; just note that 0 = <1 yr
  mutate(AGE_cont = as.numeric(AGE))
  


# Load the geographic data (using Tigris package)
tx.puma.spatial <- pumas(state = "Texas") 
tx.puma.spatial.sf <- st_as_sf(tx.puma.spatial) %>% # convert to sf & clean up to prep for eventual merge
  rename(GEOID = GEOID10) %>%
  select(-ends_with("10"))
tx.puma.spatial.sf


# Create categorical age variable
data$age.cat <- cut(data$AGE_cont, c(0, 15, 25, 35, 45, 55, 65, Inf), right=FALSE)
table(data$age.cat)


# Create summary of data at PUMA level
puma.lafb.female.age <- data %>%
  filter(BPL_factor %in% c("Mexico", "Central America", "SOUTH AMERICA")) %>%
  group_by(PUMA, age.cat, SEX_factor) %>%
  summarize(total.count = n()) %>%
  filter(SEX_factor == "Female")
puma.lafb.female.age


# Merge microdata & boundary data

# Find out what geographic ID variables are called in each dataset
names(puma.lafb.female.age)
names(tx.puma.spatial.sf)

#ipumsi <- ipums_shape_inner_join(
#  puma.lafb.female.age, 
#  tx.puma.spatial.sf,
#  by = c("PUMA" = "GEOID")
#)

# note the above did not work. Try approach from tidycensus:
rename(puma.lafb.female.age, GEOID = PUMA) # rename to make spatial join easier

ipumsi <- geo_join(tx.puma.spatial.sf, puma.lafb.female.age, by = "GEOID")
head(tx.puma.merge)
