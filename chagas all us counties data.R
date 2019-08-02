library(knitr)
library(kableExtra)
library(tidycensus)
library(tidyverse)
library(purrr)


fb.vars <- c(Total.la = "B05006_124", 
             Mexico = "B05006_139", 
             Belize = "B05006_140",
             Costa.Rica = "B05006_141",
             El.Salvador = "B05006_142",
             Guatemala = "B05006_143",
             Honduras = "B05006_144",
             Nicaragua = "B05006_145",
             Panama = "B05006_146",
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

fb.county <- get_acs(geography = "county", 
                     variables = fb.vars,
                     summary_var = "B01003_001", # Total population
                     output = "tidy",
                     year = 2016) %>% 
  mutate(pct = 100 * (estimate / summary_est))
fb.county


fb.county.wide <- get_acs(geography = "county", 
                          variables = fb.vars,
                          summary_var = "B01003_001", # Total population
                          output = "wide",
                          year = 2016) %>%
  mutate(Total.centralE = BelizeE + Costa.RicaE + El.SalvadorE + GuatemalaE + HondurasE +
           NicaraguaE + PanamaE,
         Total.south.namedE = ArgentinaE + BoliviaE + BrazilE + ChileE + ColombiaE + EcuadorE +
           GuyanaE + PeruE + UruguayE + VenezuelaE,
         Total.southE = ArgentinaE + BoliviaE + BrazilE + ChileE + ColombiaE + EcuadorE +
           GuyanaE + PeruE + UruguayE + VenezuelaE + Other.saE,
         pct.la = 100 * (Total.laE/summary_est),
         pct.ca = 100 * (Total.centralE/summary_est),
         pct.mex = 100 * (MexicoE/summary_est),
         pct.sa.named = 100 * (Total.south.namedE/summary_est),
         pct.sa = 100 * (Total.southE/summary_est))

fb.county.wide

write.csv(fb.county.wide, "/Users/amandairish/Desktop/Chagas project/County data tables/US-3-20-19.csv")
