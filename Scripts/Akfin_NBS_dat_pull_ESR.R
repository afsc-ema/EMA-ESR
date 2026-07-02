library(EMAdownload)
library(tidyverse)
library(VAST)
library(geosphere)
Version = get_latest_version( package="VAST" )
wd<-setwd("C:/Users/lia.domke/Desktop/Work/AFSC/R projects/EMA-ESR")
# pull in custom NBS user region
user_region <- readRDS("Region files/NBS_user_region.rds")

all.tax <- get_ema_taxonomy()
# QUESTIONS should we include the sand lance unident in this? tsn: 171671 *I think no b/c of catchability issues*
tsn <- c(934083, 161979, 162035, 161980, 161976, 161977, 551209, 161975, 164711, 162041, 164708, 171672)
this.year <- 2025
# so all data 2002 and before either had less than great data (1996 - 2001); 2002 used a smaller trawl so 
# people don't tend to use that data. 


# check to make sure those are the species we want
subset(all.tax, species_tsn %in% c(tsn))

# best to not continuously re run this line of code cause it throws strange errors
# instead you should remove the previous "catch" data in the global environment and re run if you want to
# make sure if you re run do rm(catch) first
df <- join_event_catch(gear = "CAN", start_year = 2003, end_year = this.year,
                       tsn = tsn, trawl_method = "S", catch0 = TRUE) 
# run it once and then adjust output

# we want just the NBS surveys (cruise_ids listed above)
# all salmon should just be juveniles
# For saffron code, herring, capelin, sandlance - group all LHS stages 
# if you include squid - its all squid species in database
# if you include jellyfish - its Aequorea sp, aurelia sp chrysora, cyanea, staurophora, and phacellephora
# include only region between 59.9 and 65.5
nbs.ts <- df %>%
  dplyr::select(-c(akfin_load_date.x, akfin_load_date.y, oceanographic_domain, bsierp_region,
                   gear_in_time, gear_in_latitude, gear_in_longitude, gear_out_time, gear_out_latitude,
                   gear_out_longitude, race_code, nodc_code)) %>%
  # filter the lat/lon
  filter(eq_latitude > 59.9,
         eq_latitude < 65.5) %>%
  unite(combo, c(species_tsn, lhs_code), remove = F) %>%
  filter(lhs_code != "I_M") %>% # remove all non-juvenile salmon species
  filter(!(combo %in% c("934083_A1+", "934083_A2+", "934083_A1","934083_U", "934083_A", # remove all non a0 pollock
                        "164711_U", # this is individuals with unknown lhs for pacific cod (keep only a0)
                        "161977_U"))) %>% # this is an unknown lhs for coho salmon
  unite(name_lhs, c(common_name, lhs_code), remove = F) %>%
  #subset(cruise_id %in% c(cruise_ids)) %>%
  mutate(total_catch_weight_kg = total_catch_weight/1000,
         cpue_kgkm2 = total_catch_weight_kg/effort,
         haul_date = format(eq_time, "%m/%d/%Y"),
         cpue_weight_kg = cpue_weight/1000) %>%
  pivot_wider(., id_cols = c(station_id, sample_year, cruise_id, haul_date, eq_time, eq_latitude,
                             eq_longitude, effort, effort_units), 
              names_from = "name_lhs", values_from = "total_catch_weight_kg") %>%
  arrange(station_id) %>% # now lets add up the values
  rowwise() %>%
  mutate(`Saffron Cod_All` = sum(`Saffron Cod_U`, `Saffron Cod_A1+`, `Saffron Cod_A0`),
         `Pacific Herring_All` = sum(`Pacific Herring_U`, `Pacific Herring_A0`, `Pacific Herring_A1+`),
         `Capelin_All` = sum(`Capelin_U`, `Capelin_A0`, `Capelin_A1+`),
         `Arctic Sand Lance_All`= sum(`Arctic Sand Lance_A1+`, `Arctic Sand Lance_A0`),
         `Rainbow Smelt_All` = sum(`Rainbow Smelt_A1+`, `Rainbow Smelt_A0`, `Rainbow Smelt_U`),
         Forage = sum(`Saffron Cod_All`, `Pacific Herring_All`, `Capelin_All`, `Rainbow Smelt_All`,
                      `Chum Salmon_J`, `Coho Salmon_J`, `Chinook Salmon_J`, `Pink Salmon_J`, `Sockeye Salmon_J`,
                      `Pollock_A0`, `Pacific Cod_A0`))

# the output is kg of catch (not effort corrected) for each species
# spot check 
head(nbs.ts)