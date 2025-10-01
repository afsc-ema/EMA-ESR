############################
# Tmp data input for VAST ##
# Author: Lia Domke       ##
# Date: 9-30-25           ##
############################

#' Need to do some cleaning and zero - catch estimation to get
#' an idea of whats going on for the ESR in case of shutdown
#' 
#' Start first with the age 0 pollock

# libraries
library(EMAdownload)
library(tidyverse)

# read data
eventyr <- read.csv("../EMA-QAQC/Data/2025_tmp_NBSevent.csv") %>% dplyr::select(-X)
paramyr <- read.csv("../EMA-QAQC/Data/2025_tmp_NBSevent_parameters.csv") %>% dplyr::select(-X)
catchyr <- read.csv("../EMA-QAQC/Data/2025_tmp_NBScatch.csv") %>% dplyr::select(-X)

# input parameters (i.e. species)
tsn <- c(934083, 161979, 162035, 161980, 161976, 161977, 551209, 161975, 164711, 162041, 164708, 171672)
#lhs <- c("A0")
tax <- get_ema_taxonomy()

## check to make sure those are the species we want
subset(tax, species_tsn %in% c(tsn))

# clean this years age 0 pollock 
# Create unique list of species and lhs
catchyr.wsp <- subset(catchyr, species_tsn %in% c(tsn)) %>%
  #subset(., lhs_code %in% c(lhs)) %>%
  dplyr::select(c(station_id, event_code, gear, species_tsn, 
                  lhs_code, sampling_method_code, total_catch_number,
                  total_catch_weight, sub_sample_weight, sub_sample_number)) %>% # shouldn't have to do this everytime fixed the QAQC export to just spit out the columns required for access upload. 
  left_join(dplyr::select(tax, c(species_tsn, common_name, scientific_name)), 
            by = "species_tsn") %>%
  unite(combo, c(species_tsn, lhs_code), remove = F) %>%
  filter(lhs_code != "I_M") %>% # remove all non-juvenile salmon species
  filter(!(combo %in% c("934083_A1+", "934083_A2+", "934083_A1","934083_U", "934083_A", # remove all non a0 pollock
                        "164711_U", # this is individuals with unknown lhs for pacific cod (keep only a0)
                        "161977_U")))

# create df of unique species names catches and lhs (feels unnecessary given one species and one lhs but will be helpful if we ever want to include multiple species): 
catch_unique <- unique(catchyr.wsp[c("species_tsn", "common_name", "scientific_name", "lhs_code")]) %>% arrange(common_name)

# joins together all gears all stations - but we filtered to include only events with CAN
event.paramyr <- eventyr %>%
  filter(gear == "CAN") %>%
  left_join(paramyr, by = c("station_id", "event_code", "gear")) 

# make sure we only have good gear performance because we dont have the auto filter in the ema donwload pkg here
unique(event.paramyr$gear_performance)

# expand grid to get zero catches - so this contains no catch infor right now just all stations and all tsn and all lhs
zero_grid <- tidyr::expand_grid(subset(event.paramyr, 
                                       select=c("station_id", "event_code", "gear")), 
                                catch_unique)
# add in the event parameter information to the catch zero grid - require to use effort
zero_join <- 
  left_join(zero_grid, 
            subset(event.paramyr, select = c("station_id", "event_code", "gear",
                                             "sample_year", "cruise_id", "haul_date",
                                             "tow_type", "gear_performance", "eq_time",
                                             "haulback_time", "eq_latitude", "eq_longitude",
                                             "haulback_latitude","haulback_longitude", 
                                             "nbs_strata", "oceanographic_domain", "effort",
                                             "effort_units", "master_station_name")), 
            by = c("station_id", "event_code", "gear"))

# create final dataframe for 2025 that has the species and lhs event params, events and zero catches
dat.yr <- left_join(zero_join, catchyr.wsp, by = c("station_id", "event_code", "gear", "species_tsn", "common_name",
                                                   "scientific_name", "lhs_code")) %>%
  mutate(total_catch_number = ifelse(is.na(total_catch_number),0,total_catch_number),
         total_catch_weight = ifelse(is.na(total_catch_weight),0,total_catch_weight),
         cpue_num= total_catch_number/effort,
         cpue_weight = total_catch_weight/effort) 


ts.25 <- dat.yr %>%
  unite(name_lhs, c(common_name, lhs_code), remove = F) %>%
  #subset(cruise_id %in% c(cruise_ids)) %>%
  mutate(total_catch_weight_kg = total_catch_weight/1000,
         cpue_kgkm2 = total_catch_weight_kg/effort,
         #haul_date = format(eq_time, "%m/%d/%Y"),
         cpue_weight_kg = cpue_weight/1000) %>%
  pivot_wider(., id_cols = c(station_id, sample_year, cruise_id, haul_date, eq_time, eq_latitude,
                             eq_longitude, effort, effort_units), 
              names_from = "name_lhs", values_from = "total_catch_weight_kg") %>%
  arrange(station_id) %>% # now lets add up the values
  rowwise() %>%
  mutate(`Saffron Cod_All` = sum(`Saffron Cod_A1+`, `Saffron Cod_A0`),
         `Pacific Herring_All` = sum(`Pacific Herring_A0`, `Pacific Herring_A1+`),
         `Capelin_All` = sum(`Capelin_U`),
         #`Arctic Sand Lance_All`= sum(`Arctic Sand Lance_A1+`, `Arctic Sand Lance_A0`), # theres no sand lance in the forage
         `Rainbow Smelt_All` = sum(`Rainbow Smelt_U`),
         Forage = sum(`Saffron Cod_All`, `Pacific Herring_All`, `Capelin_All`, `Rainbow Smelt_All`,
                      `Chum Salmon_J`, `Coho Salmon_J`, `Chinook Salmon_J`, `Pink Salmon_J`, `Sockeye Salmon_J`,
                      `Pollock_A0`, `Pacific Cod_A0`))

nrow(dat.yr)
nrow(table(dat.yr$station_id, dat.yr$combo))
length(unique(eventyr$station_id))

head(ts.25)

#write.csv(ts.25, "tmp_data/2025_tmp_allsp_ts.csv")
