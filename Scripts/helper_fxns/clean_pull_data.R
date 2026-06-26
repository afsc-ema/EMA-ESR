##############################
# Clean data for ESR indices##
# Author: Lia Domke         ##
# Date: 6-25-26             ##
##############################

# make sure the proper packages are install and loaded
pkgs <- c("tidyverse", "devtools", "here")

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    message(paste("Package '", pkg, "' is already installed and loaded.", sep = ""))
  }
}

# install EMAdownload if not installed
devtools::install_github("afsc-ema/EMAdownload", quiet = F, force = F, dependencies=TRUE)
library(EMAdownload)

# Yearly settings
## Need to include sand lance unid now because NBS has moved towards using that in catch tsn: 171671
# this is for testing within script, normally this will be input into the function
# tsns <- c(934083, 161979, 162035, 161980, 161976, 161977, 551209, 161975, 164711, 162041, 164708, 171672, 171671)
# this.year <- 2024
# region <- "NBS"

pull_clean_catch <- function(this.year, tsns, region) {
  df <- join_event_catch(gear = "CAN", start_year = 2003, end_year = this.year,
                         tsn = tsns, trawl_method = "S", catch0 = TRUE, survey_region = region)
  
  #' species to subset and combine
  #' all salmon are juveniles
  #' for saffron cod, herring, capelin, and sandlance use all LHS stages
  #' if you want squid - all squid species in db
  #' jellyfish includes: Aequorea sp., aurelia sp., chrysora, cyanea, sautrophora, and phacellephora
  
  data <- df %>%
    dplyr::select(-c(oceanographic_domain,
                     gear_in_time, gear_in_latitude, 
                     gear_in_longitude, gear_out_time, 
                     gear_out_latitude, gear_out_longitude
                     )) %>%
    filter(lhs_code != "I_M") %>% # remove all non-juvenile salmon species
    unite(combo, c(species_tsn, lhs_code), remove = F) %>%
    unite(name_lhs, c(common_name, lhs_code), remove = F) %>%
    filter(!(combo %in% c("934083_A1+", "934083_A2+", "934083_A1","934083_U", "934083_A", # remove all non a0 pollock
                          "164711_U", # this is individuals with unknown lhs for pacific cod (keep only a0)
                          "161977_U"))) %>% # this is an unknown lhs for coho salmon
    mutate(total_catch_weight_kg = total_catch_weight/1000) %>% # convert g to kg
    pivot_wider(id_cols = c(station_id, sample_year, 
                            cruise_id, haul_date, eq_time, 
                            eq_latitude, eq_longitude, effort, 
                            effort_units), 
                  names_from = "name_lhs", values_from = "total_catch_weight_kg") %>%
    arrange(station_id) %>% # now lets add up the values
    rowwise() %>%
    mutate(`Saffron Cod_All` = sum(`Saffron Cod_U`, `Saffron Cod_A1+`, `Saffron Cod_A0`),
           `Pacific Herring_All` = sum(`Pacific Herring_U`, `Pacific Herring_A0`, `Pacific Herring_A1+`),
           `Capelin_All` = sum(`Capelin_U`, `Capelin_A0`, `Capelin_A1+`),
           `Sand Lance_All`= sum(`Arctic Sand Lance_A1+`, `Arctic Sand Lance_A0`, `Sand lance, unident._U`, `Sand lance, unident._A0`),
           `Rainbow Smelt_All` = sum(`Rainbow Smelt_A1+`, `Rainbow Smelt_A0`, `Rainbow Smelt_U`),
           Forage = sum(`Saffron Cod_All`, `Pacific Herring_All`, `Capelin_All`, `Rainbow Smelt_All`,
                        `Chum Salmon_J`, `Coho Salmon_J`, `Chinook Salmon_J`, `Pink Salmon_J`, `Sockeye Salmon_J`,
                        `Pollock_A0`, `Pacific Cod_A0`),
           Lat = eq_latitude,
           Lon = eq_longitude,
           doy = yday(haul_date))
  return(data)
} # so in this output we could add in the appropriate field configs / obs models / families for the model settings and run

