##' offline join event catch
##' 
##' lia  domke
##' 9-24-26
##' 

# testing params
# start_year <- 2026
# end_year <- 2026
# survey_region = "NBS"
# tsn = NA
# lhs = NA
# ema.event <- read.csv("../EMA-QAQC/Data/2026/NBS/2026_NBSevent_2026-09-24.csv")
# ema.params <- read.csv("../EMA-QAQC/Data/2026/NBS/2026_NBSevent_parameters_2026-09-24.csv")
# ema.catch <- read.csv("../EMA-QAQC/Data/2026/NBS/2026_NBScatch_2026-09-24.csv")
# gear <- "CAN"; trawl_method = "S"; catch0 = FALSE; force_download = FALSE

join_event_catch_offline <- function(start_year=2003, end_year=3000, survey_region=NA, tsn=NA,lhs=NA,
                                     ema.event = NA, ema.catch = NA, ema.params = NA,
                             gear= c("CAN"), trawl_method="S", catch0=FALSE, force_download = FALSE) {
  
  # other inputs needed for testing
  # gear <- c("CAN", "NETS156", "Nor264"); trawl_method <- NA; start_year <- 2003;
  # end_year <- 3000; tsn <- NA; lhs <- NA; catch0 = FALSE; trawl_method <- "S"; survey_region <- NA
  # tsn <- 161975; catch0 <- TRUE
  
  if(start_year != lubridate::year(Sys.Date())) {
    stop("start year needs to be the same as this year")
  }
  
  if(end_year != lubridate::year(Sys.Date())) {
    stop("start year needs to be the same as this year")
  }
  
  # offline tables
  evnt <- ema.event |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      #gear = ifelse(gear == "NOR64", "Nor64", gear), # fix gear typo in db
      ###This code adds a "region" field.  Note that this region only effectively works for trawls since CTD/CAT stations store lat in a different field.
      # There's one NETS trawl from 2016 but it's aborted so I don't care about it.
      #large_marine_ecosystem = ifelse(large_marine_ecosystem == "Chuckchi", "Chukchi", large_marine_ecosystem),
      region = dplyr::case_when(eq_latitude <= 59.9 & !(large_marine_ecosystem == "GOA") ~ "SEBS",
                                eq_latitude > 59.9 & eq_latitude <= 65.5 & !(large_marine_ecosystem == "GOA") ~ "NBS",
                                eq_latitude > 65.5 ~ "Chukchi",
                                large_marine_ecosystem=="GOA" ~ "GOA")) |>
    dplyr::mutate(region = ifelse(is.na(region),
                                  dplyr::case_when(gear_in_latitude <= 59.9 & !(large_marine_ecosystem == "GOA") ~ "SEBS",
                                                   gear_in_latitude > 59.9 & gear_in_latitude <= 65.5 & !(large_marine_ecosystem == "GOA") ~ "NBS",
                                                   gear_in_latitude > 65.5 ~ "Chukchi",
                                                   large_marine_ecosystem == "GOA" ~ "GOA"), region)) |>
    dplyr::mutate(haul_date = lubridate::mdy(haul_date),
                  gear_in_time = lubridate::mdy_hms(gear_in_time),
                  haulback_time = lubridate::mdy_hms(haulback_time),
                  gear_out_time = lubridate::mdy_hms(gear_out_time),
                  eq_time = lubridate::mdy_hms(eq_time)
                  #haul_date = as.Date(haul_date),
                  ) |>
    ## Filter out aborted and unsatisfactory tows.
    dplyr::filter (!gear_performance %in% c("A","U")) |>
    dplyr::rename(event_notes = notes)
  
  cth <- ema.catch |>
    dplyr::rename(catch_notes = notes)
  
  taxa <- EMAdownload::get_ema_taxonomy(force_download) |>
    dplyr::rename(taxa_notes = notes)
  
  event_parameters <- ema.params
  
  
  # gear filter - only allow gears present in cth table
  if(all(gear %in% unique(cth$gear))){
    gear_vec <- c(gear)
  } else {
    stop("Gear type must be CAN, MAR, NETS156, Nor264")
  }
  
  # option trawl method/tow type
  if(all(trawl_method %in% unique(evnt$tow_type))) {
    trawl_vec <- c(trawl_method)
  } else {
    stop("Trawl method must be O, V, S, M, L, D, FP, B. See EMA look up tables for tow type descriptions")
  }
  
  # Optional survey region filter - defaults to survey_region = NA
  # this is a way of dealing with the "NA" in region column that get generated when we created region in the ema_event pull
  # these four lines can be removed if the lat/lon information gets fixed at those four stations
  if(all(survey_region %in% unique(stats::na.omit(evnt$region)))){
    survey_vec <- c(survey_region)
  } else {
    if(is.na(survey_region)){
      survey_vec <- c(unique(evnt$region))
    } else {
      stop("Survey region must be one or more of: NBS, SEBS, GOA, or Chukchi")
    }
  }
  # if(any(survey_region %in% unique(stats::na.omit(event$region)))){
  #   survey_region <- c(survey_region)
  # } else {
  #   survey_region <- c(unique(event$region)) |> stats::na.omit()
  # }
  
  
  # optional tsn filter
  if(all(is.na(tsn))) {
    cth2 <-cth |>
      dplyr::left_join(taxa, by="species_tsn")
  } else {
    cth2 <-cth |>
      # inner join with the species tsn from the fxn argument
      dplyr::inner_join(taxa |> dplyr::filter(species_tsn %in% tsn), by="species_tsn") |>
      # LHS filter code next to TSN filter code
      dplyr::filter(dplyr::case_when(any(is.na(lhs)) ~ lhs_code %in% unique(cth$lhs_code),
                                     any(!is.na(lhs)) ~ lhs_code %in% lhs))
  }
  
  # set up the catch0 ifelse statement
  if(catch0 == FALSE){
    # join into one data frame
    data <- evnt |>
      dplyr::filter(sample_year >= start_year
                    & sample_year <= end_year
                    & gear %in% gear_vec
                    & tow_type %in% trawl_vec
                    & region %in% survey_vec) |>
      # note keeping this a left_join event to catch means that there are about ~11 water hauls (or satisfactory hauls)
      # that legit got ZERO things in the net.
      dplyr::left_join(cth2, by=c("station_id"="station_id", "event_code"="event_code", "gear"="gear")) |>
      dplyr::left_join(event_parameters, by=c("station_id"="station_id", "event_code"="event_code", "gear"="gear")) |>
      dplyr::mutate(haulback_time = lubridate::ymd_hms(haulback_time),
                    eq_time = lubridate::ymd_hms(eq_time),
                    gear_in_time = lubridate::ymd_hms(gear_in_time),
                    gear_out_time = lubridate::ymd_hms(gear_out_time),
                    tow_duration = ifelse(tow_type %in% c("S", "M", "L"),
                                          difftime(haulback_time, eq_time, units = "mins"),
                                          ifelse(tow_type %in% c("O"),
                                                 difftime(gear_out_time, gear_in_time, units = "mins"), NA))) |>
      dplyr::select(sample_year, cruise_id, event_code, station_id, master_station_name, gear, gear_performance, tow_type, nbs_strata, oceanographic_domain,
                    large_marine_ecosystem, region, haul_date, eq_time, eq_latitude, eq_longitude, gear_in_time, gear_in_latitude, gear_in_longitude,
                    gear_out_time, gear_out_latitude, gear_out_longitude, haulback_time, haulback_latitude, haulback_longitude,
                    effort, effort_units, tow_duration,
                    species_tsn, common_name, scientific_name, lhs_code, total_catch_number, total_catch_weight_g, event_notes, catch_notes)
    
    
    
    # Begin catch 0 calculation
  } else { # stops the function if there are no tsn included
    if(any(is.na(tsn))) {
      stop("catch0 cannot be TRUE when tsn=NA. This will produce a data frame of up to 8,667,792 rows.")
    }
    #same event code as above to get sampling events
    event2 <- evnt |>
      dplyr::filter(sample_year >= start_year
                    & sample_year <= end_year
                    & gear %in% gear_vec
                    & tow_type %in% trawl_vec
                    & region %in% survey_vec) |>
      dplyr::mutate(haulback_time = lubridate::ymd_hms(haulback_time),
                    eq_time = lubridate::ymd_hms(eq_time),
                    gear_in_time = lubridate::ymd_hms(gear_in_time),
                    gear_out_time = lubridate::ymd_hms(gear_out_time),
                    tow_duration = ifelse(tow_type %in% c("S", "M", "L"),
                                          difftime(haulback_time, eq_time, units = "mins"),
                                          ifelse(tow_type %in% c("O"),
                                                 difftime(gear_out_time, gear_in_time, units = "mins"), NA)))
    
    
    # Creates a unique list of species_tsn's plus LHS_Codes
    cth_unique <- unique(cth2[c("species_tsn", "common_name", "scientific_name", "lhs_code")])
    
    # Builds a grid of all stations and species/lhs combinations (including nonsensicle ones)
    zero_grid <- tidyr::expand_grid(subset(event2, select=c("station_id","event_code","gear")), cth_unique)
    
    # Take the zero grid of tsn and lhs and join in event information (filtered above)
    zero_event_join <- dplyr::left_join(zero_grid, subset(event2, select=c("station_id", "event_code", "gear",
                                                                           "cruise_id","sample_year", "large_marine_ecosystem",
                                                                           "tow_type","gear_performance","region", "haul_date",
                                                                           "eq_time", "eq_latitude","eq_longitude", "gear_in_time",
                                                                           "gear_in_latitude", "gear_in_longitude",
                                                                           "gear_out_time", "gear_out_latitude", "gear_out_longitude",
                                                                           "haulback_time", "haulback_latitude", "haulback_longitude",
                                                                           "tow_duration", "event_notes")),
                                        by=c("station_id", "event_code", "gear"))
    # Add event parameter fields to the zero catch-events
    zero_event_param_join <- dplyr::left_join(zero_event_join ,subset(event_parameters,
                                                                      select=c("station_id","event_code","gear",
                                                                               "master_station_name","nbs_strata","bsierp_region",
                                                                               "oceanographic_domain","effort","effort_units")),
                                              by=c("station_id","event_code","gear"))
    # Where the magic catch zero calculation happens
    # Notice the left_join here with events and catch
    data <- dplyr::left_join(zero_event_param_join,cth2,by=c("station_id","event_code","gear",
                                                             "species_tsn","common_name","scientific_name",
                                                             "lhs_code"))|>
      dplyr::mutate(total_catch_number = ifelse(is.na(total_catch_number),0,total_catch_number),
                    total_catch_weight_g = ifelse(is.na(total_catch_weight_g),0,total_catch_weight_g),
                    cpue_num= total_catch_number/effort,
                    cpue_weight = total_catch_weight_g/effort) |>
      # make the output data frame match the non catch 0 dataframe
      dplyr::select(sample_year, cruise_id, event_code, station_id, master_station_name, gear, gear_performance, tow_type, nbs_strata, oceanographic_domain,
                    large_marine_ecosystem, region, haul_date, eq_time, eq_latitude, eq_longitude, gear_in_time, gear_in_latitude, gear_in_longitude,
                    gear_out_time, gear_out_latitude, gear_out_longitude, haulback_time, haulback_latitude, haulback_longitude,
                    effort, effort_units, tow_duration,
                    species_tsn, common_name, scientific_name, lhs_code, total_catch_number, total_catch_weight_g, event_notes, catch_notes)
    
    
  }
  
  
  return(data)
}

pull_clean_catch_offline <- function(this.year, tsns, region, joined_data) {
  df <- joined_data
  
  #' species to subset and combine
  #' all salmon are juveniles
  #' for saffron cod, herring, capelin, and sandlance use all LHS stages
  #' if you want squid - all squid species in db
  #' jellyfish includes: Aequorea sp., aurelia sp., chrysora, cyanea, sautrophora, and phacellephora
  
  # calc median doy 
  median.doy <- df %>%
    group_by(station_id) %>%
    mutate(doy = yday(haul_date)) %>%
    summarise(mean.doy = mean(doy)) %>%
    summarise(median.doy = median(mean.doy),
              mean = mean(mean.doy))
  
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
    mutate(total_catch_weight_kg = total_catch_weight_g/1000) %>% # convert g to kg
    pivot_wider(id_cols = c(station_id, sample_year, 
                            cruise_id, haul_date, eq_time, 
                            eq_latitude, eq_longitude, effort, 
                            effort_units), 
                names_from = "name_lhs", values_from = "total_catch_weight_kg") %>%
    arrange(station_id) %>% # now lets add up the values
    rowwise() %>%
    mutate(
      `Saffron Cod_All` = sum(c_across(any_of(c("Saffron Cod_U", "Saffron Cod_A1+", "Saffron Cod_A0"))), na.rm = TRUE),
      `Pacific Herring_All` = sum(c_across(any_of(c("Pacific Herring_U", "Pacific Herring_A0", "Pacific Herring_A1+"))), na.rm = TRUE),
      `Capelin_All` = sum(c_across(any_of(c("Capelin_U", "Capelin_A0", "Capelin_A1+"))), na.rm = TRUE),
      `Sand Lance_All` = sum(c_across(any_of(c("Arctic Sand Lance_A1+", "Arctic Sand Lance_A0", "Sand lance, unident._U", "Sand lance, unident._A0"))), na.rm = TRUE),
      `Rainbow Smelt_All` = sum(c_across(any_of(c("Rainbow Smelt_A1+", "Rainbow Smelt_A0", "Rainbow Smelt_U"))), na.rm = TRUE),
      Forage = sum(c_across(any_of(c("Saffron Cod_All", "Pacific Herring_All", "Capelin_All", "Rainbow Smelt_All",
                                     "Chum Salmon_J", "Coho Salmon_J", "Chinook Salmon_J", "Pink Salmon_J", "Sockeye Salmon_J",
                                     "Pollock_A0", "Pacific Cod_A0"))), na.rm = TRUE),
      Jellyfish = sum(c_across(any_of(c("Lions mane_U", "Aurelia sp._U", "Aequorea sp._U", "Northern Sea Nettle_U",
                                        "Whitecross jelly_U", "Fried egg jellyfish_U", "Aurelia labiata_U",
                                        "Cyanea sp._U", "Brownbanded Moon Jelly_U", "Aurelia aurita_U",
                                        "Chrysaora jellyfish_U", "Fried egg jelly_U"))), na.rm = TRUE),
      Lat = eq_latitude,
      Lon = eq_longitude,
      doy = yday(haul_date)
    )
  
  return(data)
} # so in this output we could add in the appropriate field configs / obs models / families for the model settings and run

