#' 2026 script to make the output of the qaqc csvs work in the same format as emadownload to use 
#' to rbind to other nbs data from the clean_pull_data.R in the fit_sdm_models
#' 

# read in the csvs
event <- read.csv("../EMA-QAQC/Data/2026/NBS/2026_NBSevent_2026-09-24.csv")
params <- read.csv("../EMA-QAQC/Data/2026/NBS/2026_NBSevent_parameters_2026-09-24.csv")
catch <- read.csv("../EMA-QAQC/Data/2026/NBS/2026_NBScatch_2026-09-24.csv")
# first get the ema event & catch function code without the api
# also brings in a help fxn to clean catch offline
source("Scripts/helper_fxns/join_event_catch_offline.R")

# get the list of tsns we need
tsns <- c(934083, 161979, 162035, 161980, 161976, 161977, 551209, 
          161975, 164711, 162041, 164708, 171672, 171671,
          # add in the jellyfish tsns (below)
          50623, 51640, 51641, 51669, 51671, 51695, 51696, 51700, 51701, 51705, 51707, 719327) 
# right now as i have this set up the tsns have to be these. Otherwise i need to adjust how they are combined in the clean_pull_data_offline
# function which is dumb but I dont have the energy to fix

# get the data formatted like emadownload
joined_data <- join_event_catch_offline(start_year=2026, end_year=2026, survey_region="NBS", tsn=tsns,lhs=NA,
                        ema.event = event, ema.catch = catch, ema.params = params,
                        gear= c("CAN"), trawl_method="S", catch0=TRUE, force_download = FALSE)

# pull the cleaning data from clean_pull_data.R
nbs2026ts <- pull_clean_catch_offline(this.year = 2026, tsns = tsns, region = "NBS", joined_data = joined_data)

## need to also pull in the data on akfin cause some of the  sebs data is classified as nbs (5 stations)
source("Scripts/helper_fxns/clean_pull_data.R")
sebs.nbs.dat <- pull_clean_catch(this.year = 2026, tsns = tsns, region = "NBS") %>%
  filter(sample_year == 2026)

allnbs2026 <- rbind(nbs2026ts, sebs.nbs.dat) %>%
  mutate(across(c("Pink Salmon_J":"Chrysaora jellyfish_U"), ~ replace_na(., 0)),
         haul_date = as.Date(haul_date))

# write it out
# write.csv(allnbs2026, "tmp_data/2026_nbs_survey_biomass.csv", row.names = FALSE)
