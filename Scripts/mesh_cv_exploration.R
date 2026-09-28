# exploring mesh size cross validation
# lia domke
#' 9-25-26
#' 
#' Given the lack of convergence for pollock when adding in the 2026 data 
#' I want to explore some different mesh sized models for different species
#' and look at how they perform with cross validation 
#' 
#' this is particularly relevant for the southern bering sea

# data pull function
source("Scripts/helper_fxns/clean_pull_data.R")

# make sure the proper packages are install and loaded
pkgs <- c("visreg", "sdmTMB", "sf", "concaveman", "cowplot", "gridExtra", "tidyverse", "EMAdownload")

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    message(paste("Package '", pkg, "' is already installed and loaded.", sep = ""))
  }
}

this.year <- 2026

tsns <- c(934083, 161979, 162035, 161980, 161976, 161977, 551209, 
          161975, 164711, 162041, 164708, 171672, 171671,
            # add in the jellyfish tsns (below)
          50623, 51640, 51641, 51669, 51671, 51695, 51696, 51700, 51701, 51705, 51707, 719327) 
# right now as i have this set up the tsns have to be these. Otherwise i need to adjust how they are 
# combined in the clean_pull_data function which is dumb but I dont have the energy to fix

ts <- pull_clean_catch(this.year = this.year, tsns = tsns, region = c("SEBS", "NBS")) %>%
  # this pivot longer wont work if you change the tsns above 
  pivot_longer(cols = c("Chinook Salmon_J":"Jellyfish"), names_to = "sp_name", values_to = "catch_kg") %>%
  add_utm_columns(., ll_names = c("Lon", "Lat"),
                  utm_names = c("X", "Y"), ll_crs = 4326, units = "km") %>%
  mutate(region = "SEBS", 
         crs = 32602)

# get the species names in a vector, there are 31, but we're only interested in a few for the ESRs 
species_names <- unique(ts$sp_name) 
species_names <- species_names[species_names %in% c("Capelin_All", "Forage", "Pacific Herring_All",
                                                    "Pollock_A0", "Jellyfish", "Pacific Cod_A0")]
ts2 <- filter(ts, sp_name %in% species_names) %>%
  filter(!(is.na(effort))) %>% # drop any stations that have NA for effort
  mutate(log_effort = log(effort)) %>%
  group_by(region, sp_name) %>%
  mutate(median_doy = median(doy),
         scale_doy = doy - median_doy,
         mean_doy = mean(doy),
         sd_doy = sd(doy),
         scale_doy2 = (doy - mean_doy)/sd_doy,
         scaled_median = (median_doy - mean_doy) / sd_doy)

# datalist just by the species name so includes region for both within a specis
ts.list <- split(ts2, list(species_names))

# fits the sdmtmb model and you can run the model with different mesh sizes using a four loop 
# need to re-write part of the function to deal with different mesh sizes
  ## wrapper function for sdmTMB
  
set_fit_TMB_meshcv <- function(
    data,
    survey_region,
    sp_name,
    run_fast = FALSE,
    n_x = 200,
    ...) {
    
    set.seed(2026)
    today <- Sys.Date()
    year <- year(today)
    
    # set directory
    path <- here("Results", paste0(year, "_ESR"), paste0(sp_name, "_meshcv"), today)
    if(!dir.exists(path)) dir.create(path, recursive = T)
    
    # change data pending region & sp name
    if (!sp_name %in% names(data)) stop("Species not found in data list")
    
    ts <- data[[sp_name]] %>%
      filter(region == survey_region) %>%
      as.data.frame()
    
    if (nrow(ts) == 0) stop("No data supplied for region: ", survey_region)
    
    
    ## print out median doy for *knowledge*
    ts_median <- unique(ts$median_doy)
    ts_scaled_median <- unique(ts$scaled_median)
    message(survey_region, " ", sp_name, " median doy: ", ts_median)
    
    ## create mesh
    mesh <- make_mesh(ts, xy_cols = c("X", "Y"), n_knots = n_x)
    
    ## identify folds
    fold_vector <- as.numeric(as.factor(ts$sample_year))
    
    ## fit model 
    fit <- sdmTMB_cv(data = ts, 
                     fold_ids = fold_vector,
                     parallel = TRUE,
                     mesh = mesh, ...)
    
    # save model object
    saveRDS(fit, file = paste0(here(path, sp_name), n_x, "_fit.RDS"))
    
    
    message("Model fits saved to:", path)
    
    return(fit)
    
  }


## fit models in a for loop for a single species
# starting with pollock
knot_list <- c(25, 50, 75, 85, 95)
sp.name <- "Pollock_A0"
region <- "SEBS"
mesh_cv_results <- list()

# if you change the species the model formula will stay the same but the 
# chose family distribution may be different 
# as of 9-28-26 forage, pollock, jellyfish, pcod are all tweedie and capelin delta-gamma

library(furrr)
plan(multisession, workers = 4)

# run t his fassst with parallel and 
mesh_cv_results <- future_map(knot_list, function(k) {
  set_fit_TMB_meshcv(
    data = ts.list,
    survey_region = region,
    sp_name = sp.name,
    n_x = k,
    formula = catch_kg ~ 0 + scale_doy2 + as.factor(sample_year),
    offset = "log_effort",
    family = tweedie(link = "log"),
    time = "sample_year",
    spatial = "on",
    spatiotemporal = "iid"
  )
}, .options = furrr_options(seed = TRUE))


## after this runs put it into a df to look at
df <- data.frame()
rmse <- data.frame()
# review the results
# summarize the results
for(i in 1:length(mesh_cv_results)) {
  mod <- mesh_cv_results[[i]]
  name <- paste0(sp.name, "_", knot_list[i])
  
  # whole dataset values
  tmp <- data.frame(
    model_cv = name,
    sum_loglike = mod$sum_loglik,
    mean_loglike = mod$sum_loglik/nrow(mod$data),
    total_rmse = sqrt(mean((mod$data$catch_kg - mod$data$cv_predicted)^2))
  )
  
  # by the folds
  tmp2 <- dplyr::group_by(mod$data, cv_fold) |> 
    dplyr::summarize(
      model_cv = name,
      mae = mean(abs(catch_kg - cv_predicted)),
      rmse = sqrt(mean((catch_kg - cv_predicted)^2)))
  
  df <- rbind(df, tmp)
  rmse <- rbind(rmse, tmp2)
  
}

# we want to have a higher logliklihood and lower rmse
# because it neg log lik it'll be the one closer to 0
arrange(df, -sum_loglike); max(df$sum_loglike) # a0 pol looks like mesh 95 - forage is 75 - jelly 75
arrange(df, -mean_loglike) # also 95 - forage is 75 - jelly 75
arrange(df, total_rmse) # this one is 75 - forage is 85 - jelly is 95

# theres a split between mesh size of 95 versus 75 beteween negloglik and rmse
# theres not a huge amount of variation between the different models and rmse, maybe we dont use that?
# what if you tried this with a different species?
ggplot() +
  geom_line(aes(x = cv_fold, rmse, color = model_cv), data = rmse)

# run.date <- "2026-09-28"
# write.csv(df, here::here("Results", paste0(this.year, "_ESR"), paste0(sp.name, "_meshcv"), run.date,
#                          paste0(sp.name,"_", region, "_", "RMSEfolds.csv")))
# 
# write.csv(rmse, here::here("Results", paste0(this.year, "_ESR"), paste0(sp.name, "_meshcv"), run.date,
#                          paste0(sp.name,"_", region, "_", "RMSE.csv")))
