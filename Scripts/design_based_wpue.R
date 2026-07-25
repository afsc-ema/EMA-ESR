# Author Lia Domke
# Date 7-24-26
# Last updated 7-24-26
# Quick comparison with design and model based indices

# here we calculate the design based indices and provide as a figure to compare with the model based indices

#### libraries ####
# make sure the proper packages are install and loaded
pkgs <- c("patchwork","lubridate", "here", "ggplotify", "tidyverse")

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    message(paste("Package '", pkg, "' is already installed and loaded.", sep = ""))
  }
}

#### Fxns ####
source("Scripts/helper_fxns/clean_pull_data.R")

#### Data ####
# pull in data using the cleaning function
tsns <- c(934083, 161979, 162035, 161980, 161976, 161977, 551209, 
          161975, 164711, 162041, 164708, 171672, 171671,
          # add in the jellyfish tsns (below)
          50623, 51640, 51641, 51669, 51671, 51695, 51696, 51700, 51701, 51705, 51707, 719327) # right now as i have this set up the tsns have to be these. Otherwise i need to adjust how they are combined in the clean_pull_data function which is dumb but I dont have the energy to fix

# adjust to the current year
this.year <- 2025

nbs.ts <- pull_clean_catch(this.year = this.year, tsns = tsns, region = "NBS") %>%
  # this pivot longer wont work if you change the tsns above 
  pivot_longer(cols = c("Chinook Salmon_J":"Jellyfish"), names_to = "sp_name", values_to = "catch_kg") %>%
  mutate(region = "NBS")

sebs.ts <- pull_clean_catch(this.year = this.year, tsns = tsns, region = "SEBS") %>%
  # this pivot longer wont work if you change the tsns above 
  pivot_longer(cols = c("Chinook Salmon_J":"Jellyfish"), names_to = "sp_name", values_to = "catch_kg") %>%
  mutate(region = "SEBS")

# combine 
ts <- rbind(sebs.ts, nbs.ts) 

# get the species names in a vector, there are 31, but we're only interested in a few for the ESRs 
species_names <- unique(ts$sp_name) 
species_names <- species_names[species_names %in% c("Capelin_All", "Forage", "Pacific Herring_All",
                                                    "Pollock_A0", "Pacific Cod_A0", "Jellyfish")]
ts <- filter(ts, sp_name %in% species_names) %>%
  filter(!(is.na(effort))) # drop any stations that have NA for effort, right now drops 5

# calculate the rolling average +/- 1SD
ts.mean <- ts %>%
  # calculate overall mean
  # group_by(region, sp_name) %>%
  # mutate(wpue = catch_kg / effort,
  #        mean_wpue = mean(wpue),
  #        sd_wpue = sd(wpue)) %>%
  # calculate yearly  mean
  #ungroup() %>%
  mutate(wpue = catch_kg / effort) %>%
  group_by(region, sp_name, sample_year) %>%
  dplyr::summarize(
         wpue_yr = mean(wpue),
         wpue_yr_sd = sd(wpue),
         num = n(),
         se = wpue_yr_sd / sqrt(num)) %>%
  # select(c("region", "sp_name", "sample_year", "wpue_yr", "wpue_yr_sd", "num", "se", "mean_wpue", "sd_wpue")) %>%
  # distinct() %>%
  group_by(region, sp_name) %>%
  complete(sample_year = 2003:year(Sys.Date())) %>%
  ungroup()

sp.list <- unique(ts.mean$sp_name)

plot_wpue <- function(species, data) {
  dat <- filter(data, sp_name == species)
  
  ggplot(data = dat, aes(x = sample_year, y = wpue_yr, group = region, 
                         color = region)) +
    geom_pointrange(aes(ymin = wpue_yr - se, ymax = wpue_yr + se, 
                        group = region, color = region))+
    geom_line() +
    theme_cowplot(12)+
    ylab(bquote("Weight per unit effort (kg/km2)"))+
    labs(title=paste(sp_name, "- Late Summer/Fall Surface Trawl Surveys"), 
         x ="Year", color = "Region", cex=3)+
    theme(legend.position = "right",
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          panel.background = element_rect(colour = "black", linewidth=1))+
    scale_x_continuous(breaks = seq(min(dat$sample_year), max(dat$sample_year), 5)) +
    #scale_y_continuous(limits = c(0, 650)) +
    scale_color_manual(values=c('#003366','#0099CC'))#+
  #scale_color_manual(values=c('#003366','#0099CC'))+
  #geom_point(size=1)
  
}

plot_list <- list()
for(i in 1:length(sp.list)) {
  sp_name <- sp.list[[i]]
  plot_list[[i]] <- plot_wpue(species = sp_name, data = ts.mean)
}

gridExtra::grid.arrange(grobs = plot_list)

plot_list[[1]]
plot_list[[2]]
plot_list[[3]]
plot_list[[4]]
plot_list[[5]]
plot_list[[6]]
