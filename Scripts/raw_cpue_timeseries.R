# quickly plotting sockeye wpue timeseries NBS/SEBS

library(EMAdownload)
library(tidyverse)


#tax <- get_ema_taxonomy()
# sockeye 161979
juv.sock <- join_event_catch(start_year = 2003, end_year = this.year,
                            survey_region = c("NBS", "SEBS"), tsn = 161979,
                            lhs = "J", gear = "CAN", trawl_method = "S", catch0 = T)


head(juv.sock)
unique(juv.sock$region); unique(juv.sock$sample_year); unique(juv.sock[c("common_name", "lhs_code")])

# to make it nice lets convert to sf & calculate cpue/wpue
juv.sock.cpue <- juv.sock %>% # remove Q perf trawl at station 40
  filter(gear_performance %in% c("G", "S")) %>%
  mutate(cpue = total_catch_number / effort,
         wpue_kg = (total_catch_weight/1000) / effort) %>%
  group_by(sample_year, region) %>%
  dplyr::summarise(mean_cpue = mean(cpue, na.rm = T),
                   mean_wpue = mean(wpue_kg, na.rm =T),
                   se_cpue = sd(cpue) / sqrt(n()),
                   se_wpue = sd(wpue_kg) / sqrt(n())) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(annual_mean_cpue = mean(mean_cpue),
         annual_mean_wpue = mean(mean_wpue),
         sd_cpue = sd(mean_cpue), 
         sd_wpue = sd(mean_wpue))
  # st_as_sf(., coords = c("eq_longitude", "eq_latitude"), 
  #          crs = 4326) %>%
  # st_shift_longitude()


ggplot() +
  geom_pointrange(aes(x = as.factor(sample_year), y = mean_wpue, ymin = mean_wpue - se_wpue,
                      ymax = mean_wpue + se_wpue), data = juv.sock.cpue) +
  geom_hline(data = juv.sock.cpue, aes(yintercept = annual_mean_wpue, group = region)) +
  geom_hline(data = juv.sock.cpue, aes(yintercept = annual_mean_wpue + sd_wpue, group = region),
             linetype = 2) +
  geom_hline(data = juv.sock.cpue, aes(yintercept = annual_mean_wpue - sd_wpue, group = region),
             linetype = 2) +
  labs(title = "Juvenile sockeye", x = "Year", y = "Average weight per unit effort (kg/km2)") +
  facet_wrap(~region, ncol = 1) +
  theme_bw()
  
