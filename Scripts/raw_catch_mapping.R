# plotting raw cpue data 
# 
# Lia Domke
# 9-29-26

# ajdustable things: 
this.year <- year(Sys.Date())

# load packages
pkgs <- c("tidyverse", "sf", "marmap", "EMAdownload")

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    message(paste("Package '", pkg, "' is already installed and loaded.", sep = ""))
  }
}


### --- LOAD BASE LAYERS --- ####
# make sure the base map / bathy is created with the advanced sf library 
sf_use_s2(TRUE)
# need to use world not world2hires cause that one is hella old and doesn't plot correctly
bs_map <- maps::map("world", regions = c("USA:Alaska", "Russia"), 
                    fill = TRUE, plot = FALSE, wrap = c(0, 360)) %>%
  st_as_sf(crs = 4326) 

# Use bounding box, have to add 360 to the bbox coords
# xmin = 185 (-175), xmax = 202 (-155)
bbox_360 <- st_bbox(c(xmin = 180, xmax = 210, ymin = 50, ymax = 68), 
                    crs = st_crs(bs_map))

# crop 
# Using st_make_valid helps w/ messy polys
bs_base <- bs_map %>% 
  st_make_valid() %>% 
  st_intersection(st_as_sfc(bbox_360)) %>%
  st_shift_longitude() %>%
  st_transform(crs = 4326)

bs_croppedUTM <- bs_base %>%
  st_transform(crs = 32602) # Move to UTM Zone 2N

#read in bathymetry file.  
bathline <- getNOAA.bathy(-180,-155,53,68)

bathline.xyz <- as.xyz(bathline)


### --- READ IN DATA --- ###
#tax <- get_ema_taxonomy() # get correct tsn 934083
pollock <- join_event_catch(start_year = this.year, end_year = this.year,
                 survey_region = c("NBS", "SEBS"), tsn = 934083,
                 lhs = "A0", gear = "CAN", trawl_method = "S", catch0 = T)
# pollock <- join_event_catch(start_year = this.year, end_year = this.year,
#                             survey_region = c("NBS", "SEBS"), tsn = 934083, 
#                             lhs = "A1", gear = "CAN", trawl_method = "S", catch0 = T)


head(pollock)
unique(pollock$region); unique(pollock$sample_year); unique(pollock[c("common_name", "lhs_code")])

# to make it nice lets convert to sf & calculate cpue/wpue
poll.sf <- pollock %>% # remove Q perf trawl at station 40
  filter(gear_performance %in% c("G", "S")) %>%
  mutate(cpue = total_catch_number / effort,
         wpue_kg = (total_catch_weight/1000) / effort) %>%
  st_as_sf(., coords = c("eq_longitude", "eq_latitude"), 
                    crs = 4326) %>%
  st_shift_longitude()

### --- SET UP MAPPING --- ###

ggplot() +
  # there are 0 catches so we need to set up a manual shape set to X those out
  # start with the data and set shape and size 
  geom_sf(data = poll.sf, mapping = aes(shape = wpue_kg == 0, size = log(wpue_kg+1)), stroke = 1.1, fill = "black") +
  # now set the specifics of the shape
  scale_shape_manual(name = expression("Log WPUE (kg/km"^2*")"), values = c("FALSE" = 19, "TRUE" = 4),
                     labels = c("Catch", "Zero Catch"),
                     guide = "none") +
  # use psedo log cause it deals with no 0s okay
  scale_size_continuous(name = expression("Log WPUE (kg/km"^2*")"),
    range = c(1.5,6),          # point diameter here min/max
    #trans = "sqrt",
    labels = scales::label_comma(),
    #breaks = c(0, 1, 2, 3, 4, 5, 6)
    ) +
  # put all aes things into a single legend
  guides(size = guide_legend(override.aes = list(shape = c(4, 19, 19, 19, 19), # First break (0) gets 'X' (4), rest get open circle (1)
                                                 size = c(1.5, 1.5, 2, 3, 4),
                                                 stroke = 1.1))) +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude", title = paste(this.year, "Age -", unique(poll.sf$lhs_code), "Walleye Pollock")) +
  # now all the background stuff: 
  # add base background
  geom_sf(data = bs_base, fill = "gray", color = "black", inherit.aes = F) +
  # add in bathymetry lines
  geom_contour(data = bathline.xyz, aes(x = V1 + 360, y = V2, z = V3),
               breaks = c(-50, -100, -200), color = "black") +
  # use the 360 coords for longitude (so -150 + 360)
  coord_sf(xlim = c(180, 202), ylim = c(54, 66), expand = F)




