#-------------------------------------------------------------------------------
# STRAVA API STREAM DATA EXTRACTION & PROCESSING
# Extract stream data for different activity types and visualize
# AUTHOR: Francine Stephens
# DATE CREATED:  4/14/21
# LAST UPDATED DATE: 4/14/21
#-------------------------------------------------------------------------------

## INITIALIZE-------------------------------------------------------------------
#devtools::install_github('fawda123/rStrava')
packages <- c(
  "tidyverse",
  "rStrava",
  "sp",
  "ggmap",
  "raster",
  "mapproj",
  "lubridate",
  "leaflet", 
  "rStrava",
  "extrafont",
  "hrbrthemes",
  "wesanderson",
  "ggtext"
)
lapply(packages, library, character.only = T)

# KEY PARAMETERS
app_name <- 'Francine Stephens' # chosen by user
app_client_id  <- '#####' # an integer, assigned by Strava
app_secret <- 'XXXXXXXXXXXXXXXXXXXXXX#################' # an alphanumeric secret, assigned by Strava
mykey <- 'XXXXXXXXXXXXXXXXXXXXXXXXXX'  # Google API key
register_google(mykey)

# CONFIG
# stoken <- httr::config(
#   token = strava_oauth(
#     app_name,
#     app_client_id,
#     app_secret,
#     app_scope="activity:read_all",
#     cache=TRUE)
# )
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])


## EXTRACT DATA-----------------------------------------------------------------
# Download Strava data
myinfo <- get_athlete(stoken, id = '37259397')

routes <- get_activity_list(stoken)
length(routes)  # GET count of activities

# CONVERT ACTIVITIES FROM LIST TO DATAFRAME
  # Set units to imperial to convert to miles.
  # To get a subset specify a slice as below. 
  # Can also filter to get a subset using dplyr filter. 
activities_data <- compile_activities(routes, units = "imperial") 
saveRDS(activities_data, file = "strava_activities_041421.rds")


# RUNS
runs <- activities_data %>% 
  filter(
    type == "Run" & !is.na(start_latitude)
  ) 
## DO SUBSETS (N=40) OF THE RUNS BC ACTIVITY STREAMS CANNOT HANDLE ALL RUNS AT A TIME
run_ids <- runs %>% 
  slice(1:39) %>%
  pull(id)

run_dates <- runs %>%
  dplyr::select(id, start_date) %>%
  mutate(date = as.Date(str_sub(start_date, end = 10))
  )

run_streams_first <- get_activity_streams(act_data=routes, 
                                   stoken, 
                                   id=run_ids, 
                                   types="latlng", 
                                   units="imperial")
run_streams_recent <- get_activity_streams(act_data=routes, 
                                           stoken, 
                                           id=run_ids, 
                                           types="latlng", 
                                           units="imperial")


all_runs <- rbind(run_streams_first, run_streams_recent) %>% 
  mutate(location = as.factor(
    if_else(lng < -120,
            "Stanford",
            "Rockwall Ranch")
  )
  ) %>%
  left_join(., run_dates, by = "id") %>%
  mutate(group_no = group_indices_(., .dots="id"),
         group_rem = group_no / 5,
         group_str = as.character(group_rem),
         group_col = case_when(
           str_detect(group_str, ".2") ~ "1",
           str_detect(group_str, ".4") ~ "2",
           str_detect(group_str, ".6") ~ "3",
           str_detect(group_str, ".8") ~ "4",
           TRUE ~ "5"
         )
  )


## VISUALIZATION----------------------------------------------------------------

##########
# RUNS 
##########
# SET COLORS AND THEMES
stanford_cardinal <- "#8C1515"
dark_green <- "#006400"

theme_runs <- theme_void(base_family = "Rockwell",
                         base_size = 20) +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = unit(rep(1, 4), "cm"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 3)
  )

theme_by_location <- theme_void(base_family = "Rockwell",
                                base_size = 13) +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = unit(rep(1, 4), "cm"),
        legend.position = "none",
        plot.title = element_markdown(hjust = 0.5, vjust = 3)
  )

colors_runs <- scale_color_manual(values = c(stanford_cardinal,
                                             dark_green)
) 

# MAP
runs_by_location <- ggplot(all_runs) +
  geom_path(aes(lng, lat, group = id, color = location), size = 0.35, lineend = "round") +
  facet_wrap(~location, scales = 'free') +
  labs(title = "Francine's 
       <span style='color:#8C1515'>Stanford</span> & <span style='color:#006400'>Rockwall Ranch, NBTX</span> Runs",
       caption = "Runs as of April 14, 2021") +
  theme_by_location +
  colors_runs
ggsave("runs_041421_by_location.png", plot = runs_by_location)


runs_moonrise <-  ggplot(all_runs) +
  geom_path(aes(lng, lat, group = id, color = group_col), size = 0.35, lineend = "round") +
  facet_wrap(~id, scales = 'free') + 
  labs(title = "Francine's Runs") + 
  theme_runs + 
  scale_color_manual(values=wes_palette(n=5, name="Moonrise3"))
ggsave("runs_041421_moonrise.png", plot = runs_moonrise)
