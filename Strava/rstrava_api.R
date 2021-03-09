#-------------------------------------------------------------------------------
# STRAVA API DATA EXTRACTION & PROCESSING
# A test run of the API. Treat as template/reference for rStrava package.
# AUTHOR: Francine Stephens
# DATE CREATED:  3/7/21
# LAST UPDATED DATE: 3/8/21
#-------------------------------------------------------------------------------

## INITIALIZE-------------------------------------------------------------------

packages <- c(
  tidyverse,
  rStrava,
  sp,
  ggmap,
  raster,
  mapproj,
  lubridate,
  leaflet
)
lapply(packages, library, character.only = T)

## KEY PARAMETERS
app_name <- 'Francine Stephens' # chosen by user
app_client_id  <- '#####' # an integer, assigned by Strava
app_secret <- 'XXXXXXXXXXXXXXXXXXXXXX#################' # an alphanumeric secret, assigned by Strava
mykey <- 'XXXXXXXXXXXXXXXXXXXXXXXXXX'  # Google API key
register_google(mykey)

## CONFIG
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"
                                            ))

## EXTRACT DATA-----------------------------------------------------------------
# download strava data
myinfo <- get_athlete(stoken, id = '37259397')

routes <- get_activity_list(stoken)
length(routes)  # GET # of activities

# CONVERT ACTIVITIES FROM LIST TO DATAFRAME
# Set units to imperial to convert to miles.
# To get a subset specify a slice as below. 
# Can also filter to get a subset. 
activities_data <- compile_activities(routes, units = "imperial") 
    # act_data <- compile_activities(routes, acts = 1:20, units = "imperial") %>% 
    #   filter(distance > 2 & total_elevation_gain > 0) 
saveRDS(activities_data, file = "strava_activities_030821.rds")


## MANIPULATE ACTIVITIES DATA---------------------------------------------------
## AGGREGATIONS OF ACTIVITIES
# CUMULATIVE DISTANCE PER YEAR
activities_data <- activities_data %>%
  mutate(start_date = as_date(start_date),
         year = year(start_date),
         day_of_year = yday(start_date),
         month = month(start_date),
         day = wday(start_date, label = T),
         week = week(start_date))

activities_data %>%
  group_by(year) %>%
  arrange(start_date) %>%
  mutate(cumulative_distance = cumsum(distance)) %>%
  ungroup() %>%
  ggplot(aes(x = day_of_year, y = cumulative_distance, color = factor(year))) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) + 
  scale_color_brewer(palette = "Set1") +
  labs(title = "Cumulative distance per year",
       subtitle = "Last 4 years highlighted",
       x = "Day of Year",
       y = "Cumulative Distance (Miles)",
       color = "Year",
       caption = "Francine's Strava Data") + 
  theme_classic()
  

# CALENDAR HEAT MAP OF DISTANCE COVERED EACH DAY
ggplot(activities_data %>% filter(year > 2018), aes(x = week, y = factor(day))) +
  geom_tile(aes(fill = distance)) +
  scale_fill_continuous(low = "lightgreen",
                        high = "red") +
  facet_wrap(~ year,
             scales = "free_x") +
  labs(x = "Week",
       y = "",
       title = "Calendar Heat Map of Distance in Miles",
       caption = "Francine's Strava Data") + 
  theme_classic()

# SPPED V. DISTANCE
activities_data %>%
  filter(average_speed !=0) %>%
  ggplot(aes(x = distance, y = average_speed)) + 
  geom_hex() +
  scale_fill_viridis_c()  +
  labs(x = "Distance (Miles)",
       y = "Average Speed (Miles/Hr)",
       title = "Distance of Workout compared to Speed",
       caption = "Francine's Strava Data") + 
  theme_classic()


## MAPPING
# types arguments = list("time", "latlng", "distance", "altitude", "velocity_smooth",
# "heartrate", "cadence", "watts", "temp", "moving", "grade_smooth")
walk_raw <- get_streams(stoken, id = "4909525072")  

walk <- walk_raw %>%
  purrr::transpose() %>% 
  tibble::as_tibble() %>% 
  dplyr::select(type, data) %>% 
  dplyr::mutate(type = unlist(type), 
                data = purrr::map(data, function(x) {
                  idx <- !sapply(x, length)
                  x[idx] <- NA
                  return(x)
                }))

lat_lng_to_df <- function(x) {
  purrr::set_names(x, nm = c("lat", "lng")) %>% tibble::as_tibble()
}

walk <- walk %>% 
  tidyr::spread(data = ., key = type, value = data) %>%
  tidyr::unnest() %>%
  dplyr::mutate(latlng = purrr::map(latlng, lat_lng_to_df)) %>%
  tidyr::unnest() %>%
  mutate(velocity_kph = 3.6 * velocity_smooth)


leaflet(walk) %>%
  addTiles() %>%
  addPolylines(lng = ~ lng, 
               lat = ~ lat)

## QOMs VISUALIZATIONS
qoms <- get_KOMs(stoken,
                 id = '37259397')

# Convert the data from list to tibble:
qoms_df <- qoms %>%
  purrr::transpose() %>%
  tibble::as_tibble() %>%
  tidyr::unnest(name, elapsed_time, distance)

qoms_df %>%
  ggplot(aes(x = fct_reorder(name, elapsed_time), y = elapsed_time, fill = distance)) +
  geom_col(color = "black") +
  coord_flip() +
  labs(x = "Name of Segment",
       y = "Time (Seconds)",
       title = "Queen of the Mountain Segments",
       caption = "Francine's Strava Data",
       fill = "Segment Distance (Ft)") +
  theme(legend.position = "bottom")  
  


# HEAT MAPPING
# all routes
activities_for_heat_mapping <- activities_data %>% 
  filter(start_longitude > -98.35) %>% 
  filter(start_latitude > 29.6) 

get_heat_map(activities_for_heat_mapping, key = mykey, col = 'darkgreen', size = 2, distlab = F, f = 0.4)

# activity id
id <- 4443408220
# plot elevation along a single ride
get_heat_map(activities_for_heat_mapping,
             id = id,
             alpha = 1,
             add_elev = T,
             f = 0.3,
             distlab = F,
             key = mykey,
             size = 2,
             col = 'Spectral',
             maptype = 'satellite',
             units = 'imperial')
# plot % gradient along a single ride
get_heat_map(activities_for_heat_mapping,
             id = id,
             alpha = 1,
             add_elev = T,
             f = 0.3,
             distlab = F,
             as_grad = T,
             key = mykey,
             size = 2,
             col = 'Spectral',
             expand = 5,
             maptype = 'satellite',
             units = 'imperial')


# GET ELEVATION PROFILES
get_elev_prof(activities_for_heat_mapping, id = id, key = mykey, units = 'imperial')
