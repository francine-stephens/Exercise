#-------------------------------------------------------------------------------
# STRAVA API DATA EXTRACTION & PROCESSING
# A test run of the API. Treat as template/reference for rStrava package.
# AUTHOR: Francine Stephens
# DATE CREATED:  3/7/21
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
  "extrafont",
  "hrbrthemes",
  "wesanderson",
  "ggtext",
  "gepaf",
  "ceramic",
  "maptools",
  "tmap",
  "RSocrata",
  "mapboxapi",
  "tigris",
  "ggpointdensity"
)
lapply(packages, library, character.only = T)

## KEY PARAMETERS
app_name <- 'Francine Stephens' # chosen by user
app_client_id  <- '52066' # an integer, assigned by Strava
app_secret <- '3aa504f5d38e6337aa2f4d8b62669203a790bee2' # an alphanumeric secret, assigned by Strava

## CONFIG
stoken <- httr::config(
  token = strava_oauth(
    app_name,
    app_client_id,
    app_secret,
    app_scope="activity:read_all",
    cache=TRUE)
  )

#### FOR NEXT TIME
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])


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
saveRDS(activities_data, file = "strava_activities_052323.rds")
activities_data <- readRDS("strava_activities_052323.rds")


# OTHER DATA 
parcels <- st_read("Parcels/geo_export_9359885a-c27f-43df-9ed4-af43d8ff3a03.shp")



## MANIPULATE ACTIVITIES DATA---------------------------------------------------
## AGGREGATIONS OF ACTIVITIES
# CUMULATIVE DISTANCE PER YEAR
activities_data_c <- activities_data %>%
  mutate(start_date = as_date(start_date),
         year = year(start_date),
         day_of_year = yday(start_date),
         month = month(start_date),
         day = wday(start_date, label = TRUE),
         week = week(start_date))

activities_data_c %>%
  group_by(year) %>%
  arrange(start_date) %>%
  mutate(cumulative_distance = cumsum(distance)) %>%
  ungroup() %>% 
  filter(year > 2019) %>%
  ggplot(aes(x = day_of_year, y = cumulative_distance, color = factor(year))) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) + 
  scale_color_brewer(palette = "Set1") +
  labs(title = "Cumulative distance per year",
       subtitle = "Last 4 years of runs, bikes, hikes, and walks",
       x = "Day of Year",
       y = "Cumulative Distance (Miles)",
       color = "Year",
       caption = "Francine's Strava Data") + 
  theme_classic()
ggsave("cumdist_graph.png", width = 5, height = 5)

# CALENDAR HEAT MAP OF DISTANCE COVERED EACH DAY
ggplot(activities_data_c %>% 
         filter(year > 2018), aes(x = week, y = factor(day))) +
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
ggsave("distance_calendar_heatmap.png", width = 5, height = 5)


# CALENDAR OF TIME
ggplot(activities_data_c %>% 
         filter(year > 2018) %>% 
         group_by(year, week, day) %>% 
         summarize(time = sum(elapsed_time, na.rm = TRUE)) %>% 
         ungroup() %>% 
         mutate(minutes = time/60), 
       aes(x = week, y = factor(day))) +
  geom_tile(aes(fill = minutes)) +
  scale_fill_continuous(low = "lightgreen",
                        high = "red") +
  facet_wrap(~ year,
             scales = "free_x") +
  labs(x = "Week",
       y = "",
       title = "Calendar Heat Map of Exercise Time in Minutes",
       caption = "Francine's Strava Data") + 
  theme_classic()
ggsave("time_calendar_heatmap.png", width = 5, height = 5)


# SPEED V. DISTANCE
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
  

## GRAB RUNS FOR PLOTTING
runs <- activities_data_c %>% 
  filter(
    type == "Run" &!is.na(start_latlng1)
    )
run_ids <- runs %>% 
  slice(1:39) %>%
  pull(id)

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

runs_walks <- activities_data_c %>% 
  filter(
    type == "Run" | type == "Walk") %>%
  filter(!is.na(start_latlng1))

runs_walks_ids <- runs_walks %>% 
  filter(start_latlng1 >= 37.5) %>% 
  pull(id)

run_dates <- runs %>%
  dplyr::select(id, start_date) %>%
  mutate(date = as.Date(str_sub(start_date, end = 10))
         )

runs_ids <- runs_walks %>% 
  filter(type == "Run") %>% 
  slice(1:10) %>%
  pull(id)
run_streams <- get_activity_streams(act_data=routes, 
                                          stoken, 
                                          #id=runs_ids, 
                                          types="latlng", 
                                          units="imperial",
                                          acts = 1)

activities_summary_polyline <- activities_data_c %>% 
  filter(!is.na(map.summary_polyline), !is.na(end_latlng1)) 
#%>%
  pull(map.summary_polyline)
  

decoded_polyline <- googlePolylines::decode(
  activities_summary_polyline$map.summary_polyline)

## ATTEMPT 2:
sfg <- lapply( decoded_polyline, function(x) sf::st_linestring( x = as.matrix(x) ) )

sfc <- sf::st_sfc( sfg )

sf <- sf::st_as_sf( cbind( activities_summary_polyline, sfc ), crs = 4326 )

activities_summary_polyline_id <- activities_summary_polyline %>% 
  rownames_to_column(var = "id_p")
unlist_polyline <- map_df(decoded_polyline, ~as.data.frame(.x), .id="id")

full_polyline_df <- unlist_polyline %>% 
  left_join(., activities_summary_polyline_id, by = c("id" = "id_p"))
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}
burmateo <- full_polyline_df %>%
  filter(lon >=-122.37 & lon <= -122.0) %>%
  filter(lat >= 37.55 & lat <= 37.65)
burmateo_sf <- sf %>% 
  filter(start_latlng1 >= 37.55 & start_latlng1 <= 37.65) %>% 
  filter(start_latlng2 >=-122.37 & start_latlng2 <= -122.0)

v_lines <- points_to_line(data = burmateo
                            #unlist_polyline
                            , 
                          long = "lon", 
                          lat = "lat", 
                          id_field = "id")

v_lines_sf <- as(v_lines, 
                 "sf") 
st_crs(v_lines_sf) <- 4326
dark_basemap <- cc_location(loc = raster::extent(v_lines_sf), 
  #zoom = 12,
  get_tiles = "https://api.mapbox.com/styles/v1/mapbox/dark-v10/tiles/{zoom}/{x}/{y}",
  access_token = "pk.eyJ1IjoiZnJhbmNpbmVzdGVwaGVucyIsImEiOiJjazhobGNramgwMDF1M2Zsa2tieHV2eDF1In0.60HwmeTBd_q8TY5yAA0kZw") 

lines_bbox <- st_as_sfc(st_bbox(v_lines_sf))
leaflet(data = v_lines_sf %>% 
          st_set_crs(4326)) %>%
  addTiles() %>%
  addPolylines()

## STATIC
mb_access_token(
  "pk.eyJ1IjoiZnJhbmNpbmVzdGVwaGVucyIsImEiOiJjazhobGNramgwMDF1M2Zsa2tieHV2eDF1In0.60HwmeTBd_q8TY5yAA0kZw", 
  install = TRUE)
readRenviron("~/.Renviron")

# PARCELS 
st_crs(parcels)
parcels_reduced <- parcels %>% 
  st_transform(., crs = 4326) %>%
  filter(city == "SAN MATEO"  | city == "BURLINGAME") %>% 
  st_intersection(., lines_bbox)

# COUNTY BACKGROUND
ca_counties <- counties("California", cb = TRUE, resolution = '20m') # 4269 epsg

smc <- ca_counties %>% 
  filter(NAME == "San Mateo") %>% 
  st_transform(., crs =4326) %>%
  st_intersection(., lines_bbox)


leaflet(data = parcels_reduced) %>% 
  addTiles() %>% 
  addPolygons() %>%
  addPolylines(data = v_lines_sf %>% 
          st_set_crs(4326), 
          color = "orange") 


tm_static_nhood <- 
  tm_static_mapbox(location = v_lines_sf %>% 
                     st_transform(7131), 
    style_url = "mapbox://styles/francinestephens/cli17a3cl00v501pohaykfiqe",
    username = "francinestephens") +
  tm_shape(v_lines_sf  %>% 
             st_transform(7131)) + 
  tm_lines(alpha = 0.4, 
           col = "steelblue") + 
  tm_layout(frame = FALSE)
tmap_save(tm_static_nhood, "static_burmateo.png", width = 5, height = 7)
tm_static_nhood

parcel_version <- 
  tm_shape(smc) + 
  tm_fill(col = "white") + 
  tm_shape(parcels_reduced) + 
  tm_fill("MAP_COLORS", palette="Greys", alpha = .25) + 
  tm_shape(v_lines_sf) + 
  tm_lines(alpha = 0.4, 
           col = "steelblue") + 
  tm_layout(frame = FALSE, bg.color = "white"
            #title = "Trips in Bur Mateo",
            #title.position = c('left', 'center'), 
            #title.fontfamily = "Georgia"
            )
parcel_version
tmap_save(parcel_version, "burmateo_parcels3.jpeg")
#width = 1584, height = 396,

#798234,#a3ad62,#d0d3a2,#fdfbe4,#f0c6c3,#df91a3,#d46780

## POINT VERSION
ggplot(
       ) +   
  geom_sf(data = parcels_reduced, col = "grey", alpha =0.8) + 
  geom_sf(data = burmateo_points_sf, 
          aes( group = id)) + 
  #geom_pointdensity(size = .8) + 
  stat_density_2d(mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
                                         y = purrr::map_dbl(geometry, ~.[2]),
                                         fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.5) +
  scale_fill_gradient2(low = "#fffdee", mid = "#d95e23", high = "#340707",
                        midpoint = 250
  ) + 
  # layer_static_mapbox(
  #                     style_url =,
  #                     username = "francinestephens") + 
  #coord_flip() + 
  labs(x = "",
       y = "") + 
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank()
        ) # '#160e16'
  #stat_pointdensity(geom = "line", size = 0.05, adjust = 10) + 
  #scale_color_gradientn(colors = c("blue", "yellow", "red")) + 
  #theme_void(base_family = "Rockwell", base_size = 13)
  # geom_path(#colour="steelblue", 
  #   alpha = 0.2, lineend = "round") 
ggsave("burmateo_points2.jpeg")

burmateo_points_sf <- burmateo %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)
  

ggplot(
) +   
  geom_sf(data = parcels_reduced, col = "grey", alpha =0.8) + 
  geom_sf(data = burmateo_sf, aes(color = average_speed)) + 
  #geom_path(aes(color = average_speed)) +
  #geom_pointdensity(size = .8) + 
  labs(x = "",
       y = "") + 
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank()
  ) # '#160e16'

#009392,#72aaa1,#b1c7b3,#f1eac8,#e5b9ad,#d98994,#d0587e
## GRIDS ##---------------------------------------------------------------------
area_fishnet_grid <- st_make_grid(burmateo_points_sf %>% 
                                    st_transform(7131), 
                                  c(50, 50), 
                                  what = "polygons", 
                                  square = T)
# To sf and add grid ID
fishnet_grid_sf <- st_sf(area_fishnet_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_fishnet_grid)))

# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
fishnet_grid_sf$n_colli <- lengths(st_intersects(fishnet_grid_sf, 
                                                 burmateo_points_sf %>% 
                                                   st_transform(7131))
                                   )

# remove grid without value of 0 (i.e. no points in side that grid)
fishnet_count <- filter(fishnet_grid_sf, n_colli > 0)

# MAP
tm_shape(parcels_reduced) + 
  tm_fill("MAP_COLORS", palette="Greys", alpha = .25) + 
tm_shape(fishnet_grid_sf) +
  tm_fill(
    col = "n_colli",
    palette = "YlOrRd",
    #style = "hclust",
    title = "",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.5
    ) + 
  tm_layout(frame = FALSE, bg.color = "white", legend.show = FALSE
            #title = "Trips in Bur Mateo",
            #title.position = c('left', 'center'), 
            #title.fontfamily = "Georgia"
  )
  

#-------------------------------------------------------------------------------

# # HEAT MAPPING
# # activity id
# id <- 4443408220
# # plot elevation along a single ride
# get_heat_map(activities_for_heat_mapping,
#              id = id,
#              alpha = 1,
#              add_elev = T,
#              f = 0.3,
#              distlab = F,
#              key = mykey,
#              size = 2,
#              col = 'Spectral',
#              maptype = 'satellite',
#              units = 'imperial')
# # plot % gradient along a single ride
# get_heat_map(activities_for_heat_mapping,
#              id = id,
#              alpha = 1,
#              add_elev = T,
#              f = 0.3,
#              distlab = F,
#              as_grad = T,
#              key = mykey,
#              size = 2,
#              col = 'Spectral',
#              expand = 5,
#              maptype = 'satellite',
#              units = 'imperial')
# 
# 
# # GET ELEVATION PROFILES
# get_elev_prof(runs, id = id, key = mykey, units = 'imperial')
