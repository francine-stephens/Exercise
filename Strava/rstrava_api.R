#-------------------------------------------------------------------------------
# STRAVA API DATA EXTRACTION & PROCESSING
# A test run of the API. Treat as template/reference for rStrava package.
# AUTHOR: Francine Stephens
# DATE CREATED:  3/7/21
# LAST UPDATED DATE: 7/4/23
#-------------------------------------------------------------------------------

## INITIALIZE-------------------------------------------------------------------

#devtools::install_github('fawda123/rStrava')
packages <- c(
  "tidyverse",
  "rStrava",
  "sp",
  "sf",
  "mapproj",
  "lubridate",
  "leaflet", 
  "maptools",
  "tmap",
  "mapboxapi",
  "tigris"
)
lapply(packages, library, character.only = T)

## KEY PARAMETERS
app_name <- '###' # chosen by user
app_client_id  <- '####' # an integer, assigned by Strava
app_secret <- '#############################' # an alphanumeric secret, assigned by Strava
wd <- getwd()
output_folder <- "/activities_downloads_repo"

## CONFIG
stoken <- httr::config(
  token = strava_oauth(
    app_name,
    app_client_id,
    app_secret,
    app_scope="activity:read_all",
    cache=TRUE)
  )


## EXTRACT DATA-----------------------------------------------------------------
# download strava data
myinfo <- get_athlete(stoken, id = '#####')

routes <- get_activity_list(stoken)
length(routes)  # GET # of activities

# CONVERT ACTIVITIES FROM LIST TO DATAFRAME
# Set units to imperial to convert to miles.
# To get a subset specify a slice as below. 
# Can also filter to get a subset. 
activities_data <- compile_activities(routes, units = "imperial") 
    # act_data <- compile_activities(routes, acts = 1:20, units = "imperial") %>% 
    #   filter(distance > 2 & total_elevation_gain > 0) 
saveRDS(activities_data, file = "strava_activities_070423.rds")

summary(activities_data)


## MANIPULATE ACTIVITIES DATA---------------------------------------------------
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

### EXTRACT POLYLINES
activities_summary_polyline <- activities_data %>% 
  filter(!is.na(map.summary_polyline), !is.na(end_latlng1)) 
  

decoded_polyline <- googlePolylines::decode(
  activities_summary_polyline$map.summary_polyline)

## CONVERT POLYLINE TO LINESTRINGS:
sfg <- lapply(decoded_polyline, function(x) sf::st_linestring( x = as.matrix(x)
                                                                ) )

sfc <- sf::st_sfc( sfg )

sf <- sf::st_as_sf( cbind( activities_summary_polyline, sfc ), crs = 4326 )

leaflet(sf) %>%
  addTiles() %>%
  addPolylines()


## POLYLINE ID POINTS
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


## PREP FOR EXPORT
v_lines_sf <- as(v_lines, 
                 "sf") 
st_crs(v_lines_sf) <- 4326

v_lines_sf <- v_lines_sf %>% 
  st_set_crs(., 4326)

crs(v_lines_sf)
leaflet(v_lines_sf) %>%
  addTiles() %>%
  addPolylines()

points_sf <- burmateo %>% 
  filter(lon >=-122.37 & lon <= -122.0) %>%
  filter(lat >= 37.55 & lat <= 37.65) %>%
  st_as_sf(.,
         coords = c("lon", "lat"), crs = 4326)

## EXPORTS
write_sf(v_lines_sf,
         paste0(wd, output_folder, "/burmateo_lines.shp"))


write_sf(points_sf,
         paste0(wd, output_folder, "/burmateo_points.shp"))
