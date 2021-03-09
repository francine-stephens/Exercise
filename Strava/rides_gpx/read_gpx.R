# AUTHOR: Francine Stephens
# DATE CREATED: 3/6/21
# LAST UPDATED: 3/7/21
# ABOUT: Code imports track waypoints from multiple GPX files in a folder,
# then extracts coordinates from each file into a single dataframe, and 
# exports them to an .rds file format. 


## INITIALIZE-------------------------------------------------------------------

setwd("~/Shapefile_Repository/Strava/rides_gpx")

library(plotKML)
library(dplyr)


files <- list.files(pattern = ".gpx") #List all filenames in folder in gpx format

wpfull <- NULL # Set dataframe for converting gpx data to dataframe


## Function to get next track point position coordinates
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }


## BUILD DATASET OF ALL TRACK WAYPOINTS-----------------------------------------
for (i in 1:length(files)) {
  # read in each file and import data into R object
  wplist <- readGPX(files[i],
                    metadata=F,
                    waypoints=F,
                    tracks=T,
                    routes=F)
  # extract latitude, longitude, elevation, time, and extensions and append to dataframe
  wpdf <- wplist$tracks[[1]][[1]]
  # shift vectors for lat and lon so that each row also contains the next position.
  wpdf$lat_next_pos <- shift.vec(wpdf$lat, -1)
  wpdf$lon_next_pos <- shift.vec(wpdf$lon, -1)
  # add column for file name
  wpdf$ride_id <- gsub("*.gpx$", "", files[i])
  # append dataframe from last index to a full track waypoint object
  wpfull <- bind_rows(wpfull, wpdf)
}

## EXPORT OBJECT WITH ALL TRACK WAYPOINTS
saveRDS(wpfull, "all_rides.rds") 
