
library(dplyr)
library(ggplot2)
library(ggmap)
library(geodist) # geodist()



###### READ IN DATA

## ev station info
path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/'
file.name <- 'intermediate_temps_stations.csv'
intermediate_ev_stations_df <- read.csv(paste0(path, file.name))
file.name <- 'cold_temps_stations.csv'
cold_ev_stations_df <- read.csv(paste0(path, file.name))
file.name <- 'warm_temps_stations.csv'
warm_ev_stations_df <- read.csv(paste0(path, file.name))

## weather station info
file.name <- 'intermediate_weather_station.csv'
intermediate_weather_stations_df <- read.csv(paste0(path, file.name))
file.name <- 'cold_weather_station.csv'
cold_weather_stations_df <- read.csv(paste0(path, file.name))
file.name <- 'warm_weather_station.csv'
warm_weather_stations_df <- read.csv(paste0(path, file.name))

rm(path, file.name) # clean up


# ### Initiate R seed
# set.seed(122)
# 
# 
# #### REALLY GOOD
# 
# station_sampled <- sample(intermediate_ev_stations_df$station_id, 1) # obtain one ev station at random
# one_station_sampled_df <- subset(intermediate_ev_stations_df,
#                                  station_id == station_sampled) # get all the ev info of that ev station
# p1 <- as.matrix(cbind(unique(one_station_sampled_df$station_latitude),
#                       unique(one_station_sampled_df$station_longitude)),
#                 ncol=2) # get the coordinates of that ev station
# 
# my.col.names <- c('lat', 'lon')
# colnames(p1) <- my.col.names
# n <- unique(intermediate_weather_stations_df$STATION) # get all the unique weather stations
# distances <- vector() # create an empty vector to store the distances from the ev station to the weather station
# corr.weather.stations <- vector() # create an empty vector to store the corresponding weather stations
# coords <- vector() # create an empty vector to store the coordinates of the weather stations
# 
# # iterate through all the weather stations, identify the distance between them and the ev station in question (in km)
# for (i in n) {
#    t <- subset(intermediate_weather_stations_df, STATION == i)
#    p2 <- as.matrix(cbind(unique(t$LATITUDE),
#                          unique(t$LONGITUDE)),
#                    ncol=2)
#    # dist <- euc.dist(p1, p2)
#    colnames(p2) <- my.col.names
#    dist <- geodist(p1, p2, measure = 'haversine')
#    dist <- dist / 1000 # convert to kilometer
#    corr.weather.stations <- append(corr.weather.stations, i)
#    distances <- append(distances, dist)
#    coords <- rbind(coords, p2)
# }
# rm(n, t, i, my.col.names) # clean up
# 
# # combine all weather stations, their coordinates, and theu distances to the ev station
# # into one dataframe
# weather_station_dists <- data.frame(dist = distances,
#                    weather_station = corr.weather.stations,
#                    coordinates = coords)
# 
# # Categorize the weather station to ev station distances into 5km increments,
# # with 'a' being within 5 km, 'b' within 10 km, etc.
# weather_station_dists$classifier <- case_when(weather_station_dists$dist <= 5 ~ 'a',
#                                               weather_station_dists$dist <= 10 ~ 'b',
#                                               weather_station_dists$dist <= 15 ~ 'c',
#                                               weather_station_dists$dist <= 20 ~ 'd',
#                                               weather_station_dists$dist <= 25 ~ 'e',
#                                               weather_station_dists$dist <= 30 ~ 'f',
#                                               weather_station_dists$dist <= 35 ~ 'g',
#                                               weather_station_dists$dist <= 40 ~ 'h',
#                                               weather_station_dists$dist <= 45 ~ 'i',
#                                               weather_station_dists$dist <= 50 ~ 'j',
#                                               TRUE ~ 'k')
# # add in the ev location
# weather_station_dists <- rbind(weather_station_dists,c(0, 1, p1[1], p1[2], 'X'))
# 
# # format disntances and coordinates
# weather_station_dists$dist <- as.numeric(weather_station_dists$dist)
# weather_station_dists$coordinates.lat <- as.numeric(weather_station_dists$coordinates.lat)
# weather_station_dists$coordinates.lon <- as.numeric(weather_station_dists$coordinates.lon)
# 
# 
# qmplot(coordinates.lon, coordinates.lat, data = weather_station_dists,
#        color=classifier, size=I(5))



# +^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^
# +^+^+^+^ PRELIMINARY
## MAP ALL WEATHER STATIONS TO EV STATION FOR OVERVIEW

### INTERMEDIATE 

ev_stations <- unique(intermediate_ev_stations_df$station_id)
my.col.names <- c('lat', 'lon')
int_weather_to_ev_map <- data.frame(ev_station=integer(),
                                ev_lat=numeric(),
                                ev_lon=numeric(),
                                weather_station=integer(),
                                weather_lat=numeric(),
                                weather_lon=numeric(),
                                distance_to_ev_km=numeric(),
                                dist_classification=character())

for (ev in ev_stations) {
   ev_df <- subset(intermediate_ev_stations_df, station_id == ev)
   p1 <- as.matrix(cbind(unique(ev_df$station_latitude),
                         unique(ev_df$station_longitude)),
                   ncol=2)
   colnames(p1) <- my.col.names
   n <- unique(intermediate_weather_stations_df$STATION)
   distances <- vector()
   corr.weather.stations <- vector()
   coords <- vector()
   
   for (weather_station in n) {
      t <- subset(intermediate_weather_stations_df, STATION==weather_station)
      p2 <- as.matrix(cbind(unique(t$LATITUDE), unique(t$LONGITUDE)), ncol=2)
      colnames(p2) <- my.col.names
      dist <- geodist(p1, p2, measure = 'vincenty')
      dist <- dist/1000
      
      distances <- append(distances, dist)
      corr.weather.stations <- append(corr.weather.stations, weather_station)
      coords <- rbind(coords, p2)
   }
   
   weather_station_dists <- data.frame(dist = distances, 
                                       weather_station = corr.weather.stations,
                                       coordinates = coords)
   
   weather_station_dists$classifier <- 
      case_when(weather_station_dists$dist <= 5 ~ 'a',
                weather_station_dists$dist <= 10 ~ 'b',
                weather_station_dists$dist <= 15 ~ 'c',
                weather_station_dists$dist <= 20 ~ 'd',
                weather_station_dists$dist <= 25 ~ 'e',
                weather_station_dists$dist <= 30 ~ 'f',
                weather_station_dists$dist <= 35 ~ 'g',
                weather_station_dists$dist <= 40 ~ 'h',
                weather_station_dists$dist <= 45 ~ 'i',
                weather_station_dists$dist <= 50 ~ 'j',
                TRUE ~ 'k')
   weather.cols.in.oder <- c('weather_station', 'coordinates.lat', 
                             'coordinates.lon', 'dist', 'classifier')
   weather_geo_df <- weather_station_dists[, weather.cols.in.oder]
   
   dims <- dim(weather_geo_df)[1]
   ev_lat <- unique(ev_df$station_latitude)
   ev_lon <- unique(ev_df$station_longitude)
   
   ev_geo_df <- cbind(rep(ev, dims), 
                      rep(ev_lat, dims), 
                      rep(ev_lon, dims))
   df <- cbind(ev_geo_df, weather_geo_df)
   int_weather_to_ev_map <- rbind(int_weather_to_ev_map, df)
}

rm(ev, ev_stations, ev_df, p1, my.col.names, n, distances, 
   corr.weather.stations, coords, weather_station, t, p2, 
   weather_station_dists, weather.cols.in.oder, dims, weather_geo_df, 
   ev_geo_df, df, dist, ev_lat, ev_lon)


colnames(int_weather_to_ev_map) <- c('ev_station', 'ev_lat', 
                                     'ev_lon', 'weather_station',
                                     'weather_lat', 'weather_lon', 
                                     'distance_to_ev_km', 
                                     'dist_classification')





### COLD

ev_stations <- unique(cold_ev_stations_df$station_id)
my.col.names <- c('lat', 'lon')
col_weather_to_ev_map <- data.frame(ev_station=integer(),
                                    ev_lat=numeric(),
                                    ev_lon=numeric(),
                                    weather_station=integer(),
                                    weather_lat=numeric(),
                                    weather_lon=numeric(),
                                    distance_to_ev_km=numeric(),
                                    dist_classification=character())

for (ev in ev_stations) {
   ev_df <- subset(cold_ev_stations_df, station_id == ev)
   p1 <- as.matrix(cbind(unique(ev_df$station_latitude),
                         unique(ev_df$station_longitude)),
                   ncol=2)
   colnames(p1) <- my.col.names
   n <- unique(cold_weather_stations_df$STATION)
   distances <- vector()
   corr.weather.stations <- vector()
   coords <- vector()
   
   for (weather_station in n) {
      t <- subset(cold_weather_stations_df, STATION==weather_station)
      p2 <- as.matrix(cbind(unique(t$LATITUDE), unique(t$LONGITUDE)), ncol=2)
      colnames(p2) <- my.col.names
      dist <- geodist(p1, p2, measure = 'vincenty')
      dist <- dist/1000
      
      distances <- append(distances, dist)
      corr.weather.stations <- append(corr.weather.stations, weather_station)
      coords <- rbind(coords, p2)
   }
   
   weather_station_dists <- data.frame(dist = distances, 
                                       weather_station = corr.weather.stations,
                                       coordinates = coords)
   
   weather_station_dists$classifier <- 
      case_when(weather_station_dists$dist <= 5 ~ 'a',
                weather_station_dists$dist <= 10 ~ 'b',
                weather_station_dists$dist <= 15 ~ 'c',
                weather_station_dists$dist <= 20 ~ 'd',
                weather_station_dists$dist <= 25 ~ 'e',
                weather_station_dists$dist <= 30 ~ 'f',
                weather_station_dists$dist <= 35 ~ 'g',
                weather_station_dists$dist <= 40 ~ 'h',
                weather_station_dists$dist <= 45 ~ 'i',
                weather_station_dists$dist <= 50 ~ 'j',
                TRUE ~ 'k')
   weather.cols.in.oder <- c('weather_station', 'coordinates.lat', 
                             'coordinates.lon', 'dist', 'classifier')
   weather_geo_df <- weather_station_dists[, weather.cols.in.oder]
   
   dims <- dim(weather_geo_df)[1]
   ev_lat <- unique(ev_df$station_latitude)
   ev_lon <- unique(ev_df$station_longitude)
   
   ev_geo_df <- cbind(rep(ev, dims), 
                      rep(ev_lat, dims), 
                      rep(ev_lon, dims))
   df <- cbind(ev_geo_df, weather_geo_df)
   col_weather_to_ev_map <- rbind(col_weather_to_ev_map, df)
}

rm(ev, ev_stations, ev_df, p1, my.col.names, n, distances, 
   corr.weather.stations, coords, weather_station, t, p2, 
   weather_station_dists, weather.cols.in.oder, dims, weather_geo_df, 
   ev_geo_df, df, dist, ev_lat, ev_lon)


colnames(col_weather_to_ev_map) <- c('ev_station', 'ev_lat', 'ev_lon', 
                                     'weather_station','weather_lat', 
                                     'weather_lon', 'distance_to_ev_km', 
                                     'dist_classification')




### WARM

ev_stations <- unique(warm_ev_stations_df$station_id)
my.col.names <- c('lat', 'lon')
war_weather_to_ev_map <- data.frame(ev_station=integer(),
                                    ev_lat=numeric(),
                                    ev_lon=numeric(),
                                    weather_station=integer(),
                                    weather_lat=numeric(),
                                    weather_lon=numeric(),
                                    distance_to_ev_km=numeric(),
                                    dist_classification=character())

for (ev in ev_stations) {
   ev_df <- subset(warm_ev_stations_df, station_id == ev)
   p1 <- as.matrix(cbind(unique(ev_df$station_latitude),
                         unique(ev_df$station_longitude)),
                   ncol=2)
   colnames(p1) <- my.col.names
   n <- unique(warm_weather_stations_df$STATION)
   distances <- vector()
   corr.weather.stations <- vector()
   coords <- vector()
   
   for (weather_station in n) {
      t <- subset(warm_weather_stations_df, STATION==weather_station)
      p2 <- as.matrix(cbind(unique(t$LATITUDE), unique(t$LONGITUDE)), ncol=2)
      colnames(p2) <- my.col.names
      dist <- geodist(p1, p2, measure = 'vincenty')
      dist <- dist/1000
      
      distances <- append(distances, dist)
      corr.weather.stations <- append(corr.weather.stations, weather_station)
      coords <- rbind(coords, p2)
   }
   
   weather_station_dists <- data.frame(dist = distances, 
                                       weather_station = corr.weather.stations,
                                       coordinates = coords)
   
   weather_station_dists$classifier <- 
      case_when(weather_station_dists$dist <= 5 ~ 'a',
                weather_station_dists$dist <= 10 ~ 'b',
                weather_station_dists$dist <= 15 ~ 'c',
                weather_station_dists$dist <= 20 ~ 'd',
                weather_station_dists$dist <= 25 ~ 'e',
                weather_station_dists$dist <= 30 ~ 'f',
                weather_station_dists$dist <= 35 ~ 'g',
                weather_station_dists$dist <= 40 ~ 'h',
                weather_station_dists$dist <= 45 ~ 'i',
                weather_station_dists$dist <= 50 ~ 'j',
                TRUE ~ 'k')
   weather.cols.in.oder <- c('weather_station', 'coordinates.lat', 
                             'coordinates.lon', 'dist', 'classifier')
   weather_geo_df <- weather_station_dists[, weather.cols.in.oder]
   
   dims <- dim(weather_geo_df)[1]
   ev_lat <- unique(ev_df$station_latitude)
   ev_lon <- unique(ev_df$station_longitude)
   
   ev_geo_df <- cbind(rep(ev, dims), 
                      rep(ev_lat, dims), 
                      rep(ev_lon, dims))
   df <- cbind(ev_geo_df, weather_geo_df)
   war_weather_to_ev_map <- rbind(war_weather_to_ev_map, df)
}

rm(ev, ev_stations, ev_df, p1, my.col.names, n, distances, 
   corr.weather.stations, coords, weather_station, t, p2, 
   weather_station_dists, weather.cols.in.oder, dims, weather_geo_df, 
   ev_geo_df, df, dist, ev_lat, ev_lon)


colnames(war_weather_to_ev_map) <- c('ev_station', 'ev_lat', 'ev_lon', 
                                     'weather_station', 'weather_lat', 
                                     'weather_lon', 'distance_to_ev_km', 
                                     'dist_classification')



# Below archived.. used to a sense of the preliminary mapping 
# dim(int_weather_to_ev_map)
# dim(col_weather_to_ev_map)
# dim(war_weather_to_ev_map)
# 
# # qplot(int_weather_to_ev_map$distance_to_ev_km, geom='histogram')
# 
# g <- ggplot(int_weather_to_ev_map, aes(x=distance_to_ev_km, 
#                                        col=dist_classification,
#                                        fill=dist_classification))
# g + geom_histogram(bins=50) +
#    labs(title = 'Distribution of distances (km) between weather stations and ev station',
#         subtitle = 'Intermediate temperatures',
#         x='Distance (km)', y='Count')
# 
# g <- ggplot(col_weather_to_ev_map, aes(x=distance_to_ev_km, 
#                                        col=dist_classification,
#                                        fill=dist_classification))
# g + geom_histogram(bins=50) +
#    labs(title = 'Distribution of distances (km) between weather stations and ev station',
#         subtitle = 'Cold temperatures',
#         x='Distance (km)', y='Count')
# 
# g <- ggplot(war_weather_to_ev_map, aes(x=distance_to_ev_km, 
#                                        col=dist_classification,
#                                        fill=dist_classification))
# g + geom_histogram(bins=50) +
#    labs(title = 'Distribution of distances (km) between weather stations and ev station',
#         subtitle = 'Warm temperatures',
#         x='Distance (km)', y='Count')
# 
# 
# # check out where most ev stations lie in terms of distances to weather stations
# head(int_weather_to_ev_map)
# 
# 
# int_station_distrs <- int_weather_to_ev_map %>% 
#    group_by(ev_station, dist_classification) %>% 
#    summarize(counts = n())
# 
# # This gives me an idea of the radius I should be using to attach a weather
# # stations' temperature recordings to an ev station.. it looks like most 
# # ev stations are within 5km of a weather station and there's a few that are
# # slightly farther and are within 25km of a weather stations
# g <- ggplot(data = int_station_distrs, aes(x=counts, 
#                                            col=dist_classification,
#                                            fill=dist_classification))
# g + geom_histogram() + labs(title = 'Distribution of ev-weather station distances among stratified by classification (5km increments)',
#                             subtitle = 'Intermediate weather', x='distances')
# 
# 
# cold_station_distrs <- col_weather_to_ev_map %>% 
#    group_by(ev_station, dist_classification) %>% 
#    summarize(counts = n())
# 
# # it looks like most ev stations are within 5km of a weather station but there's 
# # a few that are farther and are within 40-50km of a weather station
# g <- ggplot(data = cold_station_distrs, aes(x=counts, 
#                                            col=dist_classification,
#                                            fill=dist_classification))
# g + geom_histogram() + labs(title = 'Distribution of ev-weather station distances among stratified by classification (5km increments)',
#                             subtitle = 'Cold weather', x='distances')
# 
# 
# warm_station_distrs <- war_weather_to_ev_map %>% 
#    group_by(ev_station, dist_classification) %>% 
#    summarize(counts = n())
# 
# # it looks like most ev stations are within 5-10km of a weather station but there's 
# # a few that are farther and are within 45-55km of a weather station
# g <- ggplot(data = warm_station_distrs, aes(x=counts, 
#                                             col=dist_classification,
#                                             fill=dist_classification))
# g + geom_histogram() + labs(title = 'Distribution of ev-weather station distances among stratified by classification (5km increments)',
#                             subtitle = 'Warm weather', x='distances')





# +^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^+^
# +^+^+^+^ MERGE WEATHER DATA AND EV STATIONS - MERGE IT ALL



# one_station_mapped <- subset(int_weather_to_ev_map, ev_station == 269)
# 
# closest_weather_station <- one_station_mapped %>%
#    arrange(distance_to_ev_km) %>% 
#    select(weather_station) %>% 
#    head(1)
# closest_weather_station <- as.numeric(as.character(closest_weather_station))
# 
# one_station_ev_info <- subset(intermediate_ev_stations_df, station_id == 269)
# one_station_ev_info$flowdate <- as.Date(one_station_ev_info$flowdate, '%Y-%m-%d')
# one_station_weather_info <- subset(intermediate_weather_stations_df, 
#                                    STATION==closest_weather_station)
# one_station_weather_info$DATE <- as.Date(one_station_weather_info$DATE, '%Y-%m-%d')
# 
# ev_start <- min(one_station_ev_info$flowdate)
# weather_start <- min(one_station_weather_info$DATE)
# 
# ev_end <- max(one_station_ev_info$flowdate)
# weather_end <- max(one_station_weather_info$DATE)
# 
# start <- max(c(ev_start, weather_start))
# end <- min(c(ev_end, weather_end))
# 
# start <- as.Date(start, '%Y-%m-%d')
# end <- as.Date(end, '%Y-%m-%d')
# date_list <- seq(start, end, 1)
# 
# 
# # head(one_station_weather_info)
# # length(unique(one_station_weather_info$DATE))
# 
# temps_to_join <- one_station_weather_info[which(one_station_weather_info$DATE %in% date_list), ][['TEMP']]
# 
# one_station_ev_info$temps <- temps_to_join




ev_stations <- unique(intermediate_ev_stations_df$station_id)
intermediate_ev_stations_df$temps <- NA
intermediate_ev_stations_df$distance_classification <- NA
intermediate_ev_stations_df$dist_km <- NA

for (ev in ev_stations) {
   one_station_mapped <- subset(int_weather_to_ev_map, ev_station == ev)
   
   closest_weather_station <- one_station_mapped %>% 
      arrange(distance_to_ev_km) %>% select(weather_station) %>% head(1)
   closest_weather_station <- 
      as.numeric(as.character(closest_weather_station))
   
   closest_weather_station_class <- one_station_mapped %>% 
      arrange(distance_to_ev_km) %>% select(dist_classification) %>% head(1)
   closest_weather_station_class <- 
      as.character(closest_weather_station_class)

   closest_weather_station_dist <- one_station_mapped %>% 
      arrange(distance_to_ev_km) %>% select(distance_to_ev_km) %>% head(1)
   closest_weather_station_dist <- 
      as.numeric(as.character(closest_weather_station_dist))
   
   
   one_station_ev_info <- 
      subset(intermediate_ev_stations_df, station_id == ev)
   one_station_ev_info$flowdate <- 
      as.Date(one_station_ev_info$flowdate, '%Y-%m-%d')
   one_station_weather_info <- 
      subset(intermediate_weather_stations_df, STATION==closest_weather_station)
   one_station_weather_info$DATE <- 
      as.Date(one_station_weather_info$DATE, '%Y-%m-%d')

   ev_start <- min(one_station_ev_info$flowdate)
   weather_start <- min(one_station_weather_info$DATE)
   
   ev_end <- max(one_station_ev_info$flowdate)
   weather_end <- max(one_station_weather_info$DATE)
   
   start <- max(c(ev_start, weather_start))
   end <- min(c(ev_end, weather_end))
   
   start <- as.Date(start, '%Y-%m-%d')
   end <- as.Date(end, '%Y-%m-%d')
   date_list <- seq(start, end, 1)
   classification_list <- 
      rep(closest_weather_station_class, length(date_list))
   dist_list <- rep(closest_weather_station_dist, length(date_list))
   temps_to_join <- 
      one_station_weather_info[which(one_station_weather_info$DATE %in% date_list), ][['TEMP']]
   
   intermediate_ev_stations_df[which(intermediate_ev_stations_df$station_id==ev), ]$temps <- 
      temps_to_join
   intermediate_ev_stations_df[which(intermediate_ev_stations_df$station_id==ev), ]$distance_classification <- 
      classification_list
   intermediate_ev_stations_df[which(intermediate_ev_stations_df$station_id==ev), ]$dist_km <- 
      dist_list
}

rm(classification_list, closest_weather_station, 
   closest_weather_station_class, closest_weather_station_dist, 
   date_list, dist_list, end, ev, ev_end, ev_start,
   ev_stations, start, temps_to_join, weather_end, weather_start)

### COLD

ev_stations <- unique(cold_ev_stations_df$station_id)
cold_ev_stations_df$temps <- NA
cold_ev_stations_df$distance_classification <- NA
cold_ev_stations_df$dist_km <- NA

for (ev in ev_stations) {
   one_station_mapped <- subset(col_weather_to_ev_map, ev_station == ev)
   
   closest_weather_station <- one_station_mapped %>% 
      arrange(distance_to_ev_km) %>% select(weather_station) %>% head(1)
   closest_weather_station <- 
      as.numeric(as.character(closest_weather_station))
   
   closest_weather_station_class <- 
      one_station_mapped %>% arrange(distance_to_ev_km) %>% 
      select(dist_classification) %>% head(1)
   closest_weather_station_class <- 
      as.character(closest_weather_station_class)
   
   closest_weather_station_dist <- one_station_mapped %>% 
      arrange(distance_to_ev_km) %>% select(distance_to_ev_km) %>% head(1)
   closest_weather_station_dist <- 
      as.numeric(as.character(closest_weather_station_dist))
   
   
   one_station_ev_info <- subset(cold_ev_stations_df, station_id == ev)
   one_station_ev_info$flowdate <- 
      as.Date(one_station_ev_info$flowdate, '%Y-%m-%d')
   one_station_weather_info <- subset(cold_weather_stations_df,
                                      STATION==closest_weather_station)
   one_station_weather_info$DATE <- 
      as.Date(one_station_weather_info$DATE, '%Y-%m-%d')
   
   ev_start <- min(one_station_ev_info$flowdate)
   weather_start <- min(one_station_weather_info$DATE)
   
   ev_end <- max(one_station_ev_info$flowdate)
   weather_end <- max(one_station_weather_info$DATE)
   
   start <- max(c(ev_start, weather_start))
   end <- min(c(ev_end, weather_end))
   
   start <- as.Date(start, '%Y-%m-%d')
   end <- as.Date(end, '%Y-%m-%d')
   date_list <- seq(start, end, 1)
   classification_list <- 
      rep(closest_weather_station_class, length(date_list))
   dist_list <- rep(closest_weather_station_dist, length(date_list))
   temps_to_join <- 
      one_station_weather_info[which(one_station_weather_info$DATE %in% date_list), ][['TEMP']]
   
   cold_ev_stations_df[which(cold_ev_stations_df$station_id==ev), ]$temps <- 
      temps_to_join
   cold_ev_stations_df[which(cold_ev_stations_df$station_id==ev), ]$distance_classification <- 
      classification_list
   cold_ev_stations_df[which(cold_ev_stations_df$station_id==ev), ]$dist_km <- 
      dist_list
}

rm(classification_list, closest_weather_station, closest_weather_station_class, 
   closest_weather_station_dist, date_list, dist_list, end, ev, ev_end, ev_start,
   ev_stations, start, temps_to_join, weather_end, weather_start)


## WARM

ev_stations <- unique(warm_ev_stations_df$station_id)
warm_ev_stations_df$temps <- NA
warm_ev_stations_df$distance_classification <- NA
warm_ev_stations_df$dist_km <- NA

for (ev in ev_stations) {
   one_station_mapped <- subset(war_weather_to_ev_map, ev_station == ev)
   
   closest_weather_station <- one_station_mapped %>% 
      arrange(distance_to_ev_km) %>% select(weather_station) %>% head(1)
   closest_weather_station <- 
      as.numeric(as.character(closest_weather_station))
   
   closest_weather_station_class <- 
      one_station_mapped %>% arrange(distance_to_ev_km) %>% 
      select(dist_classification) %>% head(1)
   closest_weather_station_class <- 
      as.character(closest_weather_station_class)
   
   closest_weather_station_dist <- one_station_mapped %>% 
      arrange(distance_to_ev_km) %>% select(distance_to_ev_km) %>% head(1)
   closest_weather_station_dist <- 
      as.numeric(as.character(closest_weather_station_dist))
   
   
   one_station_ev_info <- subset(warm_ev_stations_df, station_id == ev)
   one_station_ev_info$flowdate <- 
      as.Date(one_station_ev_info$flowdate, '%Y-%m-%d')
   one_station_weather_info <- 
      subset(warm_weather_stations_df, STATION==closest_weather_station)
   #im here -- 72205012815
   one_station_weather_info$DATE <- 
      as.Date(one_station_weather_info$DATE, '%Y-%m-%d')
   
   ev_start <- min(one_station_ev_info$flowdate)
   weather_start <- min(one_station_weather_info$DATE)
   
   ev_end <- max(one_station_ev_info$flowdate)
   weather_end <- max(one_station_weather_info$DATE)
   
   start <- max(c(ev_start, weather_start))
   end <- min(c(ev_end, weather_end))
   
   start <- as.Date(start, '%Y-%m-%d')
   end <- as.Date(end, '%Y-%m-%d')
   date_list <- seq(start, end, 1)
   classification_list <- rep(closest_weather_station_class, length(date_list))
   dist_list <- rep(closest_weather_station_dist, length(date_list))
   temps_to_join <- 
      one_station_weather_info[which(one_station_weather_info$DATE %in% date_list), ][['TEMP']]
   
   warm_ev_stations_df[which(warm_ev_stations_df$station_id==ev), ]$temps <- 
      temps_to_join
   warm_ev_stations_df[which(warm_ev_stations_df$station_id==ev), ]$distance_classification <- 
      classification_list
   warm_ev_stations_df[which(warm_ev_stations_df$station_id==ev), ]$dist_km <- 
      dist_list
}

rm(classification_list, closest_weather_station, closest_weather_station_class, 
   closest_weather_station_dist, date_list, dist_list, end, ev, 
   ev_end, ev_start, ev_stations, start, temps_to_join, 
   weather_end, weather_start)

summary(intermediate_ev_stations_df)

hist(intermediate_ev_stations_df$dist_km)
hist(cold_ev_stations_df$dist_km)

warm_weather_stations_df$DATE <- 
   as.Date(warm_weather_stations_df$DATE, '%Y-%m-%d')




# causing the hold up
# ev = 482
# weather station = 72236192808
# plot(x=seq(1,dim(subset(warm_weather_stations_df, STATION == 72236192808))[1], by=1), 
#      y=subset(warm_weather_stations_df, STATION == 72236192808)[['TEMP']])
# 
# plot(x=subset(warm_weather_stations_df, STATION == 72236192808)['DATE'], 
#      y=subset(warm_weather_stations_df, STATION == 72236192808)[['TEMP']])





##

g <- ggplot(warm_ev_stations_df, aes(x=temps, 
                                       col=distance_classification,
                                       fill=distance_classification))
g + geom_histogram(bins=50) +
   labs(title = "Distribution of ev's temperatures (F)",
        subtitle = 'Warm temperatures',
        x='Fahrenheit', y='Count')

g <- ggplot(cold_ev_stations_df, aes(x=temps, 
                                     col=distance_classification,
                                     fill=distance_classification))
g + geom_histogram(bins=50) +
   labs(title = "Distribution of ev's temperatures (F)",
        subtitle = 'Cold temperatures',
        x='Fahrenheit', y='Count')



g <- ggplot(intermediate_ev_stations_df, aes(x=temps, 
                                     col=distance_classification,
                                     fill=distance_classification))
g + geom_histogram(bins=50) +
   labs(title = "Distribution of ev's temperatures (F)",
        subtitle = 'Intermediary temperatures',
        x='Fahrenheit', y='Count')





##


######### EXPORT FINAL DATA FOR ANALYSIS & MODELING

export_path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/final_data/'
file.name <- 'intermediary_EW_info.csv'
write.csv(intermediate_ev_stations_df, paste0(export_path, file.name))
file.name <- 'cold_EW_info.csv'
write.csv(cold_ev_stations_df, paste0(export_path, file.name))
file.name <- 'warm_EW_info.csv'
write.csv(warm_ev_stations_df, paste0(export_path, file.name))
rm(export_path, file.name)



stop.here;




























# ALL ARCHIVED BELOW 


# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)


# path_to_key <- 'C:/Users/francisco.avalos/Documents'
# api <- readLines(path_to_key)
# register_google(key = api)
# rm(path_to_key, api)


# plot(x=one_station_sampled_df$station_latitude, 
#      y=one_station_sampled_df$station_longitude)
# euc.dist <- function(x1, x2) sqrt(sum(x1-x2)^2)
# euc.dist(p1, p2)



# qmplot(LONGITUDE, LATITUDE, 
#        data = intermediate_weather_stations_df, color=I('red'))
# qmplot(LONGITUDE, LATITUDE, 
#        data = cold_weather_stations_df, color=I('red'))
# qmplot(LONGITUDE, LATITUDE, 
#        data = warm_weather_stations_df, color=I('red'))
# 
# 
# qmplot(station_longitude, station_latitude, 
#        data=intermediate_ev_stations_df, color=I('red'))
# qmplot(station_longitude, station_latitude, 
#        data=cold_ev_stations_df, color=I('red'))
# qmplot(station_longitude, station_latitude, 
#        data=warm_ev_stations_df, color=I('red'))



# type <- rep('EV', nrow(intermediate_ev_stations_df))
# type2 <- rep('Weather', nrow(intermediate_weather_stations_df))
# 
# ev_stations_locations <- 
#    cbind(type, intermediate_ev_stations_df[, c('station_latitude', 'station_longitude')])
# colnames(ev_stations_locations) <- c('TYPE', 'LATITUDE', 'LONGITUDE')
# weather_stations_locations <- 
#    cbind(type2, intermediate_weather_stations_df[, c('LATITUDE', 'LONGITUDE')])
# colnames(weather_stations_locations) <- c('TYPE', 'LATITUDE', 'LONGITUDE')
# intermediate_proximity_df <- rbind(ev_stations_locations, 
#                                    weather_stations_locations)
# 
# rm(type, type2, ev_stations_locations, weather_stations_locations) # clean up
# 
# qmplot(LONGITUDE, LATITUDE, data = intermediate_proximity_df, 
#        color=TYPE)


# type <- rep('EV', nrow(cold_ev_stations_df))
# type2 <- rep('Weather', nrow(cold_weather_stations_df))
# 
# ev_stations_locations <- 
#    cbind(type, cold_ev_stations_df[, c('station_latitude', 'station_longitude')])
# colnames(ev_stations_locations) <- c('TYPE', 'LATITUDE', 'LONGITUDE')
# weather_stations_locations <- 
#    cbind(type2, cold_weather_stations_df[, c('LATITUDE', 'LONGITUDE')])
# colnames(weather_stations_locations) <- c('TYPE', 'LATITUDE', 'LONGITUDE')
# cold_proximity_df <- rbind(ev_stations_locations, 
#                            weather_stations_locations)
# rm(type, type2, ev_stations_locations, weather_stations_locations)
# 
# qmplot(LONGITUDE, LATITUDE, data = cold_proximity_df,color=TYPE)


# type <- rep('EV', nrow(warm_ev_stations_df))
# type2 <- rep('Weather', nrow(warm_weather_stations_df))
# 
# ev_stations_locations <- 
#    cbind(type, warm_ev_stations_df[, c('station_latitude', 'station_longitude')])
# colnames(ev_stations_locations) <- c('TYPE', 'LATITUDE', 'LONGITUDE')
# weather_stations_locations <- 
#    cbind(type2, warm_weather_stations_df[, c('LATITUDE', 'LONGITUDE')])
# colnames(weather_stations_locations) <- c('TYPE', 'LATITUDE', 'LONGITUDE')
# warm_proximity_df <- rbind(ev_stations_locations, weather_stations_locations)
# 
# rm(type, type2, ev_stations_locations, weather_stations_locations) # clean up
# 
# qmplot(LONGITUDE, LATITUDE, data = warm_proximity_df, color=TYPE)