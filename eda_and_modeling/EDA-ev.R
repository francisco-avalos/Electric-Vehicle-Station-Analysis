
library(dplyr)
library(ggplot2)
library(ggmap)
library(imputeTS)
library(car)
library(sf)
library(sp)
library(raster)
library(gstat)
library(ggrepel)
library(ggvoronoi)
library(geodist)




# created formulas

### function for mean time between failures
mean_x_between_issues <- function(num, denom) {
  return(num/denom)
}

check_null <- function(x) {
  output <- is.na(x)
  return(output)
}



add_delta <- function(x) {
  output <- x + sample(seq(from=1/75, to=1/50, by=0.00001), 1)
  return(output)
}

check_null <- function(x) {
  output <- is.na(x)
  return(output)
}

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

standardize <- function(x) {
  return((x - mean(x)) / sd(x))
}

### function for mean time between failures
mean_x_between_issues <- function(num, denom) {
  return(num/denom)
}

add_delta <- function(x) {
  output <- x + sample(seq(from=1/75, to=1/50, by=0.00001), 1)
  return(output)
}

check_null <- function(x) {
  output <- is.na(x)
  return(output)
}



path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/final_data/'
file.name <- 'intermediary_EW_info.csv'
im_df <- read.csv(paste0(path, file.name))
file.name <- 'cold_EW_info.csv'
co_df <- read.csv(paste0(path, file.name))
file.name <- 'warm_EW_info.csv'
wa_df <- read.csv(paste0(path, file.name))

rm(path, file.name)

# Structure fields for imported data
co_df$flowdate <- as.Date(co_df$flowdate, '%Y-%m-%d')
co_df$im_date <- as.Date(co_df$im_date, '%Y-%m-%d')
co_df$im_start <- strptime(co_df$im_start, format = '%Y-%m-%d %H:%M:%S')
co_df$single_im_day <- as.Date(co_df$single_im_day, '%Y-%m-%d')
co_df$im_end<- strptime(co_df$im_end, format = '%Y-%m-%d %H:%M:%S')
co_df$session_start_date <- as.Date(co_df$session_start_date, '%Y-%m-%d')
co_df$start_period <- as.Date(co_df$start_period, '%Y-%m-%d')
co_df$charger_company_col <- factor(co_df$charger_company_col)
co_df$target_start_date <- as.Date(co_df$target_start_date, '%Y-%m-%d')
co_df$target_end_date <- as.Date(co_df$target_end_date, '%Y-%m-%d')
co_df$im_flag <- factor(co_df$im_flag)
co_df$station_instances <- factor(co_df$station_instances)
co_df$station_intances_adj <- factor(co_df$station_intances_adj)
co_df$distance_classification <- factor(co_df$distance_classification)


colnames(im_df) <- colnames(co_df)
im_df$flowdate <- as.Date(im_df$flowdate, '%Y-%m-%d')
im_df$im_date <- as.Date(im_df$im_date, '%Y-%m-%d')
im_df$im_start <- strptime(im_df$im_start, format = '%Y-%m-%d %H:%M:%S')
im_df$single_im_day <- as.Date(im_df$single_im_day, '%Y-%m-%d')
im_df$im_end<- strptime(im_df$im_end, format = '%Y-%m-%d %H:%M:%S')
im_df$session_start_date <- as.Date(im_df$session_start_date, '%Y-%m-%d')
im_df$start_period <- as.Date(im_df$start_period, '%Y-%m-%d')
im_df$charger_company_col <- factor(im_df$charger_company_col)
im_df$target_start_date <- as.Date(im_df$target_start_date, '%Y-%m-%d')
im_df$target_end_date <- as.Date(im_df$target_end_date, '%Y-%m-%d')
im_df$im_flag <- factor(im_df$im_flag)
im_df$station_instances <- as.factor(im_df$station_instances)
im_df$station_intances_adj <- factor(im_df$station_intances_adj)
im_df$distance_classification <- factor(im_df$distance_classification)

wa_df$flowdate <- as.Date(wa_df$flowdate, '%Y-%m-%d')
wa_df$im_date <- as.Date(wa_df$im_date, '%Y-%m-%d')
wa_df$im_start <- strptime(wa_df$im_start, format = '%Y-%m-%d %H:%M:%S')
wa_df$single_im_day <- as.Date(wa_df$single_im_day, '%Y-%m-%d')
wa_df$im_end<- strptime(wa_df$im_end, format = '%Y-%m-%d %H:%M:%S')
wa_df$session_start_date <- as.Date(wa_df$session_start_date, '%Y-%m-%d')
wa_df$start_period <- as.Date(wa_df$start_period, '%Y-%m-%d')
wa_df$charger_company_col <- factor(wa_df$charger_company_col)
wa_df$target_start_date <- as.Date(wa_df$target_start_date, '%Y-%m-%d')
wa_df$target_end_date <- as.Date(wa_df$target_end_date, '%Y-%m-%d')
wa_df$im_flag <- factor(wa_df$im_flag)
wa_df$station_instances <- factor(wa_df$station_instances)
wa_df$station_intances_adj <- factor(wa_df$station_intances_adj)
wa_df$distance_classification <- factor(wa_df$distance_classification)


## two ev stations to remove from the intermediate data due to minutes_diff anomaly
im_df <- im_df[which(!(im_df$station_id %in% c(682, 4507))), ]


# max(im_df$minutes_diff, na.rm = TRUE)
# max(co_df$minutes_diff, na.rm = TRUE)
# max(wa_df$minutes_diff, na.rm = TRUE)
# 
# min(im_df$minutes_diff, na.rm = TRUE)
# min(co_df$minutes_diff, na.rm = TRUE)
# min(wa_df$minutes_diff, na.rm = TRUE)


colnames(im_df) <- colnames(co_df)
data.df <- rbind(im_df, co_df, wa_df)
q1 <- summary(data.df$temps)[[2]]
q2 <- summary(data.df$temps)[[3]]
q3 <- summary(data.df$temps)[[5]]

data.df$temp_class <- case_when(data.df$temps <= q1 ~ 'A',
                                data.df$temps <= q2 ~ 'B',
                                data.df$temps <= q3 ~ 'C',
                                TRUE ~ 'D')


q1 <- summary(im_df$temps)[[2]]
q2 <- summary(im_df$temps)[[3]]
q3 <- summary(im_df$temps)[[5]]

im_df$temp_class <- case_when(im_df$temps <= q1 ~ 'A',
                              im_df$temps <= q2 ~ 'B',
                              im_df$temps <= q3 ~ 'C',
                              TRUE ~ 'D')

q1 <- summary(wa_df$temps)[[2]]
q2 <- summary(wa_df$temps)[[3]]
q3 <- summary(wa_df$temps)[[5]]

wa_df$temp_class <- case_when(wa_df$temps <= q1 ~ 'A',
                              wa_df$temps <= q2 ~ 'B',
                              wa_df$temps <= q3 ~ 'C',
                              TRUE ~ 'D')

q1 <- summary(co_df$temps)[[2]]
q2 <- summary(co_df$temps)[[3]]
q3 <- summary(co_df$temps)[[5]]

co_df$temp_class <- case_when(co_df$temps <= q1 ~ 'A',
                              co_df$temps <= q2 ~ 'B',
                              co_df$temps <= q3 ~ 'C',
                              TRUE ~ 'D')
rm(q1, q2, q3)


co_df$temp_class <- factor(co_df$temp_class)
im_df$temp_class <- factor(im_df$temp_class)
wa_df$temp_class <- factor(wa_df$temp_class)


co_df$uptime_minutes <- 1440 - ifelse(is.na(co_df$minutes_diff), 0, co_df$minutes_diff)
co_df$uptime_hours <- round(co_df$uptime_minutes/60)
co_df$minutes_diff <- ifelse(is.na(co_df$minutes_diff), 0, co_df$minutes_diff)
co_df$temp_region <- 'B'

wa_df$uptime_minutes <- 1440 - ifelse(is.na(wa_df$minutes_diff), 0, wa_df$minutes_diff)
wa_df$uptime_hours <- round(wa_df$uptime_minutes/60)
wa_df$minutes_diff <- ifelse(is.na(wa_df$minutes_diff), 0, wa_df$minutes_diff)
wa_df$temp_region <- 'C'

im_df$uptime_minutes <- 1440 - ifelse(is.na(im_df$minutes_diff), 0, im_df$minutes_diff)
im_df$uptime_hours <- round(im_df$uptime_minutes/60)
im_df$minutes_diff <- ifelse(is.na(im_df$minutes_diff), 0, im_df$minutes_diff)
im_df$temp_region <- 'A'


set.seed(122)
## Identify stations that share the same coordinate location and add delta to 
## uniquely identify them

##
stations_w_shared_coords <- im_df %>% 
  group_by(x_axis, y_axis) %>% 
  summarize(same_coords_stations = n_distinct(station_id), 
            toString(sort(unique(station_id)))) %>% 
  filter(same_coords_stations > 1)

list_of_stations_w_shared_coords <- 
  merge(im_df, stations_w_shared_coords, by=c('x_axis', 'y_axis')) %>% 
  distinct(station_id, x_axis, y_axis) %>% 
  dplyr::select(station_id, x_axis, y_axis)

list_of_stations_w_shared_coords$x_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$x_axis, add_delta)
list_of_stations_w_shared_coords$y_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$y_axis, add_delta)

im_df$x_axis_unique <- merge(im_df, list_of_stations_w_shared_coords,
                             by='station_id', all.x = TRUE)$x_axis_unique
im_df$y_axis_unique <- merge(im_df, list_of_stations_w_shared_coords,
                             by='station_id', all.x = TRUE)$y_axis_unique
update_stations <- 
  unique(im_df[which(sapply(im_df$x_axis_unique, check_null)), ]$station_id)

im_df[which(im_df$station_id %in% update_stations), ]$x_axis_unique <- 
  im_df[which(sapply(im_df$x_axis_unique, check_null)), ]$x_axis
im_df[which(im_df$station_id %in% update_stations), ]$y_axis_unique <- 
  im_df[which(sapply(im_df$y_axis_unique, check_null)), ]$y_axis


###
stations_w_shared_coords <- wa_df %>% 
  group_by(x_axis, y_axis) %>% 
  summarize(same_coords_stations = n_distinct(station_id), 
            toString(sort(unique(station_id)))) %>% 
  filter(same_coords_stations > 1)

list_of_stations_w_shared_coords <- 
  merge(wa_df, stations_w_shared_coords, by=c('x_axis', 'y_axis')) %>% 
  distinct(station_id, x_axis, y_axis) %>% 
  dplyr::select(station_id, x_axis, y_axis)

list_of_stations_w_shared_coords$x_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$x_axis, add_delta)
list_of_stations_w_shared_coords$y_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$y_axis, add_delta)

wa_df$x_axis_unique <- merge(wa_df, list_of_stations_w_shared_coords,
                             by='station_id', all.x = TRUE)$x_axis_unique
wa_df$y_axis_unique <- merge(wa_df, list_of_stations_w_shared_coords,
                             by='station_id', all.x = TRUE)$y_axis_unique

update_stations <- 
  unique(wa_df[which(sapply(wa_df$x_axis_unique, check_null)), ]$station_id)

wa_df[which(wa_df$station_id %in% update_stations), ]$x_axis_unique <- 
  wa_df[which(sapply(wa_df$x_axis_unique, check_null)), ]$x_axis
wa_df[which(wa_df$station_id %in% update_stations), ]$y_axis_unique <- 
  wa_df[which(sapply(wa_df$y_axis_unique, check_null)), ]$y_axis


##
stations_w_shared_coords <- co_df %>% 
  group_by(x_axis, y_axis) %>% 
  summarize(same_coords_stations = n_distinct(station_id), 
            toString(sort(unique(station_id)))) %>% 
  filter(same_coords_stations > 1)

list_of_stations_w_shared_coords <- 
  merge(co_df, stations_w_shared_coords, by=c('x_axis', 'y_axis')) %>% 
  distinct(station_id, x_axis, y_axis) %>% 
  dplyr::select(station_id, x_axis, y_axis)

list_of_stations_w_shared_coords$x_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$x_axis, add_delta)
list_of_stations_w_shared_coords$y_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$y_axis, add_delta)

co_df$x_axis_unique <- merge(co_df, list_of_stations_w_shared_coords,
                             by='station_id', all.x = TRUE)$x_axis_unique
co_df$y_axis_unique <- merge(co_df, list_of_stations_w_shared_coords,
                             by='station_id', all.x = TRUE)$y_axis_unique

update_stations <- 
  unique(co_df[which(sapply(co_df$x_axis_unique, check_null)), ]$station_id)

co_df[which(co_df$station_id %in% update_stations), ]$x_axis_unique <- 
  co_df[which(sapply(co_df$x_axis_unique, check_null)), ]$x_axis
co_df[which(co_df$station_id %in% update_stations), ]$y_axis_unique <- 
  co_df[which(sapply(co_df$y_axis_unique, check_null)), ]$y_axis

rm(stations_w_shared_coords, list_of_stations_w_shared_coords, 
   update_stations)



### shared coordinates fix on lon/lat to get field for nearest station distance

stations_w_shared_coords <- im_df %>% 
  group_by(station_longitude, station_latitude) %>% 
  summarize(same_coords_stations = n_distinct(station_id), 
            toString(sort(unique(station_id)))) %>% 
  filter(same_coords_stations > 1)

list_of_stations_w_shared_coords <- 
  merge(im_df, stations_w_shared_coords, by=c('station_longitude', 'station_latitude')) %>% 
  distinct(station_id, station_longitude, station_latitude) %>% 
  dplyr::select(station_id, station_longitude, station_latitude)

list_of_stations_w_shared_coords$station_lon_x_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$station_longitude, add_delta)
list_of_stations_w_shared_coords$station_lat_y_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$station_latitude, add_delta)


im_df$station_lon_x_axis_unique <- merge(im_df, list_of_stations_w_shared_coords,
                                         by='station_id', all.x = TRUE)$station_lon_x_axis_unique
im_df$station_lat_y_axis_unique <- merge(im_df, list_of_stations_w_shared_coords,
                                         by='station_id', all.x = TRUE)$station_lat_y_axis_unique

update_stations <- 
  unique(im_df[which(sapply(im_df$station_lon_x_axis_unique, check_null)), ]$station_id)


im_df[which(im_df$station_id %in% update_stations), ]$station_lon_x_axis_unique <- 
  im_df[which(sapply(im_df$station_lon_x_axis_unique, check_null)), ]$station_longitude
im_df[which(im_df$station_id %in% update_stations), ]$station_lat_y_axis_unique <- 
  im_df[which(sapply(im_df$station_lat_y_axis_unique, check_null)), ]$station_latitude





stations_w_shared_coords <- co_df %>% 
  group_by(station_longitude, station_latitude) %>% 
  summarize(same_coords_stations = n_distinct(station_id), 
            toString(sort(unique(station_id)))) %>% 
  filter(same_coords_stations > 1)

list_of_stations_w_shared_coords <- 
  merge(co_df, stations_w_shared_coords, by=c('station_longitude', 'station_latitude')) %>% 
  distinct(station_id, station_longitude, station_latitude) %>% 
  dplyr::select(station_id, station_longitude, station_latitude)

list_of_stations_w_shared_coords$station_lon_x_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$station_longitude, add_delta)
list_of_stations_w_shared_coords$station_lat_y_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$station_latitude, add_delta)


co_df$station_lon_x_axis_unique <- merge(co_df, list_of_stations_w_shared_coords,
                                         by='station_id', all.x = TRUE)$station_lon_x_axis_unique
co_df$station_lat_y_axis_unique <- merge(co_df, list_of_stations_w_shared_coords,
                                         by='station_id', all.x = TRUE)$station_lat_y_axis_unique

update_stations <- 
  unique(co_df[which(sapply(co_df$station_lon_x_axis_unique, check_null)), ]$station_id)


co_df[which(co_df$station_id %in% update_stations), ]$station_lon_x_axis_unique <- 
  co_df[which(sapply(co_df$station_lon_x_axis_unique, check_null)), ]$station_longitude
co_df[which(co_df$station_id %in% update_stations), ]$station_lat_y_axis_unique <- 
  co_df[which(sapply(co_df$station_lat_y_axis_unique, check_null)), ]$station_latitude



stations_w_shared_coords <- wa_df %>% 
  group_by(station_longitude, station_latitude) %>% 
  summarize(same_coords_stations = n_distinct(station_id), 
            toString(sort(unique(station_id)))) %>% 
  filter(same_coords_stations > 1)

list_of_stations_w_shared_coords <- 
  merge(wa_df, stations_w_shared_coords, by=c('station_longitude', 'station_latitude')) %>% 
  distinct(station_id, station_longitude, station_latitude) %>% 
  dplyr::select(station_id, station_longitude, station_latitude)

list_of_stations_w_shared_coords$station_lon_x_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$station_longitude, add_delta)
list_of_stations_w_shared_coords$station_lat_y_axis_unique <- 
  sapply(list_of_stations_w_shared_coords$station_latitude, add_delta)


wa_df$station_lon_x_axis_unique <- merge(wa_df, list_of_stations_w_shared_coords,
                                         by='station_id', all.x = TRUE)$station_lon_x_axis_unique
wa_df$station_lat_y_axis_unique <- merge(wa_df, list_of_stations_w_shared_coords,
                                         by='station_id', all.x = TRUE)$station_lat_y_axis_unique

update_stations <- 
  unique(wa_df[which(sapply(wa_df$station_lon_x_axis_unique, check_null)), ]$station_id)


wa_df[which(wa_df$station_id %in% update_stations), ]$station_lon_x_axis_unique <- 
  wa_df[which(sapply(wa_df$station_lon_x_axis_unique, check_null)), ]$station_longitude
wa_df[which(wa_df$station_id %in% update_stations), ]$station_lat_y_axis_unique <- 
  wa_df[which(sapply(wa_df$station_lat_y_axis_unique, check_null)), ]$station_latitude


rm(stations_w_shared_coords, list_of_stations_w_shared_coords, 
   update_stations)




# get q1, median, q3 temperatures each station id
## need to add q1, median, and q3 to the table so we can use it later
station_temps <- wa_df %>% 
  group_by(station_id) %>% 
  mutate(temp_q1 = quantile(temps, na.rm = TRUE)[2],
         temp_median = median(temps, na.rm = TRUE),
         temp_q3 = quantile(temps, na.rm = TRUE)[4])

# isolate the 3 temperature statistics for each station-id
## let's just grab these uniquely separately
q_temp_pulls <- station_temps %>% group_by(station_id) %>% dplyr::select(station_id, temp_q1, temp_median, temp_q3) %>% unique()


# wa_df <- subset(wa_df, select= -c(temp_below_q1))
## here, since we uniquely pulled q1, median, and q3 temp information for each station, we can append it to the original wa_df
## df and combine with other fields to get other interesting parameters to analyze
wa_df <- merge(x=wa_df, y=q_temp_pulls, by='station_id', all.x = TRUE)
# let's gauge each day to determine how off it is, and we can use along with other fields for IM periods  
wa_df$cooler_than_normal <- ifelse(wa_df$temps <= wa_df$temp_q1, 1, 0)
wa_df$normal <- ifelse((wa_df$temps > wa_df$temp_q1) & (wa_df$temps < wa_df$temp_q3), 1, 0)
wa_df$warmer_than_normal <- ifelse(wa_df$temps >= wa_df$temp_q3, 1, 0)
wa_df$im_flag_digit <- ifelse(wa_df$im_flag == 'IM', 1, 0)



station_temps <- im_df %>% 
  group_by(station_id) %>% 
  mutate(temp_q1 = quantile(temps, na.rm = TRUE)[2],
         temp_median = median(temps, na.rm = TRUE),
         temp_q3 = quantile(temps, na.rm = TRUE)[4])

q_temp_pulls <- station_temps %>% group_by(station_id) %>% dplyr::select(station_id, temp_q1, temp_median, temp_q3) %>% unique()

im_df <- merge(x=im_df, y=q_temp_pulls, by='station_id', all.x = TRUE)
im_df$cooler_than_normal <- ifelse(im_df$temps <= im_df$temp_q1, 1, 0)
im_df$normal <- ifelse((im_df$temps > im_df$temp_q1) & (im_df$temps < im_df$temp_q3), 1, 0)
im_df$warmer_than_normal <- ifelse(im_df$temps >= im_df$temp_q3, 1, 0)
im_df$im_flag_digit <- ifelse(im_df$im_flag == 'IM', 1, 0)



station_temps <- co_df %>% 
  group_by(station_id) %>% 
  mutate(temp_q1 = quantile(temps, na.rm = TRUE)[2],
         temp_median = median(temps, na.rm = TRUE),
         temp_q3 = quantile(temps, na.rm = TRUE)[4])

q_temp_pulls <- station_temps %>% group_by(station_id) %>% dplyr::select(station_id, temp_q1, temp_median, temp_q3) %>% unique()


co_df <- merge(x=co_df, y=q_temp_pulls, by='station_id', all.x = TRUE)
co_df$cooler_than_normal <- ifelse(co_df$temps <= co_df$temp_q1, 1, 0)
co_df$normal <- ifelse((co_df$temps > co_df$temp_q1) & (co_df$temps < co_df$temp_q3), 1, 0)
co_df$warmer_than_normal <- ifelse(co_df$temps >= co_df$temp_q3, 1, 0)
co_df$im_flag_digit <- ifelse(co_df$im_flag == 'IM', 1, 0)

rm(station_temps, q_temp_pulls)



### add field for distance proximity of nearest chargers


## cold region

# 4 major region labels
# r1 - x=c(-95, 90), y=c(38, 39)
# r2 - x=c(-86, 90), y=c(42, 44)
# r3 - x=c(-82, 85), y=c(41.5, 43)
# r4 - x=c(-75, 71), y=c(41, 44.5)

co_df$subregion <- case_when((co_df$station_lon_x_axis_unique >= -95 & 
                                co_df$station_lon_x_axis_unique <= -90) & 
                               (co_df$station_lat_y_axis_unique >=38 &
                                  co_df$station_lat_y_axis_unique <= 39) ~ 'r1',
                             (co_df$station_lon_x_axis_unique >= -90 & 
                                co_df$station_lon_x_axis_unique <= -87.5) & 
                               (co_df$station_lat_y_axis_unique >=42 &
                                  co_df$station_lat_y_axis_unique <=44) ~ 'r2',
                             (co_df$station_lon_x_axis_unique >= -85 & 
                                co_df$station_lon_x_axis_unique <= -82) & 
                               (co_df$station_lat_y_axis_unique >=42 &
                                  co_df$station_lat_y_axis_unique <=43) ~ 'r3',
                             (co_df$station_lon_x_axis_unique >= -75 & 
                                co_df$station_lon_x_axis_unique <= -69) & 
                               (co_df$station_lat_y_axis_unique >=41 &
                                  co_df$station_lat_y_axis_unique <=45) ~ 'r4')

# g <- ggplot(subset(co_df, subregion=='r1'),
#             aes(x=station_lon_x_axis_unique,
#                 y=station_lat_y_axis_unique))
# g <- ggplot(co_df,
#             aes(x=station_lon_x_axis_unique,
#                 y=station_lat_y_axis_unique))
# g + geom_point()


df_by_station_coords <- co_df %>% 
  group_by(station_id, station_lon_x_axis_unique, station_lat_y_axis_unique) %>%
  summarize(n=n())
df_by_station_coords <- subset(df_by_station_coords, select= -c(n))



datalist <- list()
sr <- co_df$subregion %>% unique()


for (s_r in sr) {
  stations <- subset(co_df, subregion==s_r)$station_id %>% unique()
  nearest_station_km <- vector()
  
  for (x in stations) {
    dist_v <- vector()
    
    p1 <- as.matrix(cbind(subset(df_by_station_coords, station_id == x)$station_lon_x_axis_unique,
                          subset(df_by_station_coords, station_id == x)$station_lat_y_axis_unique), ncol=2)
    colnames(p1) <- c('lon', 'lat')
    for (i in stations) {
      if(x!=i) {
        p2 <- as.matrix(cbind(subset(df_by_station_coords, station_id == i)$station_lon_x_axis_unique,
                              subset(df_by_station_coords, station_id == i)$station_lat_y_axis_unique), ncol=2)
        colnames(p2) <- c('lon', 'lat')
        dist <- round(geodist(p1, p2, measure = 'vincenty')/1000, 2)
        dist_v <- append(dist_v, dist)
      }
    }
    dist_df <- cbind(station = stations[stations!=x], dist_km = dist_v)
    dist_df <- dist_df[order(dist_v), ]
    dist_value <- as.character(dist_df[1,2])
    nearest_station_km <- append(nearest_station_km, dist_value)
  }
  
  df <- as.data.frame(cbind(station = as.integer(stations), 
                            distances_km = as.double(nearest_station_km)))
  
  df$subregion <- s_r
  datalist[[s_r]] <- df
}

region_distances_df <- do.call(rbind, datalist)
rownames(region_distances_df) <- NULL

rm(datalist, sr, s_r, stations, nearest_station_km, dist_v, p1, p2, dist, 
   dist_df, dist_value, df, x, i, df_by_station_coords)

co_df <- merge(co_df, region_distances_df, by.x='station_id', by.y='station')






## warm region

# 4 major region labels

wa_df$subregion <- case_when((wa_df$station_lon_x_axis_unique >= -98 & 
                                wa_df$station_lon_x_axis_unique <= -97.5) & 
                               (wa_df$station_lat_y_axis_unique >=29 &
                                  wa_df$station_lat_y_axis_unique <= 31) ~ 'r1',
                             (wa_df$station_lon_x_axis_unique >= -98 & 
                                wa_df$station_lon_x_axis_unique <= -95) & 
                               (wa_df$station_lat_y_axis_unique >=32 &
                                  wa_df$station_lat_y_axis_unique <=34) ~ 'r2',
                             (wa_df$station_lon_x_axis_unique >= -97 & 
                                wa_df$station_lon_x_axis_unique <= -94) & 
                               (wa_df$station_lat_y_axis_unique >=29 &
                                  wa_df$station_lat_y_axis_unique <=31) ~ 'r3',
                             (wa_df$station_lon_x_axis_unique >= -85 & 
                                wa_df$station_lon_x_axis_unique <= -79) & 
                               (wa_df$station_lat_y_axis_unique >=25 &
                                  wa_df$station_lat_y_axis_unique <=31) ~ 'r4')
# 
# g <- ggplot(subset(wa_df, subregion=='r4'),
#             aes(x=station_lon_x_axis_unique,
#                 y=station_lat_y_axis_unique))
# g <- ggplot(wa_df,
#             aes(x=station_lon_x_axis_unique,
#                 y=station_lat_y_axis_unique))
# g + geom_point()


df_by_station_coords <- wa_df %>% 
  group_by(station_id, station_lon_x_axis_unique, station_lat_y_axis_unique) %>%
  summarize(n=n())
df_by_station_coords <- subset(df_by_station_coords, select= -c(n))



datalist <- list()
sr <- wa_df$subregion %>% unique()


for (s_r in sr) {
  stations <- subset(wa_df, subregion==s_r)$station_id %>% unique()
  nearest_station_km <- vector()
  
  for (x in stations) {
    dist_v <- vector()
    
    p1 <- as.matrix(cbind(subset(df_by_station_coords, station_id == x)$station_lon_x_axis_unique,
                          subset(df_by_station_coords, station_id == x)$station_lat_y_axis_unique), ncol=2)
    colnames(p1) <- c('lon', 'lat')
    for (i in stations) {
      if(x!=i) {
        p2 <- as.matrix(cbind(subset(df_by_station_coords, station_id == i)$station_lon_x_axis_unique,
                              subset(df_by_station_coords, station_id == i)$station_lat_y_axis_unique), ncol=2)
        colnames(p2) <- c('lon', 'lat')
        dist <- round(geodist(p1, p2, measure = 'vincenty')/1000, 2)
        dist_v <- append(dist_v, dist)
      }
    }
    dist_df <- cbind(station = stations[stations!=x], dist_km = dist_v)
    dist_df <- dist_df[order(dist_v), ]
    dist_value <- as.character(dist_df[1,2])
    nearest_station_km <- append(nearest_station_km, dist_value)
  }
  
  df <- as.data.frame(cbind(station = as.integer(stations), 
                            distances_km = as.double(nearest_station_km)))
  
  df$subregion <- s_r
  datalist[[s_r]] <- df
}

region_distances_df <- do.call(rbind, datalist)
rownames(region_distances_df) <- NULL

rm(datalist, sr, s_r, stations, nearest_station_km, dist_v, p1, p2, dist, 
   dist_df, dist_value, df, x, i, df_by_station_coords)

wa_df <- merge(wa_df, region_distances_df, by.x='station_id', by.y='station')




## intermediate region

im_df$subregion <- 'r1'
# 
# g <- ggplot(subset(wa_df, subregion=='r4'),
#             aes(x=station_lon_x_axis_unique,
#                 y=station_lat_y_axis_unique))
# g <- ggplot(wa_df,
#             aes(x=station_lon_x_axis_unique,
#                 y=station_lat_y_axis_unique))
# g + geom_point()


df_by_station_coords <- im_df %>% 
  group_by(station_id, station_lon_x_axis_unique, station_lat_y_axis_unique) %>%
  summarize(n=n())
df_by_station_coords <- subset(df_by_station_coords, select= -c(n))



datalist <- list()
sr <- im_df$subregion %>% unique()


for (s_r in sr) {
  stations <- subset(im_df, subregion==s_r)$station_id %>% unique()
  nearest_station_km <- vector()
  
  for (x in stations) {
    dist_v <- vector()
    
    p1 <- as.matrix(cbind(subset(df_by_station_coords, station_id == x)$station_lon_x_axis_unique,
                          subset(df_by_station_coords, station_id == x)$station_lat_y_axis_unique), ncol=2)
    colnames(p1) <- c('lon', 'lat')
    for (i in stations) {
      if(x!=i) {
        p2 <- as.matrix(cbind(subset(df_by_station_coords, station_id == i)$station_lon_x_axis_unique,
                              subset(df_by_station_coords, station_id == i)$station_lat_y_axis_unique), ncol=2)
        colnames(p2) <- c('lon', 'lat')
        dist <- round(geodist(p1, p2, measure = 'vincenty')/1000, 2)
        dist_v <- append(dist_v, dist)
      }
    }
    dist_df <- cbind(station = stations[stations!=x], dist_km = dist_v)
    dist_df <- dist_df[order(dist_v), ]
    dist_value <- as.character(dist_df[1,2])
    nearest_station_km <- append(nearest_station_km, dist_value)
  }
  
  df <- as.data.frame(cbind(station = as.integer(stations), 
                            distances_km = as.double(nearest_station_km)))
  
  df$subregion <- s_r
  datalist[[s_r]] <- df
}

region_distances_df <- do.call(rbind, datalist)
rownames(region_distances_df) <- NULL

rm(datalist, sr, s_r, stations, nearest_station_km, dist_v, p1, p2, dist, 
   dist_df, dist_value, df, x, i, df_by_station_coords)

im_df <- merge(im_df, region_distances_df, by.x='station_id', by.y='station')

rm(region_distances_df, data.df)



# format.end;


###############
# Basic summary
###############

## how many stations for the three main temp regions?
length(unique(im_df$station_intances_adj)) # 853
length(unique(co_df$station_intances_adj)) # 180
length(unique(wa_df$station_intances_adj)) # 228

length(unique(im_df$station_id)) # 215
length(unique(co_df$station_id)) # 57
length(unique(wa_df$station_id)) # 69


## what's the age (days) for each station and the summary stats
station.day.old <- im_df %>% 
  group_by(station_id) %>% 
  summarize(day_count_old = n())
station.day.old <- station.day.old[order(station.day.old$day_count_old),]
g <- ggplot(station.day.old, aes(x=day_count_old))
g + 
  # geom_density() +
  # geom_function(fun=dnorm) +
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of station age (days)',
       x='days')
summary(station.day.old$day_count_old)


# what's the avg number of days a station spends IM and non-IM and the summary stats
## what's the avg number of days each stations was in operation?
station.day.old <- im_df %>% 
  group_by(station_id) %>% 
  summarize(im_day_count = sum(!is.na(im_date)),
            non_im_day_count = sum(is.na(im_date)))

g <- ggplot(station.day.old,
            aes(x=im_day_count))
g + 
  geom_histogram(bins=30) +
  labs(title = "Distribution of days in maintenance",
       subtitle = 'Intermediary location',
       x='days', y='Count')
summary(station.day.old$im_day_count)

# g <- ggplot(station.day.old,
#             aes(x=non_im_day_count))
# g + 
#   geom_histogram(bins=30) +
#   labs(title = "Distribution of days not in maintenance",
#        subtitle = 'Intermediary location',
#        x='days', y='Count')
# 
# summary(station.day.old$non_im_day_count)



# analyze distribution of young stations (those whose lifespan is less than the 2-year span)
station.day.old <- im_df %>% 
  group_by(station_id) %>% 
  summarize(day_count_old = n())
less_than_2 <- station.day.old[which(station.day.old$day_count_old < 731), ]
stations <- unique(less_than_2$station_id)

g <- ggplot(less_than_2, aes(x=day_count_old))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of ev age (days)',
       subtitle = 'EVs with less than the 2-years of operation',
       x='days')

# g <- ggplot(less_than_2, aes(y=day_count_old))
# g + 
#   geom_boxplot() +
#   labs(title = 'Distribution of ev age (days)', 
#        y='Day count')

summary(less_than_2$day_count_old)


# days spent IM and non-IM
station.day.old <- im_df[which(im_df$station_id %in% stations), ] %>% 
  group_by(station_id) %>% 
  summarize(im_day_count = sum(!is.na(im_date)),
            non_im_day_count = sum(is.na(im_date)))

g <- ggplot(station.day.old, aes(x=im_day_count))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of young ev in maintenance days', 
       y='Day count')

g <- ggplot(station.day.old, aes(x=non_im_day_count))
g +
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of young ev non in maintenance days',
       y='Day count')

summary(station.day.old$im_day_count)
summary(station.day.old$non_im_day_count)






## what's the age (days) for each station and the summary stats
station.day.old <- co_df %>% 
  group_by(station_id) %>% 
  summarize(day_count_old = n())
station.day.old <- station.day.old[order(station.day.old$day_count_old),]
g <- ggplot(station.day.old, aes(x=day_count_old))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of station age (days)',
       x='days')
summary(station.day.old$day_count_old)


# what's the avg number of days a station spends IM and non-IM and the summary stats
## what's the avg number of days each stations was in operation?
station.day.old <- co_df %>% 
  group_by(station_id) %>% 
  summarize(im_day_count = sum(!is.na(im_date)),
            non_im_day_count = sum(is.na(im_date)))

g <- ggplot(station.day.old,
            aes(x=im_day_count))
g + 
  geom_histogram(bins=30) +
  labs(title = "Distribution of days in maintenance",
       subtitle = 'Cold location',
       x='days', y='Count')
summary(station.day.old$im_day_count)

g <- ggplot(station.day.old,
            aes(x=non_im_day_count))
g + 
  geom_histogram(bins=30) +
  labs(title = "Distribution of days not in maintenance",
       subtitle = 'Cold location',
       x='days', y='Count')

summary(station.day.old$non_im_day_count)



# analyze distribution of young stations (those whose lifespan is less than the 2-year span)
station.day.old <- co_df %>% 
  group_by(station_id) %>% 
  summarize(day_count_old = n())
less_than_2 <- station.day.old[which(station.day.old$day_count_old < 731), ]
stations <- unique(less_than_2$station_id)

g <- ggplot(less_than_2, aes(x=day_count_old))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of ev age (days)',
       x='days')

# g <- ggplot(less_than_2, aes(y=day_count_old))
# g + 
#   geom_boxplot() +
#   labs(title = 'Distribution of ev age (days)', 
#        y='Day count')

summary(less_than_2$day_count_old)


# days spent IM and non-IM
station.day.old <- co_df[which(co_df$station_id %in% stations), ] %>% 
  group_by(station_id) %>% 
  summarize(im_day_count = sum(!is.na(im_date)),
            non_im_day_count = sum(is.na(im_date)))

g <- ggplot(station.day.old, aes(x=im_day_count))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of young ev in maintenance days', 
       y='Day count')

g <- ggplot(station.day.old, aes(x=non_im_day_count))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of young ev non in maintenance days', 
       y='Day count')

summary(station.day.old$im_day_count)
summary(station.day.old$non_im_day_count)






## what's the age (days) for each station and the summary stats
station.day.old <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(day_count_old = n())
station.day.old <- station.day.old[order(station.day.old$day_count_old),]
g <- ggplot(station.day.old, aes(x=day_count_old))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of station age (days)',
       x='days')
summary(station.day.old$day_count_old)


# what's the avg number of days a station spends IM and non-IM and the summary stats
## what's the avg number of days each stations was in operation?
station.day.old <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(im_day_count = sum(!is.na(im_date)),
            non_im_day_count = sum(is.na(im_date)))

g <- ggplot(station.day.old,
            aes(x=im_day_count))
g + 
  geom_histogram(bins=30) +
  labs(title = "Distribution of days in maintenance",
       subtitle = 'Warm location',
       x='days', y='Count')
summary(station.day.old$im_day_count)

g <- ggplot(station.day.old,
            aes(x=non_im_day_count))
g + 
  geom_histogram(bins=30) +
  labs(title = "Distribution of days not in maintenance",
       subtitle = 'Warm location',
       x='days', y='Count')

summary(station.day.old$non_im_day_count)



# analyze distribution of young stations (those whose lifespan is less than the 2-year span)
station.day.old <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(day_count_old = n())
less_than_2 <- station.day.old[which(station.day.old$day_count_old < 731), ]
stations <- unique(less_than_2$station_id)

g <- ggplot(less_than_2, aes(x=day_count_old))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of ev age (days)',
       x='days')

# g <- ggplot(less_than_2, aes(y=day_count_old))
# g + 
#   geom_boxplot() +
#   labs(title = 'Distribution of ev age (days)', 
#        y='Day count')

summary(less_than_2$day_count_old)


# days spent IM and non-IM
station.day.old <- wa_df[which(wa_df$station_id %in% stations), ] %>% 
  group_by(station_id) %>% 
  summarize(im_day_count = sum(!is.na(im_date)),
            non_im_day_count = sum(is.na(im_date)))

g <- ggplot(station.day.old, aes(x=im_day_count))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of young ev in maintenance days', 
       y='Day count')

g <- ggplot(station.day.old, aes(x=non_im_day_count))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Distribution of young ev non in maintenance days', 
       y='Day count')

summary(station.day.old$im_day_count)
summary(station.day.old$non_im_day_count)



######################################################################
######################################################################
######################################################################
######### session, kwh, and 

data.df <- rbind(im_df, co_df, wa_df)
g <- ggplot(data.df, aes(x=sessions))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Sessions distribution - Across all temperatures')

summary(data.df$sessions)

g <- ggplot(data.df, aes(x=minutes))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Session minutes distribution - Across all temperatures')

summary(data.df$minutes)

g <- ggplot(data.df, aes(x=session_kwh))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Session kWh distribution - Across all temperatures')
summary(data.df$session_kwh)


g <- ggplot(data.df, aes(x=unique_customers))
g + 
  geom_histogram(bins = 30) +
  labs(title = 'Session kWh distribution - Across all temperatures')
summary(data.df$unique_customers)







# test <- subset(co_df, station_id == 726)


####################################################################
####################################################################
####################################################################
####################################################################
# get mean time between failures -- days
####################################################################


overall.station.operations.co <- co_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.co, dowtime_days == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
co_df.2 <- subset(co_df, !(station_id %in% c(386, 505, 678)))
overall.station.operations.co <- co_df.2 %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.co$mtbf <- 
  mean_x_between_issues(overall.station.operations.co$uptime_days,
                        overall.station.operations.co$dowtime_days)

g <- ggplot(data = overall.station.operations.co, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Cold temperatures')

tapply(overall.station.operations.co$mtbf, 
       overall.station.operations.co$charger_company_col,
       summary)


hist(subset(overall.station.operations.co, mtbf<600)$mtbf)
summary(subset(overall.station.operations.co, mtbf<600)$mtbf)






overall.station.operations.im <- im_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.im, dowtime_days == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
im_df.2 <- subset(im_df, !(station_id %in% c(593, 6816, 40843)))
overall.station.operations.im <- im_df.2 %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.im$mtbf <- 
  mean_x_between_issues(overall.station.operations.im$uptime_days,
                        overall.station.operations.im$dowtime_days)

g <- ggplot(data = overall.station.operations.im, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Moderate temperatures')

tapply(overall.station.operations.im$mtbf, 
       overall.station.operations.im$charger_company_col,
       summary)

hist(subset(overall.station.operations.im, mtbf<600)$mtbf)
summary(subset(overall.station.operations.im, mtbf<600)$mtbf)






overall.station.operations.wa <- wa_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.wa, dowtime_days == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
# wa_df.2 <- subset(wa_df, !(station_id %in% c(386, 505, 678)))
overall.station.operations.wa <- wa_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.wa$mtbf <- 
  mean_x_between_issues(overall.station.operations.wa$uptime_days,
                        overall.station.operations.wa$dowtime_days)

g <- ggplot(data = overall.station.operations.wa, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Warm temperatures')

tapply(overall.station.operations.im$mtbf, 
       overall.station.operations.im$charger_company_col,
       summary)

hist(subset(overall.station.operations.wa, mtbf<600)$mtbf)
summary(subset(overall.station.operations.wa, mtbf<600)$mtbf)



# overall.station.operations.combined <- 

# dim(overall.station.operations.co)
# dim(overall.station.operations.im)
# dim(overall.station.operations.wa)

overall.station.operations.combined <- rbind(overall.station.operations.co,
                                             overall.station.operations.im,
                                             overall.station.operations.wa)



g <- ggplot(data = overall.station.operations.combined, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution', 
       subtitle = 'All combined')
tapply(overall.station.operations.combined$mtbf, 
       overall.station.operations.combined$charger_company_col,
       summary)



g <- ggplot(data = subset(overall.station.operations.combined, mtbf <= 600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'All temperatures with MTBF <= 600')
tapply(subset(overall.station.operations.combined, mtbf <= 600)$mtbf,
       subset(overall.station.operations.combined, mtbf <= 600)$charger_company_col, 
       summary)

g <- ggplot(data = subset(overall.station.operations.im, mtbf <= 600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Intermediate temperatures with MTBF <= 600')


g <- ggplot(data = subset(overall.station.operations.co, mtbf <= 600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Cold temperatures with MTBF <= 600')


g <- ggplot(data = subset(overall.station.operations.wa, mtbf <= 600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Warm temperatures with MTBF <= 600')






# stations.mtbf.600 <- subset(overall.station.operations.combined, mtbf <= 600)
# 
# # stations.mtbf.600 <- data.frame(stations.mtbf.600)
# head(stations.mtbf.600)
# summary(stations.mtbf.600$mtbf)
# summary(stations.mtbf.600)



####################################################################
####################################################################
####################################################################
####################################################################
# get mean time between failures -- days X charger_company_model
####################################################################


overall.station.operations.co <- co_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.co, dowtime_days == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
co_df.2 <- subset(co_df, !(station_id %in% c(386, 505, 678)))
overall.station.operations.co <- co_df.2 %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.co$mtbf <- 
  mean_x_between_issues(overall.station.operations.co$uptime_days,
                        overall.station.operations.co$dowtime_days)


g <- ggplot(data = overall.station.operations.co, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Cold temperatures')

tapply(overall.station.operations.co$mtbf, 
       overall.station.operations.co$charger_company_col,
       summary)

g <- ggplot(data = subset(overall.station.operations.co, mtbf < 600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution - MTFB < 600',
       subtitle = 'Cold temperatures')

tapply(subset(overall.station.operations.co, mtbf < 600)$mtbf, 
       subset(overall.station.operations.co, mtbf < 600)$charger_company_col,
       summary)









overall.station.operations.wa <- wa_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.wa, dowtime_days == 0)

# # remove those stations whose only IM in the two year span was less than 30 minutes
# wa_df.2 <- subset(wa_df, !(station_id %in% c(386, 505, 678)))
# overall.station.operations.wa <- wa_df.2 %>% 
#   group_by(station_id, charger_company_col) %>% 
#   summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
#             dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.wa$mtbf <- 
  mean_x_between_issues(overall.station.operations.wa$uptime_days,
                        overall.station.operations.wa$dowtime_days)


g <- ggplot(data = overall.station.operations.wa, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'Warm temperatures')

tapply(overall.station.operations.wa$mtbf, 
       overall.station.operations.wa$charger_company_col,
       summary)

g <- ggplot(data = subset(overall.station.operations.wa, mtbf< 600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution - MTBF<600',
       subtitle = 'Warm temperatures')

tapply(subset(overall.station.operations.wa, mtbf< 600)$mtbf, 
       subset(overall.station.operations.wa, mtbf< 600)$charger_company_col,
       summary)







overall.station.operations.im <- im_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.im, dowtime_days == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
im_df.2 <- subset(im_df, !(station_id %in% c(593, 6816, 40843)))
overall.station.operations.im <- im_df.2 %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            dowtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.im$mtbf <- 
  mean_x_between_issues(overall.station.operations.im$uptime_days,
                        overall.station.operations.im$dowtime_days)


g <- ggplot(data = overall.station.operations.im, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'intermediate temperatures')
tapply(overall.station.operations.im$mtbf,
       overall.station.operations.im$charger_company_col,
       summary)

g <- ggplot(data = subset(overall.station.operations.im, mtbf<600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution - MTBF < 600',
       subtitle = 'intermediate temperatures')
tapply(subset(overall.station.operations.im, mtbf<600)$mtbf,
       subset(overall.station.operations.im, mtbf<600)$charger_company_col,
       summary)






overall.station.operations.combined <- rbind(overall.station.operations.co,
                                             overall.station.operations.im,
                                             overall.station.operations.wa)




g <- ggplot(data = overall.station.operations.combined, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution',
       subtitle = 'All temperatures')
tapply(overall.station.operations.combined$mtbf,
       overall.station.operations.combined$charger_company_col,
       summary)


g <- ggplot(data = subset(overall.station.operations.combined, mtbf<600), 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean time between failures distribution-MTBF<600',
       subtitle = 'All temperatures')
tapply(subset(overall.station.operations.combined, mtbf<600)$mtbf,
       subset(overall.station.operations.combined, mtbf<600)$charger_company_col,
       summary)



####################################################################
####################################################################
####################################################################
####################################################################
# get mean time between failures -- sessions X charger-company
####################################################################


overall.station.operations.co <- co_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_sessions = sum(sessions[mtbf_flag==0], na.rm = TRUE),
            downtime_sessions = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.co, downtime_sessions == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
# co_df.2 <- subset(co_df, !(station_id %in% c(386, 443, 450, 492, 505, 675, 678,
#                                              833, 880, 1002, 3853, 3864, 4550)))
co_df.2 <- subset(co_df, !(station_id %in% c(386, 505, 678)))
overall.station.operations.co <- co_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_sessions = sum(sessions[mtbf_flag==0], na.rm = TRUE),
            downtime_sessions = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.co$mtbf <- 
  mean_x_between_issues(overall.station.operations.co$uptime_sessions,
                        overall.station.operations.co$downtime_sessions)


g <- ggplot(data = overall.station.operations.co,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean # of sessions between failures distribution',
       subtitle = 'Cold temperatures')

tapply(overall.station.operations.co$mtbf,
       overall.station.operations.co$charger_company_col,
       summary)







overall.station.operations.im <- im_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_sessions = sum(sessions[mtbf_flag==0], na.rm = TRUE),
            downtime_sessions = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.im, downtime_sessions == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
im_df.2 <- subset(im_df, !(station_id %in% c(593,6816,40843)))
# im_df.2 <- subset(im_df, !(station_id %in% c(428,439,525,581,593,605,3999,4356,
#                                              6107,6816,7315,40840,40843,41084,
#                                              41088,44911,47706)))
overall.station.operations.im <- im_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_sessions = sum(sessions[mtbf_flag==0], na.rm = TRUE),
            downtime_sessions = n_distinct(mtbf_flag[mtbf_flag!=0]))


overall.station.operations.im$mtbf <- 
  mean_x_between_issues(overall.station.operations.im$uptime_sessions,
                        overall.station.operations.im$downtime_sessions)


g <- ggplot(data = overall.station.operations.im,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean # of sessions between failures distribution',
       subtitle = 'Intermediate temperatures')

tapply(overall.station.operations.im$mtbf,
       overall.station.operations.im$charger_company_col,
       summary)








overall.station.operations.wa <- wa_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_sessions = sum(sessions[mtbf_flag==0], na.rm = TRUE),
            downtime_sessions = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.wa, downtime_sessions == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
# wa_df.2 <- subset(wa_df, !(station_id %in% c(379,415,423,516,519,550,562,563,
#                                              648,854,864,1221,1224,5325)))
overall.station.operations.wa <- wa_df %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_sessions = sum(sessions[mtbf_flag==0], na.rm = TRUE),
            downtime_sessions = n_distinct(mtbf_flag[mtbf_flag!=0]))


overall.station.operations.wa$mtbf <- 
  mean_x_between_issues(overall.station.operations.wa$uptime_sessions,
                        overall.station.operations.wa$downtime_sessions)


g <- ggplot(data = overall.station.operations.wa,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=10) +
  labs(title = 'Mean # of sessions between failures distribution',
       subtitle = 'Warm temperatures')

tapply(overall.station.operations.wa$mtbf,
       overall.station.operations.wa$charger_company_col,
       summary)


overall.station.operations.combined <- rbind(overall.station.operations.co,
                                             overall.station.operations.im,
                                             overall.station.operations.wa)




g <- ggplot(data = overall.station.operations.combined, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=30) +
  labs(title = 'Mean # of sessions between failures distribution',
       subtitle = 'All temperatures')

tapply(overall.station.operations.combined$mtbf,
       overall.station.operations.combined$charger_company_col,
       summary)





####################################################################
####################################################################
####################################################################
####################################################################
# get mean time between failures -- minutes
####################################################################



overall.station.operations.co <- co_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_minutes = sum(minutes[mtbf_flag==0], na.rm = TRUE),
            dowtime_minutes = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.co, dowtime_minutes == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
co_df.2 <- subset(co_df, !(station_id %in% c(386, 505, 678)))
overall.station.operations.co <- co_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_minutes = sum(minutes[mtbf_flag==0], na.rm = TRUE),
            dowtime_minutes = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.co$mtbf <- 
  mean_x_between_issues(overall.station.operations.co$uptime_minutes,
                        overall.station.operations.co$dowtime_minutes)


g <- ggplot(data = overall.station.operations.co,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=30) +
  labs(title = 'Mean minutes between failures distribution',
       subtitle = 'Cold temperatures')

tapply(overall.station.operations.co$mtbf,
       overall.station.operations.co$charger_company_col,
       summary)






overall.station.operations.im <- im_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_minutes = sum(minutes[mtbf_flag==0], na.rm = TRUE),
            dowtime_minutes = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.im, dowtime_minutes == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
im_df.2 <- subset(im_df, !(station_id %in% c(593,6816,40843)))
overall.station.operations.im <- im_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_minutes = sum(minutes[mtbf_flag==0], na.rm = TRUE),
            dowtime_minutes = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.im$mtbf <- 
  mean_x_between_issues(overall.station.operations.im$uptime_minutes,
                        overall.station.operations.im$dowtime_minutes)


g <- ggplot(data = overall.station.operations.im,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=30) +
  labs(title = 'Mean minutes between failures distribution',
       subtitle = 'Intermediate temperatures')

tapply(overall.station.operations.im$mtbf,
       overall.station.operations.im$charger_company_col,
       summary)










overall.station.operations.wa <- wa_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_minutes = sum(minutes[mtbf_flag==0], na.rm = TRUE),
            dowtime_minutes = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.wa, dowtime_minutes == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
# wa_df.2 <- subset(wa_df, !(station_id %in% c(379,415,423,516,519,550,562,563,
#                                              648,854,864,1221,1224,5325)))
overall.station.operations.wa <- wa_df %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_minutes = sum(minutes[mtbf_flag==0], na.rm = TRUE),
            dowtime_minutes = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.wa$mtbf <- 
  mean_x_between_issues(overall.station.operations.wa$uptime_minutes,
                        overall.station.operations.wa$dowtime_minutes)


g <- ggplot(data = overall.station.operations.wa,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean minutes between failures distribution',
       subtitle = 'Warm temperatures')
tapply(overall.station.operations.wa$mtbf,
       overall.station.operations.wa$charger_company_col,
       summary)




overall.station.operations.combined <- rbind(overall.station.operations.co,
                                             overall.station.operations.im,
                                             overall.station.operations.wa)




g <- ggplot(data = overall.station.operations.combined, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean minutes between failures distribution',
       subtitle = 'All temperatures')

tapply(overall.station.operations.combined$mtbf,
       overall.station.operations.combined$charger_company_col,
       summary)






####################################################################
####################################################################
####################################################################
####################################################################
# get mean time between failures -- customers
####################################################################



overall.station.operations.co <- co_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_customers = sum(unique_customers[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.co, dowtime_customers == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
co_df.2 <- subset(co_df, !(station_id %in% c(386, 505, 678)))
overall.station.operations.co <- co_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_customers = sum(unique_customers[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.co$mtbf <- 
  mean_x_between_issues(overall.station.operations.co$uptime_customers,
                        overall.station.operations.co$dowtime_customers)


g <- ggplot(data = overall.station.operations.co,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean # of customers between failures distribution',
       subtitle = 'Cold temperatures')

tapply(overall.station.operations.co$mtbf,
       overall.station.operations.co$charger_company_col,
       summary)




overall.station.operations.im <- im_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_customers = sum(unique_customers[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.im, dowtime_customers == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
im_df.2 <- subset(im_df, !(station_id %in% c(593,6816,40843)))
overall.station.operations.im <- im_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_customers = sum(unique_customers[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.im$mtbf <- 
  mean_x_between_issues(overall.station.operations.im$uptime_customers,
                        overall.station.operations.im$dowtime_customers)


g <- ggplot(data = overall.station.operations.im,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean # of customers between failures distribution',
       subtitle = 'Intermediate temperatures')

tapply(overall.station.operations.im$mtbf,
       overall.station.operations.im$charger_company_col,
       summary)






overall.station.operations.wa <- wa_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_customers = sum(unique_customers[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.wa, dowtime_customers == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
# wa_df.2 <- subset(wa_df, !(station_id %in% c(379,415,423,516,519,550,562,563,
#                                              648,854,864,1221,1224,5325)))
overall.station.operations.wa <- wa_df %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_customers = sum(unique_customers[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.wa$mtbf <- 
  mean_x_between_issues(overall.station.operations.wa$uptime_customers,
                        overall.station.operations.wa$dowtime_customers)
# overall.station.operations.wa$mtbf <- overall.station.operations.wa$uptime_customers /
#   overall.station.operations.wa$dowtime_customers


g <- ggplot(data = overall.station.operations.wa,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean # of customers between failures distribution',
       subtitle = 'Warm temperatures')

tapply(overall.station.operations.wa$mtbf,
       overall.station.operations.wa$charger_company_col,
       summary)


overall.station.operations.combined <- rbind(overall.station.operations.co,
                                             overall.station.operations.im,
                                             overall.station.operations.wa)




g <- ggplot(data = overall.station.operations.combined, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean # of customers between failures distribution',
       subtitle = 'All temperatures')

tapply(overall.station.operations.combined$mtbf,
       overall.station.operations.combined$charger_company_col, 
       summary)




####################################################################
####################################################################
####################################################################
####################################################################
# get mean time between failures -- kWh
####################################################################



overall.station.operations.co <- co_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_kwh = sum(session_kwh[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.co, dowtime_customers == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
co_df.2 <- subset(co_df, !(station_id %in% c(386, 505, 678)))
overall.station.operations.co <- co_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_kwh = sum(session_kwh[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.co$mtbf <- 
  mean_x_between_issues(overall.station.operations.co$uptime_kwh,
                        overall.station.operations.co$dowtime_customers)


g <- ggplot(data = overall.station.operations.co,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean kWh between failures distribution',
       subtitle = 'Cold temperatures')

tapply(overall.station.operations.co$mtbf,
       overall.station.operations.co$charger_company_col,
       summary)




overall.station.operations.im <- im_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_kwh = sum(session_kwh[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.im, dowtime_customers == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
im_df.2 <- subset(im_df, !(station_id %in% c(593,6816,40843)))
overall.station.operations.im <- im_df.2 %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_kwh = sum(session_kwh[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.im$mtbf <- 
  mean_x_between_issues(overall.station.operations.im$uptime_kwh,
                        overall.station.operations.im$dowtime_customers)


g <- ggplot(data = overall.station.operations.im,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean kWh between failures distribution',
       subtitle = 'Intermediate temperatures')

tapply(overall.station.operations.im$mtbf,
       overall.station.operations.im$charger_company_col,
       summary)






overall.station.operations.wa <- wa_df %>% 
  group_by(station_id, charger_company_col) %>% 
  summarize(uptime_kwh = sum(session_kwh[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

subset(overall.station.operations.wa, dowtime_customers == 0)

# remove those stations whose only IM in the two year span was less than 30 minutes
# wa_df.2 <- subset(wa_df, !(station_id %in% c(379,415,423,516,519,550,562,563,
#                                              648,854,864,1221,1224,5325)))
overall.station.operations.wa <- wa_df %>%
  group_by(station_id, charger_company_col) %>%
  summarize(uptime_kwh = sum(session_kwh[mtbf_flag==0], na.rm = TRUE),
            dowtime_customers = n_distinct(mtbf_flag[mtbf_flag!=0]))

overall.station.operations.wa$mtbf <- 
  mean_x_between_issues(overall.station.operations.wa$uptime_kwh,
                        overall.station.operations.wa$dowtime_customers)
# overall.station.operations.wa$mtbf <- overall.station.operations.wa$uptime_customers /
#   overall.station.operations.wa$dowtime_customers


g <- ggplot(data = overall.station.operations.wa,
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean kWh between failures distribution',
       subtitle = 'Warm temperatures')

tapply(overall.station.operations.wa$mtbf,
       overall.station.operations.wa$charger_company_col,
       summary)


overall.station.operations.combined <- rbind(overall.station.operations.co,
                                             overall.station.operations.im,
                                             overall.station.operations.wa)




g <- ggplot(data = overall.station.operations.combined, 
            aes(x=mtbf, fill=charger_company_col, col=charger_company_col))
g + geom_histogram(bins=40) +
  labs(title = 'Mean kWh between failures distribution',
       subtitle = 'All temperatures')

tapply(overall.station.operations.combined$mtbf,
       overall.station.operations.combined$charger_company_col, 
       summary)




####################################################################################################
####################################################################################################
####################################################################################################
################################################## Spatial analysis
#########################


# check where IM instances less than 30 mins
location_activity <- im_df %>% 
  group_by(station_id, x_axis_unique, y_axis_unique) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            downtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))


subset(location_activity, downtime_days == 0) # yields IM instances less than 30 mins.. remove them
location_activity <- subset(im_df, !(station_id %in% c(593, 6816, 40843))) # intermediary
rm(location_activity)


#############
# ## instances in wa_df (only in warm-df) where we need to do some remaining imputation
# wa_df %>%
#   group_by(station_id) %>%
#   summarize(empties = n_distinct(flowdate[is.na(temps)])) %>%
#   arrange(desc(empties))




im_location_activity_v2 <- subset(im_df, !(station_id %in% c(593, 6816, 40843))) %>% 
  group_by(station_id, charger_company_col, x_axis_unique, y_axis_unique) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            days_since_operation = n_distinct(flowdate),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE),
            total_sessions = sum(sessions, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            total_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            total_customers = sum(unique_customers, na.rm = TRUE),
            total_unique_customers_per_1000s = sum(unique_customers, na.rm = TRUE)/1000,
            uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            uptime_days_cooler = n_distinct(flowdate[(mtbf_flag==0) & (cooler_than_normal==1)]),
            uptime_days_normal = n_distinct(flowdate[(mtbf_flag==0) & (normal==1)]),
            uptime_days_warmer = n_distinct(flowdate[(mtbf_flag==0) & (warmer_than_normal==1)]),
            
            downtime_days = n_distinct(flowdate[mtbf_flag!=0]), 
            downtime_days_cooler = n_distinct(flowdate[(mtbf_flag!=0) & (cooler_than_normal==1)]),
            downtime_days_normal = n_distinct(flowdate[(mtbf_flag!=0) & (normal==1)]),
            downtime_days_warmer = n_distinct(flowdate[(mtbf_flag!=0) & (warmer_than_normal==1)]),
            
            
            cooler_than_normal_days = sum(cooler_than_normal), 
            normal_days = sum(normal),
            warmer_than_normal_days = sum(warmer_than_normal),
            
            total_sessions = sum(sessions, na.rm = TRUE),
            sessions_on_cooler_days = sum(sessions[cooler_than_normal==1], na.rm = TRUE),
            sessions_on_normal_days = sum(sessions[normal==1], na.rm = TRUE),
            sessions_on_warmer_days = sum(sessions[warmer_than_normal==1], na.rm = TRUE),
            
            total_customers = sum(unique_customers, na.rm = TRUE),
            uniq_customers_on_cooler_days = sum(unique_customers[cooler_than_normal==1], na.rm = TRUE),
            uniq_customers_on_normal_days = sum(unique_customers[normal==1], na.rm = TRUE),
            uniq_customers_on_warmer_days = sum(unique_customers[warmer_than_normal==1], na.rm = TRUE),
            
            total_minutes = sum(minutes, na.rm = TRUE),
            minutes_charging_on_cooler_days = sum(minutes[cooler_than_normal==1], na.rm = TRUE),
            minutes_charging_on_normal_days = sum(minutes[normal==1], na.rm = TRUE),
            minutes_charging_on_warmer_days = sum(minutes[warmer_than_normal==1], na.rm = TRUE),
            
            total_session_kWh = sum(session_kwh, na.rm = TRUE),
            session_kWh_on_cooler_days = sum(session_kwh[cooler_than_normal==1], na.rm = TRUE),
            session_kWh_on_normal_days = sum(session_kwh[normal==1], na.rm = TRUE),
            session_kWh_on_warmer_days = sum(session_kwh[warmer_than_normal==1], na.rm = TRUE),
            
            total_session_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            session_MWh_on_cooler_days = sum(session_kwh[cooler_than_normal==1], na.rm = TRUE)/1000,
            session_MWh_on_normal_days = sum(session_kwh[normal==1], na.rm = TRUE)/1000,
            session_MWh_on_warmer_days = sum(session_kwh[warmer_than_normal==1], na.rm = TRUE)/1000,
            distance_to_nearest_charger = max(distances_km), # max here due to summarize, but changes nothing
            dist_to_other_charger_classification = case_when(max(distances_km) <= 5 ~ 'A',
                                                             max(distances_km) <= 10 ~ 'B',
                                                             TRUE ~ 'C') 
  )


# Add 'Mean time between failures' metric for each station during its lifetime
im_location_activity_v2$mtbf <- 
  mean_x_between_issues(im_location_activity_v2$uptime_days,
                        im_location_activity_v2$downtime_days)
im_location_activity_v2$mtbf <- round(im_location_activity_v2$mtbf)

im_location_activity_v2$mtbf_cooler <- 
  mean_x_between_issues(im_location_activity_v2$uptime_days_cooler,
                        im_location_activity_v2$downtime_days_cooler)
im_location_activity_v2$mtbf_cooler <- round(im_location_activity_v2$mtbf_cooler)

im_location_activity_v2$mtbf_normal <- 
  mean_x_between_issues(im_location_activity_v2$uptime_days_normal,
                        im_location_activity_v2$downtime_days_normal)
im_location_activity_v2$mtbf_normal <- round(im_location_activity_v2$mtbf_normal)

im_location_activity_v2$mtbf_warmer <- 
  mean_x_between_issues(im_location_activity_v2$uptime_days_warmer,
                        im_location_activity_v2$downtime_days_warmer)
im_location_activity_v2$mtbf_warmer <- round(im_location_activity_v2$mtbf_warmer)




# Get quality of in-maintenance periods - such as duration of IM period - days, minutes, hours
im_weights_by_station <- subset(im_df, im_flag == 'IM') %>% 
  group_by(station_id, mtbf_flag) %>% 
  summarize(duration_of_in_maintenance_period_in_days = n(),
            im_minutes_during_period = sum(minutes_diff),
            im_hours_during_period = sum(minutes_diff)/60)

# get average size and average size weighted by im-minutes of IM period in days
im_weights_by_station <- im_weights_by_station %>% 
  group_by(station_id) %>% 
  summarize(avg_size_of_in_maintenance_periods_in_days = 
              mean(duration_of_in_maintenance_period_in_days),
            avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes = 
              sum(duration_of_in_maintenance_period_in_days * im_minutes_during_period) / sum(im_minutes_during_period))

im_weights_by_station$avg_size_of_in_maintenance_periods_in_days <- 
  round(im_weights_by_station$avg_size_of_in_maintenance_periods_in_days, 2)
im_weights_by_station$avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes <- 
  round(im_weights_by_station$avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes, 2)

im_location_activity_v2 <- merge(im_location_activity_v2, im_weights_by_station, by='station_id')

# location_activity$total_days_in_maintenance <- 
#   round(location_activity$total_days_in_maintenance, 2)

rm(im_weights_by_station)






location_activity <- co_df %>% 
  group_by(station_id, x_axis_unique, y_axis_unique) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            downtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))


subset(location_activity, downtime_days == 0) # yields IM instances less than 30 mins.. remove them
location_activity <- subset(co_df, !(station_id %in% c(386, 505, 678))) # cold
rm(location_activity)


#############
# ## instances in wa_df (only in warm-df) where we need to do some remaining imputation
# wa_df %>%
#   group_by(station_id) %>%
#   summarize(empties = n_distinct(flowdate[is.na(temps)])) %>%
#   arrange(desc(empties))



co_location_activity_v2 <- subset(co_df, !(station_id %in% c(386, 505, 678))) %>% 
  group_by(station_id, charger_company_col, x_axis_unique, y_axis_unique, subregion.y) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            days_since_operation = n_distinct(flowdate),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE),
            total_sessions = sum(sessions, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            total_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            total_customers = sum(unique_customers, na.rm = TRUE),
            total_unique_customers_per_1000s = sum(unique_customers, na.rm = TRUE)/1000,
            uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            uptime_days_cooler = n_distinct(flowdate[(mtbf_flag==0) & (cooler_than_normal==1)]),
            uptime_days_normal = n_distinct(flowdate[(mtbf_flag==0) & (normal==1)]),
            uptime_days_warmer = n_distinct(flowdate[(mtbf_flag==0) & (warmer_than_normal==1)]),
            
            downtime_days = n_distinct(flowdate[mtbf_flag!=0]), 
            downtime_days_cooler = n_distinct(flowdate[(mtbf_flag!=0) & (cooler_than_normal==1)]),
            downtime_days_normal = n_distinct(flowdate[(mtbf_flag!=0) & (normal==1)]),
            downtime_days_warmer = n_distinct(flowdate[(mtbf_flag!=0) & (warmer_than_normal==1)]),
            
            
            cooler_than_normal_days = sum(cooler_than_normal), 
            normal_days = sum(normal),
            warmer_than_normal_days = sum(warmer_than_normal),
            
            total_sessions = sum(sessions, na.rm = TRUE),
            sessions_on_cooler_days = sum(sessions[cooler_than_normal==1], na.rm = TRUE),
            sessions_on_normal_days = sum(sessions[normal==1], na.rm = TRUE),
            sessions_on_warmer_days = sum(sessions[warmer_than_normal==1], na.rm = TRUE),
            
            total_customers = sum(unique_customers, na.rm = TRUE),
            uniq_customers_on_cooler_days = sum(unique_customers[cooler_than_normal==1], na.rm = TRUE),
            uniq_customers_on_normal_days = sum(unique_customers[normal==1], na.rm = TRUE),
            uniq_customers_on_warmer_days = sum(unique_customers[warmer_than_normal==1], na.rm = TRUE),
            
            total_minutes = sum(minutes, na.rm = TRUE),
            minutes_charging_on_cooler_days = sum(minutes[cooler_than_normal==1], na.rm = TRUE),
            minutes_charging_on_normal_days = sum(minutes[normal==1], na.rm = TRUE),
            minutes_charging_on_warmer_days = sum(minutes[warmer_than_normal==1], na.rm = TRUE),
            
            total_session_kWh = sum(session_kwh, na.rm = TRUE),
            session_kWh_on_cooler_days = sum(session_kwh[cooler_than_normal==1], na.rm = TRUE),
            session_kWh_on_normal_days = sum(session_kwh[normal==1], na.rm = TRUE),
            session_kWh_on_warmer_days = sum(session_kwh[warmer_than_normal==1], na.rm = TRUE),
            
            total_session_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            session_MWh_on_cooler_days = sum(session_kwh[cooler_than_normal==1], na.rm = TRUE)/1000,
            session_MWh_on_normal_days = sum(session_kwh[normal==1], na.rm = TRUE)/1000,
            session_MWh_on_warmer_days = sum(session_kwh[warmer_than_normal==1], na.rm = TRUE)/1000,
            distance_to_nearest_charger = max(distances_km), # max here due to summarize, but changes nothing
            dist_to_other_charger_classification = case_when(max(distances_km) <= 5 ~ 'A',
                                                             max(distances_km) <= 10 ~ 'B',
                                                             TRUE ~ 'C') 
  )


# Add 'Mean time between failures' metric for each station during its lifetime
co_location_activity_v2$mtbf <- 
  mean_x_between_issues(co_location_activity_v2$uptime_days,
                        co_location_activity_v2$downtime_days)
co_location_activity_v2$mtbf <- round(co_location_activity_v2$mtbf)

co_location_activity_v2$mtbf_cooler <- 
  mean_x_between_issues(co_location_activity_v2$uptime_days_cooler,
                        co_location_activity_v2$downtime_days_cooler)
co_location_activity_v2$mtbf_cooler <- round(co_location_activity_v2$mtbf_cooler)

co_location_activity_v2$mtbf_normal <- 
  mean_x_between_issues(co_location_activity_v2$uptime_days_normal,
                        co_location_activity_v2$downtime_days_normal)
co_location_activity_v2$mtbf_normal <- round(co_location_activity_v2$mtbf_normal)

co_location_activity_v2$mtbf_warmer <- 
  mean_x_between_issues(co_location_activity_v2$uptime_days_warmer,
                        co_location_activity_v2$downtime_days_warmer)
co_location_activity_v2$mtbf_warmer <- round(co_location_activity_v2$mtbf_warmer)




# Get quality of in-maintenance periods - such as duration of IM period - days, minutes, hours
im_weights_by_station <- subset(co_df, im_flag == 'IM') %>% 
  group_by(station_id, mtbf_flag) %>% 
  summarize(duration_of_in_maintenance_period_in_days = n(),
            im_minutes_during_period = sum(minutes_diff),
            im_hours_during_period = sum(minutes_diff)/60)

# get average size and average size weighted by im-minutes of IM period in days
im_weights_by_station <- im_weights_by_station %>% 
  group_by(station_id) %>% 
  summarize(avg_size_of_in_maintenance_periods_in_days = 
              mean(duration_of_in_maintenance_period_in_days),
            avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes = 
              sum(duration_of_in_maintenance_period_in_days * im_minutes_during_period) / sum(im_minutes_during_period))

im_weights_by_station$avg_size_of_in_maintenance_periods_in_days <- 
  round(im_weights_by_station$avg_size_of_in_maintenance_periods_in_days, 2)
im_weights_by_station$avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes <- 
  round(im_weights_by_station$avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes, 2)

co_location_activity_v2 <- merge(co_location_activity_v2, im_weights_by_station, by='station_id')


rm(im_weights_by_station)








location_activity <- wa_df %>% 
  group_by(station_id, x_axis_unique, y_axis_unique) %>% 
  summarize(uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            downtime_days = n_distinct(mtbf_flag[mtbf_flag!=0]))


subset(location_activity, downtime_days == 0) # yields IM instances less than 30 mins.. remove them
# location_activity <- subset(wa_df, !(station_id %in% c(386, 505, 678))) # cold
rm(location_activity)


#############
# ## instances in wa_df (only in warm-df) where we need to do some remaining imputation
# wa_df %>%
#   group_by(station_id) %>%
#   summarize(empties = n_distinct(flowdate[is.na(temps)])) %>%
#   arrange(desc(empties))




wa_location_activity_v2 <- wa_df %>% 
  group_by(station_id, charger_company_col, x_axis_unique, y_axis_unique, subregion.y) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            days_since_operation = n_distinct(flowdate),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE),
            total_sessions = sum(sessions, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            total_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            total_customers = sum(unique_customers, na.rm = TRUE),
            total_unique_customers_per_1000s = sum(unique_customers, na.rm = TRUE)/1000,
            uptime_days = n_distinct(flowdate[mtbf_flag==0]),
            uptime_days_cooler = n_distinct(flowdate[(mtbf_flag==0) & (cooler_than_normal==1)]),
            uptime_days_normal = n_distinct(flowdate[(mtbf_flag==0) & (normal==1)]),
            uptime_days_warmer = n_distinct(flowdate[(mtbf_flag==0) & (warmer_than_normal==1)]),
            
            downtime_days = n_distinct(flowdate[mtbf_flag!=0]), 
            downtime_days_cooler = n_distinct(flowdate[(mtbf_flag!=0) & (cooler_than_normal==1)]),
            downtime_days_normal = n_distinct(flowdate[(mtbf_flag!=0) & (normal==1)]),
            downtime_days_warmer = n_distinct(flowdate[(mtbf_flag!=0) & (warmer_than_normal==1)]),
            
            
            cooler_than_normal_days = sum(cooler_than_normal), 
            normal_days = sum(normal),
            warmer_than_normal_days = sum(warmer_than_normal),
            
            total_sessions = sum(sessions, na.rm = TRUE),
            sessions_on_cooler_days = sum(sessions[cooler_than_normal==1], na.rm = TRUE),
            sessions_on_normal_days = sum(sessions[normal==1], na.rm = TRUE),
            sessions_on_warmer_days = sum(sessions[warmer_than_normal==1], na.rm = TRUE),
            
            total_customers = sum(unique_customers, na.rm = TRUE),
            uniq_customers_on_cooler_days = sum(unique_customers[cooler_than_normal==1], na.rm = TRUE),
            uniq_customers_on_normal_days = sum(unique_customers[normal==1], na.rm = TRUE),
            uniq_customers_on_warmer_days = sum(unique_customers[warmer_than_normal==1], na.rm = TRUE),
            
            total_minutes = sum(minutes, na.rm = TRUE),
            minutes_charging_on_cooler_days = sum(minutes[cooler_than_normal==1], na.rm = TRUE),
            minutes_charging_on_normal_days = sum(minutes[normal==1], na.rm = TRUE),
            minutes_charging_on_warmer_days = sum(minutes[warmer_than_normal==1], na.rm = TRUE),
            
            total_session_kWh = sum(session_kwh, na.rm = TRUE),
            session_kWh_on_cooler_days = sum(session_kwh[cooler_than_normal==1], na.rm = TRUE),
            session_kWh_on_normal_days = sum(session_kwh[normal==1], na.rm = TRUE),
            session_kWh_on_warmer_days = sum(session_kwh[warmer_than_normal==1], na.rm = TRUE),
            
            total_session_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            session_MWh_on_cooler_days = sum(session_kwh[cooler_than_normal==1], na.rm = TRUE)/1000,
            session_MWh_on_normal_days = sum(session_kwh[normal==1], na.rm = TRUE)/1000,
            session_MWh_on_warmer_days = sum(session_kwh[warmer_than_normal==1], na.rm = TRUE)/1000,
            distance_to_nearest_charger = max(distances_km), # max here due to summarize, but changes nothing
            dist_to_other_charger_classification = case_when(max(distances_km) <= 5 ~ 'A',
                                                             max(distances_km) <= 10 ~ 'B',
                                                             TRUE ~ 'C') 
  )


# Add 'Mean time between failures' metric for each station during its lifetime
wa_location_activity_v2$mtbf <- 
  mean_x_between_issues(wa_location_activity_v2$uptime_days,
                        wa_location_activity_v2$downtime_days)
wa_location_activity_v2$mtbf <- round(wa_location_activity_v2$mtbf)

wa_location_activity_v2$mtbf_cooler <- 
  mean_x_between_issues(wa_location_activity_v2$uptime_days_cooler,
                        wa_location_activity_v2$downtime_days_cooler)
wa_location_activity_v2$mtbf_cooler <- round(wa_location_activity_v2$mtbf_cooler)

wa_location_activity_v2$mtbf_normal <- 
  mean_x_between_issues(wa_location_activity_v2$uptime_days_normal,
                        wa_location_activity_v2$downtime_days_normal)
wa_location_activity_v2$mtbf_normal <- round(wa_location_activity_v2$mtbf_normal)

wa_location_activity_v2$mtbf_warmer <- 
  mean_x_between_issues(wa_location_activity_v2$uptime_days_warmer,
                        wa_location_activity_v2$downtime_days_warmer)
wa_location_activity_v2$mtbf_warmer <- round(wa_location_activity_v2$mtbf_warmer)




# Get quality of in-maintenance periods - such as duration of IM period - days, minutes, hours
im_weights_by_station <- subset(wa_df, im_flag == 'IM') %>% 
  group_by(station_id, mtbf_flag) %>% 
  summarize(duration_of_in_maintenance_period_in_days = n(),
            im_minutes_during_period = sum(minutes_diff),
            im_hours_during_period = sum(minutes_diff)/60)

# get average size and average size weighted by im-minutes of IM period in days
im_weights_by_station <- im_weights_by_station %>% 
  group_by(station_id) %>% 
  summarize(avg_size_of_in_maintenance_periods_in_days = 
              mean(duration_of_in_maintenance_period_in_days),
            avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes = 
              sum(duration_of_in_maintenance_period_in_days * im_minutes_during_period) / sum(im_minutes_during_period))

im_weights_by_station$avg_size_of_in_maintenance_periods_in_days <- 
  round(im_weights_by_station$avg_size_of_in_maintenance_periods_in_days, 2)
im_weights_by_station$avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes <- 
  round(im_weights_by_station$avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes, 2)

wa_location_activity_v2 <- merge(wa_location_activity_v2, im_weights_by_station, by='station_id')


rm(im_weights_by_station)









summary(location_activity_v2$sessions_on_cooler_days)
hist(location_activity_v2$sessions_on_cooler_days)
qqnorm(location_activity_v2$sessions_on_cooler_days)
qqline(location_activity_v2$sessions_on_cooler_days, col='red')

symbox(location_activity_v2$sessions_on_cooler_days)

summary(log(location_activity_v2$sessions_on_cooler_days))
hist(log(location_activity_v2$sessions_on_cooler_days))
qqnorm(log(location_activity_v2$sessions_on_cooler_days))
qqline(log(location_activity_v2$sessions_on_cooler_days), col='red')


g <- ggplot(data = location_activity_v2, aes(x=x_axis_unique, 
                                             y=y_axis_unique))
g + geom_point() +
  geom_label_repel(aes(label=round(log(avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes))), 
                   max.overlaps=Inf)

g <- ggplot(data = location_activity_v2) 
g + geom_voronoi(aes(x=x_axis_unique, y=y_axis_unique,
                     fill = sessions_on_cooler_days)) +
  guides(fill=guide_legend('my test'))
# xlim(-0.05, 1) + ylim(6,8.1) # region 2 in texas
# xlim(-0.05,0.4) + ylim(4.1, 5) # region 1 in texas
# xlim(-0.5,3.5) + ylim(3.25,8) # texas region
# xlim(14.5,18.5) + ylim(-0.5,5.5) # florida region
# Avg size of IM periods in days - weighted by IM minutes

# avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes -interesting
# avg_size_of_in_maintenance_periods_in_days -interesting
# mtbf - interesting
# downtime days-interesting
# not too good of a measurement 
# log total minutes
# total_days_in_maintenance - log transformation is best
# total_hours_in_maintenance - log transformation is best



g <- ggplot(data = location_activity_v2, 
            aes(x=x_axis_unique, y=y_axis_unique))
g + geom_point()
  # xlim(13,20) + ylim(0,5) # florida region
# xlim(-0.5,3.5) + ylim(3.25,8) # texas region

g <- ggplot(data = location_activity_v2, aes(x=x_axis_unique, y=y_axis_unique))
g + stat_voronoi(geom = 'path') +
  geom_point(aes(fill=downtime_days))
  # xlim(-0.5,3.5) + ylim(3.25,8) # texas region

# g <- ggplot(data = location_activity_v2, aes(x=x_axis_unique, 
#                                           y=y_axis_unique))
# g + geom_point(aes(size=downtime_instances,
#                    color=total_unique_customers_per_1000s,
#                    shape=charger_company_col))
# 
g <- ggplot(data=wa_df, aes(x=temps,
                            group=station_id,
                            col=charger_company_col))
g + geom_density()
# 
# g <- ggplot(data=im_df, aes(x=temps))
# g + geom_density()

# interesting
# g <- ggplot(data = location_activity_v2, aes(x=x_axis_unique, 
#                                           y=y_axis_unique))
# g + geom_point(aes(size=downtime_instances,
#                    color=mtbf))


g <- ggplot(data = location_activity_v2, aes(x=x_axis_unique, 
                                             y=y_axis_unique))
g + geom_point() +
  geom_label_repel(aes(label=mtbf), max.overlaps=Inf)
# geom_label(aes(label=total_hours_in_maintenance_log)) 
# geom_text(aes(label=downtime_instances))
# geom_text(aes(label=total_hours_in_maintenance_log), vjust=-3) +
# geom_text(aes(label=total_hours_in_maintenance_log), hjust=1, vjust=5) +
# xlim(0,5) +
# ylim(1,8)
# geom_label(aes(label=total_sessions))


subset(location_activity_v2, (x_axis_unique < 1 & y_axis_unique < 5))















# xtabs(wa_df$im_flag_digit ~ wa_df$cooler_than_normal + wa_df$normal + wa_df$warmer_than_normal)
my_test_table <- xtabs(~ wa_df$im_flag_digit + wa_df$cooler_than_normal)
# summary(xtabs(~ wa_df$im_flag_digit + wa_df$warmer_than_normal))
# my_test_table <- table(wa_df$im_flag_digit, wa_df$warmer_than_normal)
# prop.table(my_test_table, 1)
# prop.table(my_test_table, 2)
chisq.test(wa_df$im_flag_digit, wa_df$cooler_than_normal)

barplot(my_test_table, main='IM by temps')







####################################################################################################
####################################################################################################
####################################################################################################
################################################## Station performance per its IM stage point in time
################################################### similar to my intentions with Poisson Regression, 
################################################### a single station can be considered multiple times
################################################### as a point depending on the number of IM phases it
################################################### has encountered throughout FY 2019-2020.
#########################




# I've already built the running.im.adj.now field
## this puts the focus on the IM history of the station (i.e. we can evaluate station
## performance across key stats based on their history of IM instances). This can also  
## be used to gauge the temp before a station goes into IM, along with the history of IM instances
## the station has encountered. This EDA provides additional insight into a point in time as per 
## it's IM stage in life.
charging_and_temp_df <- wa_df %>% 
  group_by(station_id, running.im.adj.now) %>% 
  summarize(cooler_than_normal_days = sum(cooler_than_normal), 
            normal_days = sum(normal),
            warmer_than_normal_days = sum(warmer_than_normal),
            
            total_sessions = sum(sessions, na.rm = TRUE),
            sessions_on_cooler_days = sum(sessions[cooler_than_normal==1], na.rm = TRUE),
            sessions_on_normal_days = sum(sessions[normal==1], na.rm = TRUE),
            sessions_on_warmer_days = sum(sessions[warmer_than_normal==1], na.rm = TRUE),
            
            total_customers = sum(unique_customers, na.rm = TRUE),
            uniq_customers_on_cooler_days = sum(unique_customers[cooler_than_normal==1], na.rm = TRUE),
            uniq_customers_on_normal_days = sum(unique_customers[normal==1], na.rm = TRUE),
            uniq_customers_on_warmer_days = sum(unique_customers[warmer_than_normal==1], na.rm = TRUE),
            
            total_minutes = sum(minutes, na.rm = TRUE),
            minutes_charging_on_cooler_days = sum(minutes[cooler_than_normal==1], na.rm = TRUE),
            minutes_charging_on_normal_days = sum(minutes[normal==1], na.rm = TRUE),
            minutes_charging_on_warmer_days = sum(minutes[warmer_than_normal==1], na.rm = TRUE),
            
            total_session_kWh = sum(session_kwh, na.rm = TRUE),
            session_kWh_on_cooler_days = sum(session_kwh[cooler_than_normal==1], na.rm = TRUE),
            session_kWh_on_normal_days = sum(session_kwh[normal==1], na.rm = TRUE),
            session_kWh_on_warmer_days = sum(session_kwh[warmer_than_normal==1], na.rm = TRUE),
            
            duration_in_days_of_im = n_distinct(flowdate[im_flag=='IM']))

g <- ggplot(data = charging_and_temp_df, 
            aes(x=cooler_than_normal_days, 
                group=running.im.adj.now, 
                col=factor(running.im.adj.now)))
g + geom_density() 

g <- ggplot(data=charging_and_temp_df,
            aes(x=session_kWh_on_normal_days, 
                group=running.im.adj.now, 
                col=factor(running.im.adj.now)))
g + geom_density(aes(fill=factor(running.im.adj.now)), alpha=0.8) 

g <- ggplot(data=charging_and_temp_df, 
            aes(x=running.im.adj.now, 
                y=session_kWh_on_warmer_days, 
                group=running.im.adj.now))
g + geom_boxplot()

g <- ggplot(data=charging_and_temp_df, 
            aes(x=running.im.adj.now, 
                y=duration_in_days_of_im, 
                group=running.im.adj.now))
g + geom_boxplot()
tapply(charging_and_temp_df$duration_in_days_of_im, 
       charging_and_temp_df$running.im.adj.now,
       summary)




g <- ggplot(data=charging_and_temp_df,
            aes(x=total_sessions,
                y=total_session_kWh))

g + geom_jitter(aes(col=factor(running.im.adj.now)))



tapply(charging_and_temp_df$total_session_kWh, 
       charging_and_temp_df$running.im.adj.now,
       summary)


g <- ggplot(data=location_activity, 
            aes(x=x_axis_unique, y=y_axis_unique))
g + geom_point(aes(size=mtbf)) +
  xlim(c(0,0.5)) + ylim(c(2.5, 5))
# xlim(c(0,4)) + ylim(c(2.5, 10))










# std_response <- standardize(log(location_activity$total_hours_in_maintenance))
# hist(std_response)
# qqnorm(std_response)
# qqline(std_response, col='red', lwd=2)
# 
# # log transformation seems to be best hter
# symbox(location_activity$downtime_instances, 
#        data=location_activity)


 


################################################################################
##################### broken out by charger-company
var <- 'downtime_days'
tapply(location_activity[, var],
       location_activity$charger_company_col,
       summary)

g <- ggplot(data=location_activity, aes(x=location_activity[, var],
                                        fill=charger_company_col))
g + geom_histogram() + guides(fill=guide_legend('legend')) + labs(title = var)
# head(location_activity)
sep_by_charger <- subset(location_activity, charger_company_col == 'eeyin')
sep_by_charger <- subset(location_activity, charger_company_col == 'vxxip')
charger_comp <- as.character(unique(sep_by_charger$charger_company_col))

qqnorm(location_activity[, var], main = c('QQ-Plot: ', charger_comp, ' ', var))
qqline(location_activity[, var], col='red')

symbox(location_activity[, var], main=var)

summary(log(location_activity[, var]))
hist(log(location_activity[, var]), main=c(charger_comp, ' ', var))
qqnorm(log(location_activity[, var]), main = c('QQ-Plot: ', charger_comp))
qqline(log(location_activity[, var]), col='red')

g <- ggplot(data = sep_by_charger)
g + geom_voronoi(aes(x=x_axis_unique, y=y_axis_unique,
                     fill = log(avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes))) +
  # guides(fill=guide_legend('my test')) +
  labs(title = c(charger_comp, ' ', var)) +
  guides(fill=guide_legend('Avg size of IM periods in days (weighted by im minutes)'))



# avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes same as below but there's more of an emphasis here due to the added weights
# avg_size_of_in_maintenance_periods_in_days - interesting; follows the normal narrative
# mtbf - interesting here, because it flips the narrative on which charger-company is more reliant
# downtime_days - interesting
# total_sessions - interesting
# total_hours_in_maintenance - interesting
# total_minutes_in_maintenance - interesting
# number_of_in_maintenance_periods - interesting breakouts for each of these groups for this warm region
## would be good to explore if they're statistically different (i.e. any response var we're interested in)




