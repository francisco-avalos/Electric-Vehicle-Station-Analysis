
####################################################################################
######## R Libraries
####################################################################################

library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)

# if(!require(ggplot2)){install.packages("lubridate")}
# if(!require(ggplot2)){install.packages("tidyr")}
# if(!require(ggplot2)){install.packages("dplyr")}



####################################################################################
######## Import raw data from MySQL
####################################################################################

setwd('/Users/francisco.avalos/Desktop/thesis/data/cold.stations/')
path <- '/Users/francisco.avalos/Desktop/thesis/data/cold.stations/'
temp <- list.files(path = path, pattern = '*.csv') 


data.df <- do.call(rbind, lapply(temp, function(x) read.csv(x)))

print('Data imported...')

####################################################################################
######## Data Functions
####################################################################################

is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), 
                                                     tz = 'UTC', 
                                                     format = '%Y-%m-%d'))



####################################################################################
######## Data Preparation
####################################################################################


############################
#### Loop by day and station to mark flags of interest
########

data.df$minutes_diff <- as.integer(data.df$minutes_diff)


data.df$start_period <- NA
data.df$bucketed_im_minutes <- NA
data.df$bucketed_im_minutes.v10 <- NA
data.df$period_count <- NA
data.df$day_count <- NA
v <- unique(data.df$station_id)
rv <- vector()
bucket.v <- vector()
bucket.v10 <- vector()
n.periods.v <- vector()
n.day.periods.v <- vector()
running.im.now.v <- vector()
running.im.adjusted.now.v <- vector()
mtbf.im.flag.v <- vector()
min.im.minutes.threshold <- 30

for (i in v) {
  sub_date.df <- subset(data.df, station_id == i)
  s.flowdate <- sub_date.df$flowdate
  s.imdate <- sub_date.df$im_date
  s.im.minutes <- sub_date.df$minutes_diff
  x.dates <- numeric(nrow(sub_date.df))
  min_date <- min(sub_date.df$flowdate)
  max_date <- max(sub_date.df$flowdate)
  start.date <- min_date
  ii <- 1
  n.periods <- numeric(nrow(sub_date.df))
  n.cycle <- 1
  n.day.periods <- numeric(nrow(sub_date.df))
  n.day.period <- 1
  running.station.day <- numeric(nrow(sub_date.df))
  running.day <- 0
  running.station.adj.day <- numeric(nrow(sub_date.df))
  mtbf.im.flag.day <- numeric(nrow(sub_date.df))
  running.day.adj <- 0
  mtbf.day <- 0
  
  while(min_date <= max_date) {
    check.day.before <- 
      ifelse(ii==1, s.imdate[ii], as.character(s.imdate[ii-1]))
    db.flag <- is.convertible.to.date(check.day.before)
    check.cur.day <- 
      ifelse(identical(s.imdate[ii], character(0)), NULL, as.character(s.imdate[ii]))
    cd.flag <- is.convertible.to.date(check.cur.day)
    check.day.after <- 
      ifelse(min_date==max_date, as.character(s.imdate[ii]), as.character(s.imdate[ii+1]))
    da.flag <- is.convertible.to.date(check.day.after)
    
    if(!cd.flag) {
      x.dates[ii] <- start.date
      n.periods[ii] <- n.cycle
      n.day.periods[ii] <- n.day.period
      running.station.day[ii] <- running.day
    } else if (!(db.flag) & cd.flag & da.flag){
      start.date <- as.character(min_date)
      x.dates[ii] <- start.date
      n.cycle <- n.cycle+1
      n.cycle <- n.cycle
      n.day.periods[ii] <- 0
      n.day.period <- 0
      n.periods[ii] <- n.cycle
      running.day <- running.day+1
      running.station.day[ii] <- running.day
    } else if(db.flag & cd.flag & da.flag) {
      x.dates[ii] <- start.date
      n.periods[ii] <- n.cycle
      n.day.periods[ii] <- 0
      n.day.period <- 0
      running.station.day[ii] <- running.day
    } else if(db.flag & cd.flag & !(da.flag)) {
      x.dates[ii] <- start.date
      start.date <- as.character((as.Date(min_date)+1))
      n.periods[ii] <- n.cycle
      n.cycle <- n.cycle+1
      n.day.periods[ii] <- 0
      n.day.period <- 0
      running.station.day[ii] <- running.day
    } else if((db.flag & !(cd.flag)) | (!(db.flag) & cd.flag)) {
      start.date <- as.character(as.Date(min_date))
      x.dates[ii] <- start.date
      start.date <- as.character((as.Date(min_date)+1))
      n.cycle <- n.cycle+1
      n.periods[ii] <- n.cycle
      n.cycle <- n.cycle+1
      n.day.periods[ii] <- 0
      n.day.period <- 0
      running.day <- running.day+1
      running.station.day[ii] <- running.day
    } 
    today.im.value <- ifelse(is.na(s.im.minutes[ii]), 0, s.im.minutes[ii])
    if(!cd.flag) {
      running.station.adj.day[ii] <- running.day.adj
      mtbf.im.flag.day[ii] <- 0
    } else if( (!(db.flag) & cd.flag & da.flag) & (today.im.value >= min.im.minutes.threshold)) {
      running.day.adj <- running.day.adj+1
      running.station.adj.day[ii] <- running.day.adj
      mtbf.day <- mtbf.day+1
      mtbf.im.flag.day[ii] <- mtbf.day
    } else if((db.flag & cd.flag & da.flag) & (today.im.value >= min.im.minutes.threshold)) {
      running.station.adj.day[ii] <- running.day.adj
      mtbf.im.flag.day[ii] <- mtbf.day
    } else if((db.flag & cd.flag & !(da.flag)) & (today.im.value >= min.im.minutes.threshold)) {
      running.station.adj.day[ii] <- running.day.adj
      mtbf.im.flag.day[ii] <- mtbf.day
    } else if(((db.flag & !(cd.flag)) | (!(db.flag) & cd.flag)) & (today.im.value >= min.im.minutes.threshold)) {
      running.day.adj <- running.day.adj+1
      running.station.adj.day[ii] <- running.day.adj
      mtbf.day <- mtbf.day+1
      mtbf.im.flag.day[ii] <- mtbf.day
    } else if(((db.flag & !(cd.flag)) | (!(db.flag) & cd.flag)) & (today.im.value < min.im.minutes.threshold)) {
      running.station.adj.day[ii] <- running.day.adj
      mtbf.im.flag.day[ii] <- mtbf.day
    }
    
    min_date <- as.Date(min_date)+1
    ii <- ii+1
    n.day.period <- n.day.period+1
  }
  rv <- append(rv, x.dates)
  individual.station.holder <- 
    case_when(is.na(s.im.minutes) ~ 0,
              s.im.minutes <= 360 ~ 1,
              s.im.minutes <= 720 ~ 2,
              s.im.minutes <= 1080 ~ 3,
              s.im.minutes<= 1440 ~ 4)
  individual.station.holder.v10 <- 
    case_when(is.na(s.im.minutes) ~ 0,
              s.im.minutes <= 144 ~ 1,
              s.im.minutes <= 288 ~ 2,
              s.im.minutes <= 432 ~ 3,
              s.im.minutes <= 576 ~ 4,
              s.im.minutes <= 720 ~ 5,
              s.im.minutes <= 864 ~ 6,
              s.im.minutes <= 1008 ~ 7,
              s.im.minutes <= 1152 ~ 8,
              s.im.minutes <= 1296 ~ 9,
              s.im.minutes <= 1440 ~ 10)
  bucket.v <- append(bucket.v, individual.station.holder)
  bucket.v10 <- append(bucket.v10, individual.station.holder.v10)
  n.periods.v <- append(n.periods.v, n.periods)
  n.day.periods.v <- append(n.day.periods.v, n.day.periods)
  running.im.now.v <- append(running.im.now.v, running.station.day)
  running.im.adjusted.now.v <- 
    append(running.im.adjusted.now.v, running.station.adj.day)
  mtbf.im.flag.v <- append(mtbf.im.flag.v, mtbf.im.flag.day)
}

############################
#### Apply flags of interest to data
########

data.df$start_period <- rv
data.df$bucketed_im_minutes <- bucket.v
data.df$bucketed_im_minutes.v10 <- bucket.v10
data.df$period_count <- n.periods.v
data.df$day_count <- n.day.periods.v
data.df$running.im.now <- running.im.now.v
data.df$running.im.adj.now <- running.im.adjusted.now.v
data.df$mtbf_flag <- mtbf.im.flag.v


############################
#### Format the data
########

data.df$flowdate <- as.Date(data.df$flowdate, format='%Y-%m-%d')
data.df$im_date <- as.Date(data.df$im_date, format = '%Y-%m-%d')
data.df$im_start <- 
  strptime(data.df$im_start, format = '%Y-%m-%d %H:%M:%S')
data.df$single_im_day <- 
  as.Date(data.df$single_im_day, format = '%Y-%m-%d')
data.df$im_end<- 
  strptime(data.df$im_end, format = '%Y-%m-%d %H:%M:%S')
data.df$target_start_date <- 
  as.Date(data.df$target_start_date, format = '%Y-%m-%d')
data.df$target_end_date <- 
  as.Date(data.df$target_end_date, format = '%Y-%m-%d')
data.df$session_start_date <- 
  as.Date(data.df$session_start_date, format = '%Y-%m-%d')
data.df$sessions <- as.integer(data.df$sessions)
data.df$minutes <- as.integer(data.df$minutes)
# options(digits = 7)
data.df$session_kwh <- as.double(data.df$session_kwh)
data.df$unique_customers <- as.integer(data.df$unique_customers)
data.df$start_period <- 
  as.Date(data.df$start_period, format = '%Y-%m-%d')

data.df$im_flag <- 
  case_when(is.na(data.df$im_date) ~ "NO IM", TRUE ~ "IM")



############################
#### Cut the data to any stations in maintenance 
########

station.df <- data.df %>% 
  group_by(station_id) %>% 
  summarize(max = max(im_date, na.rm = TRUE),
            min = min(im_date, na.rm = TRUE))
station.df$max <- as.character(station.df$max)
station.df$min <- as.character(station.df$min)

stations.interested.df <- 
  subset(station.df, !is.na(max))['station_id']
cold.data.of.interest.df <- 
  merge(data.df, stations.interested.df, by='station_id')
cold.data.of.interest.df <- 
  cold.data.of.interest.df %>% relocate(station_id, .before = station_latitude)



station.days.until <- cold.data.of.interest.df %>% 
  group_by(station_id, period_count) %>% 
  summarize(max.periods = max(day_count))

cold.data.of.interest.df <- 
  merge(cold.data.of.interest.df, station.days.until, by=c('station_id', 'period_count'))
cold.data.of.interest.df$remaining_days <- 
  cold.data.of.interest.df$max.periods - cold.data.of.interest.df$day_count
cold.data.of.interest.df <- 
  cold.data.of.interest.df %>% relocate(station_id, .before = station_latitude)
cold.data.of.interest.df <- 
  cold.data.of.interest.df %>% relocate(period_count, .after = max.periods)

cold.data.of.interest.df <- 
  cold.data.of.interest.df[order(cold.data.of.interest.df$station_id, cold.data.of.interest.df$flowdate), ]



############################
#### Fill in missing spatial coordinates for each station due to MySQL file 
########

# cold.data.of.interest.df$station_latitude <- as.double(cold.data.of.interest.df$station_latitude)
# cold.data.of.interest.df$station_longitude <- as.double(cold.data.of.interest.df$station_longitude)
# 
# station_lat.vec <- vector()
# station_lon.vec <- vector()
# stations <- unique(cold.data.of.interest.df$station_id)
# for (i in stations) {
#   subset.cycle.df <- subset(cold.data.of.interest.df, station_id == i)
#   lat_length <- numeric(nrow(subset.cycle.df))
#   lot_length <- numeric(nrow(subset.cycle.df))
#   
#   lat_min <- min(subset.cycle.df$station_latitude, na.rm = TRUE)
#   lot_min <- min(subset.cycle.df$station_longitude, na.rm = TRUE)
#   
#   station_lat.vec <- append(station_lat.vec, rep(lat_min, length(lat_length)))
#   station_lon.vec <- append(station_lon.vec, rep(lot_min, length((lot_length))))
# }
# 
# cold.data.of.interest.df$station_latitude <- station_lat.vec
# cold.data.of.interest.df$station_longitude <- station_lon.vec

cold.data.of.interest.df$station_instances <- 
  paste(cold.data.of.interest.df$station_id, '-', cold.data.of.interest.df$running.im.now)
cold.data.of.interest.df$station_intances_adj <- 
  paste(cold.data.of.interest.df$station_id, '-', cold.data.of.interest.df$running.im.adj.now)

############################
#### Clean up
########

rm(v, rv, bucket.v, bucket.v10, n.periods.v, 
   n.day.periods.v, i, sub_date.df, s.flowdate, 
   s.imdate, s.im.minutes, x.dates, min_date,
   max_date, start.date, ii, n.periods, 
   n.cycle, n.day.periods, n.day.period, cd.flag,
   check.cur.day, check.day.after, check.day.before,
   da.flag, db.flag, individual.station.holder, 
   individual.station.holder.v10, running.im.now.v, running.station.day, 
   running.day, running.im.adjusted.now.v, running.station.adj.day,
   running.day.adj, today.im.value, stations, i, station_lon.vec, 
   station_lat.vec, subset.cycle.df, lat_length, lot_length, lat_min, lot_min,
   station.df, stations.interested.df, station.days.until, temp,
   mtbf.im.flag.v, mtbf.im.flag.day, mtbf.day, min.im.minutes.threshold, path)


rm(data.df)

############################
#### Export Confirmation
########


path <- 'C:/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/'
export.name <- 'cold_temps_stations.csv'
write.csv(cold.data.of.interest.df, paste0(path, export.name))

print(paste0('Exported ', export.name, ' to ', path))
print('Done!')

rm(path, export.name, min.im.minutes.threshold)

