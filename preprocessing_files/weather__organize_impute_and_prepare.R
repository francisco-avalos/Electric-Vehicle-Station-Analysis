library(dplyr)
library(imputeTS)



path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/prefinal_weather_data/'
file.name <- 'intermediate_temperature.csv'
intermediate_weather_stations_df <- read.csv(paste0(path, file.name))
file.name <- 'cold_temperature.csv'
cold_weather_stations_df <- read.csv(paste0(path, file.name))
file.name <- 'warm_temperature.csv'
warm_weather_stations_df <- read.csv(paste0(path, file.name))

rm(path, file.name)

# formats
cold_weather_stations_df$DATE <- 
  as.Date(cold_weather_stations_df$DATE, '%Y-%m-%d')
intermediate_weather_stations_df$DATE <- 
  as.Date(intermediate_weather_stations_df$DATE, '%Y-%m-%d')
warm_weather_stations_df$DATE <- 
  as.Date(warm_weather_stations_df$DATE, '%Y-%m-%d')


# remove this weather station
warm_weather_stations_df <- 
  warm_weather_stations_df[which(warm_weather_stations_df$STATION!='A0000812978'), ]
warm_weather_stations_df$STATION <- as.numeric(warm_weather_stations_df$STATION)



### Functions:
preserve_type_ifelse <- function(cond, yes, no){
  class.y <- class(yes)
  X <- ifelse(cond, yes, no)
  class(X) <- class.y; 
  return(X)
}



########################
#### Identify days where the weather station was out, fill in their temperature  
####  using Kalman Smoothing and State Space models. 
########################

##################################### INTERMEDIATE.1

weather_station_day_avail <- intermediate_weather_stations_df %>% 
  group_by(STATION) %>% 
  summarize(days = n())

weather_station_day_avail <- 
  weather_station_day_avail[order(weather_station_day_avail$days, 
                                  decreasing = FALSE), ]

intermediate_target_group <- subset(weather_station_day_avail, days < 731)
intermediate_target_group <- 
  intermediate_target_group[order(intermediate_target_group$days, 
                                  decreasing = FALSE), ]
rm(weather_station_day_avail)

cycles <- as.numeric(unlist(intermediate_target_group[1]))
filler_dates <- vector()
filler_temps <- vector()
filler_weather_station <- vector()
gap_dates <- vector()

for (i in cycles) {
  d <- subset(intermediate_weather_stations_df, STATION == i)['DATE']
  d <- as.character(t(d))
  d <- as.Date(d)
  date_range <- seq(min(d), max(d), by=1)
  gaps <- date_range[!date_range %in% d]
  for (ii in seq_along(gaps)) {
    d2019 <- paste0('2019', '-', 
                    unlist(strsplit(as.character(gaps[ii]), '-'))[2],'-',
                    unlist(strsplit(as.character(gaps[ii]), '-'))[3])
    d2020 <- paste0('2020', '-', 
                    unlist(strsplit(as.character(gaps[ii]), '-'))[2],'-',
                    unlist(strsplit(as.character(gaps[ii]), '-'))[3])
    d2019 <- as.Date(d2019, '%Y-%m-%d')
    d2020 <- as.Date(d2020, '%Y-%m-%d')
    get.date <- 
      preserve_type_ifelse(as.Date(format(gaps[ii], '%Y-%m-%d')) %in% c(d2019, d2020), 
                           c(d2019, d2020)[which(!c(d2019, d2020) %in% as.Date(format(gaps[ii], '%Y-%m-%d')))],
                           'none')
    same_date_other_year_temp <- 
      subset(intermediate_weather_stations_df, STATION == i & 
               DATE == get.date)['TEMP']
    same_date_other_year_temp <- as.numeric(same_date_other_year_temp)
    
    filler_weather_station <- append(filler_weather_station, i)
    filler_dates <- append(filler_dates, gaps[ii])
    filler_temps <- append(filler_temps, same_date_other_year_temp)
  }
}

enter_empties <- cbind.data.frame(NA, filler_weather_station, 
                                  filler_dates, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA)
colnames(enter_empties) <- colnames(intermediate_weather_stations_df)


intermediate_weather_stations_df <- 
  rbind(intermediate_weather_stations_df, enter_empties)
intermediate_weather_stations_df <- 
  intermediate_weather_stations_df[order(intermediate_weather_stations_df$STATION, 
                                         intermediate_weather_stations_df$DATE), ]

# intermediate_target_group

station_lat.vec <- vector()
station_lon.vec <- vector()

stations <- unique(filler_weather_station)
for (station in stations) {
  subset_ <- subset(intermediate_weather_stations_df, STATION==station)
  
  lat_min <- min(subset_$LATITUDE, na.rm = TRUE)
  lon_min <- min(subset_$LONGITUDE, na.rm = TRUE)
  
  lat_vec <- rep(lat_min, nrow(subset_))
  lon_vec <- rep(lon_min, nrow(subset_))
  
  intermediate_weather_stations_df[which(intermediate_weather_stations_df$STATION==station), ]$LATITUDE <- 
    lat_vec
  intermediate_weather_stations_df[which(intermediate_weather_stations_df$STATION==station), ]$LONGITUDE <- 
    lon_vec
}
rm(station_lat.vec, station_lon.vec)


intermediate_weather_stations_df$imputed <- NA
intermediate_target_group <- as.data.frame(intermediate_target_group)

for (station in intermediate_target_group$STATION) {
  one <- subset(intermediate_weather_stations_df, STATION == station)$TEMP
  imputed <- na_kalman(one)
  intermediate_weather_stations_df[which(intermediate_weather_stations_df$STATION==station),]$TEMP <-
    imputed
  flag <- subset(intermediate_weather_stations_df, STATION == station)$DEWP
  imputed_flag <- ifelse(is.na(flag), 'A', 'B')
  intermediate_weather_stations_df[which(intermediate_weather_stations_df$STATION==station),]$imputed <-
    imputed_flag
}



rm(cycles, filler_dates, filler_temps, filler_weather_station, gap_dates, d,
   d2019, d2020, date_range, gaps, get.date, i, ii, same_date_other_year_temp, 
   enter_empties, station, one, imputed, flag, imputed_flag)


##################################### COLD.2

weather_station_day_avail <- cold_weather_stations_df %>% 
  group_by(STATION) %>% 
  summarize(days = n())

weather_station_day_avail <- 
  weather_station_day_avail[order(weather_station_day_avail$days, decreasing = FALSE), ]

cold_target_group <- subset(weather_station_day_avail, days < 731)
cold_target_group <- cold_target_group[order(cold_target_group$days, decreasing = FALSE), ]
rm(weather_station_day_avail)

cycles <- as.numeric(unlist(cold_target_group[1]))
filler_dates <- vector()
filler_temps <- vector()
filler_weather_station <- vector()
gap_dates <- vector()

for (i in cycles) {
  d <- subset(cold_weather_stations_df, STATION == i)['DATE']
  d <- as.character(t(d))
  d <- as.Date(d)
  date_range <- seq(min(d), max(d), by=1)
  gaps <- date_range[!date_range %in% d]
  for (ii in seq_along(gaps)) {
    d2019 <- paste0('2019', '-', 
                    unlist(strsplit(as.character(gaps[ii]), '-'))[2],'-',
                    unlist(strsplit(as.character(gaps[ii]), '-'))[3])
    d2020 <- paste0('2020', '-', 
                    unlist(strsplit(as.character(gaps[ii]), '-'))[2],'-',
                    unlist(strsplit(as.character(gaps[ii]), '-'))[3])
    d2019 <- as.Date(d2019, '%Y-%m-%d')
    d2020 <- as.Date(d2020, '%Y-%m-%d')
    get.date <- 
      preserve_type_ifelse(as.Date(format(gaps[ii], '%Y-%m-%d')) %in% c(d2019, d2020), 
                           c(d2019, d2020)[which(!c(d2019, d2020) %in% as.Date(format(gaps[ii], '%Y-%m-%d')))],
                           'none')
    same_date_other_year_temp <- 
      subset(cold_weather_stations_df, STATION == i & 
               DATE == get.date)['TEMP']
    same_date_other_year_temp <- as.numeric(same_date_other_year_temp)
    
    filler_weather_station <- append(filler_weather_station, i)
    filler_dates <- append(filler_dates, gaps[ii])
    filler_temps <- append(filler_temps, same_date_other_year_temp)
  }
}

enter_empties <- cbind.data.frame(NA, filler_weather_station, 
                                  filler_dates, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA)
colnames(enter_empties) <- colnames(cold_weather_stations_df)
cold_weather_stations_df <- rbind(cold_weather_stations_df, enter_empties)



cold_weather_stations_df <- 
  cold_weather_stations_df[order(cold_weather_stations_df$STATION, 
                                 cold_weather_stations_df$DATE), ]

# cold_target_group

station_lat.vec <- vector()
station_lon.vec <- vector()

stations <- unique(filler_weather_station)
for (station in stations) {
  subset_ <- subset(cold_weather_stations_df, STATION==station)
  
  lat_min <- min(subset_$LATITUDE, na.rm = TRUE)
  lon_min <- min(subset_$LONGITUDE, na.rm = TRUE)
  
  lat_vec <- rep(lat_min, nrow(subset_))
  lon_vec <- rep(lon_min, nrow(subset_))
  
  cold_weather_stations_df[which(cold_weather_stations_df$STATION==station), ]$LATITUDE <- 
    lat_vec
  cold_weather_stations_df[which(cold_weather_stations_df$STATION==station), ]$LONGITUDE <- 
    lon_vec
}
rm(station_lat.vec, station_lon.vec)


cold_weather_stations_df$imputed <- NA
cold_target_group <- as.data.frame(cold_target_group)

for (station in cold_target_group$STATION) {
  one <- subset(cold_weather_stations_df, STATION == station)$TEMP
  imputed <- na_kalman(one)
  cold_weather_stations_df[which(cold_weather_stations_df$STATION==station),]$TEMP <-
    imputed
  flag <- subset(cold_weather_stations_df, STATION == station)$DEWP
  imputed_flag <- ifelse(is.na(flag), 'A', 'B')
  cold_weather_stations_df[which(cold_weather_stations_df$STATION==station),]$imputed <-
    imputed_flag
}


rm(cycles, filler_dates, filler_temps, filler_weather_station, gap_dates, d,
   d2019, d2020, date_range, gaps, get.date, i, ii, same_date_other_year_temp, 
   enter_empties, station, one, imputed, flag, imputed_flag)



## add in for 99734299999
df_last <- subset(cold_weather_stations_df, STATION==99734299999)
date_range <- seq(as.Date('2019-01-01'), as.Date('2020-12-31'), by=1)
gaps <- date_range[!date_range %in% df_last$DATE]
filler <- data.frame(x=NA, 
                     STATION=rep(99734299999, length(gaps)), 
                     DATE=gaps, 
                     LATITUDE=rep(min(df_last$LATITUDE), length(gaps)), 
                     LONGITUDE=rep(min(df_last$LONGITUDE), length(gaps)),
                     ELEVATION=NA, NAME=NA, TEMP=NA, DEWP=NA, SLP=NA,
                     STP=NA, VISIB=NA, WDSP=NA, MXSPD=NA, GUST=NA,
                     MAX=NA, MIN=NA, PRCP=NA, SNDP=NA, FRSHTT=NA, 
                     imputed=rep('A', length(gaps)))
colnames(filler) <- colnames(df_last)
merge.df <- rbind(filler, df_last)
merge.df[which(merge.df$DATE=='2019-01-01'), ]$TEMP <- 31.00 # best guess is to initiate as same temp 
# as same date next year, so the imputation algorithm has a good starting initiation
merge.df$TEMP <- na_kalman(merge.df$TEMP)
cold_weather_stations_df <- subset(cold_weather_stations_df, STATION != 99734299999)
cold_weather_stations_df <- rbind(cold_weather_stations_df, merge.df)


cold_weather_stations_df <- 
  cold_weather_stations_df[order(cold_weather_stations_df$STATION, 
                                 cold_weather_stations_df$DATE), ]

rm(df_last, date_range, gaps, filler, merge.df)



##################################### WARM.3

weather_station_day_avail <- warm_weather_stations_df %>% 
  group_by(STATION) %>% 
  summarize(days = n())

weather_station_day_avail <- 
  weather_station_day_avail[order(weather_station_day_avail$days, decreasing = FALSE), ]

warm_target_group <- subset(weather_station_day_avail, days < 731)
warm_target_group <- warm_target_group[order(warm_target_group$days, decreasing = FALSE), ]
rm(weather_station_day_avail)

cycles <- as.numeric(unlist(warm_target_group[1]))
filler_dates <- vector()
filler_temps <- vector()
filler_weather_station <- vector()
gap_dates <- vector()

for (i in cycles) {
  d <- subset(warm_weather_stations_df, STATION == i)['DATE']
  d <- as.character(t(d))
  d <- as.Date(d)
  date_range <- seq(min(d), max(d), by=1)
  gaps <- date_range[!date_range %in% d]
  for (ii in seq_along(gaps)) {
    d2019 <- paste0('2019', '-', 
                    unlist(strsplit(as.character(gaps[ii]), '-'))[2],'-',
                    unlist(strsplit(as.character(gaps[ii]), '-'))[3])
    d2020 <- paste0('2020', '-', 
                    unlist(strsplit(as.character(gaps[ii]), '-'))[2],'-',
                    unlist(strsplit(as.character(gaps[ii]), '-'))[3])
    d2019 <- as.Date(d2019, '%Y-%m-%d')
    d2020 <- as.Date(d2020, '%Y-%m-%d')
    get.date <- 
      preserve_type_ifelse(as.Date(format(gaps[ii], '%Y-%m-%d')) %in% c(d2019, d2020), 
                           c(d2019, d2020)[which(!c(d2019, d2020) %in% as.Date(format(gaps[ii], '%Y-%m-%d')))],
                           'none')
    same_date_other_year_temp <- 
      subset(warm_weather_stations_df, STATION == i & 
               DATE == get.date)['TEMP']
    same_date_other_year_temp <- as.numeric(same_date_other_year_temp)
    
    filler_weather_station <- append(filler_weather_station, i)
    filler_dates <- append(filler_dates, gaps[ii])
    filler_temps <- append(filler_temps, same_date_other_year_temp)
  }
}

enter_empties <- cbind.data.frame(NA, filler_weather_station, 
                                  filler_dates, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA)
colnames(enter_empties) <- colnames(warm_weather_stations_df)
warm_weather_stations_df <- rbind(warm_weather_stations_df, enter_empties)
warm_weather_stations_df <- 
  warm_weather_stations_df[order(warm_weather_stations_df$STATION, 
                                 warm_weather_stations_df$DATE), ]

# warm_target_group

station_lat.vec <- vector()
station_lon.vec <- vector()

stations <- unique(warm_target_group$STATION)
for (station in stations) {
  subset_ <- subset(warm_weather_stations_df, STATION==station)
  
  lat_min <- min(subset_$LATITUDE, na.rm = TRUE)
  lon_min <- min(subset_$LONGITUDE, na.rm = TRUE)
  
  lat_vec <- rep(lat_min, nrow(subset_))
  lon_vec <- rep(lon_min, nrow(subset_))
  
  warm_weather_stations_df[which(warm_weather_stations_df$STATION==station), ]$LATITUDE <- 
    lat_vec
  warm_weather_stations_df[which(warm_weather_stations_df$STATION==station), ]$LONGITUDE <- 
    lon_vec
}
rm(station_lat.vec, station_lon.vec)

warm_weather_stations_df$imputed <- NA
warm_target_group <- as.data.frame(warm_target_group) 
last_day <- as.Date('2020-12-31', '%Y-%m-%d')
for (station_ in warm_target_group$STATION) {
  one <- subset(warm_weather_stations_df, STATION == station_)
  max_date <- max(one$DATE)
  days_remaining <- seq(as.Date(max_date, '%Y-%m-%d') + 1, last_day, by = 1)
  # days_remaining <- ifelse(as.Date(max_date, '%Y-%m-%d') == '2020-12-30', '2020-12-30', seq(as.Date(max_date, '%Y-%m-%d') + 1, last_day, by = 1))
  # days_remaining <- if_else(as.Date(max_date, '%Y-%m-%d') == last_day, last_day, 
  #                          seq(as.Date(max_date, '%Y-%m-%d'), last_day, by = 1))
  day_num_remaining <- 731 - dim(one)[1]
  
  s <- rep(unique(one$STATION), day_num_remaining)
  lat <- rep(unique(one$LATITUDE), day_num_remaining)
  lon <- rep(unique(one$LONGITUDE), day_num_remaining)
  e <- rep(max(one$ELEVATION, na.rm = TRUE), day_num_remaining)
  n <- rep(max(one$NAME, na.rm = TRUE), day_num_remaining)
  
  df <- data.frame(cbind(rep('x', length(s)), s, 
                         date = as.character(days_remaining), 
                         lat, lon, e, n))
  df$date <- as.Date(df$date, '%Y-%m-%d')
  df$TEMP <- NA;    df$DEWP <- NA; 
  df$SLP <- NA;     df$STP <- NA;
  df$VISIB <- NA;   df$WDSP <- NA; 
  df$MXSPD <- NA;   df$GUST <- NA;
  df$MAX <- NA;     df$MIN <- NA; 
  df$PRCP <- NA;    df$SNDP <- NA;
  df$FRSHTT <- NA;  df$imputed <- NA;
  colnames(df) <- colnames(one)
  one$DATE <- as.Date(one$DATE, '%Y-%m-%d')
  
  one <- rbind(one, df)
  one_temps <- one$TEMP
  imputed <- na_kalman(one_temps)
  one$TEMP <- imputed
  warm_weather_stations_df <- subset(warm_weather_stations_df, STATION != station_)
  imputed_flag <- ifelse(is.na(one$DEWP), 'A', 'B')
  one$imputed <- imputed_flag
  warm_weather_stations_df <- rbind(warm_weather_stations_df, one)
  # imputed$imputed <- ifelse((is.na(imputed$X)) | (imputed$X=='x'), 
  #                           'A', 'B')
  
  # warm_weather_stations_df[which(warm_weather_stations_df$STATION==station),]$imputed <-
  #   imputed$imputed
}

warm_target_group <- warm_weather_stations_df[which(is.na(warm_weather_stations_df$TEMP)), ]$STATION %>% 
  unique()
for (s in warm_target_group) {
  df <- subset(warm_weather_stations_df, STATION == s)
  imputes <- na_kalman(df$TEMP)
  df$TEMP <- imputes
  df$imputed <- ifelse(is.na(df$DEWP), 'A', 'B')
  warm_weather_stations_df <- subset(warm_weather_stations_df, STATION != s)
  warm_weather_stations_df <- rbind(warm_weather_stations_df, df)
}


rm(cycles, filler_dates, filler_temps, filler_weather_station, gap_dates, d,
   d2019, d2020, date_range, gaps, get.date, i, ii, same_date_other_year_temp, 
   enter_empties, station, one, imputed, preserve_type_ifelse, lat_min, lat_vec,
   lon_min, lon_vec, stations, subset_, last_day, days_remaining,
   day_num_remaining, e, lat, lon, max_date, n, s, one_temps, imputed_flag, df, imputes, station_)


##########
## DOWNLOAD THIS PREPARED DATA TO USE FOLDERS
######

final_data_path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/' 
imputation_data_path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/weather_stations_day_imputations/'

file.name <- 'cold_weather_station.csv'
write.csv(cold_weather_stations_df, paste0(final_data_path, file.name))
file.name <- 'intermediate_weather_station.csv'
write.csv(intermediate_weather_stations_df, paste0(final_data_path, file.name))
file.name <- 'warm_weather_station.csv'
write.csv(warm_weather_stations_df, paste0(final_data_path, file.name))

file.name <- 'cold_imputed_stations.csv'
write.csv(cold_target_group, paste0(imputation_data_path, file.name))
file.name <- 'intermediate_imputed_stations.csv'
write.csv(intermediate_target_group, paste0(imputation_data_path, file.name))
file.name <- 'warm_imputed_stations.csv'
write.csv(warm_target_group, paste0(imputation_data_path, file.name))

rm(final_data_path, imputation_data_path, file.name, intermediate_target_group, cold_target_group, warm_target_group)



# stop.code;



## ARCHIVED CODE BELOW 



# g <- ggplot(warm_weather_stations_df, aes(x=DATE, y=TEMP, col=imputed))
# g + geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   ggtitle('Warm location temperatures - 2019-20')
# 
# g <- ggplot(cold_weather_stations_df, aes(x=DATE, y=TEMP, col=imputed))
# g + geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   ggtitle('Cold location temperatures - 2019-20')
# 
# g <- ggplot(intermediate_weather_stations_df, aes(x=DATE, y=TEMP, col=imputed))
# g + geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   ggtitle('Intermediate location temperatures - 2019-20')


# library(sf)
# library(ggplot2)
# library(ggmap)
# library(VIM) # kNN for imputation


# dim(cold_weather_stations_df)
# dim(intermediate_weather_stations_df)
# dim(warm_weather_stations_df)
# 
# 
# length(unique(cold_weather_stations_df$STATION)) # 48
# length(unique(intermediate_weather_stations_df$STATION)) # 24
# length(unique(warm_weather_stations_df$STATION)) # 58
# 
# 
# 
# length(unique(cold_weather_stations_df$DATE)) # 731
# length(unique(intermediate_weather_stations_df$DATE)) # 731
# length(unique(warm_weather_stations_df$DATE)) # 731


# g <- ggplot(subset(intermediate_weather_stations_df,
#                    STATION == 72288623130), aes(x=DATE, y=TEMP, col=imputed))
# g + geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45))

# g <- ggplot(subset(cold_weather_stations_df,
#                    STATION == 72288623130), aes(x=DATE, y=TEMP, col=imputed))
# g + geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45))

# g <- ggplot(subset(warm_weather_stations_df,
#                    STATION == 72288623130), aes(x=DATE, y=TEMP, col=imputed))
# g + geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45))




# station_in_question <- 72288623130
# one_test <- subset(intermediate_weather_stations_df, STATION == station_in_question)
# 
# imputed <- na_kalman(one_test)
# 
# imputed$flag <- ifelse(is.na(imputed$LATITUDE), 'a', 'b')
# 
# g <- ggplot(imputed, aes(x=DATE, y=TEMP, col=flag))
# g + 
#   geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45))
# 
# # plot(x=imputed$DATE, y=imputed$TEMP, 
# #      col=factor(imputed$flag))




########## acrhived 
#### A one-station schematic for analyzing
# # d <- subset(cold_weather_stations_df, STATION %in% intermediate_target_group$STATION)['DATE']
# d <- subset(warm_weather_stations_df, STATION == 72059400188)['DATE']
# d <- as.character(t(d))
# d <- as.Date(d)
# date_range <- seq(min(d), max(d), by = 1)
# gaps <- date_range[!date_range %in% d]
# 
# 
# d2019 <- paste0('2019', '-', unlist(strsplit(as.character(gaps[122]), '-'))[2],'-',unlist(strsplit(as.character(gaps[122]), '-'))[3])
# d2020 <- paste0('2020', '-', unlist(strsplit(as.character(gaps[122]), '-'))[2],'-',unlist(strsplit(as.character(gaps[122]), '-'))[3])
# d2019 <- as.Date(d2019)
# d2020 <- as.Date(d2020)
# 
# get.date <- preserve_type_ifelse(as.Date(format(gaps[122], '%Y-%m-%d')) %in% c(d2019, d2020), 
#             c(d2019, d2020)[which(!c(d2019, d2020) %in% as.Date(format(gaps[122], '%Y-%m-%d')))],
#             'none')
# same_date_other_year_temp <- subset(cold_weather_stations_df, STATION == 72059400188 & DATE == get.date)['TEMP']
# same_date_other_year_temp <- as.numeric(same_date_other_year_temp)


# install.packages('imputeTS')


# imp <- na.kalman(tsAirgap)
# 
# ggplot_na_imputations(tsAirgap, imp, tsAirgapComplete)
# 
# 
# ####
# # cold_weather_stations_df2 <- hotdeck(one_test, ord_var = 'TEMP', domain_var = c('DATE', 'TEMP'))
# 
# #### 
# cold_weather_stations_df2 <- kNN(one_test, variable = c('TEMP'), k=6)





# single <- subset(cold_weather_stations_df2, STATION == 99734299999)

# View(subset(cold_weather_stations_df2, STATION == 72646454834))

# plot(x=subset(cold_weather_stations_df, STATION == 72646454834)['DATE'], 
#      y=subset(cold_weather_stations_df, STATION == 72646454834)['TEMP'])


# length(filler_weather_station)
# as.numeric(unique(subset(cold_weather_stations_df, STATION == 99734299999)['LATITUDE']))
# as.numeric(unique(subset(cold_weather_stations_df, STATION == 99734299999)['LONGITUDE']))
# 
# rep(as.numeric(unique(subset(cold_weather_stations_df, STATION == 99734299999)['LATITUDE'])),
#     length(filler_weather_station))
# 
# View(subset(cold_weather_stations_df, STATION == 99734299999))
# View(subset(cold_weather_stations_df, STATION == 72102899999))






# 
# test <- subset(cold_weather_stations_df, STATION == 99734299999)
# test$DATE <- as.Date(test$DATE, '%Y-%m-%d')
# plot(x=test$DATE, y=test$TEMP)
# ggplot(test, aes(x=DATE, y=TEMP)) +
#   geom_point() +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 45))
# 
# table(format(test$DATE, '%Y-%m-01'))
# table(format(as.data.frame(gaps), '%Y-%m-01'))
# 
# 
# for (i in test$STATION) {
#   d <- subset(cold_weather_stations_df, STATION == i)['DATE']
#   d <- as.character(t(d))
#   d <- as.Date(d)
#   date_range <- seq(min(d), max(d), by=1)
#   gaps <- date_range[!date_range %in% d]
#   gaps <- sort(gaps)
#   # for (y in gaps) {
#   #   print(y)
#   # }
#   # print(paste0(i, '-', gaps))
# }










