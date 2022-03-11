
library(ggplot2)
library(dplyr)

# life length of the stations 

# 215
length(unique(im_df$station_id))
# 731 total days in 2019-20


df <- im_df %>% 
  group_by(STATION) %>% 
  summarize(record_days = n_distinct(flowdate))
df$buckets <- case_when(df$record_days<200 ~ '<200',
                        df$record_days<=299 ~ '200-299',
                        df$record_days<=399 ~ '300-399',
                        df$record_days<=499 ~ '400-499',
                        df$record_days<=599 ~ '500-599',
                        df$record_days<=699 ~ '600-699',
                        df$record_days<=799 ~ '700-731')
table(df$buckets)
# table(df$record_days)
hist(df$record_days)




# 57
length(unique(co_df$station_id))


df <- co_df %>% 
  group_by(station_id) %>% 
  summarize(record_days = n_distinct(flowdate))
df$buckets <- case_when(df$record_days<200 ~ '<200',
                        df$record_days<=299 ~ '200-299',
                        df$record_days<=399 ~ '300-399',
                        df$record_days<=499 ~ '400-499',
                        df$record_days<=599 ~ '500-599',
                        df$record_days<=699 ~ '600-699',
                        df$record_days<=799 ~ '700-731')
table(df$buckets)
table(df$record_days)
hist(df$record_days)




# 69
length(unique(wa_df$station_id))


df <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(record_days = n_distinct(flowdate))
df$buckets <- case_when(df$record_days<200 ~ '<200',
                        df$record_days<=299 ~ '200-299',
                        df$record_days<=399 ~ '300-399',
                        df$record_days<=499 ~ '400-499',
                        df$record_days<=599 ~ '500-599',
                        df$record_days<=699 ~ '600-699',
                        df$record_days<=799 ~ '700-731')
table(df$buckets)
table(df$record_days)
hist(df$record_days)






################## Weather stations

# 48
length(unique(cold_weather_stations_df$STATION))
# 24
length(unique(intermediate_weather_stations_df$STATION))
# 57
length(unique(warm_weather_stations_df$STATION))





# df <- warm_weather_stations_df %>%
# df <- cold_weather_stations_df %>%
df <- intermediate_weather_stations_df %>%
  group_by(STATION) %>% 
  summarize(record_days = n_distinct(DATE))

df$buckets <- case_when(df$record_days<500 ~ '<499',
                        df$record_days<550 ~ '500-549',
                        df$record_days<599 ~ '550-599',
                        df$record_days<650 ~ '600-649',
                        df$record_days<699 ~ '650-699',
                        df$record_days<759 ~ '700-731')
table(df$buckets)






final_data_path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/' 
imputation_data_path <- '/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/weather_stations_day_imputations/'





# counts of days using 'imputed'
## 26,316
dim(cold_weather_stations_df[which(is.na(cold_weather_stations_df$imputed)), ])
## 8,744
dim(cold_weather_stations_df[which(!is.na(cold_weather_stations_df$imputed)), ])
##35,060
dim(cold_weather_stations_df)


length(unique(cold_weather_stations_df$STATION)) ## total cold stations 
length(unique(cold_weather_stations_df[which(is.na(cold_weather_stations_df$imputed)), 'STATION'])) # not imputed
length(unique(cold_weather_stations_df[which(cold_weather_stations_df$imputed=='A'), 'STATION'])) # imputed




stations_with_missing_temps <- cold_weather_stations_df %>% 
  group_by(STATION) %>% 
  filter(!is.na(imputed)) %>% 
  select(STATION) %>% 
  unique()
stations_with_missing_temps <- unlist(stations_with_missing_temps)
stations_without_missing_temps <- subset(cold_weather_stations_df, !(STATION %in% stations_with_missing_temps)) %>% 
  select(STATION) %>% 
  unique()
stations_without_missing_temps <- unlist(stations_without_missing_temps)
length(unique(cold_weather_stations_df$STATION))



summary(subset(cold_weather_stations_df, STATION %in% stations_with_missing_temps)$TEMP)
summary(subset(cold_weather_stations_df, STATION %in% stations_without_missing_temps)$TEMP)
summary(cold_weather_stations_df$TEMP)


## look for any major deviation in temperature between stations with full temp data vs stations that needed temp imputation
cold_weather_stations_df$groups <- 
  case_when(cold_weather_stations_df$STATION %in% stations_with_missing_temps ~ 'A',
            TRUE ~ 'B')
g <- ggplot(data=cold_weather_stations_df, aes(x=groups, y=TEMP))
g + geom_boxplot() +
  labs(title = 'Temp variation between weather stations with some imputation (A) vs those without any imputation (B)')

g <- ggplot(data=cold_weather_stations_df, aes(x=TEMP))
g + geom_histogram() + 
  facet_wrap(~groups) +
  labs(title = 'Temp distribution between weather stations with some imputation (A) vs those without any imputation (B)')


t.test(TEMP ~ groups, data=cold_weather_stations_df)
qqnorm(cold_weather_stations_df$TEMP)
qqline(cold_weather_stations_df$TEMP)

## F-test - variation between groups
var.test(TEMP ~ groups, data=cold_weather_stations_df, alternative = 'two.sided')
# null hypothesis: variation of group 1 equals that of group 2
# alternative hypothesis: variation of group 1 is different than that of group 2
## outcome - don't reject the null hypothesis 


head(cold_weather_stations_df)
## looks for any major deviation in temperature between full temp data vs imputed temp data
cold_weather_stations_df$imputed_2 <- ifelse(is.na(cold_weather_stations_df$imputed) | 
                                               cold_weather_stations_df$imputed=='B', 'not imp', 'imp')
g <- ggplot(data=cold_weather_stations_df, aes(x=imputed_2, y=TEMP))
g + geom_boxplot() +
  labs(title = 'Temp variation between imputed vs non-imputed temperature')

g <- ggplot(data=cold_weather_stations_df, aes(x=TEMP))
g + geom_histogram() + 
  facet_wrap(~imputed_2) +
  labs(title = 'Temp variation between imputed vs non-imputed temperatures')


t.test(TEMP~imputed_2, data=cold_weather_stations_df, var.equal=TRUE)


## F-test - variation between groups
var.test(TEMP ~ imputed_2, data=cold_weather_stations_df, alternative = 'two.sided')
# null hypothesis: variation of group 1 equals that of group 2
# alternative hypothesis: variation of group 1 is different than that of group 2
## outcome - reject the null hypothesis 




completed.days.x.station <- cold_weather_stations_df %>% 
  group_by(STATION) %>% 
  summarize(tot=n())

rm(completed.days.x.station)








# counts of days using 'imputed'
## 11,696
dim(intermediate_weather_stations_df[which(is.na(intermediate_weather_stations_df$imputed)), ])
## 5,848
dim(intermediate_weather_stations_df[which(!is.na(intermediate_weather_stations_df$imputed)), ])
## 17,544
dim(intermediate_weather_stations_df)


length(unique(intermediate_weather_stations_df$STATION)) ## total cold stations 
length(unique(intermediate_weather_stations_df[which(is.na(intermediate_weather_stations_df$imputed)), 'STATION'])) # not imputed
length(unique(intermediate_weather_stations_df[which(intermediate_weather_stations_df$imputed=='A'), 'STATION'])) # imputed


head(intermediate_weather_stations_df)

stations_with_missing_temps <- intermediate_weather_stations_df %>% 
  group_by(STATION) %>% 
  filter(!is.na(imputed)) %>% 
  select(STATION) %>% 
  unique()
stations_with_missing_temps <- unlist(stations_with_missing_temps)
stations_without_missing_temps <- subset(intermediate_weather_stations_df, !(STATION %in% stations_with_missing_temps)) %>% 
  select(STATION) %>% 
  unique()
stations_without_missing_temps <- unlist(stations_without_missing_temps)
length(unique(intermediate_weather_stations_df$STATION))



summary(subset(intermediate_weather_stations_df, STATION %in% stations_with_missing_temps)$TEMP)
summary(subset(intermediate_weather_stations_df, STATION %in% stations_without_missing_temps)$TEMP)
summary(intermediate_weather_stations_df$TEMP)


## look for any major deviation in temperature between stations with full temp data vs stations that needed temp imputation
intermediate_weather_stations_df$groups <- 
  case_when(intermediate_weather_stations_df$STATION %in% stations_with_missing_temps ~ 'A',
            TRUE ~ 'B')
# subset(intermediate_weather_stations_df, STATION %in% stations_with_missing_temps)$groups
g <- ggplot(data=intermediate_weather_stations_df, aes(x=groups, y=TEMP))
g + geom_boxplot() + labs(title = 'Temp variation between weather stations with some imputation (A) vs those without any imputation (B)')

g <- ggplot(data=intermediate_weather_stations_df, aes(x=TEMP))
g + geom_histogram() + 
  facet_wrap(~groups) +
  labs(title = 'Distribution of temperatures by weather stations with some imputation (A) vs those without any any imputation (B)')

t.test(TEMP ~ groups, data=intermediate_weather_stations_df, var.equal=TRUE)


## F-test - variation between groups
var.test(TEMP ~ groups, data=intermediate_weather_stations_df, alternative = 'two.sided')
# null hypothesis: variation of group 1 equals that of group 2
# alternative hypothesis: variation of group 1 is different than that of group 2
## outcome - reject the null hypothesis 


diffs <- subset(intermediate_weather_stations_df, groups=='A')$TEMP - 
  subset(intermediate_weather_stations_df, groups=='B')$TEMP


boxplot(diffs)
qqnorm(diffs)
qqline(diffs)
t.test(diffs)

wilcox.test(diffs)


## looks for any major deviation in temperature between full temp data vs imputed temp data 
intermediate_weather_stations_df$imputed_2 <- ifelse(is.na(intermediate_weather_stations_df$imputed) | 
                                                       intermediate_weather_stations_df$imputed=='B', 'not imp', 'imp')
g <- ggplot(data=intermediate_weather_stations_df, aes(x=imputed_2, y=TEMP))
g + geom_boxplot() + labs(title = 'Temp variation between imputed temperatures vs not imputed temperatures')


g <- ggplot(data=intermediate_weather_stations_df, aes(x=TEMP))
g + geom_histogram() + 
  facet_wrap(~imputed_2) +
  labs(title = 'Distribution of imputed vs non-imputed temperatures from weather stations')

t.test(TEMP~imputed_2, data=intermediate_weather_stations_df, var.equal=TRUE)


## F-test - variation between groups
var.test(TEMP ~ imputed_2, data=intermediate_weather_stations_df, alternative = 'two.sided')
# null hypothesis: variation of group 1 equals that of group 2
# alternative hypothesis: variation of group 1 is different than that of group 2
## outcome - reject the null hypothesis 


completed.days.x.station <- intermediate_weather_stations_df %>% 
  group_by(STATION) %>% 
  summarize(tot=n())

rm(completed.days.x.station)













# counts of days using 'imputed'
## 25,585
dim(warm_weather_stations_df[which(is.na(warm_weather_stations_df$imputed)), ])
## 16,082
dim(warm_weather_stations_df[which(!is.na(warm_weather_stations_df$imputed)), ])
## 41,667
dim(warm_weather_stations_df)


length(unique(warm_weather_stations_df$STATION)) ## total cold stations 
length(unique(warm_weather_stations_df[which(is.na(warm_weather_stations_df$imputed)), 'STATION'])) # not imputed
length(unique(warm_weather_stations_df[which(warm_weather_stations_df$imputed=='A'), 'STATION'])) # imputed


head(warm_weather_stations_df)


stations_with_missing_temps <- warm_weather_stations_df %>% 
  group_by(STATION) %>% 
  filter(!is.na(imputed)) %>% 
  select(STATION) %>% 
  unique()
stations_with_missing_temps <- unlist(stations_with_missing_temps)
stations_without_missing_temps <- subset(warm_weather_stations_df, !(STATION %in% stations_with_missing_temps)) %>% 
  select(STATION) %>% 
  unique()
stations_without_missing_temps <- unlist(stations_without_missing_temps)
length(unique(warm_weather_stations_df$STATION))



summary(subset(warm_weather_stations_df, STATION %in% stations_with_missing_temps)$TEMP)
summary(subset(warm_weather_stations_df, STATION %in% stations_without_missing_temps)$TEMP)
summary(warm_weather_stations_df$TEMP)


warm_weather_stations_df$groups <- 
  case_when(warm_weather_stations_df$STATION %in% stations_with_missing_temps ~ 'A',
            TRUE ~ 'B')
# subset(warm_weather_stations_df, STATION %in% stations_with_missing_temps)$groups
g <- ggplot(data=warm_weather_stations_df, aes(x=groups, y=TEMP))
g + geom_boxplot() +
  labs(title = 'Temp distribution between weather station with some imputation (A) vs those without any imputation (B)')

g <- ggplot(data=warm_weather_stations_df, aes(x=TEMP))
g + geom_histogram() +
  facet_wrap(~groups) +
  labs(title = 'Temp distribution between weather station with some imputation (A) vs those without any imputation (B)')


t.test(TEMP ~ groups, data=warm_weather_stations_df, var.equal=TRUE)


## F-test - variation between groups
var.test(TEMP ~ groups, data=warm_weather_stations_df, alternative = 'two.sided')
# null hypothesis: variation of group 1 equals that of group 2
# alternative hypothesis: variation of group 1 is different than that of group 2
## outcome - reject the null hypothesis 


warm_weather_stations_df$imputed_2 <- ifelse(is.na(warm_weather_stations_df$imputed) | 
                                               warm_weather_stations_df$imputed=='B', 'not imp', 'imp')
g <- ggplot(data=warm_weather_stations_df, aes(x=imputed_2, y=TEMP))
g + geom_boxplot() +
  labs(title = 'Variation between imputed vs non-imputed temperatures')

g <- ggplot(data=warm_weather_stations_df, aes(x=TEMP))
g + geom_histogram() +
  facet_wrap(~imputed_2)
  labs(title = 'Temperature distribution between imputed vs non-imputed temperatures')


t.test(TEMP~imputed_2, data=warm_weather_stations_df, var.equal=TRUE)

## F-test - variation between groups
var.test(TEMP ~ imputed_2, data=warm_weather_stations_df, alternative = 'two.sided')
# null hypothesis: variation of group 1 equals that of group 2
# alternative hypothesis: variation of group 1 is different than that of group 2
## outcome - reject the null hypothesis 


completed.days.x.station <- warm_weather_stations_df %>% 
  group_by(STATION) %>% 
  summarize(tot=n())

rm(completed.days.x.station)







################################################################
################################################################
################################################################
## EDA - EV station lifetime during 2-year perid
################################################################
################################################################



df <- co_df %>% group_by(station_id) %>% summarize(tot.days = n_distinct(flowdate))
df[order(df$tot.days), ]

df <- co_df %>% 
  group_by(station_id) %>% 
  summarize(first.day=min(flowdate),
            last.day=max(flowdate), 
            active_days=n_distinct(flowdate)) %>% 
  as.data.frame()

df$month.start <- format(df$first.day, format='%Y-%m-01')
df$month.end <- format(df$last.day, format='%Y-%m-01')
df$month.start <- as.Date(df$month.start)
df$month.end <- as.Date(df$month.end)
df$months_active <- as.numeric(difftime(df$last.day, df$first.day, units = 'days'))/(365.25/12)
df$months_active <- round(df$months_active)

table(df$months_active)

g <- ggplot(data=df, aes(x='', y=active_days, group=months_active))
g + geom_bar(stat='identity', width=1) +
  coord_polar("y", start = 0)







df <- im_df %>% group_by(station_id) %>% summarize(tot.days = n_distinct(flowdate))
df[order(df$tot.days), ] %>% 
  # filter(tot.days<731) %>% 
  as.data.frame()
  # filter(station_id != 40840)

df <- im_df %>% 
  group_by(station_id) %>% 
  summarize(first.day=min(flowdate),
            last.day=max(flowdate), 
            active_days=n_distinct(flowdate)) %>% 
  as.data.frame()

df$month.start <- format(df$first.day, format='%Y-%m-01')
df$month.end <- format(df$last.day, format='%Y-%m-01')
df$month.start <- as.Date(df$month.start)
df$month.end <- as.Date(df$month.end)
df$months_active <- as.numeric(difftime(df$last.day, df$first.day, units = 'days'))/(365.25/12)
df$months_active <- round(df$months_active)

table(df$months_active)
table(df$active_days)

head(df)

g <- ggplot(df, aes(x=months_active))
g + geom_histogram()





df <- wa_df %>% group_by(station_id) %>% summarize(tot.days = n_distinct(flowdate))
df[order(df$tot.days), ] %>% 
  # filter(tot.days<731) %>% 
  as.data.frame()
# filter(station_id != 40840)

df <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(first.day=min(flowdate),
            last.day=max(flowdate), 
            active_days=n_distinct(flowdate)) %>% 
  as.data.frame()

df$month.start <- format(df$first.day, format='%Y-%m-01')
df$month.end <- format(df$last.day, format='%Y-%m-01')
df$month.start <- as.Date(df$month.start)
df$month.end <- as.Date(df$month.end)
df$months_active <- as.numeric(difftime(df$last.day, df$first.day, units = 'days'))/(365.25/12)
df$months_active <- round(df$months_active)

table(df$months_active)
table(df$active_days)




all.combined <- rbind(co_df, im_df, wa_df)
df <- all.combined %>% group_by(station_id) %>% 
  summarize(tot.days = n_distinct(flowdate))
df <- all.combined %>% 
  group_by(station_id) %>% 
  summarize(first.day=min(flowdate),
            last.day=max(flowdate), 
            active_days=n_distinct(flowdate)) %>% 
  as.data.frame()


df$month.start <- format(df$first.day, format='%Y-%m-01')
df$month.end <- format(df$last.day, format='%Y-%m-01')
df$month.start <- as.Date(df$month.start)
df$month.end <- as.Date(df$month.end)
df$months_active <- as.numeric(difftime(df$last.day, df$first.day, units = 'days'))/(365.25/12)
df$months_active <- round(df$months_active)

table(df$months_active)









################################################################
################################################################
################################################################
## EDA - IM period stats
################################################################
################################################################

head(im_df)


# quick check on the robustness of im_flag
im_df[which((im_df$im_flag=='NO IM') & im_df$minutes_diff != 0), ]
im_df[which(im_df$im_flag=='NO IM'), ]$minute_diff
im_df[which(im_df$minutes_diff==0 & im_df$im_flag=='IM'), ]

# 642 - one instance where both sockets out for less than 1 minute - checks out
# 5553 - one instance where both sockets out for less than 1 minute - checks out
View(subset(im_df, station_id == 5553))



co_df[which((co_df$im_flag=='NO IM') & co_df$minutes_diff != 0), ]
co_df[which(co_df$im_flag=='NO IM'), ]$minute_diff
# co_df[which(co_df$minutes_diff==0), ]$im_flag
# unique(co_df[which(co_df$minutes_diff==0), ]$im_flag)
co_df[which(co_df$minutes_diff==0 & co_df$im_flag=='IM'), ]

# 735  - one instance where both sockets overlap for less than 1 minute - checks out
View(subset(co_df, station_id == 735))




wa_df[which((wa_df$im_flag=='NO IM') & wa_df$minutes_diff != 0), ]
wa_df[which(wa_df$im_flag=='NO IM'), ]$minute_diff
# wa_df[which(wa_df$minutes_diff==0), ]$im_flag
# unique(wa_df[which(wa_df$minutes_diff==0), ]$im_flag)
wa_df[which(wa_df$minutes_diff==0 & wa_df$im_flag=='IM'), ]

# 864  - one instance where both sockets overlap for less than 1 minute - checks out
## *+* This discrepancy has been corrected in the 'ev__prepare_warm' file
View(subset(wa_df, station_id == 864))


# Code from EDA revealed that the im_flag failed for station-id 864 in the warm region (refer to *+* in EDA files). The query below (pulling from the
# raw data source) shows the flag failed due to the im-end-timestamp having an 0000-00-00 00:00:00 entry for a station. The station
# had both station-sockets out and IM for the failed flag instances.
## To correct, I'll have to insert 1440 values to minutes_diff field for those 18 days
# SELECT *
#   FROM my_schema.in_maintenance IM 
# LEFT JOIN my_schema.station_maintenance_exclusion SME ON IM.station_socket_id = SME.station_socket_id 
# AND IM.ref_date = SME.ref_date
# WHERE IM.station_id = 864
# -- 	AND IM.ref_date BETWEEN '2019-02-27' AND '2019-03-15'
# AND IM.ref_date BETWEEN '2019-02-27' AND '2019-03-18'
# AND IM.station_socket_id != 0
# ;






all.combined <- rbind(co_df, im_df, wa_df)

df <- all.combined %>% group_by(station_id) %>% 
  summarize(tot.days = n_distinct(flowdate))
df <- all.combined %>% 
  group_by(station_id) %>% 
  summarize(first.day=min(flowdate),
            last.day=max(flowdate), 
            active_days=n_distinct(flowdate)) %>% 
  as.data.frame() 



output <- all.combined %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            # days_since_operation = n_distinct(flowdate),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE)
  )



table(output$number_of_in_maintenance_periods)


stations.im.info <- all.combined %>% 
  group_by(station_id) %>% 
  summarize(im.periods.adj = max(mtbf_flag),
            im.periods.non.adj = max(running.im.now))

stations.im.info$instances.of.im.less.30.mins <- ifelse(stations.im.info$im.periods.adj != stations.im.info$im.periods.non.adj, 
                                                        1, 0)
table(stations.im.info$instances.of.im.less.30.mins)
# just 12% of ev stations have experienced at least one instance where the IM-period lasted less than 30 minutes. 



# pct of stations that have at least 1-IM period of less than 30 minutes, to show the distributions by climate region


# cold 
output <- co_df %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE))
table(output$number_of_in_maintenance_periods)

stations.im.info <- co_df %>% 
  group_by(station_id) %>% 
  summarize(im.periods.adj = max(mtbf_flag),
            im.periods.non.adj = max(running.im.now))

stations.im.info$instances.of.im.less.30.mins <- ifelse(stations.im.info$im.periods.adj != stations.im.info$im.periods.non.adj, 
                                                        1, 0)
table(stations.im.info$instances.of.im.less.30.mins)


# moderate
output <- im_df %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE))
table(output$number_of_in_maintenance_periods)

stations.im.info <- im_df %>% 
  group_by(station_id) %>% 
  summarize(im.periods.adj = max(mtbf_flag),
            im.periods.non.adj = max(running.im.now))

stations.im.info$instances.of.im.less.30.mins <- ifelse(stations.im.info$im.periods.adj != stations.im.info$im.periods.non.adj, 
                                                        1, 0)
table(stations.im.info$instances.of.im.less.30.mins)




# warm
output <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE))
table(output$number_of_in_maintenance_periods)

stations.im.info <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(im.periods.adj = max(mtbf_flag),
            im.periods.non.adj = max(running.im.now))

stations.im.info$instances.of.im.less.30.mins <- ifelse(stations.im.info$im.periods.adj != stations.im.info$im.periods.non.adj, 
                                                        1, 0)
table(stations.im.info$instances.of.im.less.30.mins)



hist(output$total_hours_in_maintenance)

head(output)





all.combined <- rbind(co_df, im_df, wa_df)
output <- all.combined %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE))



## now analyze stats that typically occur over the same periods across all stations 

# average / median IM hours across all the stations for each IM period
tapply(output$total_hours_in_maintenance, 
       output$number_of_in_maintenance_periods, 
       summary)

# number of unique stations in each IM period across all regions
tapply(output$station_id,
        output$number_of_in_maintenance_periods, 
        function(x) length(unique(x)))


g <- ggplot(output, aes(x=number_of_in_maintenance_periods, 
                        y=total_hours_in_maintenance, 
                        group=number_of_in_maintenance_periods))
g + geom_boxplot() +
  labs(title = 'Total hours in maintenance distribution across all EV stations by IM periods')


## now by region


# moderate
output <- im_df %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE))


# average / median IM hours across all the stations for each IM period
tapply(output$total_hours_in_maintenance, 
       output$number_of_in_maintenance_periods, 
       summary)

# number of unique stations in each IM period across all regions
tapply(output$station_id,
       output$number_of_in_maintenance_periods, 
       function(x) length(unique(x)))

g <- ggplot(output, aes(x=number_of_in_maintenance_periods, 
                        y=total_hours_in_maintenance, 
                        group=number_of_in_maintenance_periods))
g + geom_boxplot() +
  labs(title = 'Total hours in maintenance distribution across moderate EV stations by IM periods')



# cold
output <- co_df %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE))


# average / median IM hours across all the stations for each IM period
tapply(output$total_hours_in_maintenance, 
       output$number_of_in_maintenance_periods, 
       summary)

# number of unique stations in each IM period across all regions
tapply(output$station_id,
       output$number_of_in_maintenance_periods, 
       function(x) length(unique(x)))


g <- ggplot(output, aes(x=number_of_in_maintenance_periods, 
                        y=total_hours_in_maintenance, 
                        group=number_of_in_maintenance_periods))
g + geom_boxplot() +
  labs(title = 'Total hours in maintenance distribution across cold EV stations by IM periods')



# warm
output <- wa_df %>% 
  group_by(station_id) %>% 
  summarize(number_of_in_maintenance_periods = max(mtbf_flag),
            total_minutes_in_maintenance = sum(minutes, na.rm = TRUE),
            total_hours_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/60),
            total_days_in_maintenance = round(sum(minutes_diff, na.rm = TRUE)/1440),
            single_im_days = n_distinct(single_im_day, na.rm = TRUE))


# average / median IM hours across all the stations for each IM period
tapply(output$total_hours_in_maintenance, 
       output$number_of_in_maintenance_periods, 
       summary)

# number of unique stations in each IM period across all regions
tapply(output$station_id,
       output$number_of_in_maintenance_periods, 
       function(x) length(unique(x)))

g <- ggplot(output, aes(x=number_of_in_maintenance_periods, 
                        y=total_hours_in_maintenance, 
                        group=number_of_in_maintenance_periods))
g + geom_boxplot() +
  labs(title = 'Total hours in maintenance distribution across warm EV stations by IM periods')











symbox(std_im_location_activity_v2$total_hours_in_maintenance, data=std_im_location_activity_v2) # -1
symbox(std_im_location_activity_v2$mtbf, data=std_im_location_activity_v2) # log or 0.5
symbox(std_im_location_activity_v2$total_customers, data=std_im_location_activity_v2) # 0.5 or as-is
symbox(std_im_location_activity_v2$distance_to_nearest_charger, data=std_im_location_activity_v2) # -1 or -0.5


hist(std_im_location_activity_v2$total_hours_in_maintenance^-0.05)
hist((std_im_location_activity_v2$mtbf)^0.5)
hist((std_im_location_activity_v2$total_customers)^0.5)
hist((std_im_location_activity_v2$distance_to_nearest_charger)^(-0.05))

plot((std_im_location_activity_v2$mtbf)^0.5, 
     (std_im_location_activity_v2$total_customers)^0.5)

qqnorm((std_im_location_activity_v2$total_hours_in_maintenance))
qqline(std_im_location_activity_v2$total_hours_in_maintenance)



dists.df <- as.matrix(dist(cbind(std_im_location_activity_v2$x_axis_unique, 
                                 std_im_location_activity_v2$y_axis_unique)))
dists.df <- 1/dists.df
diag(dists.df) <- 0
head(dists.df)



ape::Moran.I(std_im_location_activity_v2$total_hours_in_maintenance, 
             dists.df)





################




dir.var <- variogram(mtbf ~ distance_to_nearest_charger,
          data=std_im_location_activity_v2,
          locations = ~x_axis_unique+y_axis_unique,
          alpha=c(0,45,90,135))

plot(dir.var)

g <- gstat::gstat(id="hours_im_response", 
                  formula = mtbf ~ 
                    distance_to_nearest_charger+total_customers
                  ,locations = ~x_axis_unique+y_axis_unique, 
                  data = std_im_location_activity_v2)
# directional variograms for isotropic/anisotropic analysis 

plot(variogram(g, width=0.01))
v.fit <- fit.variogram(variogram(g, width=0.2, alpha=135), 
              vgm(psil=0.1, 'Gau', nugget = 0.1))
plot(variogram(g), v.fit)

# v.fit <- fit.variogram(variogram(g, cutoff=0.5),
#                        # vgm(psil=0.1, "Ste",nugget = 1, range = 5))
#                        # vgm(psil=1, "Sph",nugget = 0.1, range = 0.1))
#                        vgm(psil=0.05, "Exp",nugget = 0.01, range = 0.1))




g <- ggplot(data=std_im_location_activity_v2, aes(x=x_axis_unique, 
                                                  y=y_axis_unique, 
                                                  fill=factor(charger_company_col),
                                                  col=factor(charger_company_col)))
g + geom_point()




ci.buckets <- 1:15
ci.buckets <- as.data.frame(ci.buckets)
ci.buckets <- ci.bounds[, c('dist.bins', 'sample.n')]

ci.buckets$class <- case_when(ci.buckets$dist.bins <= 2 ~ 'dist.bins: 1-2',
                              ci.buckets$dist.bins <= 4 ~ 'dist.bins: 3-4',
                              ci.buckets$dist.bins <= 6 ~ 'dist.bins: 5-6',
                              ci.buckets$dist.bins <= 8 ~ 'dist.bins: 7-8',
                              ci.buckets$dist.bins <= 10 ~ 'dist.bins: 9-10',
                              ci.buckets$dist.bins <= 12 ~ 'dist.bins: 11-12',
                              ci.buckets$dist.bins <= 14 ~ 'dist.bins: 13-14',
                              ci.buckets$dist.bins <= 16 ~ 'dist.bins: 15-16',
                              TRUE~'dist.bins: > 16')
# ci.buckets$class <- case_when(ci.bounds$sample.n <= 50 ~ 'less-than-50',
#                               ci.bounds$sample.n <= 100 ~ 'less-than-eq-100',
#                               ci.bounds$sample.n <= 200 ~ 'less-than-eq-200',
#                               ci.bounds$sample.n <= 300 ~ 'less-than-eq-300',
#                               ci.bounds$sample.n <= 400 ~ 'less-than-eq-400',
#                               ci.bounds$sample.n <= 500 ~ 'less-than-eq-500',
#                               ci.bounds$sample.n <= 600 ~ 'less-than-eq-600',
#                               TRUE~'over-600')

tapply(ci.buckets$sample.n,
       ci.buckets$class, 
       sum)

tapply(ci.bounds$sample.n, 
       ci.bounds$sample.n,
       sum)



# weibull EDAs
# im here 
table(merged.df$running.im.adj.now.grouping)
table(merged.df$charger_company_col)
table(merged.df$temp_class)
table(merged.df$subregion.id)
table(merged.df$hist_of_im)

tapply(merged.df$operational_days,
       merged.df$running.im.adj.now.grouping,
       sum)

tapply(merged.df$warmer_than_normal_days,
       merged.df$running.im.adj.now.grouping,
       mean)

tapply(merged.df$warmer_than_normal_days,
       merged.df$running.im.adj.now.grouping,
       median)


head(merged.df)





sum.stat <- function(x) {
  # x.n <- length(x)
  x.mean <- mean(x)
  # x.median <- median(x)
  x.sd <- sd(x)
  out <- list(x.mean, x.sd)
  return(out)
}

output.operational.days <- tapply(merged.df$operational_days, 
       merged.df$running.im.adj.now.grouping,
       sum.stat)
output.operational.days <- unlist(output.operational.days)
summary.stats.od <- data.frame(matrix(unlist(output.operational.days), nrow=20, byrow=TRUE))
colnames(summary.stats.od) <- c('mean','standard deviation')
summary.stats.od %>% mutate(across(everything(), round, 2))


output.tot.sessions <- tapply(merged.df$tot_sessions, 
                                  merged.df$running.im.adj.now.grouping,
                                  sum.stat)
output.tot.sessions <- unlist(output.tot.sessions)
summary.stats.ts <- data.frame(matrix(unlist(output.tot.sessions), nrow=20, byrow=TRUE))
colnames(summary.stats.ts) <- c('mean','standard deviation')
summary.stats.ts %>% mutate(across(everything(), round, 2))


output.warm.days <- tapply(merged.df$warmer_than_normal_days, 
                              merged.df$running.im.adj.now.grouping,
                              sum.stat)
output.warm.days <- unlist(output.warm.days)
summary.stats.wd <- data.frame(matrix(unlist(output.warm.days), nrow=20, byrow=TRUE))
colnames(summary.stats.wd) <- c('mean','standard deviation')
summary.stats.wd %>% mutate(across(everything(), round, 2))


output.cold.days <- tapply(merged.df$cooler_than_normal_days, 
                           merged.df$running.im.adj.now.grouping,
                           sum.stat)
output.cold.days <- unlist(output.cold.days)
summary.stats.cd <- data.frame(matrix(unlist(output.cold.days), nrow=20, byrow=TRUE))
colnames(summary.stats.cd) <- c('mean','standard deviation')
summary.stats.cd %>% mutate(across(everything(), round, 2))


output.mins.warm.days <- tapply(merged.df$tot_minutes_on_normal_days, 
                           merged.df$running.im.adj.now.grouping,
                           sum.stat)
output.mins.warm.days <- unlist(output.mins.warm.days)
summary.stats.tmwd <- data.frame(matrix(unlist(output.mins.warm.days), nrow=20, byrow=TRUE))
colnames(summary.stats.tmwd) <- c('mean','standard deviation')
summary.stats.tmwd %>% mutate(across(everything(), round, 2))


output.mins.cooler.days <- tapply(merged.df$tot_minutes_on_cooler_days, 
                                merged.df$running.im.adj.now.grouping,
                                sum.stat)
output.mins.cooler.days <- unlist(output.mins.cooler.days)
summary.stats.tmcd <- data.frame(matrix(unlist(output.mins.cooler.days), nrow=20, byrow=TRUE))
colnames(summary.stats.tmcd) <- c('mean','standard deviation')
summary.stats.tmcd %>% mutate(across(everything(), round, 2))

output.mins.norm.days <- tapply(merged.df$tot_minutes_on_normal_days, 
                                  merged.df$running.im.adj.now.grouping,
                                  sum.stat)
output.mins.norm.days <- unlist(output.mins.norm.days)
summary.stats.tmnd <- data.frame(matrix(unlist(output.mins.norm.days), nrow=20, byrow=TRUE))
colnames(summary.stats.tmnd) <- c('mean','standard deviation')
summary.stats.tmnd %>% mutate(across(everything(), round, 2))


cbind(summary.stats.od, 
      summary.stats.ts, summary.stats.wd, summary.stats.cd,
      summary.stats.tmnd, summary.stats.tmcd, summary.stats.tmwd) %>% round(2)







titles <- 'Charging kWh (minutes) distribution by EV station life cycle'
g <- ggplot(data = merged.df, 
            aes(x=running.im.adj.now.grouping,
                y=tot_kwh,
                group=running.im.adj.now.grouping))
g + geom_boxplot() +
  facet_wrap(~charger_company_col) +
  labs(title = titles)




titles <- 'Charging Duration (minutes) distribution by EV station life cycle'
g <- ggplot(data = merged.df, 
            aes(x=running.im.adj.now.grouping,
                y=tot_minutes,
                group=running.im.adj.now.grouping))
g + geom_boxplot() +
  facet_wrap(~charger_company_col) +
  labs(title = titles)




titles <- 'Duration (days) of life by history of IM'
g <- ggplot(data = merged.df, 
            aes(x=hist_of_im,
                y=operational_days,
                group=running.im.adj.now.grouping))
g + geom_boxplot() +
  facet_wrap(~charger_company_col)+
  labs(title = titles)    



titles <- 'Duration (days) of life by EV station life cycle & subregion'
g <- ggplot(data = merged.df, 
            aes(x=running.im.adj.now.grouping,
                y=operational_days,
                group=running.im.adj.now.grouping))
g + geom_boxplot() +
  facet_wrap(~subregion.id) +
  labs(title = titles)    
  
  
  
  
titles <- 'Duration (days) of life by EV station life cycle & region'
g <- ggplot(data = merged.df, 
            aes(x=running.im.adj.now.grouping,
                y=operational_days,
                group=running.im.adj.now.grouping))
g + geom_boxplot() +
  facet_wrap(~temp_class) +
  labs(title = titles)  
  
  

titles <- 'Duration (days) of life by EV station life cycle'
g <- ggplot(data = merged.df, 
            aes(x=running.im.adj.now.grouping,
                y=operational_days,
                group=running.im.adj.now.grouping))
g + geom_boxplot() +
  # facet_wrap(~charger_company_col) +
  labs(title = titles)


titles <- 'Distribution of days in operation'
g <- ggplot(data = merged.df, 
            aes(x=running.im.adj.now.grouping))
g + geom_histogram() +
  # facet_wrap(~charger_company_col) +
  labs(title = titles)


distr.ev.station.cycles <- merged.df %>% mutate(uniq_evs = paste(station_id, '-', running.im.adj.now.grouping))
head(distr.ev.station.cycles)

