

setwd('/Users/francisco.avalos/Desktop/thesis/data/weather/')
path <- '/Users/francisco.avalos/Desktop/thesis/data/weather/'
my.files <- c('*intermediate*', '*cold*', '*warm*')

datasets <- vector("list", length = length(my.files))
for (i in my.files) {
  files <- list.files(path = path, pattern = i)
  datasets[[i]] <- do.call(rbind, lapply(files, function(x) read.csv(x)))
}

datasets <- datasets[-c(1:3)]
for (i in 1:length(datasets)) {
  datasets[i] <- datasets[i]
  df <- as.data.frame(datasets[i])
  datasets[[i]] <- df[, -c(8, 10, 12, 14, 16, 18, 22, 24, 26)]
  colnames(datasets[[i]]) <- c('STATION','DATE','LATITUDE','LONGITUDE',
                               'ELEVATION','NAME','TEMP','DEWP','SLP',
                               'STP','VISIB','WDSP','MXSPD','GUST','MAX',
                               'MIN','PRCP','SNDP','FRSHTT')
}



intermediate.df <- as.data.frame(datasets[1])
colnames(intermediate.df) <- c('STATION','DATE','LATITUDE','LONGITUDE',
                               'ELEVATION','NAME','TEMP','DEWP','SLP',
                               'STP','VISIB','WDSP','MXSPD','GUST','MAX',
                               'MIN','PRCP','SNDP','FRSHTT')

cold.df <- as.data.frame(datasets[2])
colnames(cold.df) <- c('STATION','DATE','LATITUDE','LONGITUDE',
                               'ELEVATION','NAME','TEMP','DEWP','SLP',
                               'STP','VISIB','WDSP','MXSPD','GUST','MAX',
                               'MIN','PRCP','SNDP','FRSHTT')

warm.df <- as.data.frame(datasets[3])
colnames(warm.df) <- c('STATION','DATE','LATITUDE','LONGITUDE',
                               'ELEVATION','NAME','TEMP','DEWP','SLP',
                               'STP','VISIB','WDSP','MXSPD','GUST','MAX',
                               'MIN','PRCP','SNDP','FRSHTT')



path_to_save_output <- 'C:/Users/francisco.avalos/Desktop/thesis/data/ready_to_use_data/prefinal_weather_data/'
export_name <- 'intermediate_temperature.csv'
write.csv(intermediate.df, paste0(path_to_save_output, export_name))

export_name <- 'cold_temperature.csv'
write.csv(cold.df, paste0(path_to_save_output, export_name))

export_name <- 'warm_temperature.csv'
write.csv(warm.df, paste0(path_to_save_output, export_name))


rm(df, files, i, path, my.files, path_to_save_output, export_name, datasets, 
   warm.df, intermediate.df, cold.df)







