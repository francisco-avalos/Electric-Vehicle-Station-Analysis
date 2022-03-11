

library(dplyr)
library(ggmap)
library(stats)
library(ggplot2)
library(factoextra)
library(tidyverse)

library(class)
library(sp)
library(car)

### normalization/standardization functions

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

mask <- function(predictions,pts,grid.x,grid.y,win.size) {
  new.qqq <- predictions
  m <- nrow(predictions)
  n <- ncol(predictions)
  for (i in 1:m) {
    for (j in 1:n) {
      x.min <- i-win.size
      y.min <- j-win.size
      x.max <- i+win.size
      y.max <- j+win.size
      if (x.min < 1) { 
        x.min <- 1 
      }; 
      if (y.min < 1) { 
        y.min <- 1 
      }
      if (x.max > length(X)) { 
        x.max <- length(X) 
      };
      if (y.max > length(Y)) { 
        y.max <- length(Y) 
      }
      candidates <- pts[which(pts$x >= X[x.min] & pts$x <= X[x.max]),]
      thePoints <- candidates[which(candidates$y >= Y[y.min] &
                                      candidates$y <= Y[y.max]),]
      if (nrow(thePoints) == 0) {
        new.qqq[i,j] <- NA
      }
    }
  }
  return(new.qqq)
}

mse <- function(actual, predicted) {
  output <- mean((actual - predicted)^2)
  return(output)
}

mae <- function(actual, predicted) {
  output <- sum(abs(actual - predicted)) / length(actual)
  return(output)
}

rmse <- function(actual, predicted) {
  output <- sqrt(mean((actual - predicted)^2))
  return(output)
}



################################################################################
################### Kriging Modeling 
###################

# Prepare data for spatial modeling
## leave these cols out:
#### total_hours_in_maintenance, mtbf, total_customers, distance_to_nearest_charger
cols <- c('days_since_operation',
          
          'number_of_in_maintenance_periods', 'total_minutes_in_maintenance',
          'total_hours_in_maintenance', 'total_days_in_maintenance',
          # 'total_days_in_maintenance',
          'single_im_days', 'downtime_days',
          
          'total_sessions', 'total_minutes',
          'total_MWh', 'total_customers',
          # 'total_MWh',
          'total_unique_customers_per_1000s',
          
          'uptime_days',
          
          'cooler_than_normal_days', 'normal_days',
          'warmer_than_normal_days', 'sessions_on_cooler_days',
          'sessions_on_normal_days', 'sessions_on_warmer_days',
          'uniq_customers_on_cooler_days', 'uniq_customers_on_normal_days',
          'uniq_customers_on_warmer_days', 'minutes_charging_on_cooler_days',
          'minutes_charging_on_normal_days', 'minutes_charging_on_warmer_days',
          'total_session_kWh', 'session_kWh_on_cooler_days',
          'session_kWh_on_normal_days', 'session_kWh_on_warmer_days',
          'total_session_MWh', 'session_MWh_on_cooler_days',
          'session_MWh_on_normal_days', 'session_MWh_on_warmer_days',
          
          'distance_to_nearest_charger',
          
          'mtbf', 'mtbf_cooler',
          # 'mtbf_cooler',
          'mtbf_normal', 'mtbf_warmer',
          'avg_size_of_in_maintenance_periods_in_days',
          'avg_size_of_in_maintenance_periods_in_days_weighted_by_im_minutes')

# next step is to normalize all potential predictor variables and analyze again, this
## might yield more comparable results that could stand out due to normalization. Also, 
# prepare different dataframe formats used with geo libraries
norm <- as.data.frame(lapply(im_location_activity_v2[,cols],normalize)) # standardize
grouped_on_df <- im_location_activity_v2 %>% 
  dplyr::select(station_id, charger_company_col, 
                x_axis_unique,y_axis_unique
                # distance_to_nearest_charger,
                # total_hours_in_maintenance, 
                # mtbf, total_customers, distance_to_nearest_charger
  ) %>% unique()
std_im_location_activity_v2 <- cbind(grouped_on_df, norm)

im_location_activity_sf <- 
  st_as_sf(im_location_activity_v2, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

im_location_activity_sf_std <- 
  st_as_sf(std_im_location_activity_v2, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

## tables of importance: 
### std_im_location_activity_v2
### im_location_activity_sf
### im_location_activity_sf_std


r1 <- subset(co_location_activity_v2, subregion.y == 'r1')
r2 <- subset(co_location_activity_v2, subregion.y == 'r2')
r3 <- subset(co_location_activity_v2, subregion.y == 'r3')
r4 <- subset(co_location_activity_v2, subregion.y == 'r4')

##
norm <- as.data.frame(lapply(r1[,cols],normalize))
grouped_on_df <- r1 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_co_location_activity_v2_r1 <- cbind(grouped_on_df, norm)

co_location_activity_sf_r1 <- 
  st_as_sf(r1, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

co_location_activity_sf_std_r1 <- 
  st_as_sf(std_co_location_activity_v2_r1, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

##
norm <- as.data.frame(lapply(r2[,cols],normalize))
grouped_on_df <- r2 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_co_location_activity_v2_r2 <- cbind(grouped_on_df, norm)

co_location_activity_sf_r2 <- 
  st_as_sf(r2, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

co_location_activity_sf_std_r2 <- 
  st_as_sf(std_co_location_activity_v2_r2, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

##
norm <- as.data.frame(lapply(r3[,cols],normalize))
grouped_on_df <- r3 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_co_location_activity_v2_r3 <- cbind(grouped_on_df, norm)

co_location_activity_sf_r3 <- 
  st_as_sf(r3, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

co_location_activity_sf_std_r3 <- 
  st_as_sf(std_co_location_activity_v2_r3, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

##
norm <- as.data.frame(lapply(r4[,cols],normalize))
grouped_on_df <- r4 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_co_location_activity_v2_r4 <- cbind(grouped_on_df, norm)

co_location_activity_sf_r4 <- 
  st_as_sf(r4, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

co_location_activity_sf_std_r4 <- 
  st_as_sf(std_co_location_activity_v2_r4, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))






r1 <- subset(wa_location_activity_v2, subregion.y == 'r1')
r2 <- subset(wa_location_activity_v2, subregion.y == 'r2')
r3 <- subset(wa_location_activity_v2, subregion.y == 'r3')
r4 <- subset(wa_location_activity_v2, subregion.y == 'r4')

# r1
norm <- as.data.frame(lapply(r1[,cols],normalize))
grouped_on_df <- r1 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_wa_location_activity_v2_r1 <- cbind(grouped_on_df, norm)

wa_location_activity_sf_r1 <- 
  st_as_sf(r1, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

wa_location_activity_sf_std_r1 <- 
  st_as_sf(std_wa_location_activity_v2_r1, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

# r2
norm <- as.data.frame(lapply(r2[,cols],normalize))
grouped_on_df <- r2 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_wa_location_activity_v2_r2 <- cbind(grouped_on_df, norm)

wa_location_activity_sf_r2 <- 
  st_as_sf(r2, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

wa_location_activity_sf_std_r2 <- 
  st_as_sf(std_wa_location_activity_v2_r2, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

# r3
norm <- as.data.frame(lapply(r3[,cols],normalize))
grouped_on_df <- r3 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_wa_location_activity_v2_r3 <- cbind(grouped_on_df, norm)

wa_location_activity_sf_r3 <- 
  st_as_sf(r3, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

wa_location_activity_sf_std_r3 <- 
  st_as_sf(std_wa_location_activity_v2_r3, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

# r4
norm <- as.data.frame(lapply(r4[,cols],normalize))
grouped_on_df <- r4 %>% dplyr::select(station_id, charger_company_col, 
                                      x_axis_unique,y_axis_unique, 
                                      dist_to_other_charger_classification) %>% unique()
std_wa_location_activity_v2_r4 <- cbind(grouped_on_df, norm)

wa_location_activity_sf_r4 <- 
  st_as_sf(r4, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))

wa_location_activity_sf_std_r4 <- 
  st_as_sf(std_wa_location_activity_v2_r4, coords = c('x_axis_unique', 'y_axis_unique')) %>%
  cbind(st_coordinates(.))




rm(grouped_on_df, norm)



# Kriging on intermediate region

# "Exp", "Sph", "Gau", "Mat"
# breaking out by model-  not a good idea
# df_model <- subset(std_im_location_activity_v2, charger_company_col == 'vxxip')
# model_name <- 'eeyin'
# model_name <- 'vxxip'



### making the predictor variables normally distributed

shapiro.test(std_im_location_activity_v2$total_customers) # skewed 
hist(log(std_im_location_activity_v2$total_customers))
hist((std_im_location_activity_v2$total_customers)^(0.6627511))
powerTransform(std_im_location_activity_v2$total_customers+0.01)
bcPower(std_im_location_activity_v2$total_customers+0.01, 0.6627511)

# total_hours_in_maintenance, 
# mtbf, 
# total_customers, 
# distance_to_nearest_charger
std_im_location_activity_v2$distance_to_nearest_charger.normalized <- 1/sqrt(std_im_location_activity_v2$distance_to_nearest_charger)
# std_im_location_activity_v2$total_customers.normalized <- (std_im_location_activity_v2$total_customers^0.6627511)
# std_im_location_activity_v2$mtbf.normalized <- log(std_im_location_activity_v2$mtbf+0.01)
# std_im_location_activity_v2$total_hours_in_maintenance.normalized <- log(std_im_location_activity_v2$total_hours_in_maintenance+0.01)
std_im_location_activity_v2$total_hours_in_maintenance.normalized <- log(std_im_location_activity_v2$total_hours_in_maintenance)
std_im_location_activity_v2$mtbf.normalized <- log(std_im_location_activity_v2$mtbf)
# std_im_location_activity_v2$cooler_than_normal_days.normalized <- (std_im_location_activity_v2$cooler_than_normal_days^3.727427)
# std_im_location_activity_v2$warmer_than_normal_days.normalized <- (std_im_location_activity_v2$warmer_than_normal_days^3.783206)


im_location_activity_sf_std$distance_to_nearest_charger.normalized <- log(im_location_activity_sf_std$distance_to_nearest_charger+0.01)
im_location_activity_sf_std$total_customers.normalized <- (im_location_activity_sf_std$total_customers^0.6627511)
# im_location_activity_sf_std$cooler_than_normal_days.normalized <- (im_location_activity_sf_std$cooler_than_normal_days^3.727427)
# im_location_activity_sf_std$warmer_than_normal_days.normalized <- (im_location_activity_sf_std$warmer_than_normal_days^3.783206)


hist(std_im_location_activity_v2$distance_to_nearest_charger.normalized)
shapiro.test(std_im_location_activity_v2$distance_to_nearest_charger.normalized)


x.range <- as.integer(std_im_location_activity_v2$x_axis_unique)
y.range <- as.integer(std_im_location_activity_v2$y_axis_unique)
pred1.range <- std_im_location_activity_v2$distance_to_nearest_charger
pred2.range <- std_im_location_activity_v2$total_customers
# pred1.range <- std_im_location_activity_v2$distance_to_nearest_charger.normalized
# pred2.range <- std_im_location_activity_v2$total_customers.normalized

# pred3.range <- std_im_location_activity_v2$cooler_than_normal_days.normalized
# pred4.range <- std_im_location_activity_v2$warmer_than_normal_days.normalized
# pred1.range <- std_im_location_activity_v2$distance_to_nearest_charger
# pred2.range <- std_im_location_activity_v2$total_customers
# pred3.range <- std_im_location_activity_v2$session_kWh_on_warmer_days

# predictors: distance between ev stations
grd <- expand.grid(x_axis_unique=seq(from=min(x.range), to=max(x.range), by=0.02),
                   y_axis_unique=seq(from=min(y.range), to=max(y.range), by=0.02),
                   distance_to_nearest_charger=seq(from=min(pred1.range), to=max(pred1.range), by=0.02))

grd <- expand.grid(x_axis_unique=seq(from=min(x.range), to=max(x.range), by=0.025),
                   y_axis_unique=seq(from=min(y.range), to=max(y.range), by=0.025),
                   distance_to_nearest_charger=seq(from=min(pred1.range), to=max(pred1.range), by=0.025),
                   total_customers=seq(from=min(pred2.range), to=max(pred2.range), by=0.025))

grd <- expand.grid(x_axis_unique=seq(from=min(x.range), to=max(x.range), by=0.1),
                   y_axis_unique=seq(from=min(y.range), to=max(y.range), by=0.1),
                   distance_to_nearest_charger.normalized=seq(from=min(pred1.range), to=max(pred1.range), by=0.1),
                   total_customers.normalized=seq(from=min(pred2.range), to=max(pred2.range), by=0.1),
                   cooler_than_normal_days.normalized=seq(from=min(pred3.range), to=max(pred3.range), by=0.1),
                   warmer_than_normal_days.normalized=seq(from=min(pred3.range), to=max(pred3.range), by=0.1))

rm(x.range, y.range, pred1.range, pred2.range, pred3.range)

g.stat <- gstat::gstat(id="im_response", 
                       formula = 
                         mtbf ~ distance_to_nearest_charger +
                         total_customers
                       
                       # total_hours_in_maintenance ~ distance_to_nearest_charger +
                       # total_customers
                       # total_hours_in_maintenance ~ distance_to_nearest_charger.normalized +
                       #                         total_customers.normalized
                       
                       
                       # distance_to_nearest_charger+total_customers
                       # log(distance_to_nearest_charger+0.01)
                       # +total_customers #distance_to_nearest_charger.normalized# total_customers
                       # session_kWh_on_cooler_days+
                       # cooler_than_normal_days.normalized+
                       # warmer_than_normal_days.normalized
                       
                       # total_customers+
                       # total_session_kWh,
                       
                       # total_MWh+
                       # total_customers+
                       # cooler_than_normal_days+
                       # warmer_than_normal_days,
                       # formula = mtbf~distance_to_nearest_charger+sessions_on_cooler_days,
                       # formula = total_hours_in_maintenance~distance_to_nearest_charger,
                       ,locations = ~x_axis_unique+y_axis_unique, 
                       data = std_im_location_activity_v2)
plot(variogram(g.stat))

## total-hours-in-maintenance
plot(variogram(g.stat, width=0.8))
## total-hours-in-maintenance
v.fit <- automap::autofitVariogram(total_hours_in_maintenance ~ 
                                     distance_to_nearest_charger.normalized+
                                     total_customers.normalized
                                   # +cooler_than_normal_days.normalized+
                                   # warmer_than_normal_days.normalized
                                   ,
                                   as(im_location_activity_sf_std, 'Spatial'))$var_model
# v.fit <- automap::autofitVariogram(total_hours_in_maintenance~distance_to_nearest_charger+total_customers,
#                                    as(im_location_activity_sf_std, 'Spatial'))$var_model

# plot(variogram(g.stat, cutoff=3.5, width=3.5/15))
# plot(variogram(g.stat, width=0.75))
plot(variogram(g.stat, width=0.020, alpha=c(0,45,90,135)), 
     main='Directional Variorgams') 
# at 135 degree direction, there's a wave pattern to the variogram, indicating 
## mountains in the spatial region - total-hours-IM ~ dist.to.nearest.charger.norm + total.cust.norm

plot(std_im_location_activity_v2$x_axis_unique, 
     std_im_location_activity_v2$y_axis_unique)



### fitting the variogram

# v.fit <- fit.variogram(variogram(g, width=0.02, alpha=c(90,135)),
v.fit <- fit.variogram(variogram(g.stat, width=0.2, alpha=135),
                       # alpha here. Directional variograms show if there's any directional dependence in our spatial points
                       ## the 90 (N-S) and 135 (NW-SE) directions display some hole effect in those respective directional variograms.
                       ## Hole effects indicate mountainous regions in the data. We should include this information
                       ## when fitting the variogram 
                       # v.fit <- fit.variogram(variogram(g),
                       # vgm(psil=0.1, "Ste",nugget = 1, range = 5))
                       # vgm(psil=1, "Sph",nugget = 0.1, range = 0.1))
                       vgm(psil=0.099, model="Gau", nugget=0.9, range = 4.0)) # stable for Gau + directional
# vgm(psil=2, model="Exp", nugget=2, range = 2)) # stable for almost any psil/nugg/range for Exp + directional 
# vgm(psil=5, model="Sph", nugget=5, range = 1))
# vgm(psil=0.022, model="Spl", nugget=0.085, range = 0.35))  # stable for for spline + directional
# vgm(psil=2, model="Sph", nugget=0.015, range=10.92))  # stable for these parameters + directional
# model.type <- 'Spherical'
model.type <- 'Gaussian'
# model.type <- 'Exponential'
plot(variogram(g.stat), v.fit, 
     main=paste0(model.type, ' Model Variogram - MTBF'))
# vgm(c("Gau", "Exp", "Sph", "Exc")))

plot(std_im_location_activity_v2$x_axis_unique, 
     std_im_location_activity_v2$y_axis_unique)

# v.fit <- automap::autofitVariogram(total_hours_in_maintenance~distance_to_nearest_charger,
#                                    as(im_location_activity_sf_std, 'Spatial'))$var_model
# plot(variogram(g), v.fit)

coordinates(std_im_location_activity_v2) ~ x_axis_unique + y_axis_unique

him.krig <- krige(id = 'im_response', 
                  # mtbf~distance_to_nearest_charger+total_customers
                  total_hours_in_maintenance ~
                    distance_to_nearest_charger+total_customers
                  # total_hours_in_maintenance ~ 
                  #   distance_to_nearest_charger.normalized+total_customers.normalized
                  
                  
                  
                  # cooler_than_normal_days.normalized+
                  # warmer_than_normal_days.normalized
                  # session_kWh_on_cooler_days+
                  # session_kWh_on_warmer_days
                  # total_customers
                  # total_session_kWh
                  
                  # total_MWh+
                  # total_customers
                  # cooler_than_normal_days+
                  # warmer_than_normal_days
                  ,
                  # total_hours_in_maintenance~distance_to_nearest_charger,
                  locations=~x_axis_unique + y_axis_unique,
                  model=v.fit,
                  data=std_im_location_activity_v2,
                  newdata=grd)

xy <- him.krig[, c(1,2)]
df <- SpatialPointsDataFrame(coords = xy, data=him.krig)

# coordinates(df) = ~x_axis_unique+y_axis_unique
gridded(df) = TRUE

# spplot(df['im_response.pred'])

# p <- spplot(df['im_response.pred'], main='Exponential Kriging Intermediate temperatures - dist. to nearest charger')
# p <- spplot(df['im_response.pred'], main=paste0('Exponential Kriging Intermediate temperatures - dist. to nearest charger -', 
#                                              model_name))
# p <- spplot(df['im_response.pred'], main='Exponential Kriging Intermediate temperatures - dist.near.charger + tot.cust')
# p <- spplot(df['im_response.pred'], main=paste0('Exponential Kriging Intermediate temperatures - dist.near.charger + tot.cust -',
# model_name))
# p <- spplot(df['im_response.pred'], main='Exponential Kriging Intermediate temperatures - kwh on cool + kwh on warmer')
# p <- spplot(df['im_response.pred'], main='Exponential Kriging Intermediate temperatures - dist.near.charger + 
#             tot.cust + tot.sess.kwh')

# p <- spplot(df['im_response.pred'], main='Gaussian Kriging Intermediate temperatures - dist. to nearest charger')
# p <- spplot(df['im_response.pred'], main=paste0('Gaussian Kriging Intermediate temperatures - dist. to nearest charger -',
#                                              model_name))
p <- spplot(df['im_response.pred'], main='Gaussian Kriging Intermediate temperatures - Distance to nearest charger & total customer predictors')
# p <- spplot(df['im_response.pred'], main='Gaussian Kriging Intermediate temperatures - kwh on cool + kwh on warmer')
# p <- spplot(df['im_response.pred'], main='Gaussian Kriging Intermediate temperatures - dist.near.charger + 
#             tot.cust + tot.sess.kwh')

# p <- spplot(df['im_response.pred'], main='Matheron Kriging Intermediate temperatures - dist. to nearest charger')
# p <- spplot(df['im_response.pred'], main='Matheron Kriging Intermediate temperatures - kwh on cool + kwh on warmer')
# p <- spplot(df['im_response.pred'], main='Matheron Kriging Intermediate temperatures - dist. to nearest charger + 
#              tot.cust + tot.sess.kwh')
# p <- spplot(df['im_response.pred'], main='M. Stein\'s parameterization Intermediate temperatures - dist. to nearest charger')
# print(p)
p + latticeExtra::layer(panel.points(std_im_location_activity_v2$x_axis_unique, 
                                     std_im_location_activity_v2$y_axis_unique, col='green', pch=5), 
                        data=std_im_location_activity_v2)


### let's tailor the spatial predicion layer based on the range of X 
image.orig <- image
qqq <- matrix(him.krig$im_response.pred, 
              length(seq(from=min(x.range), to=max(x.range), by=0.025)),
              length(seq(from=min(y.range), to=max(y.range), by=0.025)))
image.orig(seq(from=min(x.range), to=max(x.range), by=0.025), 
           seq(from=min(y.range), to=max(y.range), by=0.025),
           qqq, xlab='west - east', ylab='north - south', 
           main='Predicted values')
X <- seq(from=min(x.range), to=max(x.range), by=0.025)
Y <- seq(from=min(y.range), to=max(y.range), by=0.025)

# refer to mask function 
d <- mask(qqq, std_im_location_activity_v2, X, Y, 5)
image.orig(X, Y, d, main='Gaussian Kriging - Predicting MTBF')
points(std_im_location_activity_v2$x_axis_unique, 
       std_im_location_activity_v2$y_axis_unique)




### cross-validation
model1 <- vgm(psil=0.099, "Gau",nugget = 0.9, range = 4)
model.type <- 'Gau'

folds <- 5
nearest.max <- 15
cv <- krige.cv(
  formula=mtbf~distance_to_nearest_charger+total_customers,
  # formula=total_hours_in_maintenance ~ distance_to_nearest_charger+total_customers,
  locations=~x_axis_unique+y_axis_unique,
  data=std_im_location_activity_v2,
  model=model1, 
  # nmax=nearest.max, 
  nfold=folds)
xy_cv <- cv[, c('x_axis_unique', 'y_axis_unique')]
cv_spat <- SpatialPointsDataFrame(coords=xy_cv, data=cv)
bubble(cv_spat, "residual", main='Total Hours in Maintenance: 5-Fold CV residuals')
# bubble(cv_spat, "residual", main='MTBF: 5-Fold CV residuals')

summary(cv)
mean(cv$residual) # ideally 0
mean(cv$residual^2) # ideally small
mean(cv$zscore^2) # ideally close to 1
cor(cv$observed, cv$observed-cv$residual) # ideally close to 1 - corr of observed to predicted
cor(cv$observed - cv$residual, cv$residual) # ideally close to 0 - corr of predicted to residuals 


qqnorm(cv$residual)
qqline(cv$residual, col='red')
hist(cv$residual)

# plot this N-fold CV Kriged model
cv.gridded <- cv_spat
gridded(cv.gridded) = TRUE
spplot(cv.gridded['var1.pred'], 
       main=paste0(folds, ' fold CV predictions and standard error classifications - ', model.type))



## Moran's I Index for Spatial Correlation
distances <- as.matrix(dist(cbind(std_im_location_activity_v2$x_axis_unique, std_im_location_activity_v2$y_axis_unique)))
distances.inv <- 1/distances
diag(distances.inv) <- 0

library(ape)
Moran.I((std_im_location_activity_v2$total_hours_in_maintenance+0.01)^(-0.8400911), distances.inv)
Moran.I(log(std_im_location_activity_v2$mtbf+0.01),distances.inv)





### Evaluate the significance of the variogram via the standard error. 
### Bootstrap 1000 samples of ev stations from the data and randomly place at 
### other locations in existing points. Plot these 1000 variograms and evaluate 
### the sig of my actual variogram against these samples. 

set.seed(122)

# empty plot
# plot(x=NA, y=NA, xlim=c(1,10), ylim=c(1,10), xlab="", ylab="", main="")
n <- 1000
stations <- unique(std_im_location_activity_v2$station_id)
coords <- std_im_location_activity_v2[, c('x_axis_unique', 'y_axis_unique')]
variogram.vec <- vector()
for(i in 1:n) {
  draw.ev.stations <- sample(stations, length(stations), replace=TRUE)
  df <- std_im_location_activity_v2[which(std_im_location_activity_v2$station_id %in% 
                                            draw.ev.stations), c('station_id', 'x_axis_unique', 'y_axis_unique')]
  df1 <- as.data.frame(draw.ev.stations)
  colnames(df1) <- 'station_id'
  df2 <- merge(df1, df, by='station_id')
  
  df.dat <- merge(df2, std_im_location_activity_v2, by='station_id') %>% 
    dplyr::select(station_id, x_axis_unique.x, y_axis_unique.x, total_hours_in_maintenance, 
                  total_customers, distance_to_nearest_charger, mtbf) %>% 
    dplyr::rename(station_id_rand = station_id, x_axis_unique_rand = x_axis_unique.x, y_axis_unique_rand = y_axis_unique.x)
  # sample.index <- sample(1:212, 212, replace=TRUE)
  # draw.coords <- coords[sample.index, ]
  # coords <- subset(std_im_location_activity_v2, station_id %in% draw.ev.stations)[c('station_id', 'x_axis_unique', 'y_axis_unique')]
  # df <- as.data.frame(cbind(station_id_rand=draw.ev.stations, 
  #                           x_axis_unique_rand=draw.coords$x_axis_unique, 
  #                           y_axis_unique_rand=draw.coords$y_axis_unique))
  # 
  # merged.df <- merge(x=df, y=std_im_location_activity_v2, 
  #                    by.x='station_id_rand', by.y='station_id') %>% 
  #   dplyr::select(station_id_rand, x_axis_unique_rand, y_axis_unique_rand, total_hours_in_maintenance, 
  #          total_customers, distance_to_nearest_charger, mtbf)
  g <- gstat(id='mtbf', 
             # formula = total_hours_in_maintenance ~ distance_to_nearest_charger+total_customers,
             formula = mtbf ~ distance_to_nearest_charger+total_customers,
             locations = ~x_axis_unique_rand+y_axis_unique_rand,
             data=df.dat)
  var.dist <- variogram(g)$dist
  var.gam <- variogram(g)$gamma
  variogram.i <- cbind(var.dist, var.gam)
  variogram.vec <- rbind(variogram.i, variogram.vec)
}
rm(df, df1, df2, df.dat)


# length(unique(std_im_location_activity_v2[which(std_im_location_activity_v2$station_id %in% 
#                                                   draw.ev.stations), c('station_id')]))
# subset(std_im_location_activity_v2, station_id == 325)
# cbind(draw.ev.stations, std_im_location_activity_v2[which(std_im_location_activity_v2$station_id %in% 
#                                                             draw.ev.stations), c('x_axis_unique', 'y_axis_unique')])


bootstrapped.sampled.variograms <- as.data.frame(variogram.vec)
rownames(bootstrapped.sampled.variograms) <- NULL
colnames(bootstrapped.sampled.variograms) <- c('distance', 'gamma')
plot(x=bootstrapped.sampled.variograms$distance, 
     y=bootstrapped.sampled.variograms$gamma, 
     main='1,000 Bootstrapped Variograms - Total Hours in Maintenance', #Total Hours in Maintenance 
     xlab='Distance', ylab='semivariance')
# abline(h=0.07)
# abline(h=0.128)


# cut distances into bins so as to find the confidence interval more easily
dist.intervals <- seq(min(bootstrapped.sampled.variograms$distance), 
                      max(bootstrapped.sampled.variograms$distance), 0.08)


bootstrapped.sampled.variograms$dist.bins <-
  case_when(bootstrapped.sampled.variograms$distance<=0.02013918 ~1,
            bootstrapped.sampled.variograms$distance<=0.10013918 ~2,
            bootstrapped.sampled.variograms$distance<=0.18013918 ~3,
            bootstrapped.sampled.variograms$distance<=0.26013918 ~4,
            bootstrapped.sampled.variograms$distance<=0.34013918 ~5,
            bootstrapped.sampled.variograms$distance<=0.42013918 ~6,
            bootstrapped.sampled.variograms$distance<=0.50013918 ~7,
            bootstrapped.sampled.variograms$distance<=0.58013918 ~8,
            bootstrapped.sampled.variograms$distance<=0.66013918 ~9,
            bootstrapped.sampled.variograms$distance<=0.74013918 ~10,
            bootstrapped.sampled.variograms$distance<=0.82013918 ~11,
            bootstrapped.sampled.variograms$distance<=0.90013918 ~12,
            bootstrapped.sampled.variograms$distance<=0.98013918 ~13,
            bootstrapped.sampled.variograms$distance<=1.06013918 ~14,
            bootstrapped.sampled.variograms$distance<=1.14013918 ~15,
            TRUE~16)



g <- ggplot(bootstrapped.sampled.variograms, 
            aes(x=dist.bins, y=gamma, group=dist.bins))
g + geom_boxplot() +
  labs(title = 'Boxplot of Distanced Bins') +
  xlab('Distance Bins') +
  ylab('semivariance')

# g <- ggplot(bootstrapped.sampled.variograms, 
#             aes(x=dist.bins, y=gamma))
# g + geom_point()


ci.bounds <- bootstrapped.sampled.variograms %>% 
  group_by(dist.bins) %>% 
  summarize(sample.mean = mean(gamma),
            sample.sd = sd(gamma),
            sample.n = n(),
            sample.se = mean(gamma)/sqrt(n()))

ci.bounds$t.score <- qt(p=0.05/2, df=ci.bounds$sample.n-1, lower.tail = F)
ci.bounds$error.margin <- ci.bounds$t.score * ci.bounds$sample.se
ci.bounds$lower.bound <- ci.bounds$sample.mean - ci.bounds$error.margin
ci.bounds$upper.bound <- ci.bounds$sample.mean + ci.bounds$error.margin
head(ci.bounds)
# plot(ci.bounds$dist.bins, ci.bounds$lower.bound)
# plot(ci.bounds$dist.bins, ci.bounds$upper.bound)

# g <- ggplot(data = ci.bounds, aes(x=dist.bins, y=seq(-1,3, 0.0435)))
# g + geom_point() +
#   geom_errorbar(aes(ymin=lower.bound, ymax=upper.bound))


# dim(bootstrapped.sampled.variograms)
# dim(ci.bounds)

# test.info
bootstrapped.sampled.variograms.ci.bounds <- 
  merge(bootstrapped.sampled.variograms, ci.bounds, by='dist.bins')

g <- ggplot(bootstrapped.sampled.variograms.ci.bounds, 
            aes(x=dist.bins, y=gamma))
g + geom_point() +
  # scale_x_continuous(limits = c(0,60)) +
  # scale_y_continuous(limits = c(-0.5, 1.5)) +
  # xlim(0,120) + ylim(0,0.19) +
  # geom_ribbon(aes(ymin=lower.bound, ymax=upper.bound), alpha=0.2)
  geom_errorbar(aes(ymin=lower.bound, ymax=upper.bound))
# + xlim(0,60) + ylim(0,0.19)


# g <- ggplot(bootstrapped.sampled.variograms.ci.bounds, 
#             aes(x=dist.bins, y=gamma))
# g + geom_point() +
#   geom_ribbon(aes(ymin=lower.bound, ymax=upper.bound), alpha=0.2)
# # geom_errorbar(aes(ymin=lower.bound, ymax=upper.bound))

# g <- ggplot(bootstrapped.sampled.variograms.ci.bounds, 
#             aes(x=dist.bins, y=gamma))
# g + geom_point()
 
ev.station.variogram <- variogram(g.stat) %>% as.data.frame 


ev.station.variogram$dist.bins <-
  case_when(ev.station.variogram$dist<=0.02013918 ~1,
            ev.station.variogram$dist<=0.10013918 ~2,
            ev.station.variogram$dist<=0.18013918 ~3,
            ev.station.variogram$dist<=0.26013918 ~4,
            ev.station.variogram$dist<=0.34013918 ~5,
            ev.station.variogram$dist<=0.42013918 ~6,
            ev.station.variogram$dist<=0.50013918 ~7,
            ev.station.variogram$dist<=0.58013918 ~8,
            ev.station.variogram$dist<=0.66013918 ~9,
            ev.station.variogram$dist<=0.74013918 ~10,
            ev.station.variogram$dist<=0.82013918 ~11,
            ev.station.variogram$dist<=0.90013918 ~12,
            ev.station.variogram$dist<=0.98013918 ~13,
            ev.station.variogram$dist<=1.06013918 ~14,
            ev.station.variogram$dist<=1.14013918 ~15,
            TRUE~16)



bounds <- bootstrapped.sampled.variograms.ci.bounds %>% 
  group_by(dist.bins, lower.bound, upper.bound) %>% 
  summarize(tot=n()) %>% 
  dplyr::select(dist.bins, lower.bound, upper.bound, tot)
ev.station.variogram <- merge(ev.station.variogram, bounds, by='dist.bins')

g <- ggplot(bootstrapped.sampled.variograms.ci.bounds, 
            aes(x=dist.bins, y=gamma))
g + geom_point() +
  geom_errorbar(aes(ymin=lower.bound, ymax=upper.bound)) +
  geom_point(data=ev.station.variogram,
             mapping = aes(x=dist.bins,y=gamma, color='red')) +
  scale_color_discrete(name='Observed variogram') +
  # scale_x_continuous(limits = c(0,45)) + # for MTBF
  # scale_x_continuous(limits = c(0,75)) + # for total hours in maintenance
  labs(title='95% Confidence Interval Bootstrapped Variograms & Observed Variogram - MTBF', 
       subtitle = 'Zoomed at the observed variogram rise',
       x='Distance Bins',y='semivariance')


model.type <- 'Gaussian'
plot(variogram(g.stat), v.fit, main=paste0(model.type, ' Model Variogram'))

ev.station.variogram[, c('dist', 'lower.bound', 'upper.bound')] %>% 
  mutate(outside.range = ifelse((dist <= lower.bound) || (dist >= upper.bound), 1, 0))



# test.summary <- test %>% 
#   group_by(distance) %>% 
#   summarize(total = n_distinct(gamma))
# 
# 
# stations_w_shared_coords <- im_df %>% 
#   group_by(x_axis, y_axis) %>% 
#   summarize(same_coords_stations = n_distinct(station_id), 
#             toString(sort(unique(station_id)))) %>% 
#   filter(same_coords_stations > 1)



# head(std_im_location_activity_v2)

# head(merged.df)



# Kriging on cold region - COLD REGION NOT PURSUED DUE TO NOT ENOUGH DATA

### Because the cold data is made of of 4 separate region we should evaluate each one individually. The distance
## between each region is enough to render the effect between each region as negligible.

# 
# # "Exp", "Sph", "Gau", "Mat"
x.range <- as.integer(std_co_location_activity_v2_r4$x_axis_unique)
y.range <- as.integer(std_co_location_activity_v2_r4$y_axis_unique)
pred1.range <- std_co_location_activity_v2_r4$distance_to_nearest_charger
pred2.range <- std_co_location_activity_v2_r4$total_customers
# 
# grd <- expand.grid(x_axis_unique=seq(from=min(x.range), to=max(x.range), by=0.01),
#                    y_axis_unique=seq(from=min(y.range), to=max(y.range), by=0.01),
#                    distance_to_nearest_charger=seq(from=min(pred1.range), to=max(pred1.range), by=0.01))
# 
grd <- expand.grid(x_axis_unique=seq(from=min(x.range), to=max(x.range), by=0.04),
                   y_axis_unique=seq(from=min(y.range), to=max(y.range), by=0.04),
                   distance_to_nearest_charger=seq(from=min(pred1.range), to=max(pred1.range), by=0.04),
                   total_customers=seq(from=min(pred2.range), to=max(pred2.range), by=0.04))
# 
g.stat <- gstat::gstat(id="log_response",
                       formula = total_hours_in_maintenance ~ 
                         distance_to_nearest_charger,
                       locations = ~x_axis_unique+y_axis_unique,
                       data = std_co_location_activity_v2_r4)
plot(variogram(g.stat))
plot(variogram(g.stat, width=0.020, alpha=c(0,45,90,135)), 
     main='Directional Variorgams') 

###### end kriging




################################################################################
################### Weibull Modeling 
###################

######
## preparing data for weibull modeling

df.m <- im_df
df.m$running.im.adj.now.grouping <- df.m$running.im.adj.now+1

my.vec <- vector()
stored.vec <- vector()
stations <- unique(df.m$station_id)
for(i in stations) {
  sub.df <- subset(df.m, station_id == i)
  days <- unique(sub.df$flowdate)
  for(day in as.list(days)) {
    if(sub.df[which(sub.df$flowdate==day), ]$mtbf_flag!=0) {
      sub.df[which(sub.df$flowdate==day), ]$running.im.adj.now.grouping <- sub.df[which(sub.df$flowdate==day), ]$running.im.adj.now.grouping-1
    }
  }
  my.vec <- sub.df$running.im.adj.now.grouping
  stored.vec <- append(stored.vec, my.vec)
}

df.m$running.im.adj.now.grouping <- stored.vec
df.m$temp_class <- 'moderate'
rm(my.vec, stored.vec, stations, i, sub.df, day, days)




df.c <- co_df
df.c$running.im.adj.now.grouping <- df.c$running.im.adj.now+1

my.vec <- vector()
stored.vec <- vector()
stations <- unique(df.c$station_id)
for(i in stations) {
  sub.df <- subset(df.c, station_id == i)
  days <- unique(sub.df$flowdate)
  for(day in as.list(days)) {
    if(sub.df[which(sub.df$flowdate==day), ]$mtbf_flag!=0) {
      sub.df[which(sub.df$flowdate==day), ]$running.im.adj.now.grouping <- sub.df[which(sub.df$flowdate==day), ]$running.im.adj.now.grouping-1
    }
  }
  my.vec <- sub.df$running.im.adj.now.grouping
  stored.vec <- append(stored.vec, my.vec)
}

df.c$running.im.adj.now.grouping <- stored.vec
df.c$temp_class <- 'cold'
rm(my.vec, stored.vec, stations, i, sub.df, day, days)




df.w <- wa_df
df.w$running.im.adj.now.grouping <- df.w$running.im.adj.now+1

my.vec <- vector()
stored.vec <- vector()
stations <- unique(df.w$station_id)
for(i in stations) {
  sub.df <- subset(df.w, station_id == i)
  days <- unique(sub.df$flowdate)
  for(day in as.list(days)) {
    if(sub.df[which(sub.df$flowdate==day), ]$mtbf_flag!=0) {
      sub.df[which(sub.df$flowdate==day), ]$running.im.adj.now.grouping <- sub.df[which(sub.df$flowdate==day), ]$running.im.adj.now.grouping-1
    }
  }
  my.vec <- sub.df$running.im.adj.now.grouping
  stored.vec <- append(stored.vec, my.vec)
}

df.w$running.im.adj.now.grouping <- stored.vec
df.w$temp_class <- 'warm'
rm(my.vec, stored.vec, stations, i, sub.df, day, days)


df <- rbind(df.m, df.c, df.w)

cols <- c('flowdate', 'station_id', 'station_intances_adj', 'day_count', 'charger_company_col', 'single_im_day', 'minutes_diff',
          'sessions', 'minutes', 'session_kwh', 'unique_customers', 'running.im.adj.now', 'running.im.adj.now.grouping', 'mtbf_flag',
          'im_flag', 'max.periods', 'period_count', 'remaining_days', 'temps', 'dist_km', 
          'uptime_minutes', 'temp_q1', 'temp_median', 'temp_q3', 'cooler_than_normal', 'normal', 
          'warmer_than_normal', 'subregion.y', 'temp_class')

df <- df[,cols]
head(df)


df.2 <- df %>% group_by(station_id, running.im.adj.now.grouping, 
                        charger_company_col, temp_class, subregion.y) %>% 
  summarise(operational_days=max(day_count),
            tot_sessions=sum(sessions, na.rm = TRUE),
            tot_sessions_on_cooler_days=sum(sessions[(mtbf_flag==0) & 
                                                       cooler_than_normal==1], na.rm = TRUE),
            tot_sessions_on_normal_days=sum(sessions[(mtbf_flag==0) & 
                                                       normal==1], na.rm = TRUE),
            tot_sessions_on_warmer_days=sum(sessions[(mtbf_flag==0) & 
                                                       warmer_than_normal==1], na.rm = TRUE),
            
            tot_minutes=sum(minutes, na.rm = TRUE),
            tot_minutes_on_cooler_days=sum(minutes[(mtbf_flag==0) & 
                                                       cooler_than_normal==1], na.rm = TRUE),
            tot_minutes_on_normal_days=sum(minutes[(mtbf_flag==0) & 
                                                    normal==1], na.rm = TRUE),
            tot_minutes_on_warmer_days=sum(minutes[(mtbf_flag==0) & 
                                                       warmer_than_normal==1], na.rm = TRUE),
            
            tot_kwh=sum(session_kwh, na.rm = TRUE),
            tot_kwh_on_cooler_days=sum(session_kwh[(mtbf_flag==0) & 
                                                     cooler_than_normal==1], na.rm = TRUE),
            tot_kwh_on_normal_days=sum(session_kwh[(mtbf_flag==0) & 
                                                  normal==1], na.rm = TRUE),
            tot_kwh_on_warmer_days=sum(session_kwh[(mtbf_flag==0) & 
                                                     warmer_than_normal==1], na.rm = TRUE),
            
            tot_customers=sum(unique_customers, na.rm = TRUE),
            tot_customers_on_cooler_days=sum(unique_customers[(mtbf_flag==0) & 
                                                     cooler_than_normal==1], na.rm = TRUE),
            tot_customers_on_normal_days=sum(unique_customers[(mtbf_flag==0) & 
                                                  normal==1], na.rm = TRUE),
            tot_customers_on_warmer_days=sum(unique_customers[(mtbf_flag==0) & 
                                                     warmer_than_normal==1], na.rm = TRUE),
            
            hist_of_im=max(running.im.adj.now.grouping)-1,
            remaining_days_y=max(remaining_days),
            dist_km=max(dist_km),
            avg_temp=mean(temps[mtbf_flag==0]), # average the temp of days right before going IM
            temp_q1=max(temp_q1),
            temp_q3=max(temp_q3),
            cooler_than_normal_days=sum(cooler_than_normal[mtbf_flag==0]),
            normal_days=sum(normal[mtbf_flag==0]),
            warmer_than_normal_days=sum(warmer_than_normal[mtbf_flag==0]),
            last_day_operational=max(flowdate[mtbf_flag==0]),
            last_day_im=max(flowdate[mtbf_flag!=0]))

evs_df <- rbind(im_df, co_df, wa_df) %>%
  group_by(station_id) %>%
  summarise(max_im_reached = max(mtbf_flag),
            max_end_date = max(flowdate))

merged.df <- merge(df.2, evs_df, by='station_id', all.x=TRUE)
merged.df$last_day_im <- as.character(merged.df$last_day_im)
merged.df$status <- ifelse(is.na(merged.df$last_day_im), 0, 1)
merged.df <- merged.df %>% mutate(subregion.id = paste(temp_class, subregion.y))


# as factors 
# merged.df$charger_company_col <- factor(merged.df$charger_company_col, labels = 'vxxip')
merged.df$charger_company_col <- relevel(merged.df$charger_company_col, ref = 'vxxip')

merged.df$temp_class <- factor(merged.df$temp_class)
merged.df$temp_class <- relevel(merged.df$temp_class, ref='moderate')

merged.df$hist_of_im <- factor(merged.df$hist_of_im)
merged.df$hist_of_im <- relevel(merged.df$hist_of_im, ref='0')


subset.merged.df <- subset(merged.df, !(hist_of_im %in% 8:20))
subset.merged.df$hist_of_im <- factor(subset.merged.df$hist_of_im)
# head(merged.df)

library(SPREDA)
plot(ecdf(merged.df[merged.df$status==1, 'remaining_days_y']),
     main='Cumulative Failures')
m1 <- Lifedata.MLE(Surv(operational_days, status)~1, 
                   data = merged.df,
                   dist = 'weibull')

summary(m1)
plot(m1)


library(survival)
library(PerformanceAnalytics)
library(Hmisc) # 
# install.packages('ggcorrplot')
library(ggcorrplot)


# checking variables that were not analyzed in PCA
subset.merged.df %>%
  select(-c('tot_sessions', 'tot_sessions_on_cooler_days', 'tot_sessions_on_normal_days', 'tot_sessions_on_warmer_days', 
            'tot_minutes', 'tot_minutes_on_cooler_days', 'tot_minutes_on_normal_days', 'tot_minutes_on_warmer_days', 
            'tot_kwh', 'tot_kwh_on_cooler_days', 'tot_kwh_on_normal_days', 'tot_kwh_on_warmer_days', 
            'tot_customers', 'tot_customers_on_cooler_days', 'tot_customers_on_normal_days', 'tot_customers_on_warmer_days', 
            'avg_temp', 
            'cooler_than_normal_days', 'normal_days', 'warmer_than_normal_days')) %>% 
  head()

chart.Correlation(subset.merged.df[, c(
  'operational_days', 
  'tot_sessions', 'tot_sessions_on_cooler_days', 'tot_sessions_on_normal_days', 
  'tot_sessions_on_warmer_days', 'tot_minutes', 'tot_minutes_on_cooler_days', 
  'tot_minutes_on_normal_days', 'tot_minutes_on_warmer_days', 'tot_kwh',   
  'tot_kwh_on_cooler_days', 'tot_kwh_on_normal_days', 'tot_kwh_on_warmer_days', 
  'tot_customers', 'tot_customers_on_cooler_days', 'tot_customers_on_normal_days',
  'tot_customers_on_warmer_days', 'cooler_than_normal_days', 'normal_days', 
  'warmer_than_normal_days')])
# 'avg_temp',

corr1 <- cor(merged.df[, c('operational_days', 
                  'tot_sessions', 'tot_sessions_on_cooler_days', 'tot_sessions_on_normal_days', 
                  'tot_sessions_on_warmer_days', 'tot_minutes', 'tot_minutes_on_cooler_days', 
                  'tot_minutes_on_normal_days', 'tot_minutes_on_warmer_days', 'tot_kwh',   
                  'tot_kwh_on_cooler_days', 'tot_kwh_on_normal_days', 'tot_kwh_on_warmer_days', 
                  'tot_customers', 'tot_customers_on_cooler_days', 'tot_customers_on_normal_days',
                  'tot_customers_on_warmer_days', 'cooler_than_normal_days', 'normal_days', 
                  'warmer_than_normal_days')])
# 'avg_temp',

corr1 <- cor(merged.df[, c('operational_days', 'tot_minutes', 'tot_minutes_on_cooler_days', 
                           'tot_minutes_on_normal_days', 'tot_minutes_on_warmer_days', 
                           'cooler_than_normal_days', 'normal_days', 'warmer_than_normal_days')])

ggcorrplot(corr1, 
           type = 'upper', 
           outline.color = 'white', 
           show.legend = TRUE,
           lab = TRUE) +
  ggtitle('Correlation Plot of Attributes') +
  theme(plot.title = element_text(hjust=0.5, size = 25))


corr <- rcorr(as.matrix(merged.df[,cols]), type = 'spearman')
corr$P


cols <- c('tot_sessions', 'tot_sessions_on_cooler_days', 'tot_sessions_on_normal_days', 'tot_sessions_on_warmer_days', 
         'tot_minutes', 'tot_minutes_on_cooler_days', 'tot_minutes_on_normal_days', 'tot_minutes_on_warmer_days', 
         'tot_kwh', 'tot_kwh_on_cooler_days', 'tot_kwh_on_normal_days', 'tot_kwh_on_warmer_days', 
         'tot_customers', 'tot_customers_on_cooler_days', 'tot_customers_on_normal_days', 'tot_customers_on_warmer_days', 
         
         'cooler_than_normal_days', 'normal_days', 'warmer_than_normal_days')

std_var.pca <- as.data.frame(lapply(merged.df[,cols],standardize)) # standardize

              
pcomps <- prcomp(std_var.pca)
# scale. = TRUE

# 'avg_temp', 

# pcomps$x <- pcomps$x*(-1)

summary(pcomps)

biplot(pcomps, scale=0)


loadings <- pcomps$rotation
rownames(loadings) <- colnames(pcomps)
scores <- pcomps$x


biplot(scores[, 1:6], loadings[, 1:6], scale=0, 
       xlim = c(-15, 2),
       ylim = c(-8, 5))


pcomps$sdev^2/sum(pcomps$sdev^2)
pcomps.explained <- pcomps$sdev^2/sum(pcomps$sdev^2)
qplot(1:19, pcomps.explained) +
  geom_line() +
  ylim(c(0,1))+
  xlab('Principal Components') +
  ylab('Variance Explained') +
  ggtitle('Scree plot')

qplot(1:7, pcomps.explained[1:7]) +
  geom_line() +
  ylim(c(0,1)) +
  xlab('Principal Components') +
  ylab('Variance Explained') +
  ggtitle('Scree Plot - First 6 PCs') +
  theme(plot.title = element_text(hjust=0.5, size = 15))

# subset.merged.df

### Let's cut merged.df to contain only the variables from PCA and store it 
### in final.merged.df
final.merged.df <- merged.df %>% 
  select(operational_days, status, tot_sessions, tot_minutes_on_cooler_days, 
         tot_minutes_on_warmer_days, tot_minutes_on_normal_days,
           cooler_than_normal_days, warmer_than_normal_days, charger_company_col, 
         hist_of_im, subregion.id, temp_class)

# subset into train and test groups
set.seed(11)
shuffle.df <- sort(sample(nrow(final.merged.df), nrow(final.merged.df)*0.8))
train.df <- final.merged.df[shuffle.df, ]
test.df <- final.merged.df[-shuffle.df, ]

wei.im <- survreg(Surv(operational_days, status)~
                    tot_sessions + tot_minutes_on_cooler_days + tot_minutes_on_warmer_days + 
                    tot_minutes_on_normal_days +
                    cooler_than_normal_days +
                    warmer_than_normal_days + charger_company_col + hist_of_im,
                  data = train.df,
                  dist = 'weibull')

summary(wei.im)
 
## quick test to see what the QQ/PP plots reveal when removing hist-of-im <=7
# early.check.df <- subset(train.df, !(hist_of_im %in% 7:20))
# early.check.df <- subset(train.df, operational_days <=10)
early.check.df <- subset(train.df, operational_days <= 259)
early.check.df$hist_of_im <- factor(early.check.df$hist_of_im)
fit.mod.wei.quick <- fitdistrplus::fitdist(early.check.df$operational_days, 'weibull')
fit.mod.wei.quick <- fitdistrplus::fitdist(train.df$operational_days, 'weibull')
plot(fit.mod.wei.quick)

fit.mod.wei <- fitdistrplus::fitdist(final.merged.df$operational_days, 'weibull')
weib_shape <- fit.mod.wei$estimate['shape']
weib_scale <- fit.mod.wei$estimate['scale']
summary(fit.mod.wei)

fit.mod.gam <- fitdistrplus::fitdist(final.merged.df$operational_days, 'gamma')
gam_shape <- fit.mod.gam$estimate['s hape']
gam_scale <- fit.mod.gam$estimate['rate']
summary(fit.mod.gam)

plot(fit.mod.wei)
qqplot(qweibull(ppoints(length(final.merged.df$operational_days),
                        shape=fit.mod.wei$estimate[1],
                        scale=fit.mod.wei$estimate[2]),
                final.merged.df$operational_days))

legend <- c('Weibull','Gamma')
fitdistrplus::denscomp(list(fit.mod.wei, fit.mod.gam), legendtext = legend)



a <- wei.im$scale # weibull's shape
b <- exp(predict(wei.im, type = 'lp'))

library(SurvRegCensCov)

ConvertWeibull(wei.im, conf.level = 0.95)


test.df.y <- test.df[, c('operational_days', 'status')]
test.df.x <- subset(test.df, select = -c(operational_days, status))
preds <- predict(wei.im, type = 'quantile',
                 p=0.5, 
                 newdata = subset(test.df.x, select = -c(subregion.id)),
                 se.fit = TRUE)


fits <- preds$fit
fits.se <- preds$se.fit

accuracy <- cbind(actual = test.df.y$operational_days, 
      fits, 
      fits.se, 
      lower.bound = fits - 1.96 * fits.se, 
      upper.bound = fits + 1.96 * fits.se, 
      obs.within.95.ci = ifelse((test.df.y$operational_days) >= (fits - 1.96 * fits.se) &
                                  (test.df.y$operational_days) <= fits + 1.96 * fits.se, 1, 0),
      acc.lb = fits/2,
      acc.ub = fits*2,
      accuracy = ifelse((test.df.y$operational_days >= fits/2) & 
               (test.df.y$operational_days) <= fits*2, 1, 0),
      stricter.accuracy = ifelse((test.df.y$operational_days / fits) > 0.67 &
                                   (test.df.y$operational_days / fits) < 1.33,1,0))

accuracy <- as.data.frame(accuracy)

sum(accuracy$accuracy) / dim(accuracy)[1]
sum(accuracy$stricter.accuracy) / dim(accuracy)[1]
sum(accuracy$obs.within.95.ci) / dim(accuracy)[1]

mse(actual = test.df.y$operational_days, predicted = fits)
mae(actual = test.df.y$operational_days, predicted = fits)
rmse(actual = test.df.y$operational_days, predicted = fits)

WeibullDiag(Surv(operational_days, status)~charger_company_col,
            data=train.df)

WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=subset(train.df, !hist_of_im %in% 7:20))
WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=train.df)

WeibullDiag(Surv(operational_days, status)~subregion.id,
            data=train.df)



### ### ### ### ### ### ### ### ### 
### Regionalized analysis

mod.df <- subset(merged.df, temp_class == 'moderate')
col.df <- subset(merged.df, temp_class == 'cold')
war.df <- subset(merged.df, temp_class == 'warm')


### correlation & PCA
cols <- c('tot_sessions', 'tot_sessions_on_cooler_days', 'tot_sessions_on_normal_days', 'tot_sessions_on_warmer_days', 
          'tot_minutes', 'tot_minutes_on_cooler_days', 'tot_minutes_on_normal_days', 'tot_minutes_on_warmer_days', 
          'tot_kwh', 'tot_kwh_on_cooler_days', 'tot_kwh_on_normal_days', 'tot_kwh_on_warmer_days', 
          'tot_customers', 'tot_customers_on_cooler_days', 'tot_customers_on_normal_days', 'tot_customers_on_warmer_days', 
          
          'cooler_than_normal_days', 'normal_days', 'warmer_than_normal_days')

# moderate 
corr1 <- cor(mod.df[, cols])

ggcorrplot(corr1, 
           type = 'upper', 
           outline.color = 'white', 
           show.legend = TRUE,
           lab = TRUE) +
  ggtitle('Correlation Plot of Attributes - Moderate Region') +
  theme(plot.title = element_text(hjust=0.5, size = 25))


std_var.pca <- as.data.frame(lapply(mod.df[,cols],standardize)) # standardize
pcomps.mod <- prcomp(std_var.pca)
summary(pcomps.mod)
pcomps.explained.mod <- pcomps.mod$sdev^2/sum(pcomps.mod$sdev^2)
qplot(1:19, pcomps.explained.mod) +
  geom_line() +
  ylim(c(0,1)) +
  xlab('Principal Components') +
  ylab('Variance Explained') +
  ggtitle('Scree Plot - Moderate Region') +
  theme(plot.title = element_text(hjust=0.5, size = 15))

shuffle.df <- sort(sample(nrow(mod.df), nrow(mod.df)*0.8))
train.df <- mod.df[shuffle.df, ]
test.df <- mod.df[-shuffle.df, ]

wei.im.mod <- survreg(Surv(operational_days, status)~
                    tot_sessions + warmer_than_normal_days +
                      cooler_than_normal_days +
                      tot_minutes_on_normal_days + 
                      tot_customers_on_cooler_days +
                      tot_minutes_on_warmer_days + 
                      charger_company_col + hist_of_im,
                  data = train.df,
                  dist = 'weibull')
summary(wei.im.mod)
ConvertWeibull(wei.im.mod, conf.level = 0.95)
test.df.y <- test.df[, c('operational_days', 'status')]
test.df.x <- subset(test.df, select = -c(operational_days, status))
preds <- predict(wei.im.mod, type = 'quantile',
                 p=0.5, 
                 newdata = subset(test.df.x, select = -c(subregion.id)),
                 se.fit = TRUE)
fits <- preds$fit
fits.se <- preds$se.fit

accuracy <- cbind(actual = test.df.y$operational_days, 
                  fits, 
                  fits.se, 
                  lower.bound = fits - 1.96 * fits.se, 
                  upper.bound = fits + 1.96 * fits.se, 
                  acc.lb = fits/2,
                  acc.ub = fits*2,
                  accuracy = ifelse((test.df.y$operational_days >= fits/2) & 
                                      (test.df.y$operational_days) <= fits*2, 1, 0),
                  stricter.accuracy = ifelse((test.df.y$operational_days / fits) > 0.67 &
                                               (test.df.y$operational_days / fits) < 1.33,1,0))

accuracy <- as.data.frame(accuracy)

sum(accuracy$accuracy) / dim(accuracy)[1]
sum(accuracy$stricter.accuracy) / dim(accuracy)[1]

mse(actual = test.df.y$operational_days, predicted = fits)
rmse(actual = test.df.y$operational_days, predicted = fits)
mae(actual = test.df.y$operational_days, predicted = fits)


WeibullDiag(Surv(operational_days, status)~charger_company_col,
            data=train.df)
WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=subset(train.df, !hist_of_im %in% 7:20))
WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=train.df)
WeibullDiag(Surv(operational_days, status)~subregion.id,
            data=train.df)


fit.mod.wei <- fitdistrplus::fitdist(train.df$operational_days, 'weibull')
# weib_shape <- fit.mod.wei$estimate['shape']
# weib_scale <- fit.mod.wei$estimate['scale']
summary(fit.mod.wei)

plot(fit.mod.wei)



# cold
corr1 <- cor(col.df[, cols])

ggcorrplot(corr1, 
           type = 'upper', 
           outline.color = 'white', 
           show.legend = TRUE,
           lab = TRUE) +
  ggtitle('Correlation Plot of Attributes - Cold Region') +
  theme(plot.title = element_text(hjust=0.5, size = 25))


std_var.pca <- as.data.frame(lapply(col.df[,cols],standardize)) # standardize
pcomps.col <- prcomp(std_var.pca)
summary(pcomps.col)
pcomps.explained.col <- pcomps.col$sdev^2/sum(pcomps.col$sdev^2)
qplot(1:19, pcomps.explained.col) +
  geom_line() +
  ylim(c(0,1)) +
  xlab('Principal Components') +
  ylab('Variance Explained') +
  ggtitle('Scree Plot - Cold Region') +
  theme(plot.title = element_text(hjust=0.5, size = 15))

shuffle.df <- sort(sample(nrow(col.df), nrow(col.df)*0.8))
train.df <- col.df[shuffle.df, ]
test.df <- col.df[-shuffle.df, ]

wei.im.col <- survreg(Surv(operational_days, status)~
                    tot_sessions + 
                      tot_minutes_on_cooler_days + 
                      cooler_than_normal_days +
                      tot_customers_on_cooler_days +
                      tot_kwh_on_normal_days +
                      warmer_than_normal_days +
                      charger_company_col + 
                      hist_of_im,
                  data = train.df,
                  dist = 'weibull')
summary(wei.im.col)

ConvertWeibull(wei.im.col, conf.level = 0.95)

test.df.y <- test.df[, c('operational_days', 'status')]
test.df.x <- subset(test.df, select = -c(operational_days, status))
preds <- predict(wei.im.col, type = 'quantile',
                 p=0.5, 
                 newdata = subset(test.df.x, select = -c(subregion.id)),
                 se.fit = TRUE)
fits <- preds$fit
fits.se <- preds$se.fit

accuracy <- cbind(actual = test.df.y$operational_days, 
                  fits, 
                  fits.se, 
                  lower.bound = fits - 1.96 * fits.se, 
                  upper.bound = fits + 1.96 * fits.se, 
                  acc.lb = fits/2,
                  acc.ub = fits*2,
                  accuracy = ifelse((test.df.y$operational_days >= fits/2) & 
                                      (test.df.y$operational_days) <= fits*2, 1, 0),
                  stricter.accuracy = ifelse((test.df.y$operational_days / fits) > 0.67 &
                                               (test.df.y$operational_days / fits) < 1.33,1,0))

accuracy <- as.data.frame(accuracy)

sum(accuracy$accuracy) / dim(accuracy)[1]
sum(accuracy$stricter.accuracy) / dim(accuracy)[1]

mse(actual = test.df.y$operational_days, predicted = fits)
rmse(actual = test.df.y$operational_days, predicted = fits)
mae(actual = test.df.y$operational_days, predicted = fits)


WeibullDiag(Surv(operational_days, status)~charger_company_col,
            data=train.df)

WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=subset(train.df, !hist_of_im %in% 7:20))
WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=train.df)
WeibullDiag(Surv(operational_days, status)~subregion.id,
            data=train.df)


fit.mod.wei <- fitdistrplus::fitdist(train.df$operational_days, 'weibull')
# weib_shape <- fit.mod.wei$estimate['shape']
# weib_scale <- fit.mod.wei$estimate['scale']
summary(fit.mod.wei)

plot(fit.mod.wei)



# warm

corr1 <- cor(war.df[, cols])

ggcorrplot(corr1, 
           type = 'upper', 
           outline.color = 'white', 
           show.legend = TRUE,
           lab = TRUE) +
  ggtitle('Correlation Plot of Attributes - Warm Region') +
  theme(plot.title = element_text(hjust=0.5, size = 25))


std_var.pca <- as.data.frame(lapply(war.df[,cols],standardize)) # standardize
pcomps.war <- prcomp(std_var.pca)
summary(pcomps.war)
pcomps.explained.war <- pcomps.war$sdev^2/sum(pcomps.war$sdev^2)
qplot(1:19, pcomps.explained.war) +
  geom_line() +
  ylim(c(0,1)) +
  xlab('Principal Components') +
  ylab('Variance Explained') +
  ggtitle('Scree Plot - Warm Region') +
  theme(plot.title = element_text(hjust=0.5, size = 15))

shuffle.df <- sort(sample(nrow(war.df), nrow(war.df)*0.8))
train.df <- war.df[shuffle.df, ]
test.df <- war.df[-shuffle.df, ]

wei.im.war <- survreg(Surv(operational_days, status)~
                    tot_sessions + 
                    warmer_than_normal_days +
                    cooler_than_normal_days +
                    tot_minutes_on_warmer_days + 
                    tot_minutes_on_normal_days +
                    tot_kwh_on_cooler_days+
                    charger_company_col + 
                    hist_of_im,
                  data = train.df,
                  dist = 'weibull')
summary(wei.im.war)
ConvertWeibull(wei.im, conf.level = 0.95)

test.df.y <- test.df[, c('operational_days', 'status')]
test.df.x <- subset(test.df, select = -c(operational_days, status))
preds <- predict(wei.im, type = 'quantile',
                 p=0.5, 
                 newdata = subset(test.df.x, select = -c(subregion.id)),
                 se.fit = TRUE)

fits <- preds$fit
fits.se <- preds$se.fit

accuracy <- cbind(actual = test.df.y$operational_days, 
                  fits, 
                  fits.se, 
                  lower.bound = fits - 1.96 * fits.se, 
                  upper.bound = fits + 1.96 * fits.se, 
                  acc.lb = fits/2,
                  acc.ub = fits*2,
                  accuracy = ifelse((test.df.y$operational_days >= fits/2) & 
                                      (test.df.y$operational_days) <= fits*2, 1, 0),
                  stricter.accuracy = ifelse((test.df.y$operational_days / fits) > 0.67 &
                                               (test.df.y$operational_days / fits) < 1.33,1,0))

accuracy <- as.data.frame(accuracy)

sum(accuracy$accuracy) / dim(accuracy)[1]
sum(accuracy$stricter.accuracy) / dim(accuracy)[1]


mse(actual = test.df.y$operational_days, predicted = fits)
rmse(actual = test.df.y$operational_days, predicted = fits)
mae(actual = test.df.y$operational_days, predicted = fits)


WeibullDiag(Surv(operational_days, status)~charger_company_col,
            data=train.df)
WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=subset(train.df, !hist_of_im %in% 7:20))
WeibullDiag(Surv(operational_days, status)~hist_of_im,
            data=train.df)
WeibullDiag(Surv(operational_days, status)~subregion.id,
            data=train.df)


fit.mod.wei <- fitdistrplus::fitdist(train.df$operational_days, 'weibull')
# weib_shape <- fit.mod.wei$estimate['shape']
# weib_scale <- fit.mod.wei$estimate['scale']
summary(fit.mod.wei)

plot(fit.mod.wei)




## end 



library(eha)




pgreg.evs <- phreg(Surv(operational_days, status)~temp_class +
                     charger_company_col * hist_of_im + 
                     tot_sessions + tot_sessions_on_cooler_days + tot_sessions_on_normal_days + tot_sessions_on_warmer_days + 
                     tot_minutes + tot_minutes_on_cooler_days + tot_minutes_on_normal_days + tot_minutes_on_warmer_days + 
                     tot_kwh + tot_kwh_on_cooler_days + tot_kwh_on_normal_days + tot_kwh_on_warmer_days + 
                     tot_customers + tot_customers_on_cooler_days + tot_customers_on_normal_days + tot_customers_on_warmer_days + 
                     dist_km + 
                     cooler_than_normal_days + normal_days + warmer_than_normal_days,
                   data = subset.merged.df,
                   dist = 'weibull')
cox.reg.evs <- coxreg(Surv(operational_days, status)~temp_class +
                        charger_company_col * hist_of_im + 
                        tot_sessions + tot_sessions_on_cooler_days + tot_sessions_on_normal_days + tot_sessions_on_warmer_days + 
                        tot_minutes + tot_minutes_on_cooler_days + tot_minutes_on_normal_days + tot_minutes_on_warmer_days + 
                        tot_kwh + tot_kwh_on_cooler_days + tot_kwh_on_normal_days + tot_kwh_on_warmer_days + 
                        tot_customers + tot_customers_on_cooler_days + tot_customers_on_normal_days + tot_customers_on_warmer_days + 
                        dist_km + 
                        cooler_than_normal_days + normal_days + warmer_than_normal_days,
                      data = subset.merged.df)
check.dist(cox.reg.evs, pgreg.evs, printLegend = FALSE)

library(rms)
rms::psm

psm.evs <- psm(Surv(operational_days, status)~temp_class +
                 charger_company_col * hist_of_im + 
                 tot_sessions + tot_sessions_on_cooler_days + tot_sessions_on_normal_days + tot_sessions_on_warmer_days + 
                 tot_minutes + tot_minutes_on_cooler_days + tot_minutes_on_normal_days + tot_minutes_on_warmer_days + 
                 tot_kwh + tot_kwh_on_cooler_days + tot_kwh_on_normal_days + tot_kwh_on_warmer_days + 
                 tot_customers + tot_customers_on_cooler_days + tot_customers_on_normal_days + tot_customers_on_warmer_days + 
                 dist_km + 
                 cooler_than_normal_days + normal_days + warmer_than_normal_days,
               data = subset.merged.df)
psm.evs <- psm(Surv(operational_days, status) ~ hist_of_im + 
                 tot_minutes + tot_minutes_on_cooler_days + tot_minutes_on_normal_days + tot_minutes_on_warmer_days + 
                 cooler_than_normal_days + normal_days + warmer_than_normal_days,
               data = subset.merged.df)

anova(psm.evs)
plot(anova(psm.evs))
# margin=c('chisq', 'd.f.', 'P')

fastbw(psm.evs, rule = "aic")

ggplot(Predict(psm.evs, 
               charger_company_col='eeyin',
               temp_class='cold', 
               hist_of_im=c(1:10),
               tot_sessions=100,
               tot_minutes=500,
               tot_customers=60,
               dist_km=50,
               cooler_than_normal_days=300,
               normal_days=50,
               warmer_than_normal_days=10))

# install.packages('smoothSurv')
library(smoothSurv)






surv <- seq(.99, .01, by = -.01)
new.data <- expand.grid(temp_class=levels(merged.df$temp_class),
                        charger_company_col=levels(merged.df$charger_company_col))
new.data <- expand.grid(temp_class=levels(merged.df$temp_class),
                        charger_company_col=levels(merged.df$charger_company_col),
                        hist_of_im=levels(merged.df$hist_of_im))



predicts <- predict(wei.im, type='quantile', p=1-surv, newdata = new.data)


wei.im <- survfit(Surv(operational_days, status)~charger_company_col,
                  data=merged.df)
                  # data=subset(merged.df, hist_of_im %in% c(0:1)))


surv.wmod <- surv.wmod.wide <- cbind(new.data, predicts) %>% pivot_longer(names_to = 'surv_id', 
                                           values_to = 'time',
                                           cols = -c(1:3)) %>% 
  mutate(surv_id=as.factor(as.numeric(surv_id))) %>% 
  data.frame()

surv.wmod$surv <- surv[as.numeric(surv.wmod$surv_id)]
surv.wmod$upper <- NA
surv.wmod$lower <- NA
surv.wmod$std.error <- NA
surv.wmod$strata <- NA
surv.wmod[, c('upper', 'lower', 'std.error', 'strata')] <- NA
# Visual representation of Weibull modeling predictions on a survival probability plot
ggsurvplot(surv.wmod, 
           surv.geom=geom_line, 
           linetype = 'hist_of_im',
           color='charger_company_col')
# temp_class - when using 3 plots 



plot(wei.im, col=1:10,
     ylab='Survival Probability',
     xlab='Time (Days)')
# legend(x=275, y=1.0, col=1:10, lwd=2, legend = c('vxxip', 'eeyin'))
# legend(x=275, y=1.0, col=1:10, lwd=2, legend = c('eeyin', 'vxxip'))
# legend(x=600, y=1.0, col=1:10, lwd=2, legend = c('eeyin', 'vxxip'))
# legend(x=275, y=1.0, col=1:10, lwd=2, legend = c('four', 'five', 'six'))
# legend(x=600, y=1.0, col=1:10, lwd=2, legend = c('none', 'one', 'two', 'three'))
# legend(x=600, y=1.0, col=1:10, lwd=2, legend = c('none', 'one', 'two', 'three', 'four', 'five'))


library(survminer)

ggsurvplot(wei.im,
           conf.int = FALSE,
           risk.table = FALSE,
           linetype = 'strata',
           pval = TRUE,
           title='Kaplan Meier Survival Estimates Plot')





cols <- c('downtime_instances', 
          'total_minutes_in_maintenance_in_1000',
          'total_hours_in_maintenance', 'total_days_in_maintenance',
          'single_im_days', 'total_sessions', 'total_minutes',
          'total_MWh', 'total_customers', 'total_unique_customers_per_1000s')

# all.df <- rbind(im_df, co_df, wa_df)




################################################################################
################### K-Means Clustering 
###################

################### Moderate Region

exclude_mod_stations <- c(593, 6816, 40843)
location_activity <- subset(im_df, !(station_id %in% exclude_mod_stations)) %>% 
  group_by(station_id, x_axis_unique, y_axis_unique) %>% 
  summarize(downtime_instances = n_distinct(mtbf_flag[mtbf_flag!=0]),
            days_since_operation = n_distinct(flowdate) / 731,
            total_minutes_in_maintenance_in_1000 = sum(minutes, na.rm = TRUE)/1000,
            total_hours_in_maintenance = sum(minutes_diff, na.rm = TRUE)/60,
            total_days_in_maintenance = sum(minutes_diff, na.rm = TRUE)/1440,
            single_im_days = n_distinct(single_im_day, na.rm = TRUE),
            total_sessions = sum(sessions, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            total_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            total_customers = sum(unique_customers, na.rm = TRUE),
            total_unique_customers_per_1000s = sum(unique_customers, na.rm = TRUE)/1000)

std <- as.data.frame(lapply(location_activity[,cols], standardize))
keep.marked.stations <- location_activity[, c(1:3)]
  # subset(im_df, !(station_id %in% exclude_mod_stations))$station_id
# keep.marked.stations <- unique(keep.marked.stations)
std <- cbind(keep.marked.stations, std)



km.res <- kmeans(std$total_MWh,
                 centers = 2)
fviz_cluster(km.res, data=location_activity)
fviz_nbclust(std, kmeans, method = 'wss')
plot(location_activity$x_axis_unique, location_activity$y_axis_unique, col=km.res$cluster)
  
g <- ggplot(data = std, 
            aes(x=x_axis_unique, y=y_axis_unique,
                # fill=days_since_operation,
                # col=total_minutes,
                col=factor(km.res$cluster),
                size=downtime_instances))
g + geom_point(aes(fill=factor(km.res$cluster)), color='black', pch=21)



################### Cold Region

exclude_mod_stations <- c(386, 505, 678)
location_activity <- subset(co_df, !(station_id %in% exclude_mod_stations)) %>% 
  group_by(station_id, x_axis, y_axis) %>% 
  summarize(downtime_instances = n_distinct(mtbf_flag[mtbf_flag!=0]),
            days_since_operation = n_distinct(flowdate) / 731,
            total_minutes_in_maintenance_in_1000 = sum(minutes, na.rm = TRUE)/1000,
            total_hours_in_maintenance = sum(minutes_diff, na.rm = TRUE)/60,
            total_days_in_maintenance = sum(minutes_diff, na.rm = TRUE)/1440,
            single_im_days = n_distinct(single_im_day, na.rm = TRUE),
            total_sessions = sum(sessions, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            total_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            total_customers = sum(unique_customers, na.rm = TRUE),
            total_unique_customers_per_1000s = sum(unique_customers, na.rm = TRUE)/1000,
            days_with_less_than_1quartile_temps = n_distinct(flowdate[temps<=36.1]),
            days_between_1quartile_and_median_temps = n_distinct(flowdate[temps>36.1 & temps<=50.2]),
            days_between_median_and_3quartile_temps = n_distinct(flowdate[temps>50.2 & temps<66.7]),
            days_higher_3quartile_temps = n_distinct(flowdate[temps>=66.7]))


std <- as.data.frame(lapply(location_activity[,cols], standardize))
keep.marked.stations <- subset(co_df, !(station_id %in% exclude_mod_stations))$station_id
keep.marked.stations <- unique(keep.marked.stations)
std <- cbind(keep.marked.stations, std)



km.res <- kmeans(std$total_customers,
                 centers = 3)
fviz_cluster(km.res, data=location_activity)
fviz_nbclust(std,
             kmeans, method = 'wss')
plot(location_activity$x_axis, 
     location_activity$y_axis, 
     col=km.res$cluster)


g <- ggplot(data = location_activity, 
            aes(x=x_axis, y=y_axis,
                # fill=downtime_instances,
                col=factor(km.res$cluster),
                # col=total_sessions,
                size=total_MWh))
g + geom_point()

km.res <- kmeans(location_activity$total_customers,
                 centers = 3)
fviz_cluster(km.res, data=location_activity)
plot(location_activity$x_axis, location_activity$y_axis,
     col=km.res$cluster)





################### Warm Region

location_activity <- wa_df %>% 
  group_by(station_id, x_axis, y_axis) %>% 
  summarize(downtime_instances = n_distinct(mtbf_flag[mtbf_flag!=0]),
            days_since_operation = n_distinct(flowdate) / 731,
            total_minutes_in_maintenance_in_1000 = sum(minutes, na.rm = TRUE)/1000,
            total_hours_in_maintenance = sum(minutes_diff, na.rm = TRUE)/60,
            total_days_in_maintenance = sum(minutes_diff, na.rm = TRUE)/1440,
            single_im_days = n_distinct(single_im_day, na.rm = TRUE),
            total_sessions = sum(sessions, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            total_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            total_customers = sum(unique_customers, na.rm = TRUE),
            total_unique_customers_per_1000s = sum(unique_customers, na.rm = TRUE)/1000,
            days_with_less_than_1quartile_temps = n_distinct(flowdate[temps<=63.5]),
            days_between_1quartile_and_median_temps = n_distinct(flowdate[temps>63.5 & temps<=74.9]),
            days_between_median_and_3quartile_temps = n_distinct(flowdate[temps>74.9 & temps<81.7]),
            days_higher_3quartile_temps = n_distinct(flowdate[temps>=81.7]))


std <- as.data.frame(lapply(location_activity[,cols], standardize))
keep.marked.stations <- unique(wa_df$station_id)
std <- cbind(keep.marked.stations, std)



km.res <- kmeans(std$total_customers,
                 centers = 3)
fviz_cluster(km.res, data=location_activity)
fviz_nbclust(std,
             kmeans, method = 'wss')
plot(location_activity$x_axis, 
     location_activity$y_axis, 
     col=km.res$cluster)
 


g <- ggplot(data = location_activity, 
            aes(x=x_axis, y=y_axis,
                col=factor(km.res$cluster),
                # fill=total_sessions,
                # col=total_sessions,
                size=total_sessions))
g + geom_point()

km.res <- kmeans(location_activity$total_customers,
                 centers = 4)
fviz_cluster(km.res, data=location_activity)
fviz_nbclust(location_activity,
             kmeans, method = 'wss')
plot(location_activity$x_axis, location_activity$y_axis, 
     col=km.res$cluster)




################### All Combined Region

### TO PROPERLY ASSESS, NEED TO SCALE ON INTO SINGLE COORDINATE AXIS

exclude_some_moderates <- c(593, 6816, 40843)
exclude_some_colds <- c(386, 505, 678)

m <- subset(im_df, !(station_id %in% exclude_some_moderates))
c <- subset(co_df, !(station_id %in% exclude_some_colds))
w <- wa_df
all.df <- rbind(m,c,w)



# Combine all regions into a single axis

x.origin <- min(all.df$station_longitude) + 
  min(all.df$station_longitude) * 0.1

y.origin <- min(all.df$station_latitude) + 
  min(all.df$station_latitude) * 0.1 # lower point (closer to 0)

y <- all.df$station_latitude - y.origin
x <- all.df$station_longitude - x.origin

df <- cbind(x, y)
df <- as.data.frame(df)
min.y <- min(df$y)
min.x <- min(df$x)
all.df$x_axis_unified <- df$x - min.x
all.df$y_axis_unified <- df$y - min.y

plot(all.df$x_axis_unified, 
     all.df$y_axis_unified)


location_activity <- all.df %>%
  group_by(station_id, x_axis_unified, y_axis_unified) %>%
  summarize(downtime_instances = n_distinct(mtbf_flag[mtbf_flag!=0]),
            days_since_operation = n_distinct(flowdate) / 731,
            total_minutes_in_maintenance_in_1000 = sum(minutes, na.rm = TRUE)/1000,
            total_hours_in_maintenance = sum(minutes_diff, na.rm = TRUE)/60,
            total_days_in_maintenance = sum(minutes_diff, na.rm = TRUE)/1440,
            single_im_days = n_distinct(single_im_day, na.rm = TRUE),
            total_sessions = sum(sessions, na.rm = TRUE),
            total_minutes = sum(minutes, na.rm = TRUE),
            total_MWh = sum(session_kwh, na.rm = TRUE)/1000,
            total_customers = sum(unique_customers, na.rm = TRUE),
            total_unique_customers_per_1000s = sum(unique_customers, na.rm = TRUE)/1000)

m <- unique(subset(im_df, !(station_id %in% exclude_some_moderates))$station_id)
c <- unique(subset(co_df, !(station_id %in% exclude_some_colds))$station_id)
w <- unique(wa_df$station_id)

cols <- c('x_axis_unified', 'y_axis_unified', 'downtime_instances', 
          'total_minutes_in_maintenance_in_1000',
          'total_hours_in_maintenance', 'total_days_in_maintenance',
          'single_im_days', 'total_sessions', 'total_minutes',
          'total_MWh', 'total_customers', 'total_unique_customers_per_1000s')

std <- as.data.frame(lapply(location_activity[,cols], standardize))
all.stations <- c(m,c,w)
std <- cbind(all.stations, std)

km.res <- kmeans(std$total_customers,
                 centers = 3)
fviz_cluster(km.res, data=location_activity)
fviz_nbclust(std,
             kmeans, method = 'wss')
# plot(location_activity$x_axis_unified,
#      location_activity$y_axis_unified,
#      col=factor(km.res$cluster))


g <- ggplot(data = location_activity, 
            aes(x=x_axis_unified, y=y_axis_unified,
                fill=downtime_instances,
                col=factor(downtime_instances),
                size=downtime_instances))
g + geom_point()





norm <- as.data.frame(lapply(location_activity[,cols],normalize))
all.stations <- c(m,c,w)
norm <- cbind(all.stations, norm)

km.res <- kmeans(norm[, c(4:9)],centers = 3)
fviz_cluster(km.res, data=norm)
fviz_nbclust(norm,
             kmeans, method = 'wss')
plot(location_activity$x_axis, location_activity$y_axis, 
     col=km.res$cluster)



data_spl <- sample(1:nrow(norm),size=nrow(norm)*0.7,replace = FALSE) 

train2 <- norm[data_spl,] # 70% training data
test2 <- norm[-data_spl,] # remaining 30% test data


train_labels <- norm[data_spl,5]
test_labels <-norm[-data_spl,5]
knn_pred <- knn(train=train2, test=test2, cl=train_labels, k=1)

