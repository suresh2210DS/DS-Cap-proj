getwd()
  compa <- function(x) {
  ifelse(x == 'p' or , 'Smartphone',
         ifelse(x == 'v', 'TV',
                ifelse(x == 'x', 'Laptop',
                       ifelse(x == 'q', 'Tablet', NA))))
}

  fetch  <- function(x) {
        if (x = "p") {
          "Smartphone"
        } else if (x = "v") {
          "TV"
        } else if (x = "x") {
          "Laptop"
        } else if (x= "q") {
          "Tablet"
        }   
    
  }
comp1 <- test1$company

for (i in seq(length(comp1)))
{
  tolower(comp1[i])
  if (comp1[i] %like any% c("%li%", "p%", "f%", "%ps")) {
    comp1[i] <- "philips"
  } else if (comp1[i] %like% "ak%") {
    comp1[i] <- "akzo"
   } else if (comp1[i] %like% "van%") {
      comp1[i] <- "van houten"  
  } else if (comp1[i] %like% "un%") {
    comp1[i] <- "unilever"
  }
}


---------------
  
getwd()
setwd("E:/New York Taxi")

memory.limit(size = 56000)

if (exists(".GeocodedInformation")) {
  rm(.GeocodedInformation)
}

taxi <- read_csv("train.csv")
fast1 <- read_csv("fastest_train_part_1.csv")
fast2 <- read_csv("fastest_train_part_2.csv")
fast1 <- data.frame(fast1)
fast2 <- data.frame(fast2)
taxi <- data.frame(taxi)
glimpse(taxi)
taxi1 <- sample_n(taxi, 400000)
count(taxi1)

my_map <- get_map(location = "New York City", zoom = 12, maptype = "roadmap", source = "google", color = "color")
set.seed(1234)
tax_samp <- sample_n(taxi1, 5000)
ggmap(my_map) +
  geom_point(data = tax_samp, aes(x = pickup_longitude, y = pickup_latitude), size = 0.3, alpha = 0.3, color = "blue") +
  geom_point(data = tax_samp, aes(x = dropoff_longitude, y = dropoff_latitude), size = 0.3, alpha = 0.3, color = "red") +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

fast_route <- bind_rows(fast1, fast2)
  
 osrm <- fast_route %>%
  select(
    id, total_distance, total_travel_time, number_of_steps,
    step_direction, step_maneuvers
  ) %>%
  mutate(
    fastest_speed = total_distance / total_travel_time * 2.236,
    left_turns = str_count(step_direction, "left"),
    right_turns = str_count(step_direction, "right"),
    turns = str_count(step_maneuvers, "turn")
  ) %>%
  select(-step_direction, -step_maneuvers)

   
  jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)


pick_coord <- taxi1 %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- taxi1 %>%
  select(dropoff_longitude, dropoff_latitude)
taxi1$dist <- distCosine(pick_coord, drop_coord)


taxi1$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
taxi1$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
taxi1$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
taxi1$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

taxi1 <- taxi1 %>%
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    dropoff_datetime = ymd_hms(dropoff_datetime),
    date = date(pickup_datetime)
  )
# join OSRM data with our data.
taxi1 <- left_join(taxi1, osrm, by = "id")



taxi1 <- taxi1 %>%
  mutate(
    store_and_fwd_flag = as.integer(factor(store_and_fwd_flag)),
    vendor_id = as.integer(vendor_id),
    month = as.integer(month(pickup_datetime)),
    wday = wday(pickup_datetime, label = TRUE),
    wday = as.integer(fct_relevel(wday, c("Sun", "Sat", "Mon", "Tues", "Wed", "Thurs", "Fri"))),
    hour = hour(pickup_datetime),
    work = as.integer((hour %in% seq(8, 18)) & (wday %in% c("Mon", "Tues", "Fri", "Wed", "Thurs"))),
    jfk_trip = as.integer((jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3)),
    lg_trip = as.integer((lg_dist_pick < 2e3) | (lg_dist_drop < 2e3))
  )


# predictor variables
train_cols <- c(
  "total_travel_time", "total_distance", "hour", "dist",
  "vendor_id", "jfk_trip", "lg_trip", "wday", "month",
  "pickup_longitude", "pickup_latitude", "lg_dist_drop", "jfk_dist_drop"
)
# target variable
y_col <- c("trip_duration")
# id feature
id_col <- c("id")
# cleaning variable
clean_cols <- c("jfk_dist_drop", "jfk_dist_pick")

cols <- c(train_cols, y_col, clean_cols)
final <- taxi1 %>%
  select_(.dots = cols)

###-----------------------------

final <- final %>%
  mutate(trip_duration = log(trip_duration + 1))  

set.seed(4321)
trainIndex <- createDataPartition(final$trip_duration, p = 0.7, list = FALSE, times = 1)

train<- final[trainIndex, ]
valid <- final[-trainIndex, ]

trainIndex <- createDataPartition(valid$trip_duration, p = 0.5, list = FALSE, times = 1)
valid <- valid[trainIndex, ]
test <- valid[-trainIndex, ]

glimpse(test)
count(test)
#### ------------------------

combine <- bind_rows(train %>% mutate(dset = "train"), 
                     test %>% mutate(dset = "test",
                                     dropoff_datetime = NA,
                                     trip_duration = NA))
combine <- combine %>% mutate(dset = factor(dset))

pick_good <- combine %>%
  filter(pickup_longitude > -75 & pickup_longitude < -73) %>%
  filter(pickup_latitude > 40 & pickup_latitude < 42)
pick_good <- sample_n(pick_good, 5000)

pick_good %>%
  ggplot(aes(pickup_longitude, pickup_latitude, color = dset)) +
  geom_point(size=0.1, alpha = 0.5) +
  coord_cartesian(xlim = c(-74.02,-73.77), ylim = c(40.63,40.84)) +
  facet_wrap(~ dset) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
  theme(legend.position = "none")

valid <- valid %>%
  select_(.dots = str_c("-", c(clean_cols)))

train <- train %>%
  filter(
    trip_duration < 24 * 3600,
    jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5
  ) %>%
  select_(.dots = str_c("-", c(clean_cols)))

temptrn <- train %>% select(-trip_duration)
tempval <- valid %>% select(-trip_duration)

dtrain <- xgb.DMatrix(as.matrix(temptrn), label = train$trip_duration)
dvalid <- xgb.DMatrix(as.matrix(tempval), label = valid$trip_duration)
dtest <- xgb.DMatrix(as.matrix(test))

xgb_params <- list(
  colsample_bytree = 0.7, # variables per tree
  subsample = 0.7, # data subset per tree
  booster = "gbtree",
  max_depth = 5, # tree levels
  eta = 0.3, # shrinkage
  eval_metric = "rmse",
  objective = "reg:linear",
  seed = 4321
)
watchlist <- list(train = dtrain, valid = dvalid)
set.seed(4321)
gb_dt <- xgb.train(
  params = xgb_params,
  data = dtrain,
  print_every_n = 10,
  watchlist = watchlist,
  nrounds = 100
)
xgb_cv <- xgb.cv(xgb_params, dtrain, early_stopping_rounds = 10, nfold = 5, nrounds = 100)
test_preds <- predict(gb_dt, dtest)
pred <- test_id %>%
  mutate(trip_duration = exp(test_preds) - 1)
head(test_preds)

trntemp <- train %>%
    mutate(
    dset = "train",
    trip_pred = exp(trip_duration) - 1
  )
predtmp <- pred %>%
  mutate(dset = "predict")

bind_rows(trntemp, predtmp) %>%
  ggplot(aes(trip_pred, fill = dset)) +
  geom_density(alpha = 0.5) +
  scale_x_log10()

ptm <- proc.time()
library(randomForest)
rf_md2  <- randomForest(trip_duration ~ total_travel_time + total_distance + hour + dist +
                        vendor_id + jfk_trip + lg_trip + wday +  month +
                        pickup_longitude + pickup_latitude + lg_dist_drop,
                        data = train, 
                        na.action = na.omit,
                        importance = TRUE,
                        ntree = 40,
                        do.trace=TRUE)

print(rf_benchmark)


rf_cv1 <- rf.crossValidation(rf_md2,train,p = 0.1, n = 2, seed = NULL, plot = TRUE)


proc.time() - ptm
sum(is.na(train))


#   nb_trees <- 10
#   nb_cores <- 3
#   cl <- makeCluster(nb_cores)
# registerDoParallel(cl)
# ptm <- proc.time()
# rf_md1 <- foreach(ntree = rep(nb_trees, nb_cores), .combine = combine, .packages = "randomForest") %dopar% {randomForest(trip_duration ~ total_travel_time + total_distance + hour + dist +
#                                    vendor_id + jfk_trip + lg_trip + wday +  month +
#                                    pickup_longitude + pickup_latitude + lg_dist_drop,
#                                  data = train, na.action = na.omit,
#                                  ntree = ntree,do.trace=TRUE)}
# proc.time() - ptm
# stopCluster(cl)