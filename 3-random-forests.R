return_model_data <- returns %>%
  select(-unique_id) %>%
  left_join(kick_team_distance, by = c("gameId", "playId")) %>%
  left_join(pff_one_hot, by = c("gameId", "playId")) %>%
  left_join(returner_data, by = c("gameId", "playId")) %>%
  select(gameId, playId, playDescription, yardline_100, net_return, kickLength,
         starts_with("distance"), hangTime, starts_with("kickType"), 
         starts_with("kickDir"), starts_with("num"), starts_with("returner")) %>%
  filter(!is.na(returner_o), !is.na(kickType_A), 
         !is.na(kickDirectionActual_L), !is.na(yardline_100)) %>%
  mutate(yds_from_endzone = yardline_100 - kickLength)

colSums(is.na(return_model_data))

return_train <- sample_n(return_model_data, nrow(return_model_data)/2)

return_rf <- ranger(net_return ~ yardline_100 + distance_1 +
                      distance_2 + distance_3 + distance_4 + distance_5 + distance_6 + distance_7 +
                      distance_8 + distance_9 + distance_10 + distance_11 + hangTime + kickType_A + 
                      kickType_N + kickType_R + kickDirectionActual_C + kickDirectionActual_L +
                      kickDirectionActual_R + num_gunners + num_rushers + num_vises + returner_x +
                      returner_y + returner_s + returner_o + yds_from_endzone,
                    num.trees = 50, importance = "impurity", data = return_train)

vip(return_rf, num_features = 25) + theme_reach()

punt_lands <- df_tracking %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(event == "punt_land")

punt_land_ids <- unique(punt_lands$unique_id)

punt_land_plays <- punt_plays %>%
  filter(unique_id %in% punt_land_ids) %>%
  filter(is.na(net_return)) %>%
  mutate(is_touchback = ifelse(grepl("to end zone", playDescription), 1, 0)) %>%
  left_join(pff_one_hot, by = c("gameId", "playId")) %>%
  mutate(yds_from_endzone = yardline_100 - kickLength)

tracking_lands <- df_tracking %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(unique_id %in% punt_land_ids)

landed_arrival <- tracking_lands %>%
  filter(event == "punt_land") 

football_coords <- landed_arrival %>%
  filter(displayName == "football") %>%
  select(gameId, playId, football_x = x, football_y = y, football_s = s)

punt_land_plays <- punt_land_plays %>%
  left_join(football_coords, by = c("gameId", "playId"))

touchback_model_data <- punt_land_plays %>%
  select(-kickerId, -kickReturnYardage, -returnerId, -unique_id, -net_return) %>%
  filter(!is.na(yardline_100), !is.na(kickType_N), !is.na(kickDirectionActual_L))

trsf <- touchback_model_data %>%
  select(label = is_touchback, yardline_100, hangTime, starts_with("kick"),
         starts_with("num"), starts_with("football"))

colSums(is.na(trsf))

trsf_train <- sample_n(trsf, nrow(trsf)/2)

touchback_model <-  ranger(label ~ .,
                           num.trees = 50, importance = "impurity", data = trsf_train)



vip(touchback_model, num_features = 20) + theme_reach()

touchback_preds <- as.data.frame(
  matrix(predict(touchback_model, as.matrix(trsf %>% select(-label))))) %>%
  dplyr::rename(exp_touchback = V1)

touchback_projs <- cbind(touchback_model_data, touchback_preds)

bounce_model_data <- punt_land_plays %>%
  select(-kickerId, -kickReturnYardage, -returnerId, -unique_id, -net_return) %>%
  filter(is_touchback == 0) %>%
  mutate(final_yardline = yardline_100 - playResult) %>%
  filter(!is.na(yardline_100), !is.na(kickType_N), !is.na(kickDirectionActual_L))

colSums(is.na(bounce_model_data))

trsf_bounce <- bounce_model_data %>%
  select(label = final_yardline, yardline_100, hangTime, starts_with("kick"),
         starts_with("num"), starts_with("football")) %>%
  select(-kickLength)

smp_size <- floor(0.50 * nrow(trsf_bounce))
set.seed(2014) #go lions
ind <- sample(seq_len(nrow(trsf_bounce)), size = smp_size)
train <- as.matrix(trsf_bounce[ind, ])
test <- as.matrix(trsf_bounce[-ind, ])

dim(train)
colnames(train)

bounce_model <- ranger(label ~ ., data = train,
                       num.trees = 50, importance = "impurity")

vip(bounce_model) + theme_reach()

bounce_preds <- data.frame(predict(bounce_model, data.frame(trsf))$predictions) %>%
  rename(exp_bounce_yardline = predict.bounce_model..data.frame.trsf...predictions)

no_return_projs <- cbind(touchback_projs, bounce_preds)

no_return_projs <- no_return_projs %>%
  mutate(no_return_exp = exp_touchback*20 + (1-exp_touchback)*exp_bounce_yardline)