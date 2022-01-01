returners <- tracking_punts %>%
  filter(event == "ball_snap") %>%
  arrange(-x) %>%
  group_by(gameId, playId) %>%
  mutate(rank = row_number()) %>%
  filter(rank == 1) %>%
  select(gameId, playId, ret_id = nflId)

tracking_punts <- tracking_punts %>%
  left_join(returners, by = c("gameId", "playId"))

tracking_punts <- tracking_punts %>%
  mutate(returnerId = ifelse(is.na(returnerId), ret_id, returnerId)) %>%
  select(-ret_id)

no_returner_ids <- tracking_punts %>%
  filter(is.na(returnerId)) %>%
  select(gameId, playId) %>%
  distinct() %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  pull()

punt_arrival <- tracking_punts %>%
  filter(!unique_id %in% no_returner_ids) %>%
  filter(event %in% c("punt_received", "punt_land", "fair_catch")) %>%
  mutate(is_retuner = ifelse(nflId == returnerId, 1, 0))

all_play_returners <- punt_arrival %>%
  filter(is_retuner == 1) %>%
  select(gameId, playId, real_id = returnerId) %>%
  distinct()

returner_x_y <- punt_arrival %>%
  filter(is_retuner == 1) %>%
  select(gameId, playId, returner_x = x, returner_y = y, returner_team = team)

punt_arrival <- punt_arrival %>%
  left_join(returner_x_y, by = c("gameId", "playId")) 

punt_arrival <- punt_arrival %>%
  mutate(dist_from_returner = sqrt((x - returner_x)^2 + (y - returner_y)^2),
         returner_team = ifelse(team != returner_team, 0, 1))

punt_team_distance <- punt_arrival %>%
  filter(displayName != "football") %>%
  filter(returner_team == 0) %>%
  select(gameId, playId, dist_from_returner) %>%
  ungroup() %>%
  arrange(dist_from_returner) %>%
  group_by(gameId, playId) %>%
  mutate(rank = paste0("distance_", row_number())) %>%
  arrange(gameId, playId) %>%
  pivot_wider(names_from = rank, values_from = dist_from_returner) %>%
  select(gameId:distance_11)

punt_data <- punt_arrival %>%
  filter(is_retuner == 1) %>%
  select(gameId, playId, returner_x = x, returner_y = y, returner_s = s,
         returner_o = o)

exp_return_data <- punt_plays %>%
  select(-unique_id) %>%
  left_join(punt_team_distance, by = c("gameId", "playId")) %>%
  left_join(pff_one_hot, by = c("gameId", "playId")) %>%
  left_join(punt_data, by = c("gameId", "playId")) %>%
  select(gameId, playId, playDescription, yardline_100, net_return, kickLength,
         starts_with("distance"), hangTime, starts_with("kickType"), specialTeamsResult,
         starts_with("kickDir"), starts_with("num"), starts_with("returner")) %>%
  filter(!is.na(returner_o), !is.na(kickType_A), 
         !is.na(kickDirectionActual_L), !is.na(yardline_100)) %>%
  mutate(yds_from_endzone = yardline_100 - kickLength) %>%
  distinct(gameId, playId, .keep_all = T) %>%
  left_join(all_play_returners, by = c("gameId", "playId")) %>%
  mutate(returnerId = ifelse(is.na(returnerId), real_id, returnerId)) %>%
  select(-real_id)

rf_preds <- data.frame(predict(return_rf, data.frame(exp_return_data))$predictions) %>%
  rename(exp_yards = predict.return_rf..data.frame.exp_return_data...predictions)

exp_return_projs <- cbind(exp_return_data, rf_preds)

all <- exp_return_projs %>%
  mutate(actual_decision = case_when(
    specialTeamsResult == "Downed" ~ "Let it Go",
    specialTeamsResult == "Fair Catch" ~ "Fair Caught",
    specialTeamsResult == "Muffed" ~ "Returned",
    specialTeamsResult == "Out of Bounds" ~ "Let it Go",
    specialTeamsResult == "Return" ~ "Returned",
    specialTeamsResult == "Touchback" ~ "Let it Go",
  )) %>%
  select(gameId, playId, playDescription, returnerId, yardline_100, net_return,
         kickLength, exp_yards, actual_decision) %>%
  mutate(return_yardline = yardline_100 - kickLength + exp_yards - 0.027*(yardline_100 - kickLength))

no_returns <- punt_plays %>%
  mutate(is_touchback = ifelse(grepl("to end zone", playDescription), 1, 0)) %>%
  left_join(pff_one_hot, by = c("gameId", "playId")) %>%
  mutate(yds_from_endzone = yardline_100 - kickLength)

no_return_tracking <- df_tracking %>%
  mutate(unique_id = paste0(gameId, playId))

no_return_ids <- no_returns %>% mutate(unique_id = paste0(gameId, playId)) %>% pull(unique_id)

football_x_y <- no_return_tracking %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(displayName == "football", unique_id %in% no_return_ids) %>%
  select(gameId, playId, event, football_x = x, football_y = y, football_s = s) 

football_x_y <- football_x_y %>%
  group_by(gameId, playId) %>%
  mutate(frame = row_number())

no_none <- football_x_y %>%
  filter(event != "None") %>%
  group_by(gameId, playId) %>%
  mutate(event_before = lag(event)) %>%
  filter(event_before == "punt") %>%
  mutate(filter_frame = frame - 1) %>%
  select(gameId, playId, filter_frame)

football_x_y <- football_x_y %>%
  left_join(no_none, by = c("gameId", "playId")) %>%
  filter(frame == filter_frame)

no_returns <- no_returns %>%
  left_join(football_x_y, by = c("gameId", "playId"))

touchback_data <- no_returns %>%
  select(-kickerId, -kickReturnYardage, -returnerId, 
         -unique_id, -net_return, -filter_frame) %>%
  filter(!is.na(yardline_100), !is.na(kickType_N), 
         !is.na(kickDirectionActual_L), !is.na(football_x)) 

trsf_touchback <- touchback_data %>%
  select(label = is_touchback, yardline_100, hangTime, starts_with("kick"),
         starts_with("num"), starts_with("football"))

colSums(is.na(trsf_touchback))

exp_touchback_preds <- data.frame(predict(touchback_model, data.frame(trsf_touchback))$predictions) %>%
  rename(exp_touchback = predict.touchback_model..data.frame.trsf_touchback...predictions)

exp_touchback_projs <- cbind(touchback_data, exp_touchback_preds)

bounce_data <- no_returns %>%
  select(-kickerId, -kickReturnYardage, -returnerId, -unique_id, -net_return) %>%
  mutate(final_yardline = yardline_100 - playResult) %>%
  filter(!is.na(yardline_100), !is.na(kickType_N), 
         !is.na(kickDirectionActual_L), !is.na(football_x))

exp_bounce_preds <- data.frame(predict(bounce_model, data.frame(bounce_data))$predictions) %>%
  rename(exp_bounce_yardline = predict.bounce_model..data.frame.bounce_data...predictions)

letitgo_projs <- cbind(exp_touchback_projs, exp_bounce_preds)

let_it_go <- letitgo_projs %>%
  mutate(no_return_exp = exp_touchback*20 + (1-exp_touchback)*exp_bounce_yardline,
         fair_caught_exp = 110 - football_x) %>%
  select(gameId, playId, no_return_exp, fair_caught_exp)

all <- all %>%
  left_join(let_it_go, by = c("gameId", "playId"))

all <- all %>%
  filter(!is.na(no_return_exp)) %>%
  mutate(optimal_decision = case_when(
    return_yardline > no_return_exp & return_yardline > fair_caught_exp ~ "Returned",
    no_return_exp > return_yardline & no_return_exp > fair_caught_exp ~ "Let it Go",
    fair_caught_exp > return_yardline & fair_caught_exp > no_return_exp ~ "Fair Caught",
  ),
  is_optimal = ifelse(actual_decision == optimal_decision, 1, 0))