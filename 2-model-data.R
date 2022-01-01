punt_plays <- plays %>% 
  filter(specialTeamsPlayType == "Punt") %>%
  mutate(yardline_100 = ifelse(possessionTeam == yardlineSide, 100 - yardlineNumber, yardlineNumber)) %>%
  select(gameId, playId, playDescription, quarter, down, yardsToGo, 
         yardline_100, penaltyYards, kickReturnYardage, playResult, 
         kickerId, returnerId, kickLength, specialTeamsResult) %>%
  mutate(penaltyYards = ifelse((is.na(penaltyYards) & !is.na(kickReturnYardage)), 0, penaltyYards)) %>%
  mutate(net_return = ifelse(is.na(kickReturnYardage), NA, kickReturnYardage - penaltyYards)) %>%
  mutate(unique_id = paste0(gameId, playId))

punt_ids <- punt_plays %>% pull(unique_id)

returns <- punt_plays %>% 
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(!is.na(net_return))

return_ids <- returns %>% pull(unique_id)

pff_select <- pff_scouting_data %>%
  select(gameId, playId, hangTime, kickType, kickDirectionActual,
         gunners, puntRushers, vises) %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(unique_id %in% punt_ids)

pff_select$num_gunners <- str_count(pff_select$gunners, ";") + 1
pff_select$num_rushers <- str_count(pff_select$puntRushers, ";") + 1
pff_select$num_vises <-  str_count(pff_select$vises, ";") + 1

pff_needed <- pff_select %>%
  select(gameId, playId, hangTime, kickType, kickDirectionActual,
         num_gunners, num_rushers, num_vises) %>% 
  mutate(num_gunners = ifelse(is.na(num_gunners), 0, num_gunners),
         num_rushers = ifelse(is.na(num_rushers), 0, num_rushers),
         num_vises = ifelse(is.na(num_vises), 0, num_vises)) %>%
  filter(!is.na(hangTime)) 

pff_needed$kickType <- as.factor(pff_needed$kickType)
pff_needed$kickDirectionActual <- as.factor(pff_needed$kickDirectionActual)

pff_one_hot <- one_hot(as.data.table(pff_needed))

tracking_punts <- df_tracking %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(unique_id %in% punt_ids)

tracking_punts <- tracking_punts %>%
  select(-unique_id) %>%
  left_join(punt_plays, by = c("gameId", "playId"))

tracking_returns <- df_tracking %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(unique_id %in% return_ids)

tracking_returns <- tracking_returns %>%
  select(-unique_id) %>%
  left_join(returns, by = c("gameId", "playId"))

return_arrival <- tracking_returns %>%
  filter(event == "punt_received") %>%
  mutate(is_returner = ifelse(nflId == returnerId, 1, 0))

returner_coords <- return_arrival %>%
  filter(is_returner == 1) %>%
  select(gameId, playId, returner_x = x, returner_y = y, returner_team = team)

return_arrival <- return_arrival %>%
  left_join(returner_coords, by = c("gameId", "playId")) 

return_arrival <- return_arrival %>%
  mutate(dist_from_returner = sqrt((x - returner_x)^2 + (y - returner_y)^2),
         returner_team = ifelse(team != returner_team, 0, 1))

kick_team_distance <- return_arrival %>%
  filter(displayName != "football") %>%
  filter(returner_team == 0) %>%
  select(gameId, playId, dist_from_returner) %>%
  ungroup() %>%
  arrange(dist_from_returner) %>%
  group_by(gameId, playId) %>%
  mutate(rank = paste0("distance_", row_number())) %>%
  arrange(gameId, playId) %>%
  pivot_wider(names_from = rank, values_from = dist_from_returner)

returner_data <- return_arrival %>%
  filter(is_returner == 1) %>%
  select(gameId, playId, returner_x = x, returner_y = y, returner_s = s,
         returner_o = o)