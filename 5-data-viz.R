rates <- all %>%
  mutate(total = n()) %>%
  group_by(optimal_decision, actual_decision) %>% 
  summarize(rate = round(100*n() / total, 1)) %>%
  distinct()

rates %>%
  ggplot(aes(x = actual_decision, y = optimal_decision)) +
  geom_tile(aes(fill = rate)) +
  geom_text(aes(label = paste0(rate, "%"), color = ifelse(rate > 20, "black", "white")), size = 7) +
  scale_color_identity() +
  scale_fill_viridis_c() +
  theme_reach() +
  labs(x = "Actual Decision",
       y = "Optimal Decision",
       title = "Confusion Matrix on Punt Return Decision Making",
       subtitle = "Returnable punts from 2018 to 2020 included")
ggsave('matrix.png', width = 14, height = 10, dpi = "retina")

player_ids <- players %>%
  select(player = displayName, nflId)

returner_stats <- all %>%
  group_by(returnerId) %>%
  summarize(returns = n(),
            optimal_rate = mean(is_optimal)) %>%
  filter(returns >= 25) %>%
  left_join(player_ids, by = c("returnerId" = "nflId")) %>%
  select(player, returns, optimal_rate)
