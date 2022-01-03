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

player_ids <- players %>%
  select(player = displayName, nflId)

optimal_return <- all %>%
  filter(optimal_decision == "Returned") %>%
  mutate(actual_return = ifelse(actual_decision == "Returned", 1, 0)) %>%
  group_by(returnerId) %>%
  summarize(optimal_return_rate = round(100*mean(actual_return), 1))

optimal_fair_catch <- all %>%
  filter(optimal_decision == "Fair Caught") %>%
  mutate(actual_fc= ifelse(actual_decision == "Fair Caught", 1, 0)) %>%
  group_by(returnerId) %>%
  summarize(optimal_fc =  round(100*mean(actual_fc), 1))

optimal_bounce <- all %>%
  filter(optimal_decision == "Let it Go") %>%
  mutate(actual_bounce = ifelse(actual_decision == "Let it Go", 1, 0)) %>%
  group_by(returnerId) %>%
  summarize(optimal_bounce = round(100*mean(actual_bounce), 1))


returner_stats <- all %>%
  group_by(returnerId) %>%
  summarize(returns = n(),
            optimal_rate = round(100*mean(is_optimal), 1)) %>%
  filter(returns >= 25) %>%
  left_join(player_ids, by = c("returnerId" = "nflId")) %>%
  select(player, returns, returnerId, optimal_rate) %>%
  arrange(-optimal_rate) %>%
  ungroup() %>%
  mutate(rank = row_number()) %>%
  left_join(optimal_return, by = "returnerId") %>%
  left_join(optimal_fair_catch, by = "returnerId") %>%
  left_join(optimal_bounce, by = "returnerId") 

top_10 <- returner_stats %>%
  filter(rank <= 10) %>%
  mutate(headshot = case_when(
    player == "Jakeem Grant" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2577641.png&w=350&h=254",
    player == "Deonte Harris" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4411193.png&w=350&h=254",
    player == "Diontae Johnson" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3932905.png&w=350&h=254",
    player == "K.J. Hill" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3915522.png&w=350&h=254",
    player == "Dwayne Harris" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/14100.png&w=350&h=254",
    player == "Christian Kirk" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3895856.png&w=350&h=254",
    player == "River Cracraft" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3052056.png&w=350&h=254",
    player == "Kenjon Barner" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/15921.png&w=350&h=254",
    player == "Gunner Olszewski" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4424106.png&w=350&h=254",
    player == "CeeDee Lamb" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4241389.png&w=350&h=254"
  )) %>%
  select(rank, player, headshot, returns, optimal_return_rate, optimal_fc, optimal_bounce, optimal_rate)

top_10_gt <- top_10 %>%
  gt() %>%
  gt_img_rows(headshot) %>%
  gt_plt_bar_pct(column = optimal_return_rate, scaled = TRUE, fill = "darkgreen") %>%
  gt_plt_bar_pct(column = optimal_fc, scaled = TRUE, fill = "darkblue") %>%
  gt_plt_bar_pct(column = optimal_bounce, scaled = TRUE, fill = "purple") %>%
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  cols_label(rank = "Rank",
             player = "Player",
             headshot = "",
             optimal_return_rate = "Return Rate When Optimal",
             optimal_fc = "Fair Catch Rate When Optimal",
             optimal_bounce = "No Catch Rate When Optimal",
             optimal_rate = "Optimal Decision Rate") %>%
  tab_header(
    title = md("**Top 10 Punt Returners in Optimal Decision Rate**"),
    subtitle = "2018-2020 | Minimum of 25 punt returns in that time"
  )
gtsave(top_10_gt, "top_10_gt.png") 

bottom_10 <- returner_stats %>%
  filter(rank > 62) %>%
  mutate(headshot = case_when(
    player == "Tavon Austin" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/15786.png&w=350&h=254",
    player == "Chester Rogers" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2983209.png&w=350&h=254",
    player == "Dontrell Hilliard" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3126246.png&w=350&h=254",
    player == "Brandon Powell" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3115255.png&w=350&h=254",
    player == "Julian Edelman" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/12649.png&w=350&h=254",
    player == "Cooper Kupp" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2977187.png&w=350&h=254",
    player == "Donovan Peoples-Jones" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4258195.png&w=350&h=254",
    player == "Cole Beasley" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/15349.png&w=350&h=254",
    player == "Mohamed Sanu" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/14922.png&w=350&h=254",
    player == "Darrius Shepherd" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3120588.png&w=350&h=254"
  )) %>% 
  select(rank, player, headshot, returns, optimal_return_rate, optimal_fc, optimal_bounce, optimal_rate)

bottom_10_gt <- bottom_10 %>%
  gt() %>%
  gt_img_rows(headshot) %>%
  gt_plt_bar_pct(column = optimal_return_rate, scaled = TRUE, fill = "darkgreen") %>%
  gt_plt_bar_pct(column = optimal_fc, scaled = TRUE, fill = "darkblue") %>%
  gt_plt_bar_pct(column = optimal_bounce, scaled = TRUE, fill = "purple") %>%
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  cols_label(rank = "Rank",
             player = "Player",
             headshot = "",
             optimal_return_rate = "Return Rate When Optimal",
             optimal_fc = "Fair Catch Rate When Optimal",
             optimal_bounce = "No Catch Rate When Optimal",
             optimal_rate = "Optimal Decision Rate") %>%
  tab_header(
    title = md("**Bottom 10 Punt Returners in Optimal Decision Rate**"),
    subtitle = "2018-2020 | Minimum of 25 punt returns in that time"
  )
gtsave(bottom_10_gt, "bottom_10_gt.png")
