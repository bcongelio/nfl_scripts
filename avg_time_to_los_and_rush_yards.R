library(tidyverse)
library(nflreadr)
library(ggtext)
library(ggrepel)
library(extrafont)

adv_stats <- nflreadr::load_nextgen_stats(seasons = 2024, stat_type = "rushing")

rb_stats <- adv_stats |> 
  filter(week == 0 & player_position == "RB" & !is.na(avg_time_to_los) & !is.na(expected_rush_yards)) |> 
  group_by(player_short_name, team_abbr) |> 
  summarize(
    attempts = rush_attempts,
    avg_los = avg_time_to_los,
    avg_rush_yards = avg_rush_yards) |>  
  filter(attempts >= 50)

ggplot(rb_stats, aes(x = avg_los, y = avg_rush_yards)) +
  geom_mean_lines(aes(x0 = avg_los, y0 = avg_rush_yards),
                  color = "black", linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(color = team_abbr), size = 4.5) +
  scale_color_nfl() +
  geom_text_repel(aes(label = player_short_name),
                  box.padding = 0.45,
                  size = 4.5,
                  family = "Roboto Condensed",
                  fontface = "bold") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(title = "Avgerage Time to Line of Scrimmage<br>vs. Average Rushing Yards",
       subtitle = "Weeks 1-4  |  Minimum 50 Attempts",
       x = "Average Time to Line of Scrimmage (seconds)",
       y = "Average Rush Yards per Attempt",
       caption = "Data: nflreadr  |  Chart: @BradCongelio") +
  nfl_analytics_theme()

ggsave("los_yards.png", dpi = 400)
  