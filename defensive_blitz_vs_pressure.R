library(tidyverse)
library(rvest)
library(ggtext)
library(nflverse)
library(extrafont)

### providing URL for rvest
url <- "https://www.pro-football-reference.com/years/2024/opp.htm"

### scrape data with rvest and clean with janitor
def_data <- url |> 
  read_html() |> 
  html_node("#advanced_defense") |> 
  html_table() |> 
  janitor::clean_names() |> 
  mutate(bltz_percent = readr::parse_number(bltz_percent),
         hrry_percent = readr::parse_number(hrry_percent),
         qbkd_percent = readr::parse_number(qbkd_percent),
         prss_percent = readr::parse_number(prss_percent))

### load teams from nflreadr and then merge
teams <- nflreadr::load_teams() |> 
  select(team_name, team_abbr)

def_data <- def_data |> 
  left_join(teams, by = c("tm" = "team_name")) |> 
  select(tm, team_abbr, everything())

### plot as desired now
ggplot(def_data, aes(x = bltz_percent, y = prss_percent)) +
  geom_mean_lines(aes(x0 = bltz_percent, y0 = prss_percent), color = "black",
                      linewidth = 0.8, linetype = "dashed") +
  geom_nfl_logos(aes(team_abbr = team_abbr), width = 0.065) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::label_percent(scale = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::label_percent(scale = 1)) +
  labs(title = "The <span style = 'color:#69BE28;'>SEAHAWKS</span> Pressure the QB The Most<br>Despite One of the 
       Lowest Blitz Percentages in the NFL",
       subtitle = "*Weeks 1-4 of 2024 NFL Season*",
       caption = "Data: PFR | Chart: @BradCongelio",
       x = "Blitz Percentage", y = "Pressure Percent") +
  nfl_analytics_theme()

ggsave("defensive_pressure.png", dpi = 400)
