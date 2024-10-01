library(tidyverse)
library(ggtext)
library(nflverse)
library(gt)
library(gtExtras)

options(digits = 3)

pbp <- nflreadr::load_pbp(2024)

pass_oe <- pbp |> 
  filter(xpass >= .70) |> 
  filter(!is.na(epa)) |> 
  filter(complete_pass == 1 | incomplete_pass == 1 | interception == 1, !is.na(down)) |> 
  group_by(passer, passer_id, posteam) |> 
  summarize(
    total = n(),
    mean_epa = mean(epa, na.rm = TRUE)) |> 
  filter(total >= 55)

rosters <- nflreadr::load_rosters(2024) |> 
  select(gsis_id, headshot_url)

pass_oe <- pass_oe |> 
  left_join(rosters, by = c("passer_id" = "gsis_id"))

fields_is_great <- pass_oe |> 
  ungroup() |> 
  arrange(-mean_epa) |>
  gt(rowname_col = "passer") |> 
  cols_hide(passer_id) |>
  cols_hide(posteam) |> 
  tab_header(
    title = md("**Average EPA in Pure Passing Situations**"),
    subtitle = md("*Pure Passing = Expected Pass >= 70%*")) |> 
  tab_stubhead(label = "Quarterback") |> 
  gt_img_rows(headshot_url, height = 25) |> 
  cols_label(
    passer = "Quarterback",
    headshot_url = "",
    total = "Attempts",
    mean_epa = "Mean EPA") |> 
  gt::cols_move_to_start(headshot_url) |>
  cols_align(align = "center", columns = c("passer", "total")) |> 
  gt_color_box(mean_epa, domain = -0.33:0.900,
               palette = "ggsci::blue_material", accuracy = 0.001,
               na_color = "steelblue") |> 
  tab_source_note(
    source_note = md("*Data: nflreadR | Table: @BradCongelio*")) |> 
  gtExtras::gt_theme_538()

gtsave(fields_is_great, "fields_is_great.png")

