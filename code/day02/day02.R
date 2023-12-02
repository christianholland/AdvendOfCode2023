library(here)
library(tidyverse)

# data loading and formatting
all_games <- tibble(line = readLines(here("code/day02/input"))) |>
  separate(line, into = c("game", "rest"), sep = ": ") |>
  separate(game, into = c("rest2", "game"), sep = " ") |>
  separate_rows(rest, sep = "; ") |>
  group_by(game) |>
  mutate(sample = row_number()) |>
  ungroup() |>
  separate_rows(rest, sep = ", ") |>
  separate(rest, into = c("n", "color"), sep = " ") |>
  mutate(n = as.integer(n), game = as.integer(game)) |>
  select(game, sample, color, n)

# part 1: the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes
impossible_games <- all_games |>
  pivot_wider(names_from = "color", values_from = "n", values_fill = 0) |>
  filter(red > 12 | green > 13 | blue > 14) |>
  distinct(game)

possible_games <- all_games |>
  distinct(game) |>
  anti_join(impossible_games, by = "game")

possible_games |>
  pull(game) |>
  sum()

# part 2
all_games |>
  group_by(game, color) |>
  slice_max(n) |>
  ungroup() |>
  distinct(game, color, n) |>
  pivot_wider(names_from = "color", values_from = "n", values_fill = 0) |>
  mutate(power = blue * green * red) |>
  pull(power) |>
  sum()
