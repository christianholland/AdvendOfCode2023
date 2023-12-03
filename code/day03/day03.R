library(here)
library(tidyverse)

# helper functions to extract location of numbers and symbols
get_numbers_location <- function(line) {
  numbers <- str_extract_all(line, "\\d+")[[1]]
  locations <- as_tibble(str_locate_all(line, "\\d+")[[1]])

  locations |>
    mutate(number = as.integer(numbers)) |>
    select(number, start, end)
}

get_symbols_location <- function(line) {
  symbols <- str_extract_all(line, "[^0-9.]")[[1]]
  locations <- as_tibble(str_locate_all(line, "[^0-9.]")[[1]])

  locations |>
    mutate(symbol = symbols) |>
    select(symbol, position = start)
}

# data loading and formatting
df <- tibble(content = readLines(here("code/day03/input"))) |>
  rowid_to_column("line")

numbers <- df |>
  transmute(line, numbers = map(content, get_numbers_location)) |>
  unnest(numbers)

symbols <- df |>
  transmute(line, symbols = map(content, get_symbols_location)) |>
  unnest(symbols)

# part 1
# helper function to extract for each number the adjacent symbols
extract_adjacent_symbols <- function(current_line, start_pos, end_pos, ...) {
  symbols |>
    filter(
      # check if left of start or right of end positions are symbols
      line %in% seq(current_line - 1, current_line + 1) & position %in% seq(start_pos - 1, end_pos + 1)
    ) |>
    select(symbol_line = line, symbol, position)
}

part_numbers <- numbers |>
  mutate(adjacent_symbols = pmap(list(current_line = line, start_pos = start, end_pos = end), extract_adjacent_symbols)) |>
  unnest(adjacent_symbols)


part_numbers |>
  distinct(line, number, start, end) |>
  pull(number) |>
  sum()


# part 2
# get all star symbols that are adjacent to exact two part numbers
gears <- part_numbers |>
  filter(symbol == "*") |>
  count(symbol_line, symbol, position) |>
  filter(n == 2)

part_numbers |>
  inner_join(gears) |>
  select(number, symbol_line, position) |>
  arrange(symbol_line, position) |>
  group_by(symbol_line, position) |>
  mutate(key = row_number()) |>
  ungroup() |>
  pivot_wider(names_from = key, values_from = number) |>
  mutate(gear_ratio = `1` * `2`) |>
  pull(gear_ratio) |>
  sum()
