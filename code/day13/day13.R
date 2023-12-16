library(here)
library(tidyverse)


# read data and convert it to a long format
df <- read_lines(here("code/day13/input")) |>
  str_replace_all(c(`#` = "1", `\\.` = "0")) |>
  enframe("row", "code") |>
  mutate(is_pattern = code != "") |>
  mutate(pattern = consecutive_id(is_pattern)) |>
  filter(is_pattern) |>
  select(row, code, pattern) |>
  group_by(pattern) |>
  mutate(row = 1:n()) |>
  separate_longer_position(code, 1) |>
  group_by(pattern, row) |>
  mutate(column = 1:n()) |>
  ungroup() |>
  select(row, column, code, pattern) |>
  mutate(code = as.integer(code))

# for each pattern get each row and column as a vector
df_rows <- df |>
  group_by(pattern, dim = row) |>
  summarise(vector = list(code)) |>
  ungroup()

df_cols <- df |>
  group_by(pattern, dim = column) |>
  summarise(vector = list(code)) |>
  ungroup()


# setup function to find pairs that need to be investigated for symetry
find_pairs <- function(current_id, max_id) {
  # define the window width to check
  width <- min(length(seq(current_id, 1)), length(seq(min(current_id + 1, max_id), max_id)))
  left_or_top <- seq(current_id, current_id - width + 1)
  right_or_botton <- seq(min(max_id, current_id + 1), min(max_id, current_id + width))

  return(list(left_or_top = left_or_top, right_or_bottom = right_or_botton))
}


symmetry_check <- function(zzz, left_or_top, right_or_bottom) {
  left_or_top_vectors <- zzz |>
    rowwise() |>
    filter(dim %in% left_or_top) |>
    arrange(-dim) |>
    pull(vector)

  right_or_bottom_vectors <- zzz |>
    rowwise() |>
    filter(dim %in% right_or_bottom) |>
    pull(vector)

  return(unlist(left_or_top_vectors) - unlist(right_or_bottom_vectors))
}


symmetry_mapping <- function(n, pattern, df, part = 1) {
  print(pattern)
  max_length <- floor(n / 2)
  for (i in seq(n - 1)) {
    pairs <- find_pairs(i, n)

    s <- symmetry_check(
      filter(df, pattern == {{ pattern }}),
      pairs$left_or_top, pairs$right_or_bottom
    )

    if (part == 1) {
      if (all(s == 0)) {
        return(i)
      }
    } else if (part == 2) {
      if (sum(abs(s)) == 1) {
        return(i)
      }
    }
  }
}


setup <- df |>
  group_by(pattern) |>
  slice_max(row) |>
  slice_max(column) |>
  ungroup() |>
  distinct(pattern, row, column)

# part 1
res <- setup |>
  rowwise() |>
  mutate(
    horizontal_reflection = map2(row, pattern, .f = symmetry_mapping, df = df_rows, part = 1),
    vertical_reflection = map2(column, pattern, .f = symmetry_mapping, df = df_cols, part = 1)
  ) |>
  unnest(c(horizontal_reflection, vertical_reflection))

sum(res$vertical_reflection, na.rm = TRUE) + 100 * sum(res$horizontal_reflection, na.rm = TRUE)


# part 2
res <- setup |>
  rowwise() |>
  mutate(
    horizontal_reflection = map2(row, pattern, .f = symmetry_mapping, df = df_rows, part = 2),
    vertical_reflection = map2(column, pattern, .f = symmetry_mapping, df = df_cols, part = 2)
  ) |>
  unnest(c(horizontal_reflection, vertical_reflection))

sum(res$vertical_reflection, na.rm = TRUE) + 100 * sum(res$horizontal_reflection, na.rm = TRUE)
