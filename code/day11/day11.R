library(here)
library(tidyverse)

m <- read_table(here("code/day11/input"), col_names = FALSE) |>
  rowid_to_column("col") |>
  separate_longer_position(X1, 1) |>
  group_by(col) |>
  mutate(row = row_number()) |>
  ungroup() |>
  transmute(row, col, value = if_else(X1 == "#", 1, 0)) |>
  pivot_wider(names_from = "row", values_from = "value") |>
  column_to_rownames("col") |>
  as.matrix()


galaxies <- which(m == 1, arr.ind = TRUE) |>
  as_tibble() |>
  transmute(node = str_c(row, col, sep = "|")) |>
  pull()

new_rows <- which(rowSums(m) == 0)
new_cols <- which(colSums(m) == 0)

get_distances <- function(duplicates) {
  expand_grid(from = galaxies, to = z) |>
    mutate(dist = map2(from, to, .f = function(from, to) {
      from_int <- parse_number(str_split_1(from, "\\|"))
      to_int <- parse_number(str_split_1(to, "\\|"))

      start_row <- from_int[1]
      start_col <- from_int[2]
      end_row <- to_int[1]
      end_col <- to_int[2]

      delta_row <- abs(end_row - start_row)
      delta_col <- abs(end_col - start_col)

      row_steps <- 0
      col_steps <- 0
      if (start_row != end_row) {
        row_steps <- seq(min(start_row, end_row) + 1, max(start_row, end_row))
      }
      if (start_col != end_col) {
        col_steps <- seq(min(start_col, end_col) + 1, max(start_col, end_col))
      }

      r <- duplicates - 1
      delta_row_expanded <- r * length(intersect(new_rows, row_steps)) + delta_row
      delta_col_expanded <- r * length(intersect(new_cols, col_steps)) + delta_col

      delta_row_expanded + delta_col_expanded
    })) |>
    unnest(dist) |>
    summarise(sum(dist) / 2)
}


# algorithm is correct but pretty poor performance,
# vector based implementation would be definitely more efficient
# part 1
get_distances(2)

# part 2
get_distances(1000000)
