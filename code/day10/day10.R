library(here)
library(tidyverse)

# | connecting north and south
# - east and west
# L north and east
# J north and west
# 7 south and west
# F south and east

north = c("|", "7", "F")
east = c("-", "J", "7") 
south = c("|", "L", "J")
west = c("-", "L", "F")

x = list()

x[["S"]] = list(north=north, east=east, south=south, west=west)
x[["|"]] = list(north=north, east=NULL, south=south, west=NULL)
x[["-"]] = list(north=NULL, east=east, south=NULL, west=west)
x[["L"]] = list(north=north, east=east, south=NULL, west=NULL)
x[["J"]] = list(north=north, east=NULL, south=NULL, west=west)
x[["7"]] = list(north=NULL, east=NULL, south=south, west=west)
x[["F"]] = list(north=NULL, east=east, south=south, west=NULL)

df = x |> 
  enframe("symbol", "position") |> 
  mutate(position = map(position, enframe, "position")) |> 
  unnest(position) |> 
  unnest(value) |> 
  rename(allowed_symbol = value)


lines <- read_delim(here("code/day10/input"), col_names = c("symbol"), delim = " ")

net = lines |>
  rownames_to_column("row") |>
  mutate(row = as.integer(row)) |> 
  separate_longer_position(symbol, 1) |>
  group_by(row) |>
  mutate(col = row_number()) |>
  ungroup() |>
  select(row, col, symbol)


# get symbols connected to S
get_neighbors_recursive <- function(cur_symbol, cur_row, cur_col, prev_row, prev_col, path=list()) {
  path = str_glue("{cur_symbol} ({cur_row},{cur_col})")
  # print(str_glue("{cur_symbol} ({cur_row},{cur_col})"))
  # get neighbor symbols
  neighbors = net |>
    filter(
      row == cur_row - 1 & col == cur_col | # north
        row == cur_row & col == cur_col + 1 | # east
        row == cur_row + 1 & col == cur_col | # south
        row == cur_row & col == cur_col - 1 # west
    ) |> 
    arrange(row, col) |> # order: N, W, O, S 
    mutate(position = case_when(
      row == cur_row - 1 & col == cur_col ~ "north",
        row == cur_row & col == cur_col + 1 ~ "east",
        row == cur_row + 1 & col == cur_col ~ "south",
        row == cur_row & col == cur_col - 1 ~ "west"
    )) |> 
    filter(!(col == prev_col & row == prev_row))
  
  # print(str_glue("All neighbors of: {cur_symbol} ({cur_row},{cur_col})"))
  # print(neighbors)
  
  # based on current symbol filter for valid neighbor symbols
  valid_neighbors = neighbors |> 
    semi_join(filter(df, symbol == cur_symbol), join_by(position, symbol == allowed_symbol))
  
  if ("S" %in%  neighbors$symbol & nrow(valid_neighbors) == 0) {
    print(str_glue("Done {path}"))
    return(path)
  } else {
    print(str_glue("Valid neighbors of: {cur_symbol} ({cur_row},{cur_col})"))
    print(valid_neighbors)
    print("=====================")
    for (row in 1:nrow(valid_neighbors)) {
      next_symbol = valid_neighbors[row, "symbol", drop = T]
      next_row = valid_neighbors[row, "row", drop = T]
      next_col = valid_neighbors[row, "col", drop = T]

      print(str_glue("{cur_symbol} ({cur_row},{cur_col}) --> {next_symbol} ({next_row},{next_col})"))

      return(append(path, get_neighbors_recursive(
        next_symbol,
        next_row,
        next_col,
        cur_row,
        cur_col
      )))
    }
  }
}


s_row = filter(net, symbol == "S")
checkpoints = get_neighbors("S", s_row$row, s_row$col, 0, 0) # Error: C stack usage  7971264 is too close to the limit

