library(here)
library(tidyverse)

# load data
line <- read_lines(here("code/day15/test_input"))

# function to implement the HASH algorithm
hash_algorithm <- function(ascii_num, current_value) {
  return(((current_value + ascii_num) * 17) %% 256)
}

# part 1
ascii_code <- line |>
  str_split_1(",") |>
  map(utf8ToInt)

map_int(ascii_code, function(vec) {
  current_value <- 0
  for (val in vec) {
    current_value <- hash_algorithm(val, current_value)
    result <- current_value
  }
  return(result)
}) |>
  sum()
