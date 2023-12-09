library(here)
library(tidyverse)

# load data
lines <- read_lines(here("code/day09/input"))

# helper function to recursively compute the difference of a sequence until all
# elements are 0
compute_diff <- function(seq) {
  diff_seq <- diff(seq)
  if (all(diff_seq == 0)) {
    return(append(list(seq), list(diff_seq)))
  } else {
    return(append(list(seq), list(compute_diff(diff_seq))[[1]]))
  }
}

# helper function to extrapolate forward or backward
extrapolate_forward <- function(l1, l2) {
  tail(l1, 1) + tail(l2, 1)
}

extrapolate_backwards <- function(l1, l2) {
  head(l2, 1) - head(l1, 1)
}


# part 1
list_of_diffs <- map(lines, function(line) {
  seq <- str_split_1(line, " ") |> map_int(as.numeric)
  compute_diff(seq)
})


# part 1
map_int(list_of_diffs, function(x) reduce(rev(x), extrapolate_forward)) |>
  sum()


# part 2
map_int(list_of_diffs, function(x) reduce(rev(x), extrapolate_backwards)) |>
  sum()
