library(here)
library(tidyverse)

# load and format data
df <- read_delim(here("code/day06/input"), delim = ":", col_names = c("metric", "values")) |>
  mutate(values = str_trim(values))


# part 1
df |>
  separate(values, into = c("race_1", "race_2", "race_3", "race_4"), convert = TRUE) |>
  pivot_longer(-metric, names_to = "race", values_to = "values") |>
  pivot_wider(names_from = "metric", values_from = "values") |>
  rowwise() |>
  mutate(wait_time = list(seq(Time))) |>
  unnest(wait_time) |>
  mutate(reachable_distance = (Time - wait_time) * wait_time) |>
  filter(reachable_distance > Distance) |>
  count(race) |>
  pull(n) |>
  prod()

# part 2 - brute force (will fail for larger 'Time' values)
df |>
  mutate(values = str_remove_all(values, " ")) |>
  mutate(values = as.numeric(values)) |>
  pivot_wider(names_from = "metric", values_from = "values") |>
  rowwise() |>
  mutate(wait_time = list(seq(Time))) |>
  unnest(wait_time) |>
  mutate(reachable_distance = (Time - wait_time) * wait_time) |>
  filter(reachable_distance > Distance) |>
  nrow()

# part 2 - algebraic solution
# the reachable distance is a quadratic function of the wait time:
# reachable_distance = (Time-wait_time) * wait_time => -wait_time**2 + Time*wait_time
# so we now need to solve the equation for wait_times where reachable_distance
# is greater than the record_distance

record_distance <- 242101716911252
Time <- 50748685

# applying p-q-formula to our equation (where x is the wait_time):
# 0 = x**2 - 50748685*x + 242101716911252
# leads to those solutions:

p <- Time
q <- record_distance

sol1 <- -p / 2 - sqrt((p / 2)**2 - q)
sol2 <- -p / 2 + sqrt((p / 2)**2 - q)

# since we know the parabola is opened downwards we need to take the delta of
# both solutions and need to subtract 1 since we do not want to be as good
# as the record but better
sol2 - sol1 - 1
