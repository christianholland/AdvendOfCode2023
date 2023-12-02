library(here)
library(tidyverse)

df <- tibble(line = readLines(here("code/day01/input")))


# part 1
df |>
  mutate(
    digit_1 = str_extract(line, "^[a-z]*(\\d{1})", group = 1),
    digit_2 = str_extract(line, "(\\d{1})[a-z]*$", group = 1)
  ) |>
  mutate(number = as.numeric(str_c(digit_1, digit_2))) |>
  pull(number) |>
  sum()


# part 2

# zero/null/0 does not appear in the data
digits <- tibble(
  number = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
  word = c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
)

matches <- map(df$line,\(l) str_match_all(l, "(?=(0|1|2|3|4|5|6|7|8|9|zero|one|two|three|four|five|six|seven|eight|nine))")[[1]][,2])

df |>
  mutate(digit_1 = matches,
         digit_2 = matches) |>
  pivot_longer(-line, names_to="key", values_to = "match") |>
  unnest(match) |>
  group_by(line) |>
  filter((row_number()==1 & key=="digit_1") | (row_number()==n() & key=="digit_2")) |>
  rename(word=match) |>
  left_join(digits, by="word") |>
  mutate(digit = coalesce(number, word)) |>
  select(-c(word, number)) |>
  pivot_wider(values_from="digit", names_from="key") |>
  mutate(number = as.numeric(str_c(digit_1, digit_2))) |>
  pull(number) |>
  sum()
