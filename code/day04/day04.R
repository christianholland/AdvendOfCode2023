library(here)
library(tidyverse)

# data loading and formatting
df <- tibble(lines = readLines(here("code/day04/input"))) |>
  separate(lines, into = c("card", "sets"), sep = ":") |>
  separate(sets, into = c("set1", "set2"), sep = "\\|") |>
  mutate(
    card = as.integer(parse_number(card)),
    set1 = str_trim(set1),
    set2 = str_trim(set2)
  ) |>
  pivot_longer(cols = -card, names_to = "set", values_to = "value") |>
  separate_rows(value, sep = " ") |>
  filter(value != "") |>
  mutate(value = as.integer(value)) |>
  group_by(card, set) |>
  summarise(values = list(value)) |>
  ungroup() |>
  pivot_wider(names_from = "set", values_from = "values") |>
  mutate(intersection = map2(set1, set2, intersect)) |>
  mutate(n_intersection = map(intersection, length)) |>
  unnest(n_intersection)

# part 1
df |>
  mutate(worth = case_when(
    n_intersection == 0 ~ 0,
    n_intersection > 0 ~ 2**(n_intersection - 1)
  )) |>
  pull(worth) |>
  sum()

# part 2
# extract list of cards that gets copied for each original card
card_copies <- df |>
  rowwise(card) |>
  summarise(new_card = case_when(
    n_intersection == 0 ~ list(as.integer(NULL)),
    n_intersection > 0 ~ list(seq(card + 1, card + n_intersection))
  )) |>
  ungroup()

# look up table to keep track of the number of copies per card
card_counter <- tibble(
  card = 1:nrow(df), copies = 1
)

# iterate over each card
for (i in 1:nrow(df)) {
  # for the current card, extract which cards are being copied and replicate
  # that vector as many times as we have the current card
  new_cards <- card_copies |>
    filter(card == i) |>
    unnest(new_card) |>
    mutate(rep = pull(card_counter[i, "copies"])) |>
    uncount(rep) |>
    count(card = new_card)

  # update the card counter based on the new cards
  card_counter <- card_counter |>
    left_join(new_cards, by = "card") |>
    replace_na(list(n = 0)) |>
    transmute(card, copies = copies + n)
}

card_counter |>
  pull(copies) |>
  sum()
