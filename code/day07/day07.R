library(here)
library(tidyverse)

# load data
camel_cards <- read_delim(here("code/day07/input"), delim = " ", col_names = c("hand", "bid")) |>
  rowid_to_column("id")

# labels 
types <- ordered(fct_rev(as_factor(c("Five of a kind", "Four of a kind", "Full house", "Three of a kind", "Two pair", "One pair", "High card"))))

# helper function to convert a hand to its type
hand2type <- function(hand, labels) {
  freq <- map_int(labels, \(x) str_count(hand, as.character(x)))
  freq_code <- rev(sort(freq))[1:5] |>
    str_c(collapse = "") |>
    as.integer()

  if (freq_code >= 50000) {
    type <- types[1]
  } else if (freq_code >= 40000) {
    type <- types[2]
  } else if (freq_code >= 32000) {
    type <- types[3]
  } else if (freq_code >= 30000) {
    type <- types[4]
  } else if (freq_code >= 22000) {
    type <- types[5]
  } else if (freq_code >= 20000) {
    type <- types[6]
  } else if (freq_code >= 10000) {
    type <- types[7]
  }
  type
}

# part 1
labels <- ordered(fct_rev(as_factor(c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"))))
camel_cards |>
  mutate(type = map(hand, \(x) hand2type(x, labels))) |>
  unnest(type) |>
  separate_wider_position(hand, widths = c(card1 = 1, card2 = 1, card3 = 1, card4 = 1, card5 = 1)) |>
  mutate(across(where(is.character), \(x) ordered(x, levels = rev(labels)))) |>
  arrange(type, card1, card2, card3, card4, card5) |>
  mutate(rank = row_number()) |>
  mutate(prod = bid * rank) |>
  pull(prod) |>
  sum()

# part 2
# J is now a joker and is considered the weakest card
labels2 <- ordered(fct_rev(as_factor(c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J"))))

hand2type_with_joker <- function(hand) {
  if (str_detect(hand, "J")) {
    freq <- map_int(labels2, \(x) str_count(hand, as.character(x)))

    # replace J by the label that occurs the most often to make the hand the
    # strongest type possible. For ties take the strongest card
    replacement <- max(labels2[which(freq[-13] == max(freq[-13]))])
    hand <- str_replace_all(hand, "J", as.character(replacement))
  }

  tibble(modified_hand = hand, type = hand2type(hand, labels2))
}


camel_cards |>
  mutate(df = map(hand, hand2type_with_joker)) |>
  unnest(df) |>
  separate_wider_position(hand, widths = c(card1 = 1, card2 = 1, card3 = 1, card4 = 1, card5 = 1)) |>
  mutate(across(where(is_character), \(x) ordered(x, levels = rev(labels2)))) |>
  arrange(type, card1, card2, card3, card4, card5) |>
  mutate(rank = row_number()) |>
  mutate(prod = bid * rank) |>
  pull(prod) |>
  sum()
