library(here)
library(tidyverse)
library(numbers)
library(tidygraph)
library(ggraph)

# load data
instructions <- read_lines(here("code/day08/input"), n_max = 1) |>
  str_split_1(pattern = "")

network <- read_delim(here("code/day08/input"),
  delim = " ", skip = 1,
  col_names = c("node", "tmp", "L", "R")
) |>
  transmute(node, L = str_extract(L, "[A-Z0-9]{3}"), R = str_extract(R, "[A-Z0-9]{3}")) |>
  arrange(node)

# part 1
n_steps <- 0
from <- "AAA"
destination <- "ZZZ"
network_df <- network |>
  column_to_rownames("node")

while (TRUE) {
  for (i in instructions) {
    to <- network_df[from, i]
    n_steps <- n_steps + 1
    stopifnot("Reached destination" = to != destination)
    from <- to
  }
}
n_steps

# part 2
# brute force
network_df <- network |>
  column_to_rownames("node")

start_nodes <- network_df |>
  rownames_to_column("node") |>
  filter(str_detect(node, "A$")) |>
  pull(node)

end_nodes <- network_df |>
  rownames_to_column("node") |>
  filter(str_detect(node, "Z$")) |>
  pull(node)

steps_to_destination <- map(start_nodes, function(start_node) {
  n_step <- 0
  steps_to_destination <- c()
  print(start_node)
  from <- start_node
  for (i in rep(instructions, 300)) {
    to <- network_df[from, i]
    n_step <- n_step + 1
    if (to %in% end_nodes) {
      print(str_glue("Reached possible end node at step {n_step}"))
      steps_to_destination <- append(steps_to_destination, n_step)
    }
    from <- to
  }
  return(steps_to_destination)
})

# Inf -> never at an end node at the same time
min(reduce(steps_to_destination, intersect))

# looking at the numbers reveals that the end nodes are reached periodically,
# e.g. the first start node reaches its end node every 21797 steps
steps_to_destination |>
  enframe(name = "network", "value") |>
  unnest(value) |>
  mutate(network = as_factor(network)) |>
  ggplot(aes(x = value, y = network, color = network)) +
  geom_point()

# therefore we need to find the least common multiple of all cycle lengths
cycle_lengths <- steps_to_destination |> map_int(1)

reduce(cycle_lengths, numbers::LCM) |>
  # transform to character to disable the scientific notation 2.397753e+13
  as.character()

# for fun I display the network
g <- network |>
  pivot_longer(cols = c(L, R), names_to = "direction", values_to = "to") |>
  select(from = node, to, direction) |>
  as_tbl_graph() |>
  mutate(type = case_when(
    str_detect(name, "A$") ~ "start",
    str_detect(name, "Z$") ~ "destination",
    TRUE ~ NA_character_
  ))

# turns out each start node can reach only one end node and therefore we see
# various independent networks
g |>
  ggraph(layout = "kk") +
  geom_edge_fan() +
  geom_node_point(aes(color = type))
