library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(visNetwork)

dat <- data.frame(
  var1 = c("A", "B", "B", "B", "C", "D", "E"),
  var2 = c("B", "C", "D", "E", "D", "F", "F")
)

net <- as_tbl_graph(dat, directed = FALSE)

# First plot
plot(net)

ggraph(net) +
  geom_edge_arc(strength = 0) +
  geom_node_point(size = 10, colour = "salmon") +
  geom_node_text(aes(label = name)) +
  theme_graph()

# Density
edge_density(net)


# Diameter
diameter(net)

# Degree, betweenness, closeness centrality
net1 <- net |> 
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(),
    closeness = centrality_closeness()
  )

net1

# Node colours and size
net2 <- net1 |> 
  mutate(
    group = if_else(name %in% c("A", "C", "D"),
                    "Group1",
                    "Group2")
  )

net2

ggraph(net2) +
  geom_edge_arc(strength = 0) +
  geom_node_point(aes(colour = group), size = 10, show.legend = FALSE) +
  geom_node_text(aes(label = name)) +
  theme_graph()


# Edge colour and width
net2
net3 <- net2 |> 
  activate(edges) |> 
  mutate(
    intensity = map_int(from, \(x) sample(1:30, size = 1, replace = TRUE))
  )

net3

ggraph(net3) +
  geom_edge_arc(aes(edge_width = intensity),
                strength = 0, colour = "grey60") +
  geom_node_point(aes(colour = group), size = 10, show.legend = FALSE) +
  geom_node_text(aes(label = name)) +
  theme_graph()

# Interactive network plot

## Isolate node and edge dataframes
net

nodes <- as_tibble(net) |> 
  mutate(id = row_number()) |> 
  rename(label = name)

edges <- net |> 
  activate(edges) |> 
  as_tibble()

visNetwork(nodes, edges) |> 
  visNodes(shape = "circle")



