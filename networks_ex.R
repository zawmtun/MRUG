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

# Density


# Diameter


# Degree, betweenness, closeness centrality


# Node colours and size


# Edge colour and width


# Interactive network plot

## Isolate node and edge dataframes

net

