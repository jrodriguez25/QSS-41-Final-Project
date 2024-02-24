


library(igraph)
library(tidyverse)

nodes <- read_csv("SpotifyNetworkNodes.csv")
edges <- read_csv("SpotifyNetworkEdges.csv")


#remove duplicates
nodes <- nodes %>% distinct(spotify_id, .keep_all = TRUE)

nodes$genres <- gsub("\\[", "", nodes$genres)
nodes$genres <- gsub("\\]", "", nodes$genres)

nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)
nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)

genres=nodes %>% 
  separate_rows(genres, sep = "'") %>% 
  count(genres)

# filter the genres -------------------------------------------------------
nodes_filtered <- nodes %>% 
  filter(popularity>70) %>% 
  filter(!str_detect(genres, "classic") & !str_detect(genres, "jazz"))




# Get the unique Spotify IDs from nodes
node_ids <- unique(nodes_filtered$spotify_id)

# Identify edges to remove (any edge where either id_0 or id_1 is not in node_ids)
edges_to_remove <- !(edges$id_0 %in% node_ids) | !(edges$id_1 %in% node_ids)

cleaned_edges <- edges[!edges_to_remove, ]


network_graph <- graph_from_data_frame(d = cleaned_edges, vertices = nodes_filtered, directed = FALSE)

network_simplified <- simplify(network_graph)

# gettign largest component -----------------------------------------------
decomp = components(network_simplified)

largest_comp = which.max(decomp$csize)
large_ids = V(network_simplified)[decomp$membership == largest_comp]
giant_comp = induced_subgraph(network_simplified, large_ids)

plot(giant_comp)
