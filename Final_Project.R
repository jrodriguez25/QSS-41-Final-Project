library(igraph)
library(tidyverse)

nodes <- read_csv("SpotifyNetworkNodes.csv")
edges <- read_csv("SpotifyNetworkEdges.csv")


nodes$genres <- gsub("\\[", "", nodes$genres)
nodes$genres <- gsub("\\]", "", nodes$genres)

nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)
nodes$chart_hits <- gsub("\\]", "", nodes$chart_hits)



# Get the unique Spotify IDs from nodes
node_ids <- unique(nodes$spotify_id)

# Identify edges to remove (any edge where either id_0 or id_1 is not in node_ids)
edges_to_remove <- !(edges$id_0 %in% node_ids) | !(edges$id_1 %in% node_ids)

#remove ids not in "nodes"
cleaned_edges <- edges[!edges_to_remove, ]

network_graph <- graph_from_data_frame(d = cleaned_edges, vertices = nodes, directed = FALSE)


plot(network_graph)
#Next Steps

#Combine nodes and edges into one dataset
#clean chart hits
#filter to top 600-800
#filter to pop and rap 

#centrality tests
#structural similarity
#bridges #edge betweeness 




